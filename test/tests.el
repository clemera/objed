(require 'ert)
(require 'cl-lib)
(require 'objed)
;; activate on load
(objed-mode 1)

;; defuns adapted from  lispy-test.el

(defun objed--call-object-interactively (o &optional cmd)
  (let* ((cmd (or cmd (objed--name2func o)))
         (real-this-command cmd)
         (inhibit-message t))
    (setq this-command cmd)
    ;; pre command hook...
    (funcall #'objed--push-state)
    (objed--keep-transient-p)
    (call-interactively cmd)
    ;; post command hook
    (when (memq 'objed--reinit-object-one-time
                post-command-hook)
      (objed--reinit-object-one-time))
    (setq last-command cmd)))

(defun objed-decode-keysequence (str)
  "Decode STR from e.g. \"23ab5c\" to '(23 \"a\" \"b\" 5 \"c\")"
  (let ((table (copy-sequence (syntax-table))))
    (cl-loop for i from ?0 to ?9 do
             (modify-syntax-entry i "." table))
    (cl-loop for i from ? to ? do
             (modify-syntax-entry i "w" table))
    (cl-loop for i in '(? ?\( ?\) ?\[ ?\] ?{ ?} ?\" ?\' ?\ )
             do (modify-syntax-entry i "w" table))
    (cl-mapcan (lambda (x)
                 (let ((y (ignore-errors (read x))))
                   (if (numberp y)
                       (list y)
                     (mapcar #'string x))))
               (with-syntax-table table
                 (split-string str "\\b" t)))))

(defun objed-unalias (seq)
  "Emulate pressing keys decoded from SEQ."
  (if (vectorp seq)
      (objed--unalias-key seq)
    (let ((lkeys (objed-decode-keysequence seq))
          (map nil)
          (key nil))
      (while (setq key (pop lkeys))
        (if (numberp key)
            (let ((current-prefix-arg (list key)))
              (when lkeys
                (objed--unalias-key (pop lkeys))))
          (let ((cmd (key-binding key)))
            (if (keymapp cmd)
                (setq map cmd)
              (objed--unalias-key key map)
              (setq map nil))))))))

(defun objed--unalias-key (key &optional map)
    "Call command that corresponds to KEY.
Insert KEY if there's no command."
    (let ((cmd (if map (lookup-key map key)
                 (key-binding key))))
      (if (not cmd)
          (insert key)
        (setq last-command-event (aref key 0))
        (objed--call-object-interactively nil cmd))))

;;
;; (ert-deftest objed-decode-keysequence ()
;;   (should (equal (objed-decode-keysequence "23ab50c")
;;                  '(23 "a" "b" 50 "c")))
;;   (should (equal (objed-decode-keysequence "3\C-d")
;;                  '(3 "")))
;;   (should (equal (objed-decode-keysequence "3\C-?")
;;                  '(3 ""))))

(defmacro objed-with (in body &optional object)
  (let ((init (if object `(objed--init ',object)
                '(objed--init 'char))))
  `(let ((temp-buffer (generate-new-buffer " *temp*")))
     (save-window-excursion
       (unwind-protect
           (progn
             (switch-to-buffer temp-buffer)
             (emacs-lisp-mode)
             (transient-mark-mode 1)
             (insert ,in)
             (goto-char (point-min))
             (when (search-forward "~" nil t)
               (backward-delete-char 1)
               (set-mark (point)))
             (goto-char (point-max))
             (search-backward "|")
             (delete-char 1)
             ,init
             (setq current-prefix-arg nil)
             ;; execute command
             ,@(mapcar (lambda (x)
                         (cond ((equal x '(kbd "C-u"))
                                `(setq current-prefix-arg (list 4)))
                               ((or (stringp x)
                                    (and (listp x)
                                         (eq (car x) 'kbd)))
                                `(objed-unalias ,x))
                               (t x)))
                       (list body))
             (let ((npos (point-marker)))
               (when objed--current-obj
                 (goto-char (objed--end))
                 (insert ">")
                 (goto-char (objed--beg))
                 (insert "<"))
               (goto-char npos)
               (skip-chars-backward "<" (1- (point)))
               (skip-chars-forward ">" (1+ (point)))
               (insert "|")
               (when (region-active-p)
                 (exchange-point-and-mark)
                 (insert "~")))
             (buffer-substring-no-properties
              (point-min)
              (point-max)))
         (objed--exit-objed)
         ;; reset for next test
         (setq last-command nil)
         (setq objed--last-states nil)
         (and (buffer-name temp-buffer)
              (kill-buffer temp-buffer)))))))

;; needs to come first initializes, for tests, too
(ert-deftest objed-activate ()
  (should (string= (objed-with "Testing line he|re" "\C-a")
                   "|<Testing line here>"))
  (should (string= (objed-with "Testing line he|re" (kbd "M-b"))
                   "Testing line |<here>"))
  (should (string= (objed-with "Testing line he|re" (kbd "M-f"))
                   "Testing line <here>|"))
  (should (string= (objed-with "Testing line he|re\nFollowing line here" "\C-n")
                   "Testing line here\n<Following line |here>"))
  (should (string= (objed-with "Testing line he|re" (objed--call-object-interactively 'line))
                   "|<Testing line here>"))
  (should (string= (objed-with "
(defun objed--save-start-position (&rest _)
  \"Save position of| point via `objed--opoint'.\"
  (setq objed--opoint (point)))
"  (kbd "C-M-a"))
  "<
|(defun objed--save-start-position (&rest _)
  \"Save position of point via `objed--opoint'.\"
  (setq objed--opoint (point)))
>")))


(ert-deftest objed-basic-movement ()
  (should (string= (objed-with "Testing line he|re" "rr")
                   "Testing |<line> here"))
  (should (string= (objed-with "Testing line he|re" "2r")
                   "Testing |<line> here"))
  (should (string= (objed-with "Tes|ting line here" "ss")
                   "Testing <line>| here"))
  (should (string= (objed-with "Tes|ting line here" "2s")
                   "Testing <line>| here"))
  (should (string= (objed-with "Tes|ting line" "bb")
                   "T|<e>sting line"))
  (should (string= (objed-with "Tes|ting line" "2b")
                   "T|<e>sting line"))
  (should (string= (objed-with "Tes|ting line" "fff")
                   "Testin|<g> line"))
  (should (string= (objed-with "Tes|ting line" "3f")
                   "Testin|<g> line"))
  (should (string= (objed-with ";; this is the previous line\n;; this is| the current line" "p")
                   "<;; this is| the previous line\n>;; this is the current line"))
  (should (string= (objed-with ";; this is a| test\n;; this is the next line" "n")
                   ";; this is a test\n<;; this is t|he next line>"))
  (should (string= (objed-with "Testing |line here\nFollowing line here" "npsrfb")
                   "Testing |<l>ine here\nFollowing line here"))
  (should (string= (objed-with "Testing |line here\nFollowing line here" "e")
                   "Testing <line here>|\nFollowing line here"))
  (should (string= (objed-with "Testing |line here\nFollowing line here" "a")
                   "|<Testing >line here\nFollowing line here"))
  (should (string= (objed-with "Testing |line here\nFollowing line here" "ae")
                   "<Testing line here>|\nFollowing line here")))

(ert-deftest objed-pop-state ()
  (should (string= (objed-with "Testing line he|re" "rr")
                   (objed-with "Testing line he|re" "rrr,")))
  (should (string= (objed-with "Testing |line here\nFollowing line here" "n")
                   (objed-with "Testing |line here\nFollowing line here" "npsfsb,,,,,"))))



(ert-deftest objed-choose-and-navigate-defun ()
  (should (string= (objed-with "
\(defun objed--save-start-position (&rest _)
  \"Save position of point via `objed--opoint'.\"
  (setq objed--opoint (point)))

\(defun objed--goto-start (&optional _)
  \"Goto start |of current object if there is one.\"
  (when objed--current-obj
    (goto-char (objed--beg))))

\(defun objed--object-trailing-line (pos)
  \"Activate trailing part from POS.\"
  (unless (eq objed--obj-state 'inner)
    (objed--reverse))
  (objed--change-to :beg pos :ibeg pos))"
        "cdthh")
"
\(defun objed--save-start-position (&rest _)
  \"Save position of point via `objed--opoint'.\"
  (setq objed--opoint (point)))

\(defun objed--goto-start (&optional _)
  \"Goto start of current object if there is one.\"
  (when objed--current-obj
    (goto-char (objed--beg))))
<
|(defun objed--object-trailing-line (pos)
  \"Activate trailing part from POS.\"
  (unless (eq objed--obj-state 'inner)
    (objed--reverse))
  (objed--change-to :beg pos :ibeg pos))>")))

(ert-deftest objed-beg-of-block-expansion ()

  (let ((string "
\(defun check ()
  (ignore))

;; More on same level

;; Some text
;; and |more text"))
    (should (string= (objed-with string "a")
                     "
\(defun check ()
  (ignore))

;; More on same level

;; Some text
|<;; and >more text"))
    (should (string= (objed-with string "aa")
                     "
\(defun check ()
  (ignore))

;; More on same level

|<;; Some text
;; and >more text"))
    (should (string= (objed-with string "aaa")
                     "
\(defun check ()
  (ignore))

;; More on same level
|<
;; Some text
;; and >more text"))
    (should (string= (objed-with string "aaaa")
                     "
\(defun check ()
  (ignore))

|<;; More on same level

;; Some text
;; and >more text"))
     (should (string= (objed-with string "aaaaa")
                     "|<
\(defun check ()
  (ignore))

;; More on same level

;; Some text
;; and >more text"))))

(ert-deftest objed-end-of-block-expansion ()
  (let ((string "
;; Some |text
;; and more text
;;
;;

;; Comment end

\(defun check ()
  (ignore))

;; buffer end"))
    (should (string= (objed-with string "e")
                     "
;; Some <text>|
;; and more text
;;
;;

;; Comment end

\(defun check ()
  (ignore))

;; buffer end"))
    (should (string= (objed-with string "ee")
                     "
;; Some <text
;; and more text>|
;;
;;

;; Comment end

\(defun check ()
  (ignore))

;; buffer end"))
    (should (string= (objed-with string "eee")
                     "
;; Some <text
;; and more text
;;
;;>|

;; Comment end

\(defun check ()
  (ignore))

;; buffer end"))
        (should (string= (objed-with string "eeee")
                     "
;; Some <text
;; and more text
;;
;;

;; Comment end>|

\(defun check ()
  (ignore))

;; buffer end"))
        (should (string= (objed-with string "eeeee")
                     "
;; Some <text
;; and more text
;;
;;

;; Comment end

\(defun check ()>|
  (ignore))

;; buffer end"))
        (should (string= (objed-with string "eeeeee")
                     "
;; Some <text
;; and more text
;;
;;

;; Comment end

\(defun check ()
  (ignore))

;; buffer end>|"))))


(ert-deftest objed-block-expansion ()
  (let ((string "
\(defun check ()
  (ignore))

;; More on same level

;; Some text
;; and |more text"))
    (should (string= (objed-with string "l")
                     "
\(defun check ()
  (ignore))

;; More on same level

;; Some text
|<;; and more text>"))
    (should (string= (objed-with string "ll")
                     "
\(defun check ()
  (ignore))

;; More on same level

|<;; Some text
;; and more text>"))
    (should (string= (objed-with string "lll")
                     "
\(defun check ()
  (ignore))

;; More on same level
|<
;; Some text
;; and more text>"))
      (should (string= (objed-with string "llll")
                     "
\(defun check ()
  (ignore))

|<;; More on same level

;; Some text
;; and more text>"))
      (should (string= (objed-with string "lllll")
                       "|<
\(defun check ()
  (ignore))

;; More on same level

;; Some text
;; and more text>"))

))

(ert-deftest objed-context-expansion ()
  (let ((str "
\(defun testing ()
  (let ((a nil))
    (message \"this is| a test\")))
"))
    (should (string= (objed-with str "o")
                     "
\(defun testing ()
  (let ((a nil))
    (message \"|<this is a test>\")))
"))
    (should (string= (objed-with str "oo")
                     "
\(defun testing ()
  (let ((a nil))
    (message |<\"this is a test\">)))
"))
    (should (string= (objed-with str "ooo")
                     "
\(defun testing ()
  (let ((a nil))
    |<(message \"this is a test\")>))
"))
    (should (string= (objed-with str "oooo")
                     "
\(defun testing ()
  |<(let ((a nil))
    (message \"this is a test\"))>)
"))
    (should (string= (objed-with str "ooooo")
                     "
|<\(defun testing ()
  (let ((a nil))
    (message \"this is a test\")))>
"))))

;; TODO: op tests, marking, remaining commands


(provide 'tests)
