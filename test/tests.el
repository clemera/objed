(require 'ert)
(require 'objed)
(require 'objed-objects)
(require 'cl-lib)


;; from `lispy-with'


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
          key)
      (while (setq key (pop lkeys))
        (if (numberp key)
            (let ((current-prefix-arg (list key)))
              (when lkeys
                (objed--unalias-key (pop lkeys))))
          (objed--unalias-key key))))))

(defun objed--unalias-key (key)
    "Call command that corresponds to KEY.
Insert KEY if there's no command."
    (let ((cmd (key-binding key)))
      (if (not cmd)
          (insert key)
        (setq last-command-event (aref key 0))
        (call-interactively cmd)
        (setq last-command cmd))))


(ert-deftest objed-decode-keysequence ()
  (should (equal (objed-decode-keysequence "23ab50c")
                 '(23 "a" "b" 50 "c")))
  (should (equal (objed-decode-keysequence "3\C-d")
                 '(3 "")))
  (should (equal (objed-decode-keysequence "3\C-?")
                 '(3 ""))))

;; adapated from `lispy-with'
(defmacro objed-with (in &rest body)
  `(let ((temp-buffer (generate-new-buffer " *temp*")))
     (save-window-excursion
       (unwind-protect
           (progn
             (switch-to-buffer temp-buffer)
             (emacs-lisp-mode)
             (transient-mark-mode 1)
             ;; activate objed
             (objed-mode 1)
             (set-transient-map objed-map #'objed--keep-transient-p)
             (insert ,in)
             (goto-char (point-min))
             (when (search-forward "~" nil t)
               (backward-delete-char 1)
               (set-mark (point)))
             (goto-char (point-max))
             (search-backward "|")
             (delete-char 1)
             (objed--init 'char)
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
                       body)
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
         (and (buffer-name temp-buffer)
              (kill-buffer temp-buffer))))))


(ert-deftest objed-next-line ()
  (should (string= (objed-with ";; this is a| test\n;; this is the next line" "n")
                   ";; this is a test\n<;; this is t|he next line>")))

(ert-deftest objed-previous-line ()
  (should (string= (objed-with ";; this is the previous line\n;; this is| the current line" "p")
                   "<;; this is| the previous line\n>;; this is the current line")))

(ert-deftest objed-forward-char ()
  (should (string= (objed-with "Tes|ting line" "fff")
                   "Testin|<g> line")))

(ert-deftest objed-backward-char ()
  (should (string= (objed-with "Tes|ting line" "bb")
                   "T|<e>sting line")))

(ert-deftest objed-forward-word ()
  (should (string= (objed-with "Tes|ting line here" "ss")
                   "Testing <line>| here")))


(ert-deftest objed-backward-word ()
  (should (string= (objed-with "Testing line he|re" "rr")
                   "Testing |<line> here")))

(ert-deftest objed-basic-movment ()
  (should (string= (objed-with "Testing |line here\nFollowing line here" "npsrfb")
                   "Testing |<l>ine here\nFollowing line here")))


(provide 'tests)
