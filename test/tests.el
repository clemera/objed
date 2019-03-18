(require 'ert)
(require 'cl-lib)
(require 'objed)

;; TO ADD NEW TESTS CREATE A DIR IN TESTS AND ADD TESTS FILES IN IT.
(defun objed-run-tests ()
  (interactive)
  (let ((buf (or (get-buffer "objed/tests.el")
                 (find-file-noselect "~/objed/test/tests.el"))))
    (with-current-buffer buf
      ;; create tests...
      (eval-buffer)
      (ert t))))

(declare-function objed--exit-objed "objed")
;; activate on load
(objed-mode 1)

;;* Simulate interactions
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

(defmacro objed-with (in body &optional mode)
  (let ((init '(objed--init 'char))
        (body (if (stringp body) `(kbd ,body) body)))
    `(let ((temp-buffer (generate-new-buffer " *temp*")))
       (save-window-excursion
         (unwind-protect
             (progn
               (switch-to-buffer temp-buffer)
               (or (and ',mode (funcall ',mode))
                   (emacs-lisp-mode))
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

;;* Test creation
(eval-and-compile
  (defun objed-parse-test (file)
    (let ((parsed nil))
      (with-temp-buffer
        (insert-file-contents file)
        (delete-trailing-whitespace)
        (goto-char (point-min))
        (push (buffer-substring (line-beginning-position) (line-end-position))
              parsed)
        (search-forward ";;;;" nil t)
        (forward-line 1)
        (push (buffer-substring (point) (progn (search-forward ";;;;" nil t)
                                               (forward-char -4)
                                               (point)))
              parsed)
        (search-forward ";;;;" nil t)
        (forward-line 1)
        (push (buffer-substring (point)
                                (point-max))
              parsed))
      (nreverse parsed))))


(defun objed-equal (str1 str2)
  (let* ((str1 (replace-regexp-in-string "\\( \\|\n\\|\t\\)+\\'" "" str1))
         (str1 (replace-regexp-in-string "\\`\\( \\|\n\\|\t\\)+" "" str1))
         (str2 (replace-regexp-in-string "\\( \\|\n\\|\t\\)+\\'" "" str2))
         (str2 (replace-regexp-in-string "\\`\\( \\|\n\\|\t\\)+" "" str2)))
    (string= str1 str2)))


(defmacro objed-create-test (file mode)
  (let* ((parsed (objed-parse-test file))
         (key (nth 0 parsed))
         (str1 (nth 1 parsed))
         (str2 (nth 2 parsed)))
    `(should (objed-equal (objed-with ,str1 ,key ,mode)
                          (prog1 ,str2
                            ;; show path of test in compile output
                            ,file)))))

(defmacro objed-create-tests-for (dir mode)
  (let ((files (directory-files
                (expand-file-name
                 (format "tests/%s/%s" (symbol-name mode) dir)
                 (file-name-directory
                  (or load-file-name default-directory))) t "^[^.]"))
        (body nil))
    (push 'ert-deftest body)
    (push (intern (format "objed-%s" dir)) body)
    (push nil body)
    (dolist (file files)
      (push `(objed-create-test ,file ,mode) body))
    (nreverse body)))


;; (should (string= (objed-with "Testing line he|re"
;;                              (objed--call-object-interactively 'line))
;;                  "|<Testing line here>"))

(defmacro objed-create-tests (mode)
  (let ((dirs (directory-files
               (expand-file-name
                (format "tests/%s" (symbol-name mode))
                (file-name-directory
                 (or load-file-name default-directory))) t "^[^.]"))
        (body nil))
    (push 'progn body)
    (dolist (dir dirs)
      (push `(objed-create-tests-for ,(file-name-nondirectory dir) ,mode)
            body))
    (nreverse body)))


(objed-create-tests emacs-lisp-mode)
;; TODO: op tests, marking, remaining commands, other modes

(provide 'tests)


