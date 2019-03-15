;;; -*- lexical-binding: t; -*-

;; checkdoc tests
(require 'subr-x)
(require 'checkdoc)
(let ((guess-checkdoc-error-buffer-name "*Warnings*")
      (sentence-end-double-space nil))
  ;; This buffer name is hard-coded in checkdoc and it may change
  (ignore-errors
    (kill-buffer guess-checkdoc-error-buffer-name))
  (mapc (lambda (f)
          (checkdoc-file f))
        (file-expand-wildcards "*.el"))
  (when-let ((buf (get-buffer guess-checkdoc-error-buffer-name)))
    (with-current-buffer buf
      (unless (= 0 (buffer-size))
        (kill-emacs 1)))))


;; ;; package lint
;; (require 'package-lint)
;; (let ((success t))
;;   (dolist (file (file-expand-wildcards "*.el"))
;;     (with-temp-buffer
;;       (insert-file-contents file t)
;;       (emacs-lisp-mode)
;;       (let ((checking-result (package-lint-buffer)))
;;         (when checking-result
;;           (setq success nil)
;;           (message "In `%s':" file)
;;           (pcase-dolist (`(,line ,col ,type ,message) checking-result)
;;             (message "  at %d:%d: %s: %s" line col type message))))))
;;   (kill-emacs (if success 0 1)))

;; elsa
;; (require 'elsa)
;; (defun my-elsa-run ()
;;   "Run `elsa-process-file' and output errors to stdout for flycheck."
;;   (elsa-load-config)
;;   (dolist (file (file-expand-wildcards "*.el"))
;;     (--each (reverse (oref (elsa-process-file file) errors))
;;       (princ (concat file ":" (elsa-message-format it))))))
;; (my-elsa-run)





