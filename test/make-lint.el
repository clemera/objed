;;; -*- lexical-binding: t; -*-

;; checkdoc tests
(require 'checkdoc)
(let ((guess-checkdoc-error-buffer-name "*Warnings*"))
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


;; package lint
;; (require 'package-lint)
;; (package-lint-batch-and-exit))

;; elsa
;; (require 'elsa)
;; (let ((output (with-output-to-string (elsa-run))))
;;   (unless (string-empty-p output)
;;     (princ output)
;;     (error "Elsa issues detected")))

