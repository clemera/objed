(let* ((project-tests-file "tests.el")
       (current-directory (file-name-directory load-file-name))
       (project-test-path (expand-file-name "." current-directory))
       (project-root-path (expand-file-name ".." current-directory)))

  ;; add the package being tested to 'load-path so it can be 'require-d
  (add-to-list 'load-path project-root-path)
  (add-to-list 'load-path project-test-path)

  ;; load the file with tests
  (load (expand-file-name project-tests-file project-test-path))

  ;; run the tests
  ;; (buttercup-run-discover))
  (ert-run-tests-batch-and-exit))
