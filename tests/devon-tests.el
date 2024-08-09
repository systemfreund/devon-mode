;;; devon-tests.el --- Tests for Devon Emacs Extension

;;; Commentary:
;; This file contains tests for the Devon Emacs extension.

;;; Code:

(require 'ert)
(require 'devon)

(ert-deftest devon-test-mode-initialization ()
  "Test that Devon mode initializes correctly."
  (with-temp-buffer
    (devon-mode)
    (should (eq major-mode 'devon-mode))))

(ert-deftest devon-test-keybindings ()
  "Test that Devon mode keybindings are set correctly."
  (with-temp-buffer
    (devon-mode)
    (should (eq (lookup-key devon-mode-map (kbd "RET")) 'devon-handle-user-input))))

(ert-deftest devon-test-initialize-buffer ()
  "Test that devon-initialize-buffer creates and sets up the buffer correctly."
  (let ((test-port 8080))
    (unwind-protect
        (progn
          (devon-initialize-buffer t)  ; Pass t to skip event loop
          (should (get-buffer "*Devon*"))
          (with-current-buffer "*Devon*"
            (should (eq major-mode 'devon-mode))
            (should-not buffer-read-only)))
      (when (get-buffer "*Devon*")
        (kill-buffer "*Devon*"))
      (setq devon-port nil))))

(provide 'devon-tests)

;;; devon-tests.el ends here
