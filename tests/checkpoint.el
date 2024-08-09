;;; test-devon.el --- Tests for devon.el

;;; Commentary:
;; This file contains tests for devon.el

;;; Code:

(require 'ert)
(require 'devon)

(ert-deftest devon-test-display-event-checkpoint ()
  "Test if devon-display-event correctly handles Checkpoint events."
  (let ((devon-checkpoint-ids nil)
        (test-buffer (get-buffer-create "*test-devon*")))
    (with-current-buffer test-buffer
      ;; Test with JSON object content
      (erase-buffer)
      (devon-display-event '((type . "Checkpoint") (content . "test-checkpoint-123")))
      
      (goto-char (point-min))
      (should (search-forward "Checkpoint: test-checkpoint-123" nil t))
            
      (should (member "test-checkpoint-123" devon-checkpoint-ids))
    (kill-buffer test-buffer))))

(provide 'test-devon)
;;; test-devon.el ends here
