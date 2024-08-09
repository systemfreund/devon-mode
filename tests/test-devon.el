;;; test-devon.el --- Tests for devon.el

;;; Commentary:
;; This file contains tests for devon.el

;;; Code:

(require 'ert)
(require 'devon)

(ert-deftest test-devon-display-event-checkpoint ()
  "Test if devon-display-event correctly handles Checkpoint events."
  (let ((devon-checkpoint-ids nil)
        (test-buffer (get-buffer-create "*test-devon*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      (devon-display-event '((type . "Checkpoint") (content . ((id . "test-checkpoint-123")))))
      
      ;; Check if the checkpoint is displayed correctly
      (goto-char (point-min))
      (should (search-forward "Checkpoint: test-checkpoint-123" nil t))
      
      ;; Check if the text has the correct face properties
      (let ((checkpoint-face (get-text-property (point) 'face)))
        (should (equal (plist-get checkpoint-face :foreground) "purple"))
        (should (equal (plist-get checkpoint-face :background) "light yellow")))
      
      ;; Check if the checkpoint ID was added to devon-checkpoint-ids
      (should (member "test-checkpoint-123" devon-checkpoint-ids)))
    
    (kill-buffer test-buffer)))

(provide 'test-devon)
;;; test-devon.el ends here
