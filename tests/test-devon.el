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
      ;; Test with JSON object content
      (erase-buffer)
      (devon-display-event '((type . "Checkpoint") (content . ((id . "test-checkpoint-123")))))
      
      (goto-char (point-min))
      (should (search-forward "Checkpoint: test-checkpoint-123" nil t))
      
      (let ((checkpoint-face (get-text-property (point) 'face)))
        (should (equal (plist-get checkpoint-face :foreground) "purple"))
        (should (equal (plist-get checkpoint-face :background) "light yellow")))
      
      (should (member "test-checkpoint-123" devon-checkpoint-ids))
      
      ;; Test with string content
      (erase-buffer)
      (devon-display-event '((type . "Checkpoint") (content . "test-checkpoint-456")))
      
      (goto-char (point-min))
      (should (search-forward "Checkpoint: test-checkpoint-456" nil t))
      
      (let ((checkpoint-face (get-text-property (point) 'face)))
        (should (equal (plist-get checkpoint-face :foreground) "purple"))
        (should (equal (plist-get checkpoint-face :background) "light yellow")))
      
      (should (member "test-checkpoint-456" devon-checkpoint-ids))
      
      ;; Check if the checkpoint is displayed differently from other events
      (erase-buffer)
      (devon-display-event '((type . "OtherEvent") (content . "Some other event")))
      (goto-char (point-min))
      (should-not (search-forward "Checkpoint:" nil t))
      
      ;; Check if multiple checkpoints are handled correctly
      (should (= (length devon-checkpoint-ids) 2))
      (should (member "test-checkpoint-123" devon-checkpoint-ids))
      (should (member "test-checkpoint-456" devon-checkpoint-ids)))
    
    (kill-buffer test-buffer)))

(provide 'test-devon)
;;; test-devon.el ends here
