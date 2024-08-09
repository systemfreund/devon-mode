;;; test-devon.el --- Tests for devon.el -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for devon.el

;;; Code:

(require 'ert)
(require 'devon)

(ert-deftest test-devon-display-event-checkpoint ()
  "Test if devon-display-event correctly handles Checkpoint events with timestamps."
  (let ((devon-checkpoint-ids nil)
        (test-buffer (get-buffer-create "*test-devon*")))
    (with-current-buffer test-buffer
      ;; Test with JSON object content
      (erase-buffer)
      (devon-display-event '((type . "Checkpoint") (content . ((id . "test-checkpoint-123")))))
      
      (goto-char (point-min))
      (should (string-match-p "Checkpoint: test-checkpoint-123 (.*)" (buffer-string)))
      
      (let ((checkpoint-face (get-text-property (point) 'face)))
        (should (equal (plist-get checkpoint-face :foreground) "purple"))
        (should (equal (plist-get checkpoint-face :background) "light yellow")))
      
      (should (assoc "test-checkpoint-123" devon-checkpoint-ids))
      
      ;; Test with string content
      (erase-buffer)
      (devon-display-event '((type . "Checkpoint") (content . "test-checkpoint-456")))
      
      (goto-char (point-min))
      (should (string-match-p "Checkpoint: test-checkpoint-456 (.*)" (buffer-string)))
      
      (let ((checkpoint-face (get-text-property (point) 'face)))
        (should (equal (plist-get checkpoint-face :foreground) "purple"))
        (should (equal (plist-get checkpoint-face :background) "light yellow")))
      
      (should (assoc "test-checkpoint-456" devon-checkpoint-ids))
      
      ;; Test with invalid content (neither string nor JSON object with 'id')
      (erase-buffer)
      (devon-display-event '((type . "Checkpoint") (content . ((not-id . "invalid")))))
      
      (goto-char (point-min))
      (should (string-match-p "Checkpoint: Unknown (.*)" (buffer-string)))
      
      (let ((checkpoint-face (get-text-property (point) 'face)))
        (should (equal (plist-get checkpoint-face :foreground) "purple"))
        (should (equal (plist-get checkpoint-face :background) "light yellow")))
      
      (should (assoc "Unknown" devon-checkpoint-ids))
      
      ;; Check if the checkpoint is displayed differently from other events
      (erase-buffer)
      (devon-display-event '((type . "OtherEvent") (content . "Some other event")))
      (goto-char (point-min))
      (should-not (search-forward "Checkpoint:" nil t))
      
      ;; Check if multiple checkpoints are handled correctly
      (should (= (length devon-checkpoint-ids) 3))
      (should (assoc "test-checkpoint-123" devon-checkpoint-ids))
      (should (assoc "test-checkpoint-456" devon-checkpoint-ids))
      (should (assoc "Unknown" devon-checkpoint-ids))

      ;; Test devon-select-checkpoint function
      (let ((completing-read-args nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest args)
                     (setq completing-read-args args)
                     "Checkpoint test-checkpoint-123")))
          (devon-select-checkpoint)
          (should (string-match-p "Checkpoint test-checkpoint-123 (.*)" (car completing-read-args)))))))
    
    (kill-buffer test-buffer)))

(provide 'test-devon)
;;; test-devon.el ends here
