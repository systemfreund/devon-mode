;;; test-devon-stream.el --- Tests for Devon stream processing -*- lexical-binding: t -*-

;;; Commentary:
;; This file contains tests for the Devon stream processing functionality.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "/home/oemer/src/devon-emacs"))
(require 'devon)

(defvar test-stream-data
  "HTTP/1.1 200 OK
date: Sat, 27 Jul 2024 10:28:12 GMT
server: uvicorn
content-type: text/event-stream; charset=utf-8
Transfer-Encoding: chunked


data: {\"type\": \"EnvironmentRequest\", \"content\": \"cat '/devon-emacs/devon.el'\", \"producer\": \"tool\", \"consumer\": \"local\"}

")

(defvar test-processed-events '())

(defun test-devon-process-event (event)
  "Store the processed EVENT for testing."
  (push event test-processed-events))

(ert-deftest test-devon-stream-filter ()
  "Test that devon-stream-filter correctly processes the test stream data."
  (let ((devon-stream-buffer "")
        (devon-process-event #'test-devon-process-event))
    (setq test-processed-events '())
    
    ;; Process the entire test stream data at once
    (devon-stream-filter nil test-stream-data)
    
    ;; Check that one event was processed
    (should (= 1 (length test-processed-events)))
    
    ;; Check the contents of the processed event
    (let ((event (car test-processed-events)))
      (should (equal "EnvironmentRequest" (alist-get 'type event)))
      (should (equal "cat '/devon-emacs/devon.el'" (alist-get 'content event)))
      (should (equal "tool" (alist-get 'producer event)))
      (should (equal "local" (alist-get 'consumer event))))
    
    ;; Check that the stream buffer is empty after processing
    (should (string-empty-p devon-stream-buffer))))

(provide 'test-devon-stream)
;;; test-devon-stream.el ends here
