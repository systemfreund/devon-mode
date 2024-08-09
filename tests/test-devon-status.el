;;; test-devon-status.el --- Tests for devon-status-consume-event

;;; Commentary:
;; This file contains tests for the devon-status-consume-event function

;;; Code:

(require 'ert)
(require 'devon)

(ert-deftest test-devon-status-consume-event-thinking ()
  "Test that 'ModelRequest' event sets status to 'thinking'."
  (let ((devon-status nil))
    (devon-status-consume-event '((type . "ModelRequest")))
    (should (eq devon-status 'thinking))))

(ert-deftest test-devon-status-consume-event-waiting-for-user ()
  "Test that 'UserRequest' event sets status to 'waiting-for-user'."
  (let ((devon-status nil))
    (devon-status-consume-event '((type . "UserRequest")))
    (should (eq devon-status 'waiting-for-user))))

(ert-deftest test-devon-status-consume-event-stopped ()
  "Test that 'Stop' event sets status to 'stopped'."
  (let ((devon-status nil))
    (devon-status-consume-event '((type . "Stop")))
    (should (eq devon-status 'stopped))))

(ert-deftest test-devon-status-consume-event-unchanged ()
  "Test that unknown event type doesn't change the status."
  (let ((devon-status 'initial-status))
    (devon-status-consume-event '((type . "UnknownEvent")))
    (should (eq devon-status 'initial-status))))

(ert-deftest test-devon-display-event-conversation-filter ()
  "Test that 'conversation' filter displays appropriate events."
  (let ((devon-events-filter 'conversation)
        (devon-buffer (get-buffer-create "*devon-test*")))
    (with-current-buffer devon-buffer
      (erase-buffer)
      (devon-display-event '((type . "UserRequest") (content . "Test request")))
      (should (string-match-p "Devon:" (buffer-string)))
      (erase-buffer)
      (devon-display-event '((type . "UserResponse") (content . "Test response")))
      (should (string-match-p "Human:" (buffer-string)))
      (erase-buffer)
      (devon-display-event '((type . "Checkpoint") (content . "checkpoint-id")))
      (should (string-match-p "Checkpoint:" (buffer-string)))
      (erase-buffer)
      (devon-display-event '((type . "GitAskUser") (content . "Git question")))
      (should (string-match-p "GitAskUser:" (buffer-string)))
      (erase-buffer)
      (devon-display-event '((type . "GitResolve") (content . "Git resolution")))
      (should (string-match-p "GitResolve:" (buffer-string)))
      (erase-buffer)
      (devon-display-event '((type . "ModelResponse") (content . "{\"thought\":\"Test thought\",\"action\":\"Test action\"}")))
      (should (string-match-p "Devon (Thought):\nTest thought" (buffer-string)))
      (erase-buffer)
      (devon-display-event '((type . "OtherEvent") (content . "Should not display")))
      (should (string= "" (buffer-string)))))
  (kill-buffer "*devon-test*"))

(ert-deftest test-devon-display-event-all-filter ()
  "Test that 'all' filter displays appropriate events."
  (let ((devon-events-filter 'all)
        (devon-buffer (get-buffer-create "*devon-test*")))
    (with-current-buffer devon-buffer
      (erase-buffer)
      (devon-display-event '((type . "UserRequest") (content . "Test request")))
      (should (string-match-p "Devon:" (buffer-string)))
      (erase-buffer)
      (devon-display-event '((type . "UserResponse") (content . "Test response")))
      (should (string-match-p "Human:" (buffer-string)))
      (erase-buffer)
      (devon-display-event '((type . "Checkpoint") (content . "checkpoint-id")))
      (should (string-match-p "Checkpoint:" (buffer-string)))
      (erase-buffer)
      (devon-display-event '((type . "GitAskUser") (content . "Git question")))
      (should (string-match-p "GitAskUser:" (buffer-string)))
      (erase-buffer)
      (devon-display-event '((type . "GitResolve") (content . "Git resolution")))
      (should (string-match-p "GitResolve:" (buffer-string)))
      (erase-buffer)
      (devon-display-event '((type . "ModelResponse") (content . "{\"thought\":\"Test thought\",\"action\":\"Test action\"}")))
      (should (string-match-p "Devon (Thought):\nTest thought" (buffer-string)))
      (should (string-match-p "Devon (Action):\nTest action" (buffer-string)))
      (erase-buffer)
      (devon-display-event '((type . "OtherEvent") (content . "Should display")))
      (should (string-match-p "OtherEvent:\nShould display" (buffer-string)))))
  (kill-buffer "*devon-test*"))

(provide 'test-devon-status)
;;; test-devon-status.el ends here
