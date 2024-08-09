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

(provide 'test-devon-status)
;;; test-devon-status.el ends here
