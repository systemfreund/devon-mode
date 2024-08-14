;;; devon.el --- Devon Emacs Extension -*- lexical-binding: t -*-

;; Author: Devon Development Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: ai, assistant, development
;; URL: https://github.com/yourusername/devon-emacs

;;; Commentary:
;; This is an Emacs extension that provides an interactive interface for communicating with the Devon AI assistant.

;;; Code:

(require 'json)
(require 'url)

;; Customization group and variables
(defgroup devon nil
  "Customization group for Devon."
  :group 'applications)

(defcustom devon-backend-url "http://localhost"
  "URL of the Devon backend server."
  :type 'string
  :group 'devon)

(defcustom devon-port 10000
  "Default port for Devon server."
  :type 'integer
  :group 'devon)

(defcustom devon-request-timeout 5
  "Timeout in seconds for HTTP requests to the Devon server."
  :type 'integer
  :group 'devon)

(defcustom devon-session-id "devon-mode"
  "Devon session id."
  :type 'string
  :group 'devon)

(defcustom devon-events-filter 'all
  "Filter for Devon events.
Possible values are:
'all - Show all events
'conversation - Show only conversation events"
  :type '(choice (const :tag "All events" all)
                 (const :tag "Conversation events only" conversation))
  :group 'devon)

(defcustom devon-default-model "claude-3-5-sonnet"
  "The LLM Devon should use."
  :type '(choice (const :tag "claude-3-5-sonnet" "claude-3-5-sonnet")
                 (const :tag "claude-3-haiku" "claude-3-haiku")
                 (const :tag "ollama/deepseek-coder:6.7b" "ollama/deepseek-coder:6.7b"))
  :group 'devon)

(defvar devon-mode-line-format
  '(:eval
    (concat
     " Devon["
     (symbol-name devon-session-state)
     (if devon-event-stream-status
         (format ", Stream: %s" devon-event-stream-status)
       "")
     "]"
     (format ": %s" (symbol-name devon-status))))
  "Mode line format for Devon mode.")

(defvar devon-status 'idle
  "Current status of Devon: 'idle, 'thinking, or 'waiting-for-user.")

(defvar devon-session-state nil
  "Current state of the server session: 'paused, 'running.")

(defvar devon-event-stream-status nil
  "Current status of the Devon event stream: 'active, 'closed, 'disconnected, or nil.")

(defvar devon-stream-process nil
  "Process object for the Devon event stream.")

;; Keymap
(defvar devon-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'devon-handle-user-input)
    (define-key map (kbd "C-c d c") 'devon-clear-buffer)
    (define-key map (kbd "C-c d r") 'devon-reload-events)
    (define-key map (kbd "C-c C-s r") 'devon-reset-session)
    (define-key map (kbd "C-c C-s c") 'devon-create-session)
    (define-key map (kbd "C-c C-s s") 'devon-start-session)
    (define-key map (kbd "C-c C-s e") 'devon-stop-event-stream)
    map)
  "Keymap for Devon mode.
\\{devon-mode-map}

\\[devon-handle-user-input] - Handle user input
\\[devon-clear-buffer] - Clear Devon buffer
\\[devon-reload-events] - Reload events
\\[devon-reset-session] - Reset Devon session
\\[devon-create-session] - Create a new Devon session
\\[devon-start-session] - Start Devon session
\\[devon-stop-event-stream] - Stop Devon event stream")

(defun devon-handle-network-error (error-symbol data)
  "Handle network errors in Devon operations.
ERROR-SYMBOL is the type of error, DATA contains error details."
  (let ((error-message (error-message-string data)))
    (devon-log "Devon network error: %s" error-message)
    (when (called-interactively-p 'any)
      (signal error-symbol data))))

(defun devon-url-http-around-advice (orig-fun &rest args)
  "Advice to handle network errors for url-http requests.
ORIG-FUN is the original function, ARGS are its arguments."
  (condition-case err
      (apply orig-fun args)
    (error
     (devon-handle-network-error (car err) (cdr err)))))

(advice-add 'url-http :around #'devon-url-http-around-advice)

(defun devon-start-event-stream (&optional replay-events)
  "Start streaming events from the Devon server.
If REPLAY-EVENTS is non-nil, replay existing events after starting the stream."
  (interactive)
  (devon-stop-event-stream)
  (let ((url (format "%s:%d/sessions/%s/events/stream" devon-backend-url devon-port devon-session-id)))
    (setq devon-stream-process
          (make-network-process
           :name "devon-event-stream"
           :buffer (generate-new-buffer "*devon-event-stream*")
           :host (url-host (url-generic-parse-url devon-backend-url))
           :service devon-port
           :family 'ipv4
           :filter 'devon-stream-filter
           :sentinel 'devon-stream-sentinel))
    (process-send-string
     devon-stream-process
     (format "GET %s HTTP/1.1\r\nHost: %s\r\n\r\n"
             (url-filename (url-generic-parse-url url))
             (url-host (url-generic-parse-url devon-backend-url))))
    (devon-set-event-stream-status "active")
    (when replay-events
      (mapc (lambda (event)
              (devon-stream-filter nil (concat "data: " (json-encode event) "\n\n")))
            (devon-fetch-events)))))

(defun devon-stop-event-stream ()
  "Stop the Devon event stream and clean up associated resources."
  (interactive)
  (when devon-stream-process
    (delete-process devon-stream-process)
    (setq devon-stream-process nil)
    (setq devon-stream-buffer "")
    (setq devon-session-state nil)
    (when (get-buffer "*devon-event-stream*")
      (kill-buffer "*devon-event-stream*"))
    (devon-set-event-stream-status nil)))

(defvar devon-stream-buffer ""
  "Buffer to accumulate incoming SSE data.")

(defvar devon-update-timer nil
  "Timer object for updating Devon session state.")

(defun devon-start-update-timer ()
  "Start the timer to update Devon session state every 5 seconds."
  (when (and (get-buffer "*Devon*")
             (not devon-update-timer)
             (fboundp 'devon-update-session-state))
    (setq devon-update-timer
          (run-with-timer 0 5 #'devon-update-session-state))))

(defun devon-stop-update-timer ()
  "Stop the Devon update timer."
  (when devon-update-timer
    (cancel-timer devon-update-timer)
    (setq devon-update-timer nil)))

(defun devon-fetch-config ()
  "Fetch the current session data and configuration from the Devon server."
  (let* ((url (format "%s:%d/sessions/%s/config" devon-backend-url devon-port devon-session-id))
         (url-request-method "GET")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (buffer (with-timeout (devon-request-timeout
                                (error "Request timed out"))
                   (url-retrieve-synchronously url t)))
         config)
    (if buffer
        (with-current-buffer buffer
          (goto-char (point-min))
          (re-search-forward "^$")
          (setq config (json-read))
          (kill-buffer))
      (error "Failed to fetch Devon configuration"))
    config))

(defun devon-select-checkpoint ()
  "Let the user select a checkpoint from the current session configuration.
Returns the checkpoint_id of the selected checkpoint."
  (interactive)
  (let* ((config (devon-fetch-config))
         (checkpoints (cdr (assoc 'checkpoints config)))
         (formatted-checkpoints
          (mapcar (lambda (checkpoint)
                    (let ((id (cdr (assoc 'checkpoint_id checkpoint)))
                          (message (cdr (assoc 'commit_message checkpoint))))
                      (cons (format "%s: %s" id message) id)))
                  (reverse checkpoints)))
         (selection (completing-read "Select a checkpoint: "
                                     (mapcar #'car formatted-checkpoints)
                                     nil t)))
    (cdr (assoc selection formatted-checkpoints))))

(defun devon-get-state-info ()
  "Fetch the current scratchpad and task from the Devon server."
  (let* ((config (devon-fetch-config))
         (state (cdr (assoc 'state config)))
         (scratchpad (cdr (assoc 'scratchpad state)))
         (task (cdr (assoc 'task state))))
    (list :scratchpad scratchpad :task task)))

(defun devon-stream-filter (proc string)
  "Process incoming data from the Devon event stream."
  (setq devon-stream-buffer (concat devon-stream-buffer string))
  (while (string-match "\\(data: \\(.+\\)\n\n\\)\\|\\(: keepalive\n\n\\)" devon-stream-buffer)
      (let ((match (match-string 0 devon-stream-buffer))
            (match-stop (match-end 0)))
        (if (string-prefix-p ": keepalive" match)
            (devon-log "Received keepalive")
          (string-match "\\(data: \\(.+\\)\\)" match)
          (let* ((json-string (match-string 2 match)))
            (devon-log "Received event: %s" json-string)
            (let ((event (json-read-from-string json-string)))
              (devon-status-consume-event event)
              (devon-update-buffer (list event) t))))
        
        (setq devon-stream-buffer (substring devon-stream-buffer match-stop)))))

(defun devon-modeline-update ()
  "Update the mode line to reflect the current Devon status."
  (force-mode-line-update))

(defun devon-set-status (new-status)
  "Set the Devon status and update the mode line."
  (setq devon-status new-status)
  (devon-log "Devon status changed to: %s" new-status)
  (devon-modeline-update))

(defun devon-insert-start-session-link ()
  "Insert a clickable link in the Devon buffer to start the session."
  (with-current-buffer (get-buffer-create "*Devon*")
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\nSession is currently paused.\n")
      (insert-text-button "[Start Session]"
                          'action (lambda (_) (devon-start-session))
                          'follow-link t
                          'help-echo "Click to start the Devon session")
      (insert "\n"))))

(defun devon-set-session-state (new-state)
  "Set the Devon session state and update the mode line."
  (let ((new-state-symbol (intern new-state)))
    (when (not (eq devon-session-state new-state-symbol))
      (setq devon-session-state new-state-symbol)
      (devon-log "Devon session state changed to: %s" new-state)
      (devon-modeline-update)
      (when (eq 'paused devon-session-state)
        (devon-insert-start-session-link)))))

(defun devon-set-event-stream-status (new-status)
  "Set the Devon event stream status and update the mode line."
  (setq devon-event-stream-status new-status)
  (devon-log "Devon event stream status changed to: %s" new-status)
  (devon-modeline-update))

(defun devon-status-consume-event (event)
  "Update devon-status based on the event type."
  (let ((type (cdr (assoc 'type event)))
        (new-status nil))
    (cond
     ((string= type "ModelRequest")
      (setq new-status 'thinking))
     ((string= type "UserRequest")
      (setq new-status 'waiting-for-user))
     ((string= type "GitAskUser")
      (setq new-status 'waiting-for-user))
     ((string= type "Stop")
      (setq new-status 'stopped))
     ((string= type "GitResolve")
      (setq devon-pending-git-question nil)
      (setq devon-git-options nil)))
    (when new-status
      (devon-set-status new-status))))

(defun devon-stream-sentinel (proc event)
  "Handle status changes in the Devon event stream."
  (when (string-match "\\(open\\|closed\\|connection broken by remote peer\\)" event)
    (let ((status (match-string 1 event)))
      (devon-set-event-stream-status
       (cond
        ((string= status "open") "active")
        ((string= status "closed") "closed")
        ((string= status "connection broken by remote peer") "disconnected")
        (t nil)))
      (devon-log "Devon event stream %s" status)
      (when (string-match "closed\\|connection broken by remote peer" event)
        (run-with-timer 5 nil 'devon-start-event-stream)))))

(defun devon-fetch-events ()
  "Fetch events from the Devon server."
  (let* ((url (format "%s:%d/sessions/%s/events" devon-backend-url devon-port devon-session-id))
         (url-request-method "GET")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (buffer (with-timeout (devon-request-timeout
                                (error "Request timed out"))
                   (url-retrieve-synchronously url t))))
    (if buffer
        (with-current-buffer buffer
          (goto-char (point-min))
          (re-search-forward "^$")
          (delete-region (point-min) (point))
          (condition-case err
              (let ((json-array-type 'list))
                (json-read))
            (error
             (devon-log "Error parsing Devon JSON response: %s" (error-message-string err))
             nil)))
      (devon-log "Error fetching Devon events from server: No response received")
      nil)))

(defun devon-reload-events ()
  "Fetch events from the Devon server and update the buffer."
  (interactive)
  (let ((events (devon-fetch-events)))
    (if events
        (progn
          (devon-update-buffer events)
          (when-let ((last-event (car (last events))))
            (devon-status-consume-event last-event)))
          (devon-update-buffer events))))

(defun devon-send-response (response)
  "Send a RESPONSE to the session with the given PORT."
  (let ((url-request-method "POST")
        (url (format "%s:%d/sessions/%s/response?response=%s" devon-backend-url devon-port devon-session-id (url-hexify-string response))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun devon-reset-session ()
  (interactive)
  (devon-update-buffer '())
  (let ((url-request-method "PATCH")
        (url (format "%s:%d/sessions/%s/reset" devon-backend-url devon-port devon-session-id)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (json-read))
    (devon-start-event-stream)))

(defun devon-start-session ()
  "Start the Devon session and update the session state."
  (interactive)
  (let ((url-request-method "PATCH")
        (url (format "%s:%d/sessions/%s/start" devon-backend-url devon-port devon-session-id)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (let ((response (json-read)))
        (devon-log "Devon session started")
        (devon-update-session-state)
        response))))

(defun devon-update-session-state ()
  "Get the Devon session state from server and update locally."
  (interactive)
  (when (get-buffer "*Devon*")
    (condition-case err
        (let ((url-request-method "GET")
              (url (format "%s:%d/sessions/%s/status" devon-backend-url devon-port devon-session-id)))
          (with-current-buffer (url-retrieve-synchronously url nil t devon-request-timeout)
            (if (>= url-http-response-status 400)
                (error "HTTP error: %s" url-http-response-status)
              (goto-char url-http-end-of-headers)
              (let ((response (json-read)))
                (devon-set-session-state response)
                response))))
      (error
       (devon-log "Error updating session state: %s" (error-message-string err))
       (devon-set-session-state 'error)))))

(defcustom devon-versioning-type 'none
  "Versioning type"
  :type '(choice (const :tag "none" none)
                 (const :tag "git" git))
  :group 'devon)

(defun devon-create-session (project-path &optional)
  "Create a new Devon session for the project at PROJECT-PATH."
  (interactive "DProject path: ")
  (let* ((url-request-method "POST")
         (url (format "%s:%d/sessions/%s?path=%s" devon-backend-url devon-port devon-session-id project-path))
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data
          (json-encode
           `(
             ("model" . ,devon-default-model)
             ("versioning_type" . ,(or devon-versioning-type "none"))
             )))
         (response-buffer (url-retrieve-synchronously url)))
    (with-current-buffer response-buffer
      (goto-char url-http-end-of-headers)
      (let ((response (json-read)))
        (devon-log "New Devon session created with ID: %s" response)
        (devon-initialize-buffer)
        response))))

(defun devon-send-event (event)
  "Send an event to Devon"
  (let* ((url-request-method "POST")
         (url (format "%s:%d/sessions/%s/event" devon-backend-url devon-port devon-session-id))
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (json-encode event))
         (response-buffer (url-retrieve-synchronously url)))
    (with-current-buffer response-buffer
      (goto-char url-http-end-of-headers)
      (let ((response (json-read)))
        (devon-log "Event sent to Devon session %s: %s" devon-session-id response)        
        response))))

(defun devon-git-resolve (answer &optional)
  (interactive "MAnswer: ")
  (devon-send-event
   `(
     ("type" . "GitResolve")
     ("content" . (("action" . ,answer)))
     ("producer" . "human")
     ("consumer" . "devon"))))

(defun devon-display-event (event)
  "Display an EVENT in the Devon buffer.  Respects the `devon-events-filter` setting."
  (let* ((type (cdr (assoc 'type event)))
         (content (cdr (assoc 'content event)))
         (display-event
          (cond
           ((eq devon-events-filter 'all) t)
           ((eq devon-events-filter 'conversation)
            (member type '("UserRequest" "UserResponse" "Checkpoint" "GitError" "GitCorrupted" "GitAskUser" "GitResolve" "ModelResponse" "Task" "Error" "Stop")))
           (t t))))
    (when display-event
      (cond
       ((string= type "Checkpoint")
        (insert (format "Checkpoint: %s\n\n" content)))
       ((string= type "UserResponse")
        (insert (format "Human:\n%s\n\n\n" content)))
       ((string= type "UserRequest")
        (insert (format "Devon:\n%s\n\n\n" content)))
       ((string= type "ModelResponse")
        (let* ((body (with-temp-buffer
                       (insert content)
                       (goto-char (point-min))
                       (json-read)))
              (thought (cdr (assoc 'thought body)))
              (action (cdr (assoc 'action body))))
          (insert (format "Devon (Thought):\n%s\n\n\n" thought))
          (when (and action (eq devon-events-filter 'all))
            (insert (format "Devon (Action):\n%s\n\n\n" action)))))
       ((string= type "GitAskUser")
        (setq devon-pending-git-question t)
        (let* ((message (cdr (assoc 'message content)))
               (options (cdr (assoc 'options content))))
          (setq devon-git-options (mapcar #'identity options))
          (insert (format "Git:\n%s\n\n" message))
          (dolist (option devon-git-options)
            (insert-text-button (concat "[" option "]  ")
                                'action #'devon-git-option-click
                                'follow-link t
                                'help-echo "Click to select this option"))
          (insert "\n")))
       (t
        (insert (format "%s:\n%s\n\n\n" type content)))))))

(defvar devon-font-lock-keywords
  '(("Checkpoint: " . font-lock-comment-delimiter-face)
    ("Devon (Thought):" . font-lock-keyword-face)
    ("Devon (Action):" . font-lock-comment-face)
    ("Devon:" . font-lock-keyword-face)
    ("Human:" . font-lock-function-name-face)
    ("Command:" . font-lock-constant-face)
    ("Result:" . font-lock-string-face)
    ("Error:" . font-lock-warning-face)
    ("Task:" . font-lock-doc-face)
    ("Git:" . font-lock-warning-face)
    ("Interrupt:" . font-lock-preprocessor-face))
  "Font-lock keywords for Devon mode.")

(defun devon-update-buffer (events &optional append)
  "Update the Devon buffer with EVENTS.
If APPEND is non-nil, append the events to the existing buffer content.
Otherwise, replace the buffer content with the new events."
  (with-current-buffer (get-buffer-create "*Devon*")
    (let ((inhibit-read-only t))
      (unless append
        (erase-buffer))
      (save-excursion
        (goto-char (point-max))
        (mapc (lambda (event)
                (devon-display-event event))
              events))
      (goto-char (point-max)))))

(defvar devon-debug-mode nil
  "Flag to indicate if Devon is in debug mode.")

(defun devon-log (format-string &rest args)
  "Write a log message to the *Devon Log* buffer.
FORMAT-STRING and ARGS are the same as for `message'."
  (with-current-buffer (get-buffer-create "*Devon Log*")
    (goto-char (point-max))
    (insert (apply #'format format-string args))
    (insert "\n")))

(defun devon-toggle-debug-mode ()
  "Toggle Devon debug mode."
  (interactive)
  (setq devon-debug-mode (not devon-debug-mode))
  (devon-log "Devon debug mode %s" (if devon-debug-mode "enabled" "disabled")))

(defun devon-set-events-filter (filter)
  "Set the events filter for Devon.
FILTER can be 'all, 'conversation, or 'no-environment."
  (interactive
   (list (completing-read "Choose filter: " '("all" "conversation" "no-environment") nil t)))
  (let ((filter-symbol (intern filter)))
    (setq devon-events-filter filter-symbol)
    (devon-log "Devon events filter set to %s" filter)))


(defun devon-configure ()
  "Configure Devon settings."
  (interactive)
  (customize-group 'devon))

(defcustom devon-default-port 10000
  "Default port for Devon server."
  :type 'integer
  :group 'devon)

;;;###autoload
(define-derived-mode devon-mode special-mode "Devon"
  "Major mode for Devon Emacs extension."
  :group 'devon
  (setq font-lock-defaults '(devon-font-lock-keywords))
  (font-lock-mode 1)
  (setq-local mode-line-format (list "" mode-line-format devon-mode-line-format)))

(defvar devon-pending-git-question nil
  "Stores the pending Git question if there is one.")

(defvar devon-git-options nil
  "Stores the options for the pending Git question.")

(defun devon-handle-user-input ()
  "Handle user input and send responses to the Devon session."
  (interactive)
  (if (not devon-pending-git-question)
      (let ((input (read-string "To Devon> ")))
        (devon-send-response input)
        (devon-set-status 'thinking))
    (message "Please click on one of the Git options above.")))

(defun devon-git-option-click (button)
  "Handle clicks on Git option buttons."
  (if devon-pending-git-question
      (let ((option (button-label button)))
    (devon-git-resolve option)
    (setq devon-pending-git-question nil)
    (setq devon-git-options nil)
    (devon-set-status 'thinking)
    (message "Selected Git option: %s" option))))

(defun devon-initialize-buffer (&optional skip-event-loop)
  "Initialize the Devon buffer and optionally start the event loop with PORT.
If SKIP-EVENT-LOOP is non-nil, don't start the event loop (useful for testing)."
  (interactive)
  (let ((buffer (get-buffer-create "*Devon*")))
    (with-current-buffer buffer
      (devon-mode)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq buffer-read-only nil)
      (add-hook 'kill-buffer-hook #'devon-stop-event-stream nil t)
      (add-hook 'kill-buffer-hook #'devon-stop-update-timer nil t))
    (switch-to-buffer buffer))
  (unless skip-event-loop
    ;;(devon-start-update-timer)
    (devon-start-event-stream t)
    (devon-update-session-state)))

(provide 'devon)

;;; devon.el ends here
