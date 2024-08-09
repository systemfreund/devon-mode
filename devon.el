;;; devon.el --- Devon Emacs Extension -*- lexical-binding: t -*-

;; Author: Devon Development Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: ai, assistant, development
;; URL: https://github.com/yourusername/devon-emacs

;;; Commentary:
;; This is an Emacs extension that replicates the features of the Devon TUI.
;; It provides an interactive interface for communicating with the Devon AI assistant.

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
                 (const :tag "claude-3-haiku" "claude-3-haiku"))
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

(defvar devon-session-state 'paused
  "Current state of the server session: 'paused, 'running.")

(defvar devon-event-stream-status nil
  "Current status of the Devon event stream: 'active, 'closed, 'disconnected, or nil.")

(defvar devon-stream-process nil
  "Process object for the Devon event stream.")

(defvar devon-checkpoint-ids nil
  "List of checkpoint IDs and their timestamps encountered during the session.")

(defun devon-add-checkpoint-id (id)
  "Add a checkpoint ID and its timestamp to the list of encountered checkpoints."
  (let ((timestamp (current-time-string)))
    (add-to-list 'devon-checkpoint-ids (cons id timestamp) t)))

;; Keymap
(defvar devon-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'devon-handle-user-input)
    (define-key map (kbd "C-c d u") 'devon-update-config)
    (define-key map (kbd "C-c d c") 'devon-clear-buffer)
    (define-key map (kbd "C-c d r") 'devon-fetch-and-display-events)
    (define-key map (kbd "C-c C-s r") 'devon-reset-session)
    (define-key map (kbd "C-c C-s c") 'devon-create-session)
    (define-key map (kbd "C-c C-s s") 'devon-start-session)
    (define-key map (kbd "C-c C-s e") 'devon-stop-event-stream)
    (define-key map (kbd "C-c d p") 'devon-select-checkpoint)
    map)
  "Keymap for Devon mode.
\\{devon-mode-map}

\\[devon-handle-user-input] - Handle user input
\\[devon-update-config] - Update Devon configuration
\\[devon-clear-buffer] - Clear Devon buffer
\\[devon-fetch-and-display-events] - Fetch and display events
\\[devon-reset-session] - Reset Devon session
\\[devon-create-session] - Create a new Devon session
\\[devon-start-session] - Start Devon session
\\[devon-stop-event-stream] - Stop Devon event stream
\\[devon-select-checkpoint] - Select a checkpoint")

(defun devon-select-checkpoint ()
  "Interactively select a checkpoint from the list of encountered checkpoints."
  (interactive)
  (if (null devon-checkpoint-ids)
      (devon-log "No checkpoints available.")
    (let* ((sorted-checkpoints (sort (copy-sequence devon-checkpoint-ids)
                                     (lambda (a b) (string-greaterp (cdr a) (cdr b)))))
           (checkpoint-options (mapcar (lambda (checkpoint)
                                         (cons (format "Checkpoint %s (%s)" (car checkpoint) (cdr checkpoint))
                                               (car checkpoint)))
                                       sorted-checkpoints))
           (selected-checkpoint (completing-read "Select a checkpoint: " checkpoint-options nil t)))
      (devon-log "Selected checkpoint: %s" (cdr (assoc selected-checkpoint checkpoint-options))))))

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

;;;###autoload
(defun devon-update-config ()
  "Update Devon configuration interactively."
  (interactive)
  (customize-group 'devon))

(defvar devon-stream-process nil
  "Process object for the Devon event stream.")

(defun devon-start-event-stream ()
  "Start streaming events from the Devon server."
  (interactive)
  (when devon-stream-process
    (delete-process devon-stream-process))
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
    (devon-set-event-stream-status "active")))

(defun devon-stop-event-stream ()
  "Stop the Devon event stream and clean up associated resources."
  (interactive)
  (when devon-stream-process
    (delete-process devon-stream-process)
    (setq devon-stream-process nil)
    (when (get-buffer "*devon-event-stream*")
      (kill-buffer "*devon-event-stream*"))
    (devon-set-event-stream-status nil)))

(defvar devon-stream-buffer ""
  "Buffer to accumulate incoming SSE data.")

(defun devon-stream-filter (proc string)
  "Process incoming data from the Devon event stream."
  (setq devon-stream-buffer (concat devon-stream-buffer string))
  (while (string-match "\\(data: \\(.+\\)\n\n\\)\\|\\(: keepalive\n\n\\)" devon-stream-buffer)
      (let ((match (match-string 0 devon-stream-buffer))
            (match-start (match-beginning 0))
            (match-end (match-end 0)))
        
        (if (string-prefix-p ": keepalive" match)
            (devon-log "Received keepalive")
          (string-match "\\(data: \\(.+\\)\\)" match)
          (let* ((json-string (match-string 2 match)))
            (devon-log "Received event: %s" json-string)
            (let ((event (json-read-from-string json-string)))
              (devon-process-event event))))
        
        (setq devon-stream-buffer (substring devon-stream-buffer match-end)))))

(defun devon-modeline-update ()
  "Update the mode line to reflect the current Devon status."
  (force-mode-line-update))

(defun devon-set-status (new-status)
  "Set the Devon status and update the mode line."
  (setq devon-status new-status)
  (devon-log "Devon status changed to: %s" new-status)
  (devon-modeline-update))

(defun devon-set-session-state (new-state)
  "Set the Devon session state and update the mode line."
  (setq devon-session-state new-state)
  (devon-log "Devon session state changed to: %s" new-state)
  (devon-modeline-update))

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
     ((string= type "Stop")
      (setq new-status 'stopped)))
    (when new-status
      (devon-set-status new-status))))

(defun devon-process-event (event)
  "Process a single event from the Devon event stream."
  (devon-status-consume-event event)
  (when (string= (cdr (assoc 'type event)) "Stop")
    (devon-log "Devon has left the chat."))
  (devon-update-buffer (list event) t))

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

(defun devon-fetch-and-display-events ()
  "Fetch events from the Devon server and update the buffer.
This function retrieves events using `devon-fetch-events` and then
updates the Devon buffer using `devon-update-buffer`. If no events
are fetched, a message is displayed to the user."
  (interactive)
  (setq devon-checkpoint-ids nil)  ; Clear the list of checkpoints
  (let ((events (devon-fetch-events)))
    (if events
        (progn
          (devon-update-buffer events)
          (when-let ((last-event (car (last events))))
            (devon-status-consume-event last-event)))
          (devon-update-buffer events)
          (devon-log "Devon buffer updated with new events."))
      (devon-log "No new events to display.")))

(defun devon-send-response (response)
  "Send a RESPONSE to the session with the given PORT."
  (let ((url-request-method "POST")
        (url (format "%s:%d/sessions/%s/response?response=%s" devon-backend-url devon-port devon-session-id (url-hexify-string response))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun devon-reset-session ()
  (interactive)
  (let ((url-request-method "PATCH")
        (url (format "%s:%d/sessions/%s/reset" devon-backend-url devon-port devon-session-id)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun devon-start-session ()
  "Start the Devon session and update the session state."
  (interactive)
  (devon-set-session-state 'running)
  (let ((url-request-method "PATCH")
        (url (format "%s:%d/sessions/%s/start" devon-backend-url devon-port devon-session-id)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (let ((response (json-read)))
        (devon-log "Devon session started")
        response))))

(defcustom devon-versioning-type 'none
  "Versioning type"
  :type '(choice (const :tag "none" none)
                 (const :tag "git" git)
                 (const :tag "fossil" fossil))
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
  "Display an EVENT in the Devon buffer, handling Checkpoint events specially.
For Checkpoint events, extract the ID, add it to `devon-checkpoint-ids` with timestamp,
but display only the ID. For UserResponse and UserRequest, use 'Human:' and 'Devon:' labels respectively.
For all other events, display as 'type:\ncontent'. Respects the `devon-events-filter` setting."
  (let* ((type (cdr (assoc 'type event)))
         (content (cdr (assoc 'content event)))
         (display-event
          (cond
           ((eq devon-events-filter 'all) t)
           ((eq devon-events-filter 'conversation)
            (member type '("UserRequest" "UserResponse" "Checkpoint" "GitAskUser" "GitResolve")))
           (t t))))
    (when (string= type "Checkpoint")
      (devon-add-checkpoint-id content))
    (when display-event
      (cond
       ((string= type "Checkpoint")
        (insert (format "Checkpoint:\n%s\n\n\n" content)))
       ((string= type "UserResponse")
        (insert (format "Human:\n%s\n\n\n" content)))
       ((string= type "UserRequest")
        (insert (format "Devon:\n%s\n\n\n" content)))
       (t
        (insert (format "%s:\n%s\n\n\n" type content)))))))

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


(defvar devon-font-lock-keywords
  '(("Devon (Thought): " . font-lock-keyword-face)
    ("Devon: " . font-lock-keyword-face)
    ("Human: " . font-lock-function-name-face)
    ("Command: " . font-lock-constant-face)
    ("Result: " . font-lock-string-face)
    ("Error: " . font-lock-warning-face)
    ("Task: " . font-lock-doc-face)
    ("Interrupt: " . font-lock-preprocessor-face))
  "Font-lock keywords for Devon mode.")

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

(defun devon-handle-user-input ()
  "Handle user input and send responses to the Devon session."
  (interactive)
  (let ((input (read-string "To Devon> ")))
    (devon-send-response input)
    (devon-set-status 'thinking)))

(defun devon-initialize-buffer (&optional skip-event-loop)
  "Initialize the Devon buffer and optionally start the event loop with PORT.
If SKIP-EVENT-LOOP is non-nil, don't start the event loop (useful for testing)."
  (interactive)
  (let ((buffer (get-buffer-create "*Devon*")))
    (with-current-buffer buffer
      (devon-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (devon-fetch-and-display-events))
      (setq buffer-read-only nil))  ; Ensure the buffer is writable
    (switch-to-buffer buffer))
  (unless skip-event-loop
    (devon-start-event-stream))
  )

(provide 'devon)

;;; devon.el ends here
