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

(defcustom devon-default-port 10000
  "Default port for Devon server."
  :type 'integer
  :group 'devon)

(defcustom devon-default-session "devon-mode"
  "Default devon session id."
  :type 'string
  :group 'devon)

(defcustom devon-events-filter 'all
  "Filter for Devon events.
Possible values are:
'all - Show all events
'conversation - Show only conversation events
'no-environment - Show all events except environment-related ones"
  :type '(choice (const :tag "All events" all)
                 (const :tag "Conversation events only" conversation)
                 (const :tag "All except environment events" no-environment))
  :group 'devon)

(defvar devon-port devon-default-port
  "The port number for the Devon session.  Defaults to `devon-default-port`.")

(defvar devon-session-id devon-default-session
  "The current Devon session ID.")

(defvar devon-auto-scroll t
  "Whether to automatically scroll the Devon buffer.")

(defvar devon-request-timeout 5
  "Timeout in seconds for HTTP requests to the Devon server.")

(defvar devon-stream-process nil
  "Process object for the Devon event stream.")

(defvar devon-checkpoint-ids nil
  "List of checkpoint IDs encountered during the session.")

(defun devon-add-checkpoint-id (id)
  "Add a checkpoint ID to the list of encountered checkpoints."
  (add-to-list 'devon-checkpoint-ids id t))

;; Keymap
(defvar devon-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'devon-handle-user-input)
    (define-key map (kbd "C-c d u") 'devon-update-config)
    (define-key map (kbd "C-c d s") 'devon-print-session-state)
    (define-key map (kbd "C-c d c") 'devon-clear-buffer)
    (define-key map (kbd "C-c d r") 'devon-fetch-and-display-events)
    (define-key map (kbd "C-c d a") 'devon-toggle-auto-scroll)
    (define-key map (kbd "C-c C-s r") 'devon-reset-session)
    (define-key map (kbd "C-c C-s c") 'devon-create-session)
    (define-key map (kbd "C-c C-s s") 'devon-start-session)
    (define-key map (kbd "C-c d p") 'devon-select-checkpoint)
    map)
  "Keymap for Devon mode.")

(defun devon-select-checkpoint ()
  "Interactively select a checkpoint from the list of encountered checkpoints."
  (interactive)
  (if (null devon-checkpoint-ids)
      (message "No checkpoints available.")
    (let* ((checkpoint-options (mapcar (lambda (id) (cons (format "Checkpoint %s" id) id)) devon-checkpoint-ids))
           (selected-checkpoint (completing-read "Select a checkpoint: " checkpoint-options nil t)))
      (message "Selected checkpoint: %s" (cdr (assoc selected-checkpoint checkpoint-options))))))

(defun devon-handle-network-error (error-symbol data)
  "Handle network errors in Devon operations.
ERROR-SYMBOL is the type of error, DATA contains error details."
  (let ((error-message (error-message-string data)))
    (message "Devon network error: %s" error-message)
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

;;;###autoload
(defun devon-print-session-state ()
  "Fetch and print the current Devon server session state in a new buffer."
  (interactive)
  (let* ((url (format "%s:%d/sessions/%s/state" devon-backend-url devon-port devon-session-id))
         (url-request-method "GET")
         (response-buffer (url-retrieve-synchronously url nil nil devon-request-timeout))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))
    (if (not response-buffer)
        (message "Error: Unable to fetch Devon server state")
      (with-current-buffer response-buffer
        (goto-char (point-min))
        (re-search-forward "^$")
        (delete-region (point-min) (point))
        (condition-case err
            (let ((response (json-read-from-string (buffer-string))))
              (with-current-buffer (get-buffer-create "*Devon State*")
                (erase-buffer)
                (insert "--- Devon Server State ---\n\n")
                (maphash (lambda (k v)
                           (insert (format "%s:\n" k))
                           (insert (devon-format-state-value v 1))
                           (insert "\n"))
                         response)
                (insert "----------------------------\n")
                (goto-char (point-min))
                (display-buffer (current-buffer))))
          (error
           (message "Error parsing Devon server state: %s" (error-message-string err))))
        (kill-buffer response-buffer)))))
(defun devon-format-state-value (value indent)
  "Format VALUE with INDENT level for better readability in state output."
  (let ((indent-str (make-string (* indent 2) ?\s)))
    (cond
     ((hash-table-p value)
      (let ((result ""))
        (maphash (lambda (k v)
                   (setq result (concat result (format "%s%s: %s\n" indent-str k (devon-format-state-value v (1+ indent))))))
                 value)
        result))
     ((listp value)
      (if (seq-empty-p value)
          "[]"
        (concat "[\n"
                (mapconcat (lambda (item) (concat indent-str "  " (devon-format-state-value item (1+ indent)))) value ",\n")
                (format "\n%s]" indent-str))))
     ((null value) "nil")
     (t (format "%s" value)))))

;; Ensure all functions have proper documentation strings
(dolist (sym '(devon-conversation-history devon-status devon-port
               devon-debug-mode devon-font-lock-keywords devon-default-port
               devon-backend-url))
  (when (and (boundp sym) (null (documentation-property sym 'variable-documentation)))
    (put sym 'variable-documentation
         (format "Devon: %s" (symbol-name sym)))))

(dolist (sym '(devon-fetch-events
               devon-send-response devon-display-event
               devon-add-message devon-update-buffer devon-event-loop
               devon-initialize-buffer
               devon-toggle-debug-mode
               devon-configure
               devon-mode-line 
               devon-handle-network-error 
               devon-update-config))
  (when (and (fboundp sym) (null (documentation sym)))
    (put sym 'function-documentation
         (format "Devon: Function to %s" (replace-regexp-in-string "-" " " (symbol-name sym))))))

;;;###autoload
(defun devon-clear-buffer ()
  "Clear the Devon buffer while keeping the session active."
  (interactive)
  (with-current-buffer (get-buffer "*Devon*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "> ")
      (goto-char (point-max)))))

;;;###autoload
(defun devon-toggle-auto-scroll ()
  "Toggle automatic scrolling in the Devon buffer."
  (interactive)
  (setq devon-auto-scroll (not devon-auto-scroll))
  (message "Devon auto-scroll %s" (if devon-auto-scroll "enabled" "disabled")))

(defvar devon-auto-scroll t
  "Whether to automatically scroll the Devon buffer.")

(defvar devon-request-timeout 5
  "Timeout in seconds for HTTP requests to the Devon server.")

(defvar devon-stream-process nil
  "Process object for the Devon event stream.")

(defun devon-start-event-stream ()
  "Start streaming events from the Devon server."
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
             (url-host (url-generic-parse-url devon-backend-url))))))

(defun devon-stream-filter (proc string)
  "Process incoming data from the Devon event stream."
  (when (string-match "data: \\(.+\\)\n" string)
    (let* ((json-string (match-string 1 string))
           (event (json-read-from-string json-string)))
      (let ((type (cdr (assoc 'type event))))
            (cond
             ((string= type "ModelRequest")
              (setq devon-status 'thinking)
              (devon-update-status))
             ((string= type "ModelResponse")
              (setq devon-status 'idle)
              (devon-update-status))
             ((string= type "UserRequest")
              (setq devon-status 'waiting-for-user)
              (devon-update-status))
             ((string= type "Stop")
              (message "Devon has left the chat.")
              (return))))
      (devon-update-buffer (list event) t))))

(defun devon-stream-sentinel (proc event)
  "Handle Devon event stream connection state changes."
  (when (string-match "\\(open\\|closed\\|connection broken by remote peer\\)" event)
    (message "Devon event stream %s" (match-string 1 event))
    (when (string-match "closed\\|connection broken by remote peer" event)
      (run-with-timer 5 nil 'devon-start-event-stream))))

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
             (message "Error parsing Devon JSON response: %s" (error-message-string err))
             nil)))
      (message "Error fetching Devon events from server: No response received")
      nil)))

(defun devon-fetch-and-display-events ()
  "Fetch events from the Devon server and update the buffer.
This function retrieves events using `devon-fetch-events` and then
updates the Devon buffer using `devon-update-buffer`. If no events
are fetched, a message is displayed to the user."
  (interactive)
  (let ((events (devon-fetch-events)))
    (if events
        (progn
          (devon-update-buffer events)
          (message "Devon buffer updated with new events."))
      (message "No new events to display."))))

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
  (interactive)
  (let ((url-request-method "PATCH")
        (url (format "%s:%d/sessions/%s/start" devon-backend-url devon-port devon-session-id)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (json-read))))

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
             ("model" . "claude-3-5-sonnet")             
             ("versioning_type" . ,(or devon-versioning-type "none"))
             )))
         (response-buffer (url-retrieve-synchronously url)))
    (with-current-buffer response-buffer
      (goto-char url-http-end-of-headers)
      (let ((response (json-read)))
        (message "New Devon session created with ID: %s" response)
        (devon-initialize-buffer)
        response))))

(defvar devon-event-faces
  '((UserResponse . (:background "#E6F3FF" :foreground "black" :weight bold))
    (UserRequest . (:background "#E6FFE6" :foreground "blue" :weight bold))
    (ModelResponse . (:background "#FFE6F0" :foreground "red" :weight bold)))
  "Faces for different event types in Devon mode.")

(defun devon-display-event (event)
  "Format and display an EVENT in the Devon buffer based on `devon-events-filter`."
  (let* ((type (cdr (assoc 'type event)))
         (content (cdr (assoc 'content event)))
         (face (cdr (assoc (intern type) devon-event-faces))))
    (when (or (eq devon-events-filter 'all)
              (and (eq devon-events-filter 'conversation)
                   (or (string= type "UserResponse")
                       (string= type "UserRequest")
                       (string= type "Task")))
              (string= type "Checkpoint"))
      (with-current-buffer (get-buffer-create "*Devon*")
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (cond
           ((string= type "Checkpoint")
            (let ((checkpoint-id (cdr (assoc 'id content))))
              (insert (propertize (format "Checkpoint: %s\n" checkpoint-id)
                                  'face '(:foreground "purple" :background "light yellow")))
              (devon-add-checkpoint-id checkpoint-id)))
           (t
            (insert (propertize (format "%s: " type) 'face face))
            (insert (format "%s\n" content))))
          (goto-char (point-max))
          (when devon-auto-scroll
            (recenter -1)))))))
              (and (eq devon-events-filter 'no-environment)
                   (not (or (string= type "EnvironmentRequest")
                            (string= type "EnvironmentResponse")))))
      (insert "\n\n\n") ; Add 3 lines of margin at the top
      (cond
       ((string= type "UserResponse")
        (insert (propertize (format "Human: %s\n" content) 'face face)))
       ((string= type "UserRequest")
        (insert (propertize (format "Devon: %s\n" content) 'face face)))
       ((and (string= type "ModelResponse")
             (not (eq devon-events-filter 'conversation)))
        (let ((parsed-content (json-read-from-string content)))
          (insert (propertize "Devon (Thought): " 'face face))
        (insert (propertize (format "%s\n" (cdr (assoc 'thought parsed-content))) 'face '(:foreground "blue")))
        (when (cdr (assoc 'command parsed-content))
          (insert (propertize "Devon (Command): " 'face '(:foreground "green" :weight bold)))
          (insert (propertize (format "%s\n" (cdr (assoc 'command parsed-content))) 'face '(:foreground "green"))))))
    ((string= type "ToolRequest")
     (insert (propertize "Command: " 'face '(:foreground "yellow" :weight bold)))
     (insert (propertize (format "%s\n" (cdr (assoc 'raw_command content))) 'face '(:foreground "yellow"))))
    ((string= type "ToolResponse")
     (insert (propertize "Result: " 'face '(:foreground "cyan" :weight bold)))
     (insert (propertize (format "%s\n" content) 'face '(:foreground "cyan"))))
    ((string= type "Error")
     (insert (propertize "Error: " 'face '(:foreground "red" :weight bold)))
     (insert (propertize (format "%s\n" content) 'face '(:foreground "red"))))
    ((string= type "Task")
     (insert (propertize "Task: " 'face '(:foreground "magenta" :weight bold)))
     (insert (propertize (format "%s\n" content) 'face '(:foreground "magenta"))))
    ((string= type "Interrupt")
     (insert (propertize "Interrupt: " 'face '(:foreground "orange" :weight bold)))
     (insert (propertize (format "%s\n" content) 'face '(:foreground "orange"))))
    ((string= type "Checkpoint")
     (let ((checkpoint-id (cdr (assoc 'id content))))
       (when checkpoint-id
         (devon-add-checkpoint-id checkpoint-id))
       (insert (propertize "Checkpoint: " 'face '(:foreground "purple" :weight bold :background "light yellow")))
       (insert (propertize (format "%s\n" (or checkpoint-id content)) 'face '(:foreground "purple" :background "light yellow")))))
    (t (insert (format "%s: %s\n" type content))))
      (insert "\n\n\n")))) ; Add 3 lines of margin at the bottom
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

(defun devon-toggle-debug-mode ()
  "Toggle Devon debug mode."
  (interactive)
  (setq devon-debug-mode (not devon-debug-mode))
  (message "Devon debug mode %s" (if devon-debug-mode "enabled" "disabled")))

(defun devon-set-events-filter (filter)
  "Set the events filter for Devon.
FILTER can be 'all, 'conversation, or 'no-environment."
  (interactive
   (list (completing-read "Choose filter: " '("all" "conversation" "no-environment") nil t)))
  (let ((filter-symbol (intern filter)))
    (setq devon-events-filter filter-symbol)
    (message "Devon events filter set to %s" filter)))


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

(defun devon-mode-line ()
  "Return the mode line format for Devon mode."
  (let ((status (devon-status-indicator)))
    (list
     " Devon: "
     (propertize status 'face
                         (cond
                          ((string= status "Idle") 'success)
                          ((string= status "Thinking...") 'warning)
                          ((string= status "Waiting for input") 'font-lock-keyword-face)
                          (t 'error)))
     )))

;; Add documentation strings to all functions and variables
(dolist (sym '(devon-status devon-port devon-session-id
               devon-debug-mode devon-font-lock-keywords devon-default-port))
  (when (and (boundp sym) (null (documentation-property sym 'variable-documentation)))
    (put sym 'variable-documentation
         (format "Devon: %s" (symbol-name sym)))))

;;;###autoload
(define-derived-mode devon-mode special-mode "Devon"
  "Major mode for Devon Emacs extension."
  :group 'devon
  (setq font-lock-defaults '(devon-font-lock-keywords))
  (font-lock-mode 1)
  (setq-local mode-line-format (append mode-line-format '((:eval (devon-mode-line))))))

(provide 'devon)

;;; devon.el ends here

(defvar devon-status 'idle
  "Current status of Devon: 'idle, 'thinking, or 'waiting-for-user.")

(defvar devon-session-state 'paused
  "Current state of the server session: 'paused, 'running.")

(defun devon-update-status ()
  "Update the mode line to reflect the current Devon status."
  (force-mode-line-update))

(defun devon-status-indicator ()
  "Return a string indicating the current Devon status and session state."
  (let ((status-str (pcase devon-status
                      ('idle "Idle")
                      ('thinking "Thinking...")
                      ('waiting-for-user "Waiting for input")
                      (_ "Devon: Unknown status"))))
    (if (eq devon-session-state 'paused)
        (concat "[Paused] " status-str)
      status-str)))

(defun devon-handle-user-input ()
  "Handle user input and send responses to the Devon session."
  (interactive)
  (let ((input (read-string "To Devon> ")))
    (devon-send-response input)
    (setq devon-status 'thinking)
    (devon-update-status)))

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
