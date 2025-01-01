;;; eden.el --- AI assistant interface for OpenAI LLM models and Perplexity -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Tony Aldon
;;
;; Author: Tony Aldon <tony@tonyaldon.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://tonyaldon.com
;;
;;; Commentary:
;;
;; Eden is an AI assistant package for Emacs.
;;
;; It offers a simple interface for:
;;
;; 1) OpenAI LLM models (like gpt-4o and o1) and
;; 2) Perplexity AI powered answer engine.
;;
;; Get started in minutes:
;;
;; 1) Add your API keys to ~/.authinfo.gpg file (or ~/.authinfo),
;; 2) Call the command eden to switch to eden prompt buffer,
;; 3) Enter your prompt (i.e. ask something),
;; 4) Hit C-c C-c to send the request to the current API (OpenAI by
;;    default with gpt-4o-mini model),
;; 5) Receive the response asynchronously in a dedicated buffer that pops
;;    up upon receipt.
;;; Code:

;;; AI Assistant API to send asynchronous requests to OpenAI
(require 'json)
(require 'ox-md)

(defun rich-ai-json-encode (object)
  "..."
  ;; ...
  (let ((json-false :false)
        (json-encoding-pretty-print "  ")
        (json-encoding-pretty-print t))
    (json-encode object)))

(defun rich-ai-json-read ()
  "..."
  ;; ...
  (let ((json-false :false)
        (json-key-type 'keyword)
        (json-object-type 'plist)
        (json-array-type 'vector))
    (json-read)))

(defun rich-ai-request-dir (req)
  "..."
  (let ((ai-dir (plist-get req :ai-dir))
        (uuid (plist-get req :uuid)))
    (when (or (not (stringp ai-dir)) (not (stringp uuid)))
      (error "Request mal formed.  `req' must contain `:ai-dir' and `:uuid' keys and their value must be strings: %S"
             req))
    (concat (file-name-as-directory ai-dir) uuid "/")))

(defun rich-ai-request-file (file req)
  "..."
  (let* ((filenames '((error         . "error.json")
                      (response      . "response.json")
                      (response-org  . "response.org")
                      (request       . "request.json")
                      (api           . "api.json")
                      (prompt        . "prompt.org")
                      (system-prompt . "system-prompt.org")
                      (exchanges     . "exchanges.json")
                      (command       . "command")))
         (filename (alist-get file filenames)))
    (if filename
        (concat (rich-ai-request-dir req) filename)
      (error "`file' argument must be one of %s, not `%s'"
             (mapcar #'car filenames) file))))

(defun rich-ai-request-read (file req)
  "..."
  (let* ((-file (rich-ai-request-file file req)))
    (if (not (file-exists-p -file))
        (error "Missing `%s' file." -file)
      (with-temp-buffer
        (insert-file-contents (rich-ai-request-file file req))
        (if (string= (file-name-extension -file) "json")
            (rich-ai-json-read)
          (buffer-substring-no-properties (point-min) (point-max)))))))

(defalias 'rich-ai-get-in 'map-nested-elt)

(defun rich-ai-request-assistant-content (resp)
  "..."
  (rich-ai-get-in resp [:choices 0 :message :content]))

(defun rich-ai-request-user-content (request)
  "..."
  (let* ((messages (plist-get request :messages))
         (last-message (aref messages (1- (length messages)))))
    (plist-get last-message :content)))

(defun rich-ai-request-check (req)
  "Return t if REQ did complete.

Raise an error in the following cases:

- the request doesn't exist,
- the request has failed in a prior attempt (so `error.json' file exists),
- the request in incomplete, specificaly when at least one of the following
  files is missing:

  - prompt.org
  - request.json
  - response.json
  - response.org
  - exchanges.json"
  (let ((req-dir (rich-ai-request-dir req)))
    (cond
     ((not (file-exists-p (rich-ai-request-dir req)))
      (error "Request `%s' doesn't exist." req-dir))
     ((file-exists-p (rich-ai-request-file 'error req))
      (error "Request `%s' has failed in a prior attempt.  See why in `%s' file."
             req-dir (rich-ai-request-file 'error req)))
     ((not (file-exists-p (rich-ai-request-file 'prompt req)))
      (error "Missing `%s' file." (rich-ai-request-file 'prompt req)))
     ((not (file-exists-p (rich-ai-request-file 'request req)))
      (error "Missing `%s' file." (rich-ai-request-file 'request req)))
     ((not (file-exists-p (rich-ai-request-file 'response req)))
      (error "Missing `%s' file." (rich-ai-request-file 'response req)))
     ((not (file-exists-p (rich-ai-request-file 'response-org req)))
      (error "Missing `%s' file." (rich-ai-request-file 'response-org req)))
     ((not (file-exists-p (rich-ai-request-file 'exchanges req)))
      (error "Missing `%s' file." (rich-ai-request-file 'exchanges req)))
     (t t))))

(defun rich-ai-request-conversation (req)
  (rich-ai-request-check req)
  (let* ((exchanges (rich-ai-request-read 'exchanges req))
         (last-exchange
          `((:uuid ,(plist-get req :uuid)
             :prompt ,(rich-ai-request-read 'prompt req)
             :user ,(rich-ai-request-user-content
                     (rich-ai-request-read 'request req))
             :assistant ,(rich-ai-request-assistant-content
                          (rich-ai-request-read 'response req))
             :response ,(rich-ai-request-read 'response-org req)))))
    (apply 'vector (append exchanges last-exchange))))

(defun rich-ai-request-conversation-path (req)
  (when (condition-case nil (rich-ai-request-check req) (error nil))
    (let* ((uuids (mapcar (lambda (exchange) (plist-get exchange :uuid))
                          (rich-ai-request-read 'exchanges req)))
           (last-uuid (list (plist-get req :uuid))))
      (apply 'vector (append uuids last-uuid)))))

(defun rich-ai-request-conversation-path-alist (path)
  (when path
    (let* ((tail (append (reverse path) '()))
           (alist (list (cons (pop tail) t))))
      (while tail
        (setq alist (list (cons (pop tail) alist))))
      alist)))

(defun rich-ai-request-perplexity-citations (req)
  (let ((ai-dir (plist-get req :ai-dir)))
    (seq-reduce
     (lambda (acc exchange)
       (let* ((uuid-exchange (plist-get exchange :uuid))
              (req-exchange `(:ai-dir ,ai-dir :uuid ,uuid-exchange)))
         (if (condition-case nil (rich-ai-request-check req-exchange) (error nil))
             (let* ((resp (rich-ai-request-read 'response req-exchange))
                    (citations (plist-get resp :citations)))
               (append acc citations '()))
           acc)))
     (rich-ai-request-conversation req)
     '())))

(defun rich-ai-request-timestamp (req)
  (when-let ((filename
              (car (directory-files
                    (rich-ai-request-dir req) nil "timestamp-.*"))))
    (string-to-number (string-trim-left filename "timestamp-"))))

(defun rich-ai-request-date (req)
  (when-let ((timestamp (rich-ai-request-timestamp req)))
    (format-time-string "[%Y-%m-%d %a]" (seconds-to-time (floor timestamp)))))

(defun rich-ai-request-write (file req content)
  (let ((inhibit-message t)
        (message-log-max nil)
        (file-path (if (eq file 'timestamp)
                       (format "%stimestamp-%s"
                               (rich-ai-request-dir req) (time-to-seconds))
                     (rich-ai-request-file file req))))
    (make-directory (rich-ai-request-dir req) 'parents)
    (with-temp-buffer
      (insert content)
      (write-file file-path))))

(defun rich-ai-write-request (req)
  "..."
  (let ((request (rich-ai-json-encode (plist-get req :req)))
        (api (rich-ai-json-encode (plist-get req :api)))
        (prompt (plist-get req :prompt))
        (system-prompt (or (plist-get req :system-prompt) ""))
        (exchanges (rich-ai-json-encode (plist-get req :exchanges))))
    (rich-ai-request-write 'timestamp req "")
    (rich-ai-request-write 'request req request)
    (rich-ai-request-write 'api req api)
    (rich-ai-request-write 'prompt req prompt)
    (rich-ai-request-write 'system-prompt req system-prompt)
    (rich-ai-request-write 'exchanges req exchanges)))

(defun rich-ai-write-command (command-no-api-key req)
  "..."
  (message "%s" command-no-api-key)
  (rich-ai-request-write 'command req command-no-api-key))

(defun rich-ai-markdown-to-org (markdown-str)
  "Return MARKDOWN-STR markdown string converted into org-mode string."
  (interactive)
  (let ((file-markdown (concat (make-temp-file "ai-response-") ".md"))
        (file-org (concat (make-temp-file "ai-response-") ".org"))
        (file-lua-filter (concat (make-temp-file "ai-lua-filter-") ".lua"))
        ;; 1) Remove org properties added by pandoc org backend.  Specifically,
        ;;    a CUSTOM_ID property is added to all headings which is annoying.
        ;; 2) Don't use example blocks, only source blocks with language name
        (lua-filter "function Header(el)
    return pandoc.Header(el.level, el.content, pandoc.Attr())
end

function CodeBlock(block)
  if block.classes[1] ~= nil then
    return pandoc.RawBlock('org', string.format(\"#+BEGIN_SRC %s\\n%s\\n#+END_SRC\\n\\n\", block.classes[1], block.text))
  else
    return pandoc.RawBlock('org', string.format(\"#+BEGIN_SRC text\\n%s\\n#+END_SRC\\n\\n\", block.text))
  end
end")
        (inhibit-message t))
    (with-temp-buffer
      (insert markdown-str)
      (write-file file-markdown))
    (with-temp-buffer
      (insert lua-filter)
      (write-file file-lua-filter))
    (call-process "pandoc" nil nil nil file-markdown "-o" file-org
                  (format "--lua-filter=%s" file-lua-filter))
    (with-temp-buffer
      (insert-file-contents file-org)
      (buffer-string))))

(defun rich-ai-org-replace-perplexity-citations (org-str citations)
  (let* ((citations-len (length citations)))
    (with-temp-buffer
      (org-mode)
      (save-excursion (insert org-str))
      (font-lock-ensure)
      (while (re-search-forward "\\[\\([0-9]+\\)\\]" nil t)
        (let* ((citation-number-str (match-string 1))
               (citation-idx (1- (string-to-number citation-number-str)))
               (citation-faces (get-text-property (match-beginning 1) 'face)))
          (message "%s" citation-number-str)
          (message "%S" citation-faces)
          (when (and (not (seq-contains-p citation-faces 'org-code))
                     (not (seq-contains-p citation-faces 'org-verbatim))
                     (not (seq-contains-p citation-faces 'org-block)))
            (if (<= 0 citation-idx citations-len)
                (replace-match (format "[[%s][%s]]"
                                       (aref citations citation-idx)
                                       citation-number-str)
                               nil nil nil 1)))))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun rich-ai-write-response (resp-str resp req)
  "..."
  (rich-ai-request-write 'response req resp-str)
  (let* ((assistant-content (rich-ai-request-assistant-content resp))
         (response-org (rich-ai-markdown-to-org assistant-content))
         (citations (plist-get resp :citations))
         (response-org
          (if (and citations (vectorp citations))
              (rich-ai-org-replace-perplexity-citations response-org citations)
            response-org)))
    (rich-ai-request-write 'response-org req response-org)))

(defun rich-ai-write-error (err req)
  "..."
  (rich-ai-request-write 'error req (rich-ai-json-encode err)))

(defvar rich-ai-errors
  '((rich-ai-error-api . "API error")
    (rich-ai-error-api-key . "Error using AI assitant: API key not set correctly")
    (rich-ai-error-callback . "Error while calling callback function in sentinel")
    (rich-ai-error-callback-error . "Error while calling callback-error function when signaling an error in sentinel")
    (rich-ai-error-json-read . "Error while parsing JSON in process buffer")
    (rich-ai-error-process . "The process did not finished correctly")
    (rich-ai-error-process-buffer . "The process buffer got killed while processing the request")))

(dolist (err rich-ai-errors)
  (define-error (car err) (cdr err)))

(cl-defun rich-ai-error-log-and-signal (type req process
                                             &key error event process-stdout
                                             callback-error info)
  ""
  (let* ((error-function
          (lambda (type req error event process-stdout original-error)
            (delq nil
                  `(:type ,(symbol-name type)
                    :message ,(alist-get type rich-ai-errors)
                    :directory ,(rich-ai-request-dir req)
                    :request ,(plist-get req :req)
                    ,@(when error `(:error ,error))
                    ,@(when event `(:process-event ,event))
                    ,@(when process-stdout
                        `(:process-buffer-content ,process-stdout))
                    ,@(when original-error
                        `(:original-error ,original-error))))))
         (err (funcall error-function type req error event process-stdout nil)))
    (kill-buffer (process-buffer process))
    (when callback-error
      (condition-case -error
          (funcall callback-error req err info)
        (error
         (setq type 'rich-ai-error-callback-error)
         (setq err (funcall error-function type req -error nil nil err)))))
    (rich-ai-write-error err req)
    (signal type err)))

(defmacro rich-ai-sentinel (req callback callback-error info)
  "Return a sentinel to be used in `rich-ai-request-send'.

The return sentinel is a function that takes two argument `process'
and `event' as described in `make-process'.

When no error occurs during execution of the sentinel, CALLBACK function
is called.  It takes 3 arguments

- req   - plist of information about the request where :req
          complies with openai API

              (:req (:stream :false
                     :model \"gpt-4o-mini\"
                     :temperature 1
                     :messages [(:role \"user\" :content \"foo bar baz\")])
               :api (:service \"chatgpt\"
                     :endpoint \"https://api.openai.com/v1/chat/completions\")
               :prompt \"prompt in org mode\"
               :system-prompt \"system prompt in org mode\"
               :attachments [\"path-to-file\" \"path-to-directory/\" \"...\"])

- resp  - plist of the response received from openai
- info  - plist of data to act on, can be nil.

and must be use for side effects.

When an error occurs, CALLBACK-ERROR function (if not nil) is called
just before signaling the error.  It takes 3 arguments:

- req  - the same as decscribed above,
- err  - plist describing the error wich is also the data that is
         associated with error when signaled with `signal' function
         in `rich-ai-error-log-and-signal'.  For instance if we provid
         OpenAI with a wrong API key `err' looks like this:

             (:type \"rich-ai-error-api\"
              :message \"API error\"
              :directory \"/tmp/rich-ai-VKTqOa/uuid-foo/\"
              :request (:stream :false
                        :model \"gpt-4o-mini\"
                        :temperature 1
                        :messages [(:role \"user\" :content \"foo bar baz\")])
              :error (:message \"Incorrect API key provided: eesk-pro***WmEA. You can find your API key at https://platform.openai.com/account/api-keys.\"
                      :type \"invalid_request_error\"
                      :param nil
                      :code \"invalid_api_key\"))

         The keys `:type', `:message', `:directory', `:request' are
         always present while `:error', `:process-event',
         `:process-buffer-content' keys are optionals and depend of the
         type of error.

         See `rich-ai-errors' and `rich-ai-error-log-and-signal'.
- info  - plist of data to act on, can be nil."
  `(lambda (process event)
     (let ((stdout (lambda (process)
                     (with-current-buffer (process-buffer process)
                       (buffer-string)))))
       (cond
        ((not (buffer-name (process-buffer process)))
         (rich-ai-error-log-and-signal
          'rich-ai-error-process-buffer ,req process
          :callback-error ,callback-error
          :info ,info))
        ((string= event "finished\n")
         (let ((resp (condition-case err
                         (with-current-buffer (process-buffer process)
                           (goto-char (point-min))
                           (rich-ai-json-read))
                       (error (rich-ai-error-log-and-signal
                               'rich-ai-error-json-read ,req process
                               :error err
                               :process-stdout (funcall stdout process)
                               :callback-error ,callback-error
                               :info ,info)))))
           (if-let ((err (plist-get resp :error)))
               (rich-ai-error-log-and-signal
                'rich-ai-error-api ,req process
                :error err
                :callback-error ,callback-error
                :info ,info)
             (condition-case err
                 (progn
                   (rich-ai-write-response (funcall stdout process) resp ,req)
                   (kill-buffer (process-buffer process))
                   (funcall ,callback ,req resp ,info))
               (error (rich-ai-error-log-and-signal
                       'rich-ai-error-callback ,req process
                       :error err
                       :callback-error ,callback-error
                       :info ,info))))))
        (t (rich-ai-error-log-and-signal
            'rich-ai-error-process ,req process
            :process-stdout (funcall stdout process)
            :event event
            :callback-error ,callback-error
            :info ,info))))))

(defun rich-ai-api-key-symbol (service)
  "Return the symbol we use for holding api key for SERVICE service.

It is used in `rich-ai-request-command'.

When we want to use `rich-ai-request-send' programmatically without
asking the user (and so gpg) for the encrytped key in ~/.authinfo.gpg
file we can use `rich-ai-api-key-symbol' to set the api key like this
assumming SERVICE is \"openai\":

    (let ((api-key-symbol (rich-ai-api-key-symbol \"openai\")))
      (defvar-1 api-key-symbol nil)
      (set api-key-symbol \"secret-api-key\")
      nil)"
  (intern (format "rich-ai-api-key-%s" service)))

(defun rich-ai-request-command (req)
  "Return list of commands (command command-no-api-key).

`command' contains the real api key and `command-no-api-key' does not.
This way we can use the latter for logging.

req must have the following keys as in this example

    (:api (:service \"openai-service\"
           :endpoint \"https://openai-endpoint\")
     :ai-dir \"/tmp/ai-dir/\"
     :uuid \"uuid-foo\")

Also set AI api key (the first time) from `~/.authinfo.gpg'
file (encrypted with gpg) or `~/.authinfo' file

    machine openai-service password <foo-bar-baz>

where `openai-service' is :service key of :api key of REQ request.

The AI api key is stored in `rich-ai-api-key-<service-name>' which
is in our case `rich-ai-api-key-openai-service'."
  (let* ((endpoint (rich-ai-get-in req [:api :endpoint]))
         (service (rich-ai-get-in req [:api :service]))
         (api-key-symbol (rich-ai-api-key-symbol service))
         (request-file (rich-ai-request-file 'request req))
         (command-fmt (concat "curl -s -X POST %s "
                              "-H 'Authorization: Bearer %s' "
                              "-H 'Content-Type: application/json' -d @%s")))
    (when (not (boundp api-key-symbol))
      ;; because we need a dynamic variable
      (defvar-1 api-key-symbol nil))
    (when (not (stringp (eval api-key-symbol)))
      ;; don't use `setq' here because we want to set the symbol
      ;; hold by the let-binded variable `api-key-symbol'
      (set api-key-symbol (auth-source-pick-first-password :host service)))
    (when (null (eval api-key-symbol))
      (signal 'rich-ai-error-api-key
              (format
               (concat "Do you have a line in ~/.authinfo.gpg file declaring "
                       "the API key of service `%s' like this: "
                       "machine %s password <api-key>")
               service service)))
    (list
     (format command-fmt endpoint (eval api-key-symbol) request-file)
     (format command-fmt endpoint "<api-key>" request-file))))

(defun rich-ai-request-send (req callback &optional callback-error info)
  "..."
  (interactive)
  (seq-let (command command-no-api-key) (rich-ai-request-command req)
    (rich-ai-write-request req)
    (rich-ai-write-command command-no-api-key req)
    (make-process
     :name "ai"
     :buffer (generate-new-buffer-name "rich-ai")
     :command (list "sh" "-c" command)
     :connection-type 'pipe
     :sentinel (rich-ai-sentinel req callback callback-error info))))

;;; AI Assistant UI

(defun rich-ai-uuid ()
  "Generate a random-based UUID using `uuidgen' linux utility."
  (interactive)
  (string-remove-suffix "\n" (shell-command-to-string "uuidgen")))

(defvar rich-ai-model "gpt-4o-mini" "...")
(defvar rich-ai-temperature nil "...")
(defvar rich-ai-api
  '(:service "openai"
    :endpoint "https://api.openai.com/v1/chat/completions"
    :default-model "gpt-4o-mini"
    :models ("gpt-4o-mini" "gpt-4o" "o1-mini" "o1"))
  "...")
(defvar rich-ai-apis
  '((:service "openai"
     :endpoint "https://api.openai.com/v1/chat/completions"
     :default-model "gpt-4o-mini"
     :models ("gpt-4o-mini" "gpt-4o" "o1-mini" "o1"))
    (:service "perplexity"
     :endpoint "https://api.perplexity.ai/chat/completions"
     :default-model "llama-3.1-sonar-small-128k-online"
     :models ("llama-3.1-sonar-small-128k-online"
              "llama-3.1-sonar-large-128k-online"
              "llama-3.1-sonar-huge-128k-online")))
  "...")
(defvar rich-ai-system-prompt nil "...")
(defvar rich-ai-dir (concat (temporary-file-directory) "rich-ai/") "...")

(defvar rich-ai-history-requests nil "...")
(defvar rich-ai-prompt-history-state [nil nil nil] "...")
(defvar rich-ai-pending-requests nil "...")
(defvar rich-ai-pending-timer nil "...")
(defvar rich-ai-conversations nil "...")
(defvar rich-ai-conversation-id nil "...")
(defvar rich-ai-org-property-date "AI_ASSISTANT_DATE" "...")
(defvar rich-ai-org-property-req "AI_ASSISTANT_REQ" "...")

(defun rich-ai-history-requests-set ()
  "..."
  (let* ((timestamp-files
          (directory-files-recursively rich-ai-dir "timestamp-.*")))
    (setq rich-ai-history-requests
          (thread-last
            timestamp-files
            (mapcar (lambda (f)
                      (string-match ".*/\\([^/]+\\)/timestamp-\\(.*\\)" f)
                      (cons (match-string 1 f)
                            (string-to-number (match-string 2 f)))))
            (seq-sort (lambda (t1 t2) (> (cdr t1) (cdr t2))))
            (mapcar 'car)))))

(defun rich-ai-history-previous (state &optional prompt discard-current)
  "..."
  (when (and prompt discard-current)
    (error (format "`prompt' and `discard-current' arguments cannot be both non-nil: %S, %S"
                   prompt discard-current)))
  (let ((prev-items (aref state 0))
        (current (aref state 1))
        (next-items (aref state 2)))
    (if prev-items
        (vector (cdr prev-items)
                (car prev-items)
                (delq nil (append
                           (list prompt (when (not discard-current) current))
                           next-items)))
      state)))

(defun rich-ai-history-next (state &optional prompt discard-current)
  "..."
  (when (and prompt discard-current)
    (error (format "`prompt' and `discard-current' arguments cannot be both non-nil: %S, %S"
                   prompt discard-current)))
  (let ((prev-items (aref state 0))
        (current (aref state 1))
        (next-items (aref state 2)))
    (if next-items
        (vector (delq nil (append
                           (list prompt (when (not discard-current) current))
                           prev-items))
                (car next-items)
                (cdr next-items))
      state)))

(defun rich-ai-prompt-current-buffer ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun rich-ai-prompt-current-req-uuid ()
  (when-let ((current (aref rich-ai-prompt-history-state 1)))
    (when (not (consp current)) current)))

(defun rich-ai-prompt-current ()
  (let ((current (aref rich-ai-prompt-history-state 1)))
    (cond
     ((null current) nil)
     ((consp current) (plist-get current :prompt))
     (t (rich-ai-request-read 'prompt `(:ai-dir ,rich-ai-dir :uuid ,current))))))

(defun rich-ai-prompt-current-goto-req-dir ()
  (interactive)
  (if-let* ((req-uuid (rich-ai-prompt-current-req-uuid))
            (req-dir (rich-ai-request-dir
                      `(:ai-dir ,rich-ai-dir :uuid ,req-uuid))))
      (progn
        (when (> (length (window-list)) 1)
          (delete-window))
        (dired-other-window req-dir))
    (message (concat "Current prompt is not associated with a request.  "
                     "Try navigating the prompt history with `M-p' and `M-n', "
                     "default binding of `rich-ai-prompt-previous' and `rich-ai-prompt-next'."))))

(defun rich-ai-prompt-history-state-set ()
  (setq rich-ai-prompt-history-state
        (vector rich-ai-history-requests nil nil)))

(defun rich-ai-prompt-discard-current-p ()
  (let ((current (aref rich-ai-prompt-history-state 1)))
    (when (not (or (null current) (consp current)))
      (let ((req `(:ai-dir ,rich-ai-dir :uuid ,current)))
        (if (not (condition-case nil (rich-ai-request-read 'prompt req) (error nil)))
            t)))))

(defun rich-ai-prompt-history (direction)
  (let (prompts f)
    (pcase direction
      ('previous (setq prompts (aref rich-ai-prompt-history-state 0))
                 (setq f 'rich-ai-history-previous))
      ('next (setq prompts (aref rich-ai-prompt-history-state 2))
             (setq f 'rich-ai-history-next)))
    (cond
     ((null prompts)
      (message "No more requests or prompts in history."))
     ((rich-ai-prompt-discard-current-p)
      (setq rich-ai-prompt-history-state
            (funcall f rich-ai-prompt-history-state nil 'discard-current))
      (rich-ai-prompt-history direction))
     (t (let* ((pcb (rich-ai-prompt-current-buffer))
               (pc (rich-ai-prompt-current))
               (prompt (when (or (null pc) (not (string= pcb pc)))
                         `(:prompt ,pcb))))
          (setq rich-ai-prompt-history-state
                (funcall f rich-ai-prompt-history-state prompt))
          (if (rich-ai-prompt-discard-current-p)
              (rich-ai-prompt-history direction)
            (erase-buffer)
            (insert (or (rich-ai-prompt-current) ""))))))))

(defun rich-ai-prompt-previous ()
  "..."
  (interactive)
  (rich-ai-prompt-history 'previous))

(defun rich-ai-prompt-next ()
  "..."
  (interactive)
  (rich-ai-prompt-history 'next))

(defun rich-ai-org-to-markdown (markdown-str)
  (let ((org-export-with-toc nil)
        (org-md-headline-style 'atx))
    ;; Default md backend uses `org-md-example-block' to export
    ;; example blocks and source blocks and it prefares 4 leading
    ;; spaces over triple backticks.  Here we redefine it to always
    ;; use triple backticks and the language name of the block when
    ;; there is one.
    (cl-letf (((symbol-function 'org-md-example-block)
               (lambda (block _contents info)
                 (let ((lang (org-element-property :language block))
                       (code (org-remove-indentation
                              (org-export-format-code-default block info))))
                   (format "```%s\n%s\n```" (or lang "") code)))))
      (string-trim
       (org-export-string-as markdown-str 'md nil)))))

(cl-defun rich-ai-request (&key prompt system-prompt exchanges
                                stream model temperature
                                api ai-dir)
  (when (null prompt)
    (error "You must provide a prompt via `:prompt' key to build a request."))
  (let* ((-system-prompt
          (or system-prompt (cdr-safe rich-ai-system-prompt) ""))
         (-messages
          `(,(when (not (string-empty-p -system-prompt))
               `(:role "system" :content ,(rich-ai-org-to-markdown -system-prompt)))
            ,@(seq-reduce
               (lambda (acc exchange)
                 (append acc
                         `((:role "user" :content ,(plist-get exchange :user))
                           (:role "assistant" :content ,(plist-get exchange :assistant)))))
               exchanges
               '())
            (:role "user" :content ,(rich-ai-org-to-markdown prompt))))
         (req-messages (apply 'vector (remq nil -messages))))
    `(:req (:stream ,(or (and stream t) :false)
            :model ,(or model rich-ai-model)
            :temperature ,(or temperature rich-ai-temperature)
            :messages ,req-messages)
      :api ,(or api rich-ai-api)
      :prompt ,prompt
      :system-prompt ,-system-prompt
      :exchanges ,exchanges
      :ai-dir ,(or ai-dir
                   rich-ai-dir
                   (concat (temporary-file-directory) "rich-ai/"))
      :uuid ,(rich-ai-uuid))))

(defun rich-ai-org-demote (org-str level)
  "Demote ORG-STR `org-mode' string to LEVEL level.

LEVEL must be 3 or 4."
  (when (not (seq-contains-p '(3 4) level))
    (error "`%S' not accepted.  `level' must be 3 or 4" level))
  (with-temp-buffer
    (org-mode)
    (save-excursion (insert org-str))
    (let ((headline-top-level
           (cond
            ((save-excursion (re-search-forward "^\\* .+" nil t)) 1)
            ((save-excursion (re-search-forward "^\\*\\* .+" nil t)) 2)
            ((save-excursion (re-search-forward "^\\*\\*\\* .+" nil t)) 3)
            (t 4))))
      (when (< headline-top-level 4)
        (while (re-search-forward "^\\*+ " nil t)
          (save-excursion
            (goto-char (match-beginning 0))
            (insert (make-string (- level headline-top-level) ?*))))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun rich-ai-insert-conversation (req &optional title append start-from)
  ""
  (rich-ai-request-check req)
  (let* ((uuid (plist-get req :uuid))
         (format-exchange
          (lambda (exchange)
            (list (rich-ai-org-demote (plist-get exchange :prompt) 4)
                  (rich-ai-org-demote (plist-get exchange :response) 4))))
         (conversation
          (mapcar format-exchange (rich-ai-request-conversation req)))
         (conversation
          (if (or append start-from) (last conversation) conversation)))
    (if (and append
             (progn (goto-char (point-min))
                    (re-search-forward
                     (format "^:%s: \\(.*\\)" rich-ai-org-property-req) nil t)))
        (progn
          (replace-match uuid nil nil nil 1)
          (goto-char (point-max)))
      (insert
       "** " (or title "Conversation") "\n"
       ":PROPERTIES:\n"
       ":" rich-ai-org-property-date ": " (or (rich-ai-request-date req) "")
       "\n"
       ":" rich-ai-org-property-req ": " uuid "\n"
       ":END:\n"))
    (dolist (exchange conversation)
      (seq-let (prompt response) exchange
        (insert "*** prompt\n\n" prompt)
        (ensure-empty-lines 1)
        (insert "*** response\n\n" response)
        (ensure-empty-lines 1)))))

(defun rich-ai-pending-remove (req)
  "..."
  (setq rich-ai-pending-requests
        (seq-remove (lambda (p)
                      (string=
                       (rich-ai-get-in p [:req :uuid])
                       (plist-get req :uuid)))
                    rich-ai-pending-requests)))

(defun rich-ai-running-p ()
  ""
  (when-let ((proc (plist-get (car-safe rich-ai-pending-requests) :proc)))
    (and (processp proc) (buffer-name (process-buffer proc)) t)))

(defun rich-ai-kill-last-request ()
  ""
  (interactive)
  (when (rich-ai-running-p)
    (message "Killing last request")
    (let ((proc (plist-get (car-safe rich-ai-pending-requests) :proc)))
      ;; So that the error signaled in sentinel is not printed
      ;; in the echo area.
      (setq inhibit-message t)
      (kill-process proc)))
  ;; After 0.2s, the error has been signaled and the message logged,
  ;; so we can reestablish `inhibit-message' default value
  (run-at-time 0.2 nil (lambda () (setq inhibit-message nil))))

(defun rich-ai-mode-line-waiting (action)
  "..."
  (pcase action
    ('maybe-start
     (when (null rich-ai-pending-timer)
       (setq rich-ai-pending-timer
             (run-with-timer
              0 0.66
              (let ((idx 0))
                (lambda ()
                  (progn
                    (setq global-mode-string
                          `(:propertize
                            ,(format "• AI Assistant.%s"
                                     (make-string (mod idx 3) ?.))
                            face (:weight bold)))
                    (force-mode-line-update 'all)
                    (cl-incf idx))))))))
    ('maybe-stop
     (when (and (not (rich-ai-running-p)) rich-ai-pending-timer)
       (cancel-timer rich-ai-pending-timer)
       (setq rich-ai-pending-timer nil)
       (setq global-mode-string nil)
       (force-mode-line-update 'all)))))

(defun rich-ai-conversation (action title &optional req-uuid)
  "...

See variables `rich-ai-conversations' and `rich-ai-dir'."
  (cond
   ((seq-some (lambda (c) (equal title (plist-get (cdr c) :title)))
              rich-ai-conversations)
    (error "Conversation with title `%s' already exists in `rich-ai-conversations'"
           title))
   ((not (seq-contains-p [start start-from continue-from] action))
    (error "Conversation `action' must be `start', `start-from' or `continue-from' not `%S'"
           action))
   ((and (eq action 'start) req-uuid)
    (error "When action is `start', `req-uuid' argument must be nil or omitted, not `%s'"
           req-uuid))
   ((seq-contains-p [start-from continue-from] action)
    (let ((req `(:ai-dir ,rich-ai-dir :uuid ,req-uuid)))
      (when (null req-uuid)
        (error "When action is `%s', `req-uuid' argument is mandatory."
               action req-uuid))
      (condition-case err
          (rich-ai-request-check req)
        (error
         (error "Cannot start nor continue from that request.  %s"
                (error-message-string err)))))))
  (let ((conversation-id (rich-ai-uuid)))
    (push (cons conversation-id
                `(:title ,title :action ,action :last-req-uuid ,req-uuid))
          rich-ai-conversations)
    (setq rich-ai-conversation-id conversation-id)))

(defun rich-ai-conversation-title (conversation-id)
  "..."
  (rich-ai-get-in rich-ai-conversations `(,conversation-id :title)))

(defun rich-ai-conversation-action (conversation-id)
  "..."
  (rich-ai-get-in rich-ai-conversations `(,conversation-id :action)))

(defun rich-ai-conversation-last-req (conversation-id)
  "..."
  (when-let ((uuid (rich-ai-get-in
                    rich-ai-conversations `(,conversation-id :last-req-uuid))))
    `(:uuid ,uuid :ai-dir ,rich-ai-dir)))

(defun rich-ai-conversation-update (info req)
  "...

See `rich-ai-conversation' and `rich-ai-conversations'."
  (let ((conversation-id (plist-get info :conversation-id))
        (req-uuid (plist-get req :uuid)))
    (when-let ((conversation-data
                (cdr (assoc conversation-id rich-ai-conversations))))
      (let ((data (thread-first
                    conversation-data
                    (plist-put :action 'continue-from)
                    (plist-put :last-req-uuid req-uuid))))
        (setf conversation-data data)))))

(defun rich-ai-conversation-locked-p (conversation-id)
  (seq-some
   (lambda (r)
     (when-let ((id (plist-get r :conversation-id)))
       (string= conversation-id id)))
   rich-ai-pending-requests))

(cl-defun rich-ai-send-request (&key req callback info)
  "...

The CALLBACK function must call the following three functions
in that order

- `rich-ai-pending-requests',
- `rich-ai-conversation-update' and
- `rich-ai-mode-line-waiting'

with `rich-ai-pending-requests' being called first.

Here's a valid CALLBACK function that appends responses
in the buffer \"*ai*\":

    (lambda (req resp info)
        (with-current-buffer (get-buffer-create \"*ai*\")
          (org-mode)
          (save-excursion
            (widen)
            (goto-char (point-max))
            (rich-ai-insert-conversation req)
            (save-buffer)))
        (rich-ai-pending-remove req)
        (rich-ai-conversation-update info req)
        (rich-ai-mode-line-waiting \\='maybe-stop)
        (message \"AI assistant received a response\"))

If REQ is part of a conversation present in `rich-ai-conversations',
the conversation id must be specified in INFO argument as value of
`:conversation-id' key.

For instance, if \"conversation-id-foo\" is the id of the some
conversation and REQ is part of that conversation (the next request
in the conversation), INFO argument must be a plist that looks
like this:

    (:conversation-id \"conversation-id-foo\" ...)"
  (let ((conversation-id (plist-get info :conversation-id)))
    (if (rich-ai-conversation-locked-p conversation-id)
        (progn
          (message "Cannot send two concurrent requests in the same conversation.")
          (let ((inhibit-message t))
            (message "req: %S\nconversation: %s" req conversation-id)))
      (let ((callback-error (lambda (req err info)
                              (rich-ai-pending-remove req)
                              (rich-ai-mode-line-waiting 'maybe-stop))))
        (push (list :req req
                    :conversation-id conversation-id
                    :proc (rich-ai-request-send req callback callback-error info))
              rich-ai-pending-requests)
        (push (plist-get req :uuid) rich-ai-history-requests)
        (rich-ai-prompt-history-state-set)
        (rich-ai-mode-line-waiting 'maybe-start)))))

(defun rich-ai-conversation-buffer-name (conversation-id)
  (when-let ((title (rich-ai-conversation-title conversation-id)))
    (format "*ai %s*" title)))

(defun rich-ai-show-current-conversation ()
  (interactive)
  (let ((buff-name (rich-ai-conversation-buffer-name rich-ai-conversation-id))
        (title (rich-ai-conversation-title rich-ai-conversation-id))
        (action (rich-ai-conversation-action rich-ai-conversation-id))
        (last-req (rich-ai-conversation-last-req rich-ai-conversation-id)))
    (cond
     ((null buff-name) (message "No current conversation to display."))
     ((null last-req) (message "Current conversation is empty."))
     (t (when (not (get-buffer buff-name))
          (with-current-buffer (get-buffer-create buff-name)
            (save-excursion
              (org-mode)
              (rich-ai-insert-conversation
               last-req title nil (eq action 'start-from)))))
        (when (> (length (window-list)) 1)
          (delete-window))
        (select-window
         (display-buffer buff-name '(nil (inhibit-same-window . t))))))))

(defun rich-ai-show-current-conversation-in-req-history ()
  (interactive)
  (if-let* ((req-uuid (rich-ai-prompt-current-req-uuid))
            (req `(:ai-dir ,rich-ai-dir :uuid ,req-uuid)))
      (if (condition-case nil (rich-ai-request-check req) (error nil))
          (let* ((title "Current conversation in history")
                 (buff-name (get-buffer-create (format "*ai <%s>*" title))))
            (with-current-buffer buff-name
              (save-excursion
                (erase-buffer)
                (org-mode)
                (rich-ai-insert-conversation req title)))
            (when (> (length (window-list)) 1)
              (delete-window))
            (select-window
             (display-buffer buff-name '(nil (inhibit-same-window . t)))))
        (message (concat "Current prompt is associated with a failed or missing request.  "
                         "Try navigating the prompt history with `M-p' and `M-n', "
                         "default binding of `rich-ai-prompt-previous' and `rich-ai-prompt-next'.")))
    (message (concat "Current prompt is not associated with a request.  "
                     "Try navigating the prompt history with `M-p' and `M-n', "
                     "default binding of `rich-ai-prompt-previous' and `rich-ai-prompt-next'."))))

(defun rich-ai-show-last-conversations ()
  (interactive)
  (let* ((num-of-days (read-number "Enter the number of days: "))
         (conversations (rich-ai-conversations num-of-days))
         (buff-name (get-buffer-create "*ai <Last conversations>*")))
    (with-current-buffer buff-name
      (save-excursion
        (erase-buffer)
        (org-mode)
        (dolist (req-uuid conversations)
          (rich-ai-insert-conversation `(:ai-dir ,rich-ai-dir :uuid ,req-uuid)))))
    (when (> (length (window-list)) 1)
      (delete-window))
    (select-window
     (display-buffer buff-name '(nil (inhibit-same-window . t))))))

(defun rich-ai-show-last-requests ()
  (interactive)
  (let* ((num-of-days (read-number "Enter the number of days: "))
         (requests (rich-ai-requests num-of-days))
         (buff-name (get-buffer-create "*ai <Last requests>*")))
    (with-current-buffer buff-name
      (save-excursion
        (erase-buffer)
        (org-mode)
        (dolist (req-uuid requests)
          (rich-ai-insert-conversation
           `(:ai-dir ,rich-ai-dir :uuid ,req-uuid)
           "Request" nil 'start-from))))
    (when (> (length (window-list)) 1)
      (delete-window))
    (select-window
     (display-buffer buff-name '(nil (inhibit-same-window . t))))))

(defun rich-ai-conversation-switch ()
  (interactive)
  (if (null rich-ai-conversations)
      (message "No conversation to switch to yet.")
    (let* ((conversations
            (mapcar (lambda (c) (cons (plist-get (cdr c) :title) (car c)))
                    rich-ai-conversations))
           (title (completing-read "Conversation title: "
                                   (mapcar #'car conversations)
                                   nil 'require-match)))
      (setq rich-ai-conversation-id
            (alist-get title conversations nil nil #'string=)))))

(defun rich-ai-conversation-start ()
  "..."
  (interactive)
  (rich-ai-conversation
   'start (read-string "Enter a conversation title: ")))

(defun rich-ai-conversation-start-from-req-history ()
  "..."
  (interactive)
  (if-let ((req-uuid (rich-ai-prompt-current-req-uuid)))
      (rich-ai-conversation
       'start-from (read-string "Enter a conversation title: ") req-uuid)
    (message (concat "Current prompt is not associated with a request.  "
                     "Try navigating the prompt history with `M-p' and `M-n', "
                     "default binding of `rich-ai-prompt-previous' and `rich-ai-prompt-next'."))))

(defun rich-ai-conversation-continue-from-req-history ()
  "..."
  (interactive)
  (if-let ((req-uuid (rich-ai-prompt-current-req-uuid)))
      (rich-ai-conversation
       'continue-from (read-string "Enter a conversation title: ") req-uuid)
    (message (concat "Current prompt is not associated with a request.  "
                     "Try navigating the prompt history with `M-p' and `M-n', "
                     "default binding of `rich-ai-prompt-previous' and `rich-ai-prompt-next'."))))

(defun rich-ai-conversation-pause ()
  "..."
  (interactive)
  (setq rich-ai-conversation-id nil))

(defun rich-ai-api-set ()
  "..."
  (interactive)
  (if-let* ((services
             (mapcar (lambda (api) (plist-get api :service)) rich-ai-apis)))
      (let* ((service (completing-read
                       (format "Choose an API from the following services: "
                               services)
                       services nil 'require-match))
             (api (seq-some (lambda (api)
                              (when (string= (plist-get api :service) service)
                                api))
                            rich-ai-apis))
             (default-model (plist-get api :default-model)))
        (setq rich-ai-api api)
        (when default-model (setq rich-ai-model default-model)))
    (error (format "`rich-ai-apis' variable must be a list of API specifications like this

((:service \"openai\"
  :endpoint \"https://api.openai.com/v1/chat/completions\"
  :default-model \"gpt-4o-mini\"
  :models (\"gpt-4o-mini\" \"gpt-4o\" \"o1-mini\" \"o1\"))
 (:service \"perplexity\"
  :endpoint \"https://api.perplexity.ai/chat/completions\"
  :default-model \"llama-3.1-sonar-small-128k-online\"
  :models (\"llama-3.1-sonar-small-128k-online\"
           \"llama-3.1-sonar-large-128k-online\"
           \"llama-3.1-sonar-huge-128k-online\")))

not `%S'" rich-ai-apis))))

(defun rich-ai-model-set ()
  "..."
  (interactive)
  (let* ((service (plist-get rich-ai-api :service))
         (models (plist-get rich-ai-api :models))
         (model (completing-read
                 (format "Choose a model for the service `%s': " service)
                 models)))
    (setq rich-ai-model model)))

(defun rich-ai-temperature-set ()
  "..."
  (interactive)
  (let ((temperature
         (read-string "Enter a float number [0-2] or (leave blank for none) to set model temperature: ")))
    (setq rich-ai-temperature
          (when (not (string-empty-p temperature))
            (string-to-number temperature)))))

(defvar rich-ai-system-prompts nil
  "Alist of (\"title\" . \"system prompt\") to choose from.

For instance we can set `rich-ai-system-prompts' to:

    ((\"writer\" . \"You're a good writer who only writes in Italian.\")
     (\"programmer\" . \"You're a programmer who only answers with code snippets.\"))

See `rich-ai-system-prompt-set' command.")

(defun rich-ai-system-prompt-set ()
  (interactive)
  (let ((err (format "`rich-ai-system-prompts' variable must be nil or an alist like this

((\"writer\" . \"You're a good writer who only writes in Italian.\")
 (\"programmer\" . \"You're a programmer who only answers with code snippets.\"))

not `%S'" rich-ai-system-prompts)))
    (cond
     ((null rich-ai-system-prompts)
      (message "There's no system prompt to select from `rich-ai-system-prompts' variable which is nil."))
     ((not (listp rich-ai-system-prompts)) (error err))
     (t (if-let* ((system-prompt-titles
                   (delq nil (mapcar 'car-safe rich-ai-system-prompts))))
            (let ((title (completing-read
                          "System prompt title (leave blank for none): "
                          system-prompt-titles)))
              (setq rich-ai-system-prompt (assoc title rich-ai-system-prompts)))
          (error err))))))

(transient-define-prefix rich-ai-menu ()
  "Insert menu"
  [["Conversation"
    ("n" "Start new conversation" rich-ai-conversation-start)
    ("s" "Start conversation from current request in history" rich-ai-conversation-start-from-req-history)
    ("c" "Continue conversation from current request in history" rich-ai-conversation-continue-from-req-history)
    ("SPC" "Pause current conversation" rich-ai-conversation-pause)
    ("TAB" "Switch conversation" rich-ai-conversation-switch)]]
  [["Conversations and requests"
    ("v" "Show current conversation" rich-ai-show-current-conversation)
    ("h" "Show current conversation in history" rich-ai-show-current-conversation-in-req-history)
    ("l" "Show last conversations" rich-ai-show-last-conversations)
    ("r" "Show last requests" rich-ai-show-last-requests)
    ("k" "Kill last request" rich-ai-kill-last-request)
    ("g" "Go to directory of current request in history" rich-ai-prompt-current-goto-req-dir)
    ]]
  [["Model"
    ("a" "Set current API" rich-ai-api-set)
    ("m" "Set model for current API" rich-ai-model-set)
    ("t" "Set temperature" rich-ai-temperature-set)
    ("p" "Set system prompt" rich-ai-system-prompt-set)
    ]]
  )

(defun rich-ai-req-at-point-uuid ()
  (if-let* ((req-uuid (org-entry-get nil rich-ai-org-property-req))
            (req-dir (rich-ai-request-dir
                      `(:ai-dir ,rich-ai-dir :uuid ,req-uuid))))
      (if (file-exists-p req-dir)
          req-uuid
        (error "Request `%s' doesn't exist." req-dir))
    (error "No request at point found.")))

(defun rich-ai-req-at-point-start-conversation ()
  "..."
  (interactive)
  (when-let ((req-uuid (rich-ai-req-at-point-uuid)))
    (rich-ai-conversation
     'start-from (read-string "Enter a conversation title: ") req-uuid))
  (rich-ai))

(defun rich-ai-req-at-point-continue-conversation ()
  "..."
  (interactive)
  (when-let ((req-uuid (rich-ai-req-at-point-uuid)))
    (rich-ai-conversation
     'continue-from (read-string "Enter a conversation title: ") req-uuid))
  (rich-ai))

(defun rich-ai-req-at-point-show-requests ()
  (interactive)
  (when-let* ((req-uuid (rich-ai-req-at-point-uuid))
              (req `(:ai-dir ,rich-ai-dir :uuid ,req-uuid))
              (requests
               (mapcar
                (lambda (exchange)
                  `(:ai-dir ,rich-ai-dir :uuid ,(plist-get exchange :uuid)))
                (rich-ai-request-conversation req)))
              (buff (get-buffer-create "*ai Requests of conversation at point*")))
    (with-current-buffer buff
      (erase-buffer)
      (org-mode)
      (save-excursion
        (dolist (req requests)
          (rich-ai-insert-conversation req "Request" nil 'start-from))))
    (select-window
     (display-buffer buff '(nil (inhibit-same-window . t))))))

(defun rich-ai-req-at-point-show-perplexity-citations ()
  (interactive)
  (when-let* ((req-uuid (rich-ai-req-at-point-uuid))
              (req `(:ai-dir ,rich-ai-dir :uuid ,req-uuid)))
    (if-let ((citations (rich-ai-request-perplexity-citations req)))
        (let ((buff (get-buffer-create "*ai Perplexity citations*")))
          (with-current-buffer buff
            (erase-buffer)
            (org-mode)
            (save-excursion
              (dolist (citation citations)
                (insert (format "- %s\n" citation)))))
          (display-buffer buff '(nil (inhibit-same-window . t))))
      (message "No citations for `%s' conversation."
               (rich-ai-request-dir req)))))

(defun rich-ai-req-at-point-goto ()
  (interactive)
  (when-let ((req-uuid (rich-ai-req-at-point-uuid))
             (req-dir (rich-ai-request-dir
                       `(:ai-dir ,rich-ai-dir :uuid ,req-uuid))))
    (dired req-dir)))

(transient-define-prefix rich-ai-req-at-point-menu ()
  "Insert menu"
  [["Conversation/Request at point"
    ("s" "Start conversation from request at point" rich-ai-req-at-point-start-conversation)
    ("c" "Continue conversation from request at point" rich-ai-req-at-point-continue-conversation)
    ("r" "Show requests of conversation at point" rich-ai-req-at-point-show-requests)
    ("C" "Show Perplexity citations of conversation at point" rich-ai-req-at-point-show-perplexity-citations)
    ("g" "Go to directory of request at point" rich-ai-req-at-point-goto)
    ]]
  )

(defun rich-ai-conversation-exchanges (conversation-id)
  (when-let ((last-req (rich-ai-conversation-last-req
                        conversation-id))
             (conversation (rich-ai-request-conversation last-req)))
    (pcase (rich-ai-conversation-action conversation-id)
      ('start-from (vector (aref conversation (1- (length conversation)))))
      ('continue-from conversation))))

(defun rich-ai-send ()
  ""
  (interactive)
  (rich-ai-send-request
   :req (rich-ai-request
         :prompt (buffer-substring-no-properties (point-min) (point-max))
         :exchanges (rich-ai-conversation-exchanges rich-ai-conversation-id))
   :info `(:conversation-id ,rich-ai-conversation-id)
   :callback (lambda (req resp info)
               (let* ((conversation-id (plist-get info :conversation-id))
                      (title (rich-ai-conversation-title conversation-id))
                      (append (and conversation-id t))
                      (buff-name
                       (or (rich-ai-conversation-buffer-name conversation-id)
                           "*ai*"))
                      (buff-already-exist-p (get-buffer buff-name))
                      (buff (get-buffer-create buff-name)))
                 (with-current-buffer buff
                   (save-excursion
                     (if (not buff-already-exist-p)
                         (progn
                           (org-mode)
                           (rich-ai-insert-conversation req title))
                       (widen)
                       (goto-char (point-max))
                       (rich-ai-insert-conversation req title append))))
                 (cond
                  ((equal (window-buffer (selected-window)) buff) nil)
                  ((get-buffer-window buff)
                   (with-selected-window (get-buffer-window buff)
                     (goto-char (point-max))
                     (when (re-search-backward "^\\*\\*\\* response" nil t)
                       (recenter-top-bottom 0))))
                  (t (let ((w (selected-window)))
                       (pop-to-buffer buff)
                       (goto-char (point-max))
                       (when (re-search-backward "^\\*\\*\\* response" nil t)
                         (recenter-top-bottom 0))
                       (select-window w))))
                 (rich-ai-pending-remove req)
                 (rich-ai-conversation-update info req)
                 (rich-ai-mode-line-waiting 'maybe-stop)
                 (message "AI assistant received a response"))))
  (erase-buffer)
  (when (> (length (window-list)) 1)
    (delete-window))
  (message "AI assistant sent a request"))

(defvar rich-ai-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-p") 'rich-ai-prompt-previous)
    (define-key map (kbd "M-n") 'rich-ai-prompt-next)
    (define-key map (kbd "C-c C-c") #'rich-ai-send)
    map)
  "Keymap for `rich-ai-mode'.")

(define-derived-mode rich-ai-mode org-mode "Rich AI"
  "Rich AI assitant mode."
  (setq
   mode-line-format
   '(" "
     mode-line-buffer-identification
     " "
     (:eval (plist-get rich-ai-api :service))
     "/"
     (:eval rich-ai-model)
     (:eval (when rich-ai-temperature (format " %s" rich-ai-temperature)))
     (:eval (when rich-ai-conversation-id
              (format " <%s>"
                      (rich-ai-conversation-title rich-ai-conversation-id))))
     (:eval (when-let ((system-prompt-title (car-safe rich-ai-system-prompt)))
              (format " * %s" system-prompt-title)))
     " "
     mode-line-misc-info))
  (rich-ai-history-requests-set)
  (rich-ai-prompt-history-state-set))

(defvar rich-ai-prompt-buffer-name "*Rich AI*" "...")

(defun rich-ai (&optional arg)
  ""
  (interactive "P")
  (cond
   (arg (call-interactively 'rich-ai-req-at-point-menu))
   ((string= (buffer-name) rich-ai-prompt-buffer-name)
    (call-interactively 'rich-ai-menu))
   (t (let ((rich-ai-buffer-p (get-buffer rich-ai-prompt-buffer-name))
            (buff (get-buffer-create rich-ai-prompt-buffer-name)))
        (select-window
         (display-buffer-at-bottom buff '(display-buffer-below-selected
                                          (window-height . 4))))
        (when (not rich-ai-buffer-p)
          (rich-ai-mode))))))

(define-key input-decode-map "\C-i" [C-i])
(global-set-key [C-i] #'rich-ai)

(defun rich-ai-paths (num-of-days)
  (let* ((today (calendar-current-date))
         (midnight (encode-time `(0 0 0 ,(nth 1 today) ,(nth 0 today) ,(nth 2 today))))
         (timestamp-start
          (thread-last (days-to-time (1- num-of-days))
                       (time-subtract midnight)
                       (float-time)))
         (timestamp-files
          (directory-files-recursively rich-ai-dir "timestamp-.*")))
    (thread-last
      timestamp-files
      (mapcar (lambda (f)
                (string-match ".*/\\([^/]+\\)/timestamp-\\(.*\\)" f)
                (cons (match-string 1 f)
                      (string-to-number (match-string 2 f)))))
      (seq-sort (lambda (t1 t2) (< (cdr t1) (cdr t2))))
      (mapcar (lambda (r)
                (when (< timestamp-start (cdr r))
                  (let ((req `(:ai-dir ,rich-ai-dir
                               :uuid ,(car r))))
                    (rich-ai-request-conversation-path req)))))
      (delq nil))))

(defun rich-ai-requests (num-of-days)
  (mapcar (lambda (p) (aref p (1- (length p))))
          (rich-ai-paths num-of-days)))

(defun rich-ai-conversations (num-of-days)
  (rich-ai-conversations-keep (rich-ai-paths num-of-days)))

(defun rich-ai-conversations-keep (paths)
  (let ((tail (reverse paths))
        alist-paths-of-kept
        kept)
    (while tail
      (let ((path-vec (pop tail)))
        (when (not (map-nested-elt alist-paths-of-kept path-vec))
          (push path-vec kept)
          (setq alist-paths-of-kept
                (append alist-paths-of-kept
                        (rich-ai-request-conversation-path-alist path-vec))))))
    (mapcar (lambda (path) (aref path (1- (length path))))
            kept)))

(provide 'eden)

;;; eden.el ends here
