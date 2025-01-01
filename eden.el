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

(defun eden-json-encode (object)
  "..."
  ;; ...
  (let ((json-false :false)
        (json-encoding-pretty-print "  ")
        (json-encoding-pretty-print t))
    (json-encode object)))

(defun eden-json-read ()
  "..."
  ;; ...
  (let ((json-false :false)
        (json-key-type 'keyword)
        (json-object-type 'plist)
        (json-array-type 'vector))
    (json-read)))

(defun eden-request-dir (req)
  "..."
  (let ((ai-dir (plist-get req :ai-dir))
        (uuid (plist-get req :uuid)))
    (when (or (not (stringp ai-dir)) (not (stringp uuid)))
      (error "Request mal formed.  `req' must contain `:ai-dir' and `:uuid' keys and their value must be strings: %S"
             req))
    (concat (file-name-as-directory ai-dir) uuid "/")))

(defun eden-request-file (file req)
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
        (concat (eden-request-dir req) filename)
      (error "`file' argument must be one of %s, not `%s'"
             (mapcar #'car filenames) file))))

(defun eden-request-read (file req)
  "..."
  (let* ((-file (eden-request-file file req)))
    (if (not (file-exists-p -file))
        (error "Missing `%s' file." -file)
      (with-temp-buffer
        (insert-file-contents (eden-request-file file req))
        (if (string= (file-name-extension -file) "json")
            (eden-json-read)
          (buffer-substring-no-properties (point-min) (point-max)))))))

(defalias 'eden-get-in 'map-nested-elt)

(defun eden-request-assistant-content (resp)
  "..."
  (eden-get-in resp [:choices 0 :message :content]))

(defun eden-request-user-content (request)
  "..."
  (let* ((messages (plist-get request :messages))
         (last-message (aref messages (1- (length messages)))))
    (plist-get last-message :content)))

(defun eden-request-check (req)
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
  (let ((req-dir (eden-request-dir req)))
    (cond
     ((not (file-exists-p (eden-request-dir req)))
      (error "Request `%s' doesn't exist." req-dir))
     ((file-exists-p (eden-request-file 'error req))
      (error "Request `%s' has failed in a prior attempt.  See why in `%s' file."
             req-dir (eden-request-file 'error req)))
     ((not (file-exists-p (eden-request-file 'prompt req)))
      (error "Missing `%s' file." (eden-request-file 'prompt req)))
     ((not (file-exists-p (eden-request-file 'request req)))
      (error "Missing `%s' file." (eden-request-file 'request req)))
     ((not (file-exists-p (eden-request-file 'response req)))
      (error "Missing `%s' file." (eden-request-file 'response req)))
     ((not (file-exists-p (eden-request-file 'response-org req)))
      (error "Missing `%s' file." (eden-request-file 'response-org req)))
     ((not (file-exists-p (eden-request-file 'exchanges req)))
      (error "Missing `%s' file." (eden-request-file 'exchanges req)))
     (t t))))

(defun eden-request-conversation (req)
  (eden-request-check req)
  (let* ((exchanges (eden-request-read 'exchanges req))
         (last-exchange
          `((:uuid ,(plist-get req :uuid)
             :prompt ,(eden-request-read 'prompt req)
             :user ,(eden-request-user-content
                     (eden-request-read 'request req))
             :assistant ,(eden-request-assistant-content
                          (eden-request-read 'response req))
             :response ,(eden-request-read 'response-org req)))))
    (apply 'vector (append exchanges last-exchange))))

(defun eden-request-conversation-path (req)
  (when (condition-case nil (eden-request-check req) (error nil))
    (let* ((uuids (mapcar (lambda (exchange) (plist-get exchange :uuid))
                          (eden-request-read 'exchanges req)))
           (last-uuid (list (plist-get req :uuid))))
      (apply 'vector (append uuids last-uuid)))))

(defun eden-request-conversation-path-alist (path)
  (when path
    (let* ((tail (append (reverse path) '()))
           (alist (list (cons (pop tail) t))))
      (while tail
        (setq alist (list (cons (pop tail) alist))))
      alist)))

(defun eden-request-perplexity-citations (req)
  (let ((ai-dir (plist-get req :ai-dir)))
    (seq-reduce
     (lambda (acc exchange)
       (let* ((uuid-exchange (plist-get exchange :uuid))
              (req-exchange `(:ai-dir ,ai-dir :uuid ,uuid-exchange)))
         (if (condition-case nil (eden-request-check req-exchange) (error nil))
             (let* ((resp (eden-request-read 'response req-exchange))
                    (citations (plist-get resp :citations)))
               (append acc citations '()))
           acc)))
     (eden-request-conversation req)
     '())))

(defun eden-request-timestamp (req)
  (when-let ((filename
              (car (directory-files
                    (eden-request-dir req) nil "timestamp-.*"))))
    (string-to-number (string-trim-left filename "timestamp-"))))

(defun eden-request-date (req)
  (when-let ((timestamp (eden-request-timestamp req)))
    (format-time-string "[%Y-%m-%d %a]" (seconds-to-time (floor timestamp)))))

(defun eden-request-write (file req content)
  (let ((inhibit-message t)
        (message-log-max nil)
        (file-path (if (eq file 'timestamp)
                       (format "%stimestamp-%s"
                               (eden-request-dir req) (time-to-seconds))
                     (eden-request-file file req))))
    (make-directory (eden-request-dir req) 'parents)
    (with-temp-buffer
      (insert content)
      (write-file file-path))))

(defun eden-write-request (req)
  "..."
  (let ((request (eden-json-encode (plist-get req :req)))
        (api (eden-json-encode (plist-get req :api)))
        (prompt (plist-get req :prompt))
        (system-prompt (or (plist-get req :system-prompt) ""))
        (exchanges (eden-json-encode (plist-get req :exchanges))))
    (eden-request-write 'timestamp req "")
    (eden-request-write 'request req request)
    (eden-request-write 'api req api)
    (eden-request-write 'prompt req prompt)
    (eden-request-write 'system-prompt req system-prompt)
    (eden-request-write 'exchanges req exchanges)))

(defun eden-write-command (command-no-api-key req)
  "..."
  (message "%s" command-no-api-key)
  (eden-request-write 'command req command-no-api-key))

(defun eden-markdown-to-org (markdown-str)
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

(defun eden-org-replace-perplexity-citations (org-str citations)
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

(defun eden-write-response (resp-str resp req)
  "..."
  (eden-request-write 'response req resp-str)
  (let* ((assistant-content (eden-request-assistant-content resp))
         (response-org (eden-markdown-to-org assistant-content))
         (citations (plist-get resp :citations))
         (response-org
          (if (and citations (vectorp citations))
              (eden-org-replace-perplexity-citations response-org citations)
            response-org)))
    (eden-request-write 'response-org req response-org)))

(defun eden-write-error (err req)
  "..."
  (eden-request-write 'error req (eden-json-encode err)))

(defvar eden-errors
  '((eden-error-api . "API error")
    (eden-error-api-key . "Error using AI assitant: API key not set correctly")
    (eden-error-callback . "Error while calling callback function in sentinel")
    (eden-error-callback-error . "Error while calling callback-error function when signaling an error in sentinel")
    (eden-error-json-read . "Error while parsing JSON in process buffer")
    (eden-error-process . "The process did not finished correctly")
    (eden-error-process-buffer . "The process buffer got killed while processing the request")))

(dolist (err eden-errors)
  (define-error (car err) (cdr err)))

(cl-defun eden-error-log-and-signal (type req process
                                             &key error event process-stdout
                                             callback-error info)
  ""
  (let* ((error-function
          (lambda (type req error event process-stdout original-error)
            (delq nil
                  `(:type ,(symbol-name type)
                    :message ,(alist-get type eden-errors)
                    :directory ,(eden-request-dir req)
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
         (setq type 'eden-error-callback-error)
         (setq err (funcall error-function type req -error nil nil err)))))
    (eden-write-error err req)
    (signal type err)))

(defmacro eden-sentinel (req callback callback-error info)
  "Return a sentinel to be used in `eden-request-send'.

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
         in `eden-error-log-and-signal'.  For instance if we provid
         OpenAI with a wrong API key `err' looks like this:

             (:type \"eden-error-api\"
              :message \"API error\"
              :directory \"/tmp/eden-VKTqOa/uuid-foo/\"
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

         See `eden-errors' and `eden-error-log-and-signal'.
- info  - plist of data to act on, can be nil."
  `(lambda (process event)
     (let ((stdout (lambda (process)
                     (with-current-buffer (process-buffer process)
                       (buffer-string)))))
       (cond
        ((not (buffer-name (process-buffer process)))
         (eden-error-log-and-signal
          'eden-error-process-buffer ,req process
          :callback-error ,callback-error
          :info ,info))
        ((string= event "finished\n")
         (let ((resp (condition-case err
                         (with-current-buffer (process-buffer process)
                           (goto-char (point-min))
                           (eden-json-read))
                       (error (eden-error-log-and-signal
                               'eden-error-json-read ,req process
                               :error err
                               :process-stdout (funcall stdout process)
                               :callback-error ,callback-error
                               :info ,info)))))
           (if-let ((err (plist-get resp :error)))
               (eden-error-log-and-signal
                'eden-error-api ,req process
                :error err
                :callback-error ,callback-error
                :info ,info)
             (condition-case err
                 (progn
                   (eden-write-response (funcall stdout process) resp ,req)
                   (kill-buffer (process-buffer process))
                   (funcall ,callback ,req resp ,info))
               (error (eden-error-log-and-signal
                       'eden-error-callback ,req process
                       :error err
                       :callback-error ,callback-error
                       :info ,info))))))
        (t (eden-error-log-and-signal
            'eden-error-process ,req process
            :process-stdout (funcall stdout process)
            :event event
            :callback-error ,callback-error
            :info ,info))))))

(defun eden-api-key-symbol (service)
  "Return the symbol we use for holding api key for SERVICE service.

It is used in `eden-request-command'.

When we want to use `eden-request-send' programmatically without
asking the user (and so gpg) for the encrytped key in ~/.authinfo.gpg
file we can use `eden-api-key-symbol' to set the api key like this
assumming SERVICE is \"openai\":

    (let ((api-key-symbol (eden-api-key-symbol \"openai\")))
      (defvar-1 api-key-symbol nil)
      (set api-key-symbol \"secret-api-key\")
      nil)"
  (intern (format "eden-api-key-%s" service)))

(defun eden-request-command (req)
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

The AI api key is stored in `eden-api-key-<service-name>' which
is in our case `eden-api-key-openai-service'."
  (let* ((endpoint (eden-get-in req [:api :endpoint]))
         (service (eden-get-in req [:api :service]))
         (api-key-symbol (eden-api-key-symbol service))
         (request-file (eden-request-file 'request req))
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
      (signal 'eden-error-api-key
              (format
               (concat "Do you have a line in ~/.authinfo.gpg file declaring "
                       "the API key of service `%s' like this: "
                       "machine %s password <api-key>")
               service service)))
    (list
     (format command-fmt endpoint (eval api-key-symbol) request-file)
     (format command-fmt endpoint "<api-key>" request-file))))

(defun eden-request-send (req callback &optional callback-error info)
  "..."
  (interactive)
  (seq-let (command command-no-api-key) (eden-request-command req)
    (eden-write-request req)
    (eden-write-command command-no-api-key req)
    (make-process
     :name "ai"
     :buffer (generate-new-buffer-name "eden")
     :command (list "sh" "-c" command)
     :connection-type 'pipe
     :sentinel (eden-sentinel req callback callback-error info))))

;;; AI Assistant UI

(defun eden-uuid ()
  "Generate a random-based UUID using `uuidgen' linux utility."
  (interactive)
  (string-remove-suffix "\n" (shell-command-to-string "uuidgen")))

(defvar eden-model "gpt-4o-mini" "...")
(defvar eden-temperature nil "...")
(defvar eden-api
  '(:service "openai"
    :endpoint "https://api.openai.com/v1/chat/completions"
    :default-model "gpt-4o-mini"
    :models ("gpt-4o-mini" "gpt-4o" "o1-mini" "o1"))
  "...")
(defvar eden-apis
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
(defvar eden-system-prompt nil "...")
(defvar eden-dir (concat (temporary-file-directory) "eden/") "...")

(defvar eden-history-requests nil "...")
(defvar eden-prompt-history-state [nil nil nil] "...")
(defvar eden-pending-requests nil "...")
(defvar eden-pending-timer nil "...")
(defvar eden-conversations nil "...")
(defvar eden-conversation-id nil "...")
(defvar eden-org-property-date "AI_ASSISTANT_DATE" "...")
(defvar eden-org-property-req "AI_ASSISTANT_REQ" "...")

(defun eden-history-requests-set ()
  "..."
  (let* ((timestamp-files
          (directory-files-recursively eden-dir "timestamp-.*")))
    (setq eden-history-requests
          (thread-last
            timestamp-files
            (mapcar (lambda (f)
                      (string-match ".*/\\([^/]+\\)/timestamp-\\(.*\\)" f)
                      (cons (match-string 1 f)
                            (string-to-number (match-string 2 f)))))
            (seq-sort (lambda (t1 t2) (> (cdr t1) (cdr t2))))
            (mapcar 'car)))))

(defun eden-history-previous (state &optional prompt discard-current)
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

(defun eden-history-next (state &optional prompt discard-current)
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

(defun eden-prompt-current-buffer ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun eden-prompt-current-req-uuid ()
  (when-let ((current (aref eden-prompt-history-state 1)))
    (when (not (consp current)) current)))

(defun eden-prompt-current ()
  (let ((current (aref eden-prompt-history-state 1)))
    (cond
     ((null current) nil)
     ((consp current) (plist-get current :prompt))
     (t (eden-request-read 'prompt `(:ai-dir ,eden-dir :uuid ,current))))))

(defun eden-prompt-current-goto-req-dir ()
  (interactive)
  (if-let* ((req-uuid (eden-prompt-current-req-uuid))
            (req-dir (eden-request-dir
                      `(:ai-dir ,eden-dir :uuid ,req-uuid))))
      (progn
        (when (> (length (window-list)) 1)
          (delete-window))
        (dired-other-window req-dir))
    (message (concat "Current prompt is not associated with a request.  "
                     "Try navigating the prompt history with `M-p' and `M-n', "
                     "default binding of `eden-prompt-previous' and `eden-prompt-next'."))))

(defun eden-prompt-history-state-set ()
  (setq eden-prompt-history-state
        (vector eden-history-requests nil nil)))

(defun eden-prompt-discard-current-p ()
  (let ((current (aref eden-prompt-history-state 1)))
    (when (not (or (null current) (consp current)))
      (let ((req `(:ai-dir ,eden-dir :uuid ,current)))
        (if (not (condition-case nil (eden-request-read 'prompt req) (error nil)))
            t)))))

(defun eden-prompt-history (direction)
  (let (prompts f)
    (pcase direction
      ('previous (setq prompts (aref eden-prompt-history-state 0))
                 (setq f 'eden-history-previous))
      ('next (setq prompts (aref eden-prompt-history-state 2))
             (setq f 'eden-history-next)))
    (cond
     ((null prompts)
      (message "No more requests or prompts in history."))
     ((eden-prompt-discard-current-p)
      (setq eden-prompt-history-state
            (funcall f eden-prompt-history-state nil 'discard-current))
      (eden-prompt-history direction))
     (t (let* ((pcb (eden-prompt-current-buffer))
               (pc (eden-prompt-current))
               (prompt (when (or (null pc) (not (string= pcb pc)))
                         `(:prompt ,pcb))))
          (setq eden-prompt-history-state
                (funcall f eden-prompt-history-state prompt))
          (if (eden-prompt-discard-current-p)
              (eden-prompt-history direction)
            (erase-buffer)
            (insert (or (eden-prompt-current) ""))))))))

(defun eden-prompt-previous ()
  "..."
  (interactive)
  (eden-prompt-history 'previous))

(defun eden-prompt-next ()
  "..."
  (interactive)
  (eden-prompt-history 'next))

(defun eden-org-to-markdown (markdown-str)
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

(cl-defun eden-request (&key prompt system-prompt exchanges
                                stream model temperature
                                api ai-dir)
  (when (null prompt)
    (error "You must provide a prompt via `:prompt' key to build a request."))
  (let* ((-system-prompt
          (or system-prompt (cdr-safe eden-system-prompt) ""))
         (-messages
          `(,(when (not (string-empty-p -system-prompt))
               `(:role "system" :content ,(eden-org-to-markdown -system-prompt)))
            ,@(seq-reduce
               (lambda (acc exchange)
                 (append acc
                         `((:role "user" :content ,(plist-get exchange :user))
                           (:role "assistant" :content ,(plist-get exchange :assistant)))))
               exchanges
               '())
            (:role "user" :content ,(eden-org-to-markdown prompt))))
         (req-messages (apply 'vector (remq nil -messages))))
    `(:req (:stream ,(or (and stream t) :false)
            :model ,(or model eden-model)
            :temperature ,(or temperature eden-temperature)
            :messages ,req-messages)
      :api ,(or api eden-api)
      :prompt ,prompt
      :system-prompt ,-system-prompt
      :exchanges ,exchanges
      :ai-dir ,(or ai-dir
                   eden-dir
                   (concat (temporary-file-directory) "eden/"))
      :uuid ,(eden-uuid))))

(defun eden-org-demote (org-str level)
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

(defun eden-insert-conversation (req &optional title append start-from)
  ""
  (eden-request-check req)
  (let* ((uuid (plist-get req :uuid))
         (format-exchange
          (lambda (exchange)
            (list (eden-org-demote (plist-get exchange :prompt) 4)
                  (eden-org-demote (plist-get exchange :response) 4))))
         (conversation
          (mapcar format-exchange (eden-request-conversation req)))
         (conversation
          (if (or append start-from) (last conversation) conversation)))
    (if (and append
             (progn (goto-char (point-min))
                    (re-search-forward
                     (format "^:%s: \\(.*\\)" eden-org-property-req) nil t)))
        (progn
          (replace-match uuid nil nil nil 1)
          (goto-char (point-max)))
      (insert
       "** " (or title "Conversation") "\n"
       ":PROPERTIES:\n"
       ":" eden-org-property-date ": " (or (eden-request-date req) "")
       "\n"
       ":" eden-org-property-req ": " uuid "\n"
       ":END:\n"))
    (dolist (exchange conversation)
      (seq-let (prompt response) exchange
        (insert "*** prompt\n\n" prompt)
        (ensure-empty-lines 1)
        (insert "*** response\n\n" response)
        (ensure-empty-lines 1)))))

(defun eden-pending-remove (req)
  "..."
  (setq eden-pending-requests
        (seq-remove (lambda (p)
                      (string=
                       (eden-get-in p [:req :uuid])
                       (plist-get req :uuid)))
                    eden-pending-requests)))

(defun eden-running-p ()
  ""
  (when-let ((proc (plist-get (car-safe eden-pending-requests) :proc)))
    (and (processp proc) (buffer-name (process-buffer proc)) t)))

(defun eden-kill-last-request ()
  ""
  (interactive)
  (when (eden-running-p)
    (message "Killing last request")
    (let ((proc (plist-get (car-safe eden-pending-requests) :proc)))
      ;; So that the error signaled in sentinel is not printed
      ;; in the echo area.
      (setq inhibit-message t)
      (kill-process proc)))
  ;; After 0.2s, the error has been signaled and the message logged,
  ;; so we can reestablish `inhibit-message' default value
  (run-at-time 0.2 nil (lambda () (setq inhibit-message nil))))

(defun eden-mode-line-waiting (action)
  "..."
  (pcase action
    ('maybe-start
     (when (null eden-pending-timer)
       (setq eden-pending-timer
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
     (when (and (not (eden-running-p)) eden-pending-timer)
       (cancel-timer eden-pending-timer)
       (setq eden-pending-timer nil)
       (setq global-mode-string nil)
       (force-mode-line-update 'all)))))

(defun eden-conversation (action title &optional req-uuid)
  "...

See variables `eden-conversations' and `eden-dir'."
  (cond
   ((seq-some (lambda (c) (equal title (plist-get (cdr c) :title)))
              eden-conversations)
    (error "Conversation with title `%s' already exists in `eden-conversations'"
           title))
   ((not (seq-contains-p [start start-from continue-from] action))
    (error "Conversation `action' must be `start', `start-from' or `continue-from' not `%S'"
           action))
   ((and (eq action 'start) req-uuid)
    (error "When action is `start', `req-uuid' argument must be nil or omitted, not `%s'"
           req-uuid))
   ((seq-contains-p [start-from continue-from] action)
    (let ((req `(:ai-dir ,eden-dir :uuid ,req-uuid)))
      (when (null req-uuid)
        (error "When action is `%s', `req-uuid' argument is mandatory."
               action req-uuid))
      (condition-case err
          (eden-request-check req)
        (error
         (error "Cannot start nor continue from that request.  %s"
                (error-message-string err)))))))
  (let ((conversation-id (eden-uuid)))
    (push (cons conversation-id
                `(:title ,title :action ,action :last-req-uuid ,req-uuid))
          eden-conversations)
    (setq eden-conversation-id conversation-id)))

(defun eden-conversation-title (conversation-id)
  "..."
  (eden-get-in eden-conversations `(,conversation-id :title)))

(defun eden-conversation-action (conversation-id)
  "..."
  (eden-get-in eden-conversations `(,conversation-id :action)))

(defun eden-conversation-last-req (conversation-id)
  "..."
  (when-let ((uuid (eden-get-in
                    eden-conversations `(,conversation-id :last-req-uuid))))
    `(:uuid ,uuid :ai-dir ,eden-dir)))

(defun eden-conversation-update (info req)
  "...

See `eden-conversation' and `eden-conversations'."
  (let ((conversation-id (plist-get info :conversation-id))
        (req-uuid (plist-get req :uuid)))
    (when-let ((conversation-data
                (cdr (assoc conversation-id eden-conversations))))
      (let ((data (thread-first
                    conversation-data
                    (plist-put :action 'continue-from)
                    (plist-put :last-req-uuid req-uuid))))
        (setf conversation-data data)))))

(defun eden-conversation-locked-p (conversation-id)
  (seq-some
   (lambda (r)
     (when-let ((id (plist-get r :conversation-id)))
       (string= conversation-id id)))
   eden-pending-requests))

(cl-defun eden-send-request (&key req callback info)
  "...

The CALLBACK function must call the following three functions
in that order

- `eden-pending-requests',
- `eden-conversation-update' and
- `eden-mode-line-waiting'

with `eden-pending-requests' being called first.

Here's a valid CALLBACK function that appends responses
in the buffer \"*ai*\":

    (lambda (req resp info)
        (with-current-buffer (get-buffer-create \"*ai*\")
          (org-mode)
          (save-excursion
            (widen)
            (goto-char (point-max))
            (eden-insert-conversation req)
            (save-buffer)))
        (eden-pending-remove req)
        (eden-conversation-update info req)
        (eden-mode-line-waiting \\='maybe-stop)
        (message \"AI assistant received a response\"))

If REQ is part of a conversation present in `eden-conversations',
the conversation id must be specified in INFO argument as value of
`:conversation-id' key.

For instance, if \"conversation-id-foo\" is the id of the some
conversation and REQ is part of that conversation (the next request
in the conversation), INFO argument must be a plist that looks
like this:

    (:conversation-id \"conversation-id-foo\" ...)"
  (let ((conversation-id (plist-get info :conversation-id)))
    (if (eden-conversation-locked-p conversation-id)
        (progn
          (message "Cannot send two concurrent requests in the same conversation.")
          (let ((inhibit-message t))
            (message "req: %S\nconversation: %s" req conversation-id)))
      (let ((callback-error (lambda (req err info)
                              (eden-pending-remove req)
                              (eden-mode-line-waiting 'maybe-stop))))
        (push (list :req req
                    :conversation-id conversation-id
                    :proc (eden-request-send req callback callback-error info))
              eden-pending-requests)
        (push (plist-get req :uuid) eden-history-requests)
        (eden-prompt-history-state-set)
        (eden-mode-line-waiting 'maybe-start)))))

(defun eden-conversation-buffer-name (conversation-id)
  (when-let ((title (eden-conversation-title conversation-id)))
    (format "*ai %s*" title)))

(defun eden-show-current-conversation ()
  (interactive)
  (let ((buff-name (eden-conversation-buffer-name eden-conversation-id))
        (title (eden-conversation-title eden-conversation-id))
        (action (eden-conversation-action eden-conversation-id))
        (last-req (eden-conversation-last-req eden-conversation-id)))
    (cond
     ((null buff-name) (message "No current conversation to display."))
     ((null last-req) (message "Current conversation is empty."))
     (t (when (not (get-buffer buff-name))
          (with-current-buffer (get-buffer-create buff-name)
            (save-excursion
              (org-mode)
              (eden-insert-conversation
               last-req title nil (eq action 'start-from)))))
        (when (> (length (window-list)) 1)
          (delete-window))
        (select-window
         (display-buffer buff-name '(nil (inhibit-same-window . t))))))))

(defun eden-show-current-conversation-in-req-history ()
  (interactive)
  (if-let* ((req-uuid (eden-prompt-current-req-uuid))
            (req `(:ai-dir ,eden-dir :uuid ,req-uuid)))
      (if (condition-case nil (eden-request-check req) (error nil))
          (let* ((title "Current conversation in history")
                 (buff-name (get-buffer-create (format "*ai <%s>*" title))))
            (with-current-buffer buff-name
              (save-excursion
                (erase-buffer)
                (org-mode)
                (eden-insert-conversation req title)))
            (when (> (length (window-list)) 1)
              (delete-window))
            (select-window
             (display-buffer buff-name '(nil (inhibit-same-window . t)))))
        (message (concat "Current prompt is associated with a failed or missing request.  "
                         "Try navigating the prompt history with `M-p' and `M-n', "
                         "default binding of `eden-prompt-previous' and `eden-prompt-next'.")))
    (message (concat "Current prompt is not associated with a request.  "
                     "Try navigating the prompt history with `M-p' and `M-n', "
                     "default binding of `eden-prompt-previous' and `eden-prompt-next'."))))

(defun eden-show-last-conversations ()
  (interactive)
  (let* ((num-of-days (read-number "Enter the number of days: "))
         (conversations (eden-conversations num-of-days))
         (buff-name (get-buffer-create "*ai <Last conversations>*")))
    (with-current-buffer buff-name
      (save-excursion
        (erase-buffer)
        (org-mode)
        (dolist (req-uuid conversations)
          (eden-insert-conversation `(:ai-dir ,eden-dir :uuid ,req-uuid)))))
    (when (> (length (window-list)) 1)
      (delete-window))
    (select-window
     (display-buffer buff-name '(nil (inhibit-same-window . t))))))

(defun eden-show-last-requests ()
  (interactive)
  (let* ((num-of-days (read-number "Enter the number of days: "))
         (requests (eden-requests num-of-days))
         (buff-name (get-buffer-create "*ai <Last requests>*")))
    (with-current-buffer buff-name
      (save-excursion
        (erase-buffer)
        (org-mode)
        (dolist (req-uuid requests)
          (eden-insert-conversation
           `(:ai-dir ,eden-dir :uuid ,req-uuid)
           "Request" nil 'start-from))))
    (when (> (length (window-list)) 1)
      (delete-window))
    (select-window
     (display-buffer buff-name '(nil (inhibit-same-window . t))))))

(defun eden-conversation-switch ()
  (interactive)
  (if (null eden-conversations)
      (message "No conversation to switch to yet.")
    (let* ((conversations
            (mapcar (lambda (c) (cons (plist-get (cdr c) :title) (car c)))
                    eden-conversations))
           (title (completing-read "Conversation title: "
                                   (mapcar #'car conversations)
                                   nil 'require-match)))
      (setq eden-conversation-id
            (alist-get title conversations nil nil #'string=)))))

(defun eden-conversation-start ()
  "..."
  (interactive)
  (eden-conversation
   'start (read-string "Enter a conversation title: ")))

(defun eden-conversation-start-from-req-history ()
  "..."
  (interactive)
  (if-let ((req-uuid (eden-prompt-current-req-uuid)))
      (eden-conversation
       'start-from (read-string "Enter a conversation title: ") req-uuid)
    (message (concat "Current prompt is not associated with a request.  "
                     "Try navigating the prompt history with `M-p' and `M-n', "
                     "default binding of `eden-prompt-previous' and `eden-prompt-next'."))))

(defun eden-conversation-continue-from-req-history ()
  "..."
  (interactive)
  (if-let ((req-uuid (eden-prompt-current-req-uuid)))
      (eden-conversation
       'continue-from (read-string "Enter a conversation title: ") req-uuid)
    (message (concat "Current prompt is not associated with a request.  "
                     "Try navigating the prompt history with `M-p' and `M-n', "
                     "default binding of `eden-prompt-previous' and `eden-prompt-next'."))))

(defun eden-conversation-pause ()
  "..."
  (interactive)
  (setq eden-conversation-id nil))

(defun eden-api-set ()
  "..."
  (interactive)
  (if-let* ((services
             (mapcar (lambda (api) (plist-get api :service)) eden-apis)))
      (let* ((service (completing-read
                       (format "Choose an API from the following services: "
                               services)
                       services nil 'require-match))
             (api (seq-some (lambda (api)
                              (when (string= (plist-get api :service) service)
                                api))
                            eden-apis))
             (default-model (plist-get api :default-model)))
        (setq eden-api api)
        (when default-model (setq eden-model default-model)))
    (error (format "`eden-apis' variable must be a list of API specifications like this

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

not `%S'" eden-apis))))

(defun eden-model-set ()
  "..."
  (interactive)
  (let* ((service (plist-get eden-api :service))
         (models (plist-get eden-api :models))
         (model (completing-read
                 (format "Choose a model for the service `%s': " service)
                 models)))
    (setq eden-model model)))

(defun eden-temperature-set ()
  "..."
  (interactive)
  (let ((temperature
         (read-string "Enter a float number [0-2] or (leave blank for none) to set model temperature: ")))
    (setq eden-temperature
          (when (not (string-empty-p temperature))
            (string-to-number temperature)))))

(defvar eden-system-prompts nil
  "Alist of (\"title\" . \"system prompt\") to choose from.

For instance we can set `eden-system-prompts' to:

    ((\"writer\" . \"You're a good writer who only writes in Italian.\")
     (\"programmer\" . \"You're a programmer who only answers with code snippets.\"))

See `eden-system-prompt-set' command.")

(defun eden-system-prompt-set ()
  (interactive)
  (let ((err (format "`eden-system-prompts' variable must be nil or an alist like this

((\"writer\" . \"You're a good writer who only writes in Italian.\")
 (\"programmer\" . \"You're a programmer who only answers with code snippets.\"))

not `%S'" eden-system-prompts)))
    (cond
     ((null eden-system-prompts)
      (message "There's no system prompt to select from `eden-system-prompts' variable which is nil."))
     ((not (listp eden-system-prompts)) (error err))
     (t (if-let* ((system-prompt-titles
                   (delq nil (mapcar 'car-safe eden-system-prompts))))
            (let ((title (completing-read
                          "System prompt title (leave blank for none): "
                          system-prompt-titles)))
              (setq eden-system-prompt (assoc title eden-system-prompts)))
          (error err))))))

(transient-define-prefix eden-menu ()
  "Insert menu"
  [["Conversation"
    ("n" "Start new conversation" eden-conversation-start)
    ("s" "Start conversation from current request in history" eden-conversation-start-from-req-history)
    ("c" "Continue conversation from current request in history" eden-conversation-continue-from-req-history)
    ("SPC" "Pause current conversation" eden-conversation-pause)
    ("TAB" "Switch conversation" eden-conversation-switch)]]
  [["Conversations and requests"
    ("v" "Show current conversation" eden-show-current-conversation)
    ("h" "Show current conversation in history" eden-show-current-conversation-in-req-history)
    ("l" "Show last conversations" eden-show-last-conversations)
    ("r" "Show last requests" eden-show-last-requests)
    ("k" "Kill last request" eden-kill-last-request)
    ("g" "Go to directory of current request in history" eden-prompt-current-goto-req-dir)
    ]]
  [["Model"
    ("a" "Set current API" eden-api-set)
    ("m" "Set model for current API" eden-model-set)
    ("t" "Set temperature" eden-temperature-set)
    ("p" "Set system prompt" eden-system-prompt-set)
    ]]
  )

(defun eden-req-at-point-uuid ()
  (if-let* ((req-uuid (org-entry-get nil eden-org-property-req))
            (req-dir (eden-request-dir
                      `(:ai-dir ,eden-dir :uuid ,req-uuid))))
      (if (file-exists-p req-dir)
          req-uuid
        (error "Request `%s' doesn't exist." req-dir))
    (error "No request at point found.")))

(defun eden-req-at-point-start-conversation ()
  "..."
  (interactive)
  (when-let ((req-uuid (eden-req-at-point-uuid)))
    (eden-conversation
     'start-from (read-string "Enter a conversation title: ") req-uuid))
  (eden))

(defun eden-req-at-point-continue-conversation ()
  "..."
  (interactive)
  (when-let ((req-uuid (eden-req-at-point-uuid)))
    (eden-conversation
     'continue-from (read-string "Enter a conversation title: ") req-uuid))
  (eden))

(defun eden-req-at-point-show-requests ()
  (interactive)
  (when-let* ((req-uuid (eden-req-at-point-uuid))
              (req `(:ai-dir ,eden-dir :uuid ,req-uuid))
              (requests
               (mapcar
                (lambda (exchange)
                  `(:ai-dir ,eden-dir :uuid ,(plist-get exchange :uuid)))
                (eden-request-conversation req)))
              (buff (get-buffer-create "*ai Requests of conversation at point*")))
    (with-current-buffer buff
      (erase-buffer)
      (org-mode)
      (save-excursion
        (dolist (req requests)
          (eden-insert-conversation req "Request" nil 'start-from))))
    (select-window
     (display-buffer buff '(nil (inhibit-same-window . t))))))

(defun eden-req-at-point-show-perplexity-citations ()
  (interactive)
  (when-let* ((req-uuid (eden-req-at-point-uuid))
              (req `(:ai-dir ,eden-dir :uuid ,req-uuid)))
    (if-let ((citations (eden-request-perplexity-citations req)))
        (let ((buff (get-buffer-create "*ai Perplexity citations*")))
          (with-current-buffer buff
            (erase-buffer)
            (org-mode)
            (save-excursion
              (dolist (citation citations)
                (insert (format "- %s\n" citation)))))
          (display-buffer buff '(nil (inhibit-same-window . t))))
      (message "No citations for `%s' conversation."
               (eden-request-dir req)))))

(defun eden-req-at-point-goto ()
  (interactive)
  (when-let ((req-uuid (eden-req-at-point-uuid))
             (req-dir (eden-request-dir
                       `(:ai-dir ,eden-dir :uuid ,req-uuid))))
    (dired req-dir)))

(transient-define-prefix eden-req-at-point-menu ()
  "Insert menu"
  [["Conversation/Request at point"
    ("s" "Start conversation from request at point" eden-req-at-point-start-conversation)
    ("c" "Continue conversation from request at point" eden-req-at-point-continue-conversation)
    ("r" "Show requests of conversation at point" eden-req-at-point-show-requests)
    ("C" "Show Perplexity citations of conversation at point" eden-req-at-point-show-perplexity-citations)
    ("g" "Go to directory of request at point" eden-req-at-point-goto)
    ]]
  )

(defun eden-conversation-exchanges (conversation-id)
  (when-let ((last-req (eden-conversation-last-req
                        conversation-id))
             (conversation (eden-request-conversation last-req)))
    (pcase (eden-conversation-action conversation-id)
      ('start-from (vector (aref conversation (1- (length conversation)))))
      ('continue-from conversation))))

(defun eden-send ()
  ""
  (interactive)
  (eden-send-request
   :req (eden-request
         :prompt (buffer-substring-no-properties (point-min) (point-max))
         :exchanges (eden-conversation-exchanges eden-conversation-id))
   :info `(:conversation-id ,eden-conversation-id)
   :callback (lambda (req resp info)
               (let* ((conversation-id (plist-get info :conversation-id))
                      (title (eden-conversation-title conversation-id))
                      (append (and conversation-id t))
                      (buff-name
                       (or (eden-conversation-buffer-name conversation-id)
                           "*ai*"))
                      (buff-already-exist-p (get-buffer buff-name))
                      (buff (get-buffer-create buff-name)))
                 (with-current-buffer buff
                   (save-excursion
                     (if (not buff-already-exist-p)
                         (progn
                           (org-mode)
                           (eden-insert-conversation req title))
                       (widen)
                       (goto-char (point-max))
                       (eden-insert-conversation req title append))))
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
                 (eden-pending-remove req)
                 (eden-conversation-update info req)
                 (eden-mode-line-waiting 'maybe-stop)
                 (message "AI assistant received a response"))))
  (erase-buffer)
  (when (> (length (window-list)) 1)
    (delete-window))
  (message "AI assistant sent a request"))

(defvar eden-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-p") 'eden-prompt-previous)
    (define-key map (kbd "M-n") 'eden-prompt-next)
    (define-key map (kbd "C-c C-c") #'eden-send)
    map)
  "Keymap for `eden-mode'.")

(define-derived-mode eden-mode org-mode "Eden"
  "Eden AI assitant mode."
  (setq
   mode-line-format
   '(" "
     mode-line-buffer-identification
     " "
     (:eval (plist-get eden-api :service))
     "/"
     (:eval eden-model)
     (:eval (when eden-temperature (format " %s" eden-temperature)))
     (:eval (when eden-conversation-id
              (format " <%s>"
                      (eden-conversation-title eden-conversation-id))))
     (:eval (when-let ((system-prompt-title (car-safe eden-system-prompt)))
              (format " * %s" system-prompt-title)))
     " "
     mode-line-misc-info))
  (eden-history-requests-set)
  (eden-prompt-history-state-set))

(defvar eden-prompt-buffer-name "*Eden*" "...")

(defun eden (&optional arg)
  ""
  (interactive "P")
  (cond
   (arg (call-interactively 'eden-req-at-point-menu))
   ((string= (buffer-name) eden-prompt-buffer-name)
    (call-interactively 'eden-menu))
   (t (let ((eden-buffer-p (get-buffer eden-prompt-buffer-name))
            (buff (get-buffer-create eden-prompt-buffer-name)))
        (select-window
         (display-buffer-at-bottom buff '(display-buffer-below-selected
                                          (window-height . 4))))
        (when (not eden-buffer-p)
          (eden-mode))))))

(define-key input-decode-map "\C-i" [C-i])
(global-set-key [C-i] #'eden)

(defun eden-paths (num-of-days)
  (let* ((today (calendar-current-date))
         (midnight (encode-time `(0 0 0 ,(nth 1 today) ,(nth 0 today) ,(nth 2 today))))
         (timestamp-start
          (thread-last (days-to-time (1- num-of-days))
                       (time-subtract midnight)
                       (float-time)))
         (timestamp-files
          (directory-files-recursively eden-dir "timestamp-.*")))
    (thread-last
      timestamp-files
      (mapcar (lambda (f)
                (string-match ".*/\\([^/]+\\)/timestamp-\\(.*\\)" f)
                (cons (match-string 1 f)
                      (string-to-number (match-string 2 f)))))
      (seq-sort (lambda (t1 t2) (< (cdr t1) (cdr t2))))
      (mapcar (lambda (r)
                (when (< timestamp-start (cdr r))
                  (let ((req `(:ai-dir ,eden-dir
                               :uuid ,(car r))))
                    (eden-request-conversation-path req)))))
      (delq nil))))

(defun eden-requests (num-of-days)
  (mapcar (lambda (p) (aref p (1- (length p))))
          (eden-paths num-of-days)))

(defun eden-conversations (num-of-days)
  (eden-conversations-keep (eden-paths num-of-days)))

(defun eden-conversations-keep (paths)
  (let ((tail (reverse paths))
        alist-paths-of-kept
        kept)
    (while tail
      (let ((path-vec (pop tail)))
        (when (not (map-nested-elt alist-paths-of-kept path-vec))
          (push path-vec kept)
          (setq alist-paths-of-kept
                (append alist-paths-of-kept
                        (eden-request-conversation-path-alist path-vec))))))
    (mapcar (lambda (path) (aref path (1- (length path))))
            kept)))

(provide 'eden)

;;; eden.el ends here
