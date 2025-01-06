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

(require 'json)
(require 'ox-md)

(defalias 'eden-get-in 'map-nested-elt)

;;; API to send asynchronous requests to OpenAI and Perplexity

(defun eden-json-encode (object)
  "Return a JSON representation of OBJECT as a string.

The JSON representation is pretty-printed."
  (let ((json-false :false)
        (json-encoding-pretty-print "  ")
        (json-encoding-pretty-print t))
    (json-encode object)))

(defun eden-json-read ()
  "Parse and return the JSON object following point.

Advances point just past JSON object.

If called with the following JSON after point

    {\"a\": [1, 2, {\"c\": false}],
     \"b\": \"foo\"}

we get the following plist:

    (:a [1 2 (:c :false)]
     :b \"foo\")"
  (let ((json-false :false)
        (json-key-type 'keyword)
        (json-object-type 'plist)
        (json-array-type 'vector))
    (json-read)))

(defun eden-request-dir (req)
  "Return the directory where REQ's information is stored.

For instance:

    (eden-request-dir \\='(:dir \"/tmp/eden/\" :uuid \"foo\"))
    ;; \"/tmp/eden/foo/\"

Signal an error, if either `:dir' or `:uuid' key is missing in REQ."
  (let ((dir (plist-get req :dir))
        (uuid (plist-get req :uuid)))
    (when (or (not (stringp dir)) (not (stringp uuid)))
      (error "Request mal formed.  `req' must contain `:dir' and `:uuid' keys and their value must be strings: %S"
             req))
    (concat (file-name-as-directory dir) uuid "/")))

(defun eden-request-file (file-type req)
  "Return full path of file of FILE-TYPE of REQ request.

Signal an error if FILE-TYPE is not one of the following symbols:

    error, response, response-org, request, api, prompt,
    system-prompt, exchanges, command.

For instance

    (eden-request-file \\='request \\='(:dir \"/tmp/eden/\" :uuid \"foo\"))
    ;; \"/tmp/eden/foo/request.json\"
    (eden-request-file \\='prompt \\='(:dir \"/tmp/eden/\" :uuid \"foo\"))
    ;; \"/tmp/eden/foo/prompt.org\""
  (let* ((filenames '((error         . "error.json")
                      (response      . "response.json")
                      (response-org  . "response.org")
                      (request       . "request.json")
                      (api           . "api.json")
                      (prompt        . "prompt.org")
                      (system-prompt . "system-prompt.org")
                      (exchanges     . "exchanges.json")
                      (command       . "command")))
         (filename (alist-get file-type filenames)))
    (if filename
        (concat (eden-request-dir req) filename)
      (error "`file-type' argument must be one of %s, not `%s'"
             (mapcar #'car filenames) file-type))))

(defun eden-request-read (file-type req)
  "Return file content of FILE-TYPE of REQ request as string or object.

If file extension is \".json\", return an object (that may be
a string).  If not, return a string.
Accepted symbols for FILE-TYPE are described in `eden-request-file'.

Signal an error if the file is missing.

For instance if the file \"request.json\" of the request
`(:dir \"/tmp/eden/\" :uuid \"foo\")' contains

    {
      \"stream\": false,
      \"model\": \"gpt-4o-mini\",
      \"temperature\": null,
      \"messages\": [
        {
          \"role\": \"user\",
          \"content\": \"foo bar baz\"
        }
      ]
    }

we have the following:

    (eden-request-read \\='request \\='(:dir \"/tmp/eden/\" :uuid \"foo\"))
    ;; (:stream :false
    ;;  :model \"gpt-4o-mini\"
    ;;  :temperature nil
    ;;  :messages [(:role \"user\" :content \"foo bar baz\")])

And for a non JSON file like the file \"prompt.org\" (assuming the
user prompt was \"foo bar baz\") we get something like this:

    (eden-request-read \\='prompt \\='(:dir \"/tmp/eden/\" :uuid \"foo\"))
    ;; \"foo bar baz\""



  (let* ((-file (eden-request-file file-type req)))
    (if (not (file-exists-p -file))
        (error "Missing `%s' file." -file)
      (with-temp-buffer
        (insert-file-contents (eden-request-file file-type req))
        (if (string= (file-name-extension -file) "json")
            (eden-json-read)
          (buffer-substring-no-properties (point-min) (point-max)))))))

(defun eden-request-assistant-content (resp)
  "Return the first message in RESP response from OpenAI API.

For instance (with some keys omitted from a real response from
OpenAI API) we have:

    (let ((resp \\='(:id \"chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8\"
                  :object \"chat.completion\"
                  :model \"gpt-4o-mini-2024-07-18\"
                  :choices [(:index 0
                             :message (:role \"assistant\"
                                       :content \"foo assistant\"
                                       :refusal nil)
                             :logprobs nil
                             :finish_reason \"stop\")])))
      (eden-request-assistant-content resp))
    ;; \"foo assistant\""
  (eden-get-in resp [:choices 0 :message :content]))

(defun eden-request-user-content (request)
  "Return the last message of REQUEST, an OpenAI API comliant request.

For instance we have:

    (let* ((request \\='(:stream :false
                      :model \"gpt-4o-mini\"
                      :temperature 1
                      :messages [(:role \"system\" :content \"baz system\")
                                 (:role \"user\" :content \"foo user\")
                                 (:role \"assistant\" :content \"foo assistant\")
                                 (:role \"user\" :content \"bar prompt\")
                                 (:role \"assistant\" :content \"bar assistant\")
                                 (:role \"user\" :content \"baz user\")])))
      (eden-request-user-content request))
    ;; \"baz user\""
  (let* ((messages (plist-get request :messages))
         (last-message (aref messages (1- (length messages)))))
    (plist-get last-message :content)))

(defun eden-request-check (req)
  "Return t if REQ did complete.

Signal an error in the following cases:

- the request doesn't exist,
- the request has failed in a prior attempt (so `error.json' file exists),
- the request is incomplete, specifically when at least one of the following
  files is missing:

  - prompt.org
  - request.json
  - response.json
  - response.org
  - exchanges.json"
  (let ((req-dir (eden-request-dir req)))
    (cond
     ((not (file-exists-p (eden-request-dir req)))
      (error "Request `%s' doesn't exist" req-dir))
     ((file-exists-p (eden-request-file 'error req))
      (error "Request `%s' has failed in a prior attempt.  See why in `%s' file"
             req-dir (eden-request-file 'error req)))
     ((not (file-exists-p (eden-request-file 'prompt req)))
      (error "Missing `%s' file" (eden-request-file 'prompt req)))
     ((not (file-exists-p (eden-request-file 'request req)))
      (error "Missing `%s' file" (eden-request-file 'request req)))
     ((not (file-exists-p (eden-request-file 'response req)))
      (error "Missing `%s' file" (eden-request-file 'response req)))
     ((not (file-exists-p (eden-request-file 'response-org req)))
      (error "Missing `%s' file" (eden-request-file 'response-org req)))
     ((not (file-exists-p (eden-request-file 'exchanges req)))
      (error "Missing `%s' file" (eden-request-file 'exchanges req)))
     (t t))))

(defun eden-request-conversation (req)
  "Return all exchanges of the conversation whose last request is REQ.

The return value is a vector of plists (exchanges) containing the
following keys:

- :uuid      - uuid of the request for that exchange
- :prompt    - prompt of that exchange (`org-mode' string)
- :user      - prompt of that exchange sent to OpenAI (markdown string)
- :assitant  - content message of the response of that exchange
               received from OpenAI (markdown string)
- :response  - content message of the response of that exchange
               received from OpenAI converted to Org (`org-mode' string)

Signal an error if REQ doesn't pass `eden-request-check' check.

For instance, assuming \"uuid-baz\" is the uuid of the last request
of a conversation whose previous exchanges are the requests whose
uuids are \"uuid-foo\" and \"uuid-bar\" in that order, the following
function call

    (eden-request-conversation \\='(:dir \"/tmp/eden/\" :uuid \"uuid-baz\"))

gives use the following conversation:

    [(:uuid \"uuid-foo\"
      :prompt \"foo prompt\"
      :user \"foo user\"
      :assistant \"foo assistant\"
      :response \"foo assistant\")
     (:uuid \"uuid-bar\"
      :prompt \"bar prompt\"
      :user \"bar user\"
      :assistant \"bar assistant\"
      :response \"bar assistant\")
     (:uuid \"uuid-baz\"
      :prompt \"baz user prompt\"
      :user \"baz user\"
      :assistant \"baz assistant\"
      :response \"baz assistant\")]"
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
  "Return the path of the conversation whose last request is REQ.

Return nil if REQ doesn't pass `eden-request-check' check.

For instance, assuming \"uuid-baz\" is the uuid of the last request
of a conversation whose previous exchanges are the requests whose
uuids are \"uuid-foo\" and \"uuid-bar\" in that order, we have
the following:

    (eden-request-conversation-path \\='(:dir \"/tmp/eden/\" :uuid \"uuid-baz\"))
    ;; [\"uuid-foo\" \"uuid-bar\" \"uuid-baz\"]"
  (when (condition-case nil (eden-request-check req) (error nil))
    (let* ((uuids (mapcar (lambda (exchange) (plist-get exchange :uuid))
                          (eden-request-read 'exchanges req)))
           (last-uuid (list (plist-get req :uuid))))
      (apply 'vector (append uuids last-uuid)))))

(defun eden-request-conversation-path-alist (path)
  "Convert PATH of a conversation in a nested alist.

For instance, we have the following:

    (eden-request-conversation-path-alist [\"uuid-foo\"])
    ;; ((\"uuid-foo\" . t))

    (eden-request-conversation-path-alist [\"uuid-foo\" \"uuid-bar\" \"uuid-baz\"])
    ;; ((\"uuid-foo\" . ((\"uuid-bar\" . ((\"uuid-baz\" . t))))))

See `eden-request-conversation-path' and `eden-last-conversations-keep'."
  (when path
    (let* ((tail (append (reverse path) '()))
           (alist (list (cons (pop tail) t))))
      (while tail
        (setq alist (list (cons (pop tail) alist))))
      alist)))

(defun eden-request-perplexity-citations (req)
  "Return the list of Perplexity citations of the conversation REQ.

More precisely of the conversation whose last request is REQ.

When using Perplexity API, the JSON response to a request may
contain citations (urls).  These citations are accessible
under the key \"citations\" represented as an array.

The function `eden-request-perplexity-citations' returns the
concatenation of all these citations for the conversation
whose last request is REQ.

For instance, assuming \"uuid-baz\" is the uuid of the third and
last request of a conversation whose first conversation has the
citations

    [\"https://foo-1.com\" \"https://foo-2.com\" \"https://foo-3.com\"]

the second request contains no citations at all and the third
request has the citations

    [\"https://baz-1.com\" \"https://baz-2.com\"]

then the following function call

    (eden-request-perplexity-citations \\='(:dir \"/tmp/eden/\" :uuid \"uuid-baz\"))

gives use the following citations:

    (\"https://foo-1.com\" \"https://foo-2.com\" \"https://foo-3.com\"
     \"https://baz-1.com\" \"https://baz-2.com\")

See `eden-req-at-point-show-perplexity-citations'."
  (let ((dir (plist-get req :dir)))
    (seq-reduce
     (lambda (acc exchange)
       (let* ((uuid-exchange (plist-get exchange :uuid))
              (req-exchange `(:dir ,dir :uuid ,uuid-exchange)))
         (if (condition-case nil (eden-request-check req-exchange) (error nil))
             (let* ((resp (eden-request-read 'response req-exchange))
                    (citations (plist-get resp :citations)))
               (append acc citations '()))
           acc)))
     (eden-request-conversation req)
     '())))

(defun eden-request-timestamp (req)
  "Return the timestamp (a float) of the moment REQ request was sent to OpenAI.

Just before we send a request to OpenAI, we store the information
related to the request using `eden-write-request' function.  One
of the file we write to disk is a timestamp file whose filename
relative to the request's directory is of the form

    timestamp-<timestamp>

where <timestamp> is the float number of seconds since the epoch
at the moment we write it to disk.

The function `eden-request-timestamp' returns this <timestamp> as a
float for REQ request."
  (when-let ((filename
              (car (directory-files
                    (eden-request-dir req) nil "timestamp-.*"))))
    (string-to-number (string-trim-left filename "timestamp-"))))

(defun eden-request-date (req)
  "Return the date in `org-mode' format of when REQ request was sent.

For instance if the some request with \"foo\" uuid logged in
\"/tmp/eden/\" directory has the timestamp file
\"timestamp-1733921715.2331347\" in its directory, then we
have the following:

    (eden-request-timestamp (:dir \"/tmp/eden/\" :uuid \"foo\"))
    ;; [2024-12-11 Wed]

See `eden-request-timestamp'."
  (when-let ((timestamp (eden-request-timestamp req)))
    (format-time-string "[%Y-%m-%d %a]" (seconds-to-time (floor timestamp)))))

(defun eden-request-write (file-type req content)
  "Write CONTENT string in file of FILE-TYPE in REQ's directory.

All accepted symbols for FILE-TYPE, except for the `timestamp'
symbol, are listed in `eden-request-file', along with the
file where the content is saved.

If FILE-TYPE is equal to `timestamp' symbol, a timestamp file is written
to disk.  See `eden-request-timestamp' for more details on that file."
  (let ((inhibit-message t)
        (message-log-max nil)
        (file-path (if (eq file-type 'timestamp)
                       (format "%stimestamp-%s"
                               (eden-request-dir req) (time-to-seconds))
                     (eden-request-file file-type req))))
    (make-directory (eden-request-dir req) 'parents)
    (with-temp-buffer
      (insert content)
      (write-file file-path))))

(defun eden-write-request (req)
  "Write REQ's information in several files in REQ's directory.

Specifically, 6 files are written to disk:

- a `timestamp' file     - see `eden-request-timestamp' for more details
                           on that file,
- a `request' file       - a JSON file with content being the value
                           under `:req' key of REQ plist,
- an `api' file          - a JSON file with content being the value
                           under `:api' key of REQ plist,
- a `prompt' file        - an `org-mode' file with content being the value
                           under `:prompt' key,
- a `system-prompt' file - an `org-mode' file with content being the value
                           under `:system-prompt' key,
- an `exchanges' file    - a JSON file with content being the value
                           under `:exchanges' key of REQ plist.

Here an example with a typical request (third of a conversation)
that we would send to OpenAI API.  Evaluating the following
expression

    (let ((req \\='(:req (:stream :false
                       :model \"gpt-4o-mini\"
                       :temperature 1
                       :messages [(:role \"system\" :content \"baz system prompt\")
                                  (:role \"user\" :content \"foo user\")
                                  (:role \"assistant\" :content \"foo assistant\")
                                  (:role \"user\" :content \"bar prompt\")
                                  (:role \"assistant\" :content \"bar assistant\")
                                  (:role \"user\" :content \"baz user prompt\")])
                 :api (:service \"openai\"
                       :endpoint \"https://api.openai.com/v1/chat/completions\")
                 :prompt \"baz user prompt\"
                 :system-prompt \"baz system prompt\"
                 :exchanges [(:uuid \"uuid-foo\"
                              :prompt \"foo prompt\"
                              :user \"foo user\"
                              :assistant \"foo assistant\"
                              :response \"foo response\")
                             (:uuid \"uuid-bar\"
                              :prompt \"bar prompt\"
                              :user \"bar user\"
                              :assistant \"bar assistant\"
                              :response \"bar response\")]
                 :dir \"/tmp/eden/\"
                 :uuid \"uuid-baz\")))
      (eden-write-request req))

produces the following files (with timestamp file depending on
the moment we evaluate that expression):

- /tmp/eden/uuid-baz/api.json
- /tmp/eden/uuid-baz/exchanges.json
- /tmp/eden/uuid-baz/prompt.org
- /tmp/eden/uuid-baz/request.json
- /tmp/eden/uuid-baz/system-prompt.org
- /tmp/eden/uuid-baz/timestamp-1736137516.050655"
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
     :dir \"/tmp/eden/\"
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

(defun eden-write-command (command-no-api-key req)
  "..."
  (message "%s" command-no-api-key)
  (eden-request-write 'command req command-no-api-key))

(defun eden-markdown-to-org (markdown-str)
  "Return MARKDOWN-STR markdown string converted into org-mode string."
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
    (eden-error-api-key . "Error setting API key")
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

(defun eden-request-send (req callback &optional callback-error info)
  "..."
  (seq-let (command command-no-api-key) (eden-request-command req)
    (eden-write-request req)
    (eden-write-command command-no-api-key req)
    (make-process
     :name "eden"
     :buffer (generate-new-buffer-name "eden")
     :command (list "sh" "-c" command)
     :connection-type 'pipe
     :sentinel (eden-sentinel req callback callback-error info))))

;;; UI
;;;; User options

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
(defvar eden-model "gpt-4o-mini" "...")
(defvar eden-temperature nil "...")
(defvar eden-system-prompt nil "...")
(defvar eden-system-prompts nil
  "Alist of (\"title\" . \"system prompt\") to choose from.

For instance we can set `eden-system-prompts' to:

    ((\"writer\" . \"You\\='re a good writer who only writes in Italian.\")
     (\"programmer\" . \"You\\='re a programmer who only answers with code snippets.\"))

See `eden-system-prompt-set' command.")
(defvar eden-dir (concat (temporary-file-directory) "eden/") "...")
(defvar eden-org-property-date "EDEN_DATE" "...")
(defvar eden-org-property-req "EDEN_REQ" "...")
(defvar eden-pops-up-upon-receipt t "...")
(defvar eden-prompt-buffer-name "*eden*" "...")

;;;; Utils

(defun eden-uuid ()
  "Generate a random-based UUID using `uuidgen' linux utility."
  (string-remove-suffix "\n" (shell-command-to-string "uuidgen")))

(defun eden-maybe-delete-window-prompt-buffer ()
  (when-let ((prompt-buffer-window (get-buffer-window eden-prompt-buffer-name)))
    (when (and (equal (selected-window) prompt-buffer-window)
               (> (length (window-list)) 1))
      (delete-window))))

(defun eden-org-to-markdown (org-str)
  (let ((org-export-with-toc nil)
        (org-md-headline-style 'atx))
    ;; Default md backend uses `org-md-example-block' to export
    ;; example blocks and source blocks and it prefers 4 leading
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
       (org-export-string-as org-str 'md nil)))))

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

;;;; Prompt and Request history

(defvar eden-request-history nil "...")

(defun eden-request-history-set ()
  "..."
  (when (file-exists-p eden-dir)
    (let* ((timestamp-files
            (directory-files-recursively eden-dir "timestamp-.*")))
      (setq eden-request-history
            (thread-last
              timestamp-files
              (mapcar (lambda (f)
                        (string-match ".*/\\([^/]+\\)/timestamp-\\(.*\\)" f)
                        (cons (match-string 1 f)
                              (string-to-number (match-string 2 f)))))
              (seq-sort (lambda (t1 t2) (> (cdr t1) (cdr t2))))
              (mapcar 'car))))))

(defvar eden-prompt-history-state [nil nil nil] "...")

(defun eden-prompt-history-state-set ()
  (setq eden-prompt-history-state
        (vector eden-request-history nil nil)))

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
     (t (eden-request-read 'prompt `(:dir ,eden-dir :uuid ,current))))))

(defun eden-prompt-current-goto ()
  (interactive)
  (if-let* ((req-uuid (eden-prompt-current-req-uuid))
            (req-dir (eden-request-dir
                      `(:dir ,eden-dir :uuid ,req-uuid))))
      (progn
        (eden-maybe-delete-window-prompt-buffer)
        (dired-other-window req-dir))
    (message (concat "Current prompt is not associated with a request.  "
                     "Try navigating the prompt history with `M-p' and `M-n', "
                     "default binding of `eden-prompt-previous' and `eden-prompt-next'."))))

(defun eden-prompt-history-previous (state &optional prompt discard-current)
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

(defun eden-prompt-history-next (state &optional prompt discard-current)
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

(defun eden-prompt-discard-current-p ()
  (let ((current (aref eden-prompt-history-state 1)))
    (when (not (or (null current) (consp current)))
      (let ((req `(:dir ,eden-dir :uuid ,current)))
        (if (not (condition-case nil (eden-request-read 'prompt req) (error nil)))
            t)))))

(defun eden-prompt-history (direction)
  (let (prompts f)
    (pcase direction
      ('previous (setq prompts (aref eden-prompt-history-state 0))
                 (setq f 'eden-prompt-history-previous))
      ('next (setq prompts (aref eden-prompt-history-state 2))
             (setq f 'eden-prompt-history-next)))
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

;;;; Conversations

(defvar eden-conversations nil "...")
(defvar eden-conversation-id nil "...")

(defun eden-conversation-with-title-exists-p (title)
  (seq-some (lambda (c) (equal title (plist-get (cdr c) :title)))
            eden-conversations))

(defun eden-conversation (action title &optional req-uuid)
  "...

See variables `eden-conversations' and `eden-dir'."
  (cond
   ((eden-conversation-with-title-exists-p title)
    (error "Conversation with title `%s' already exists in `eden-conversations'"
           title))
   ((not (seq-contains-p [start start-from continue-from] action))
    (error "Conversation `action' must be `start', `start-from' or `continue-from' not `%S'"
           action))
   ((and (eq action 'start) req-uuid)
    (error "When action is `start', `req-uuid' argument must be nil or omitted, not `%s'"
           req-uuid))
   ((seq-contains-p [start-from continue-from] action)
    (let ((req `(:dir ,eden-dir :uuid ,req-uuid)))
      (when (null req-uuid)
        (error "When action is `%s', `req-uuid' argument is mandatory"
               action))
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
    `(:uuid ,uuid :dir ,eden-dir)))

(defun eden-conversation-buffer-name (conversation-id)
  (when-let ((title (eden-conversation-title conversation-id)))
    (format "*eden<%s>*" title)))

(defun eden-conversation-exchanges (conversation-id)
  (when-let ((last-req (eden-conversation-last-req
                        conversation-id))
             (conversation (eden-request-conversation last-req)))
    (pcase (eden-conversation-action conversation-id)
      ('start-from (vector (aref conversation (1- (length conversation)))))
      ('continue-from conversation))))

(defun eden-conversation-rename (conversation-id new-title)
  (when (eden-conversation-with-title-exists-p new-title)
    (error "Cannot rename conversation with `%s' which is already a title used by another conversation in `eden-conversations'"
           new-title))
  (when-let ((conversation-data
              (seq-copy
               (cdr (assoc conversation-id eden-conversations)))))
    (setq eden-conversations
          (cons (cons conversation-id
                      (plist-put conversation-data :title new-title))
                (remove (assoc conversation-id eden-conversations)
                        eden-conversations)))))

(defun eden-conversation-rename-current ()
  (interactive)
  (if-let ((buff-name (eden-conversation-buffer-name eden-conversation-id)))
      (let ((old-title (eden-conversation-title eden-conversation-id))
            (new-title (read-string "Rename conversation (to new name): ")))
        (if (string-empty-p new-title)
            (message "Cannot rename current conversation with an empty title.  Please enter a non empty string.")
          (eden-conversation-rename eden-conversation-id new-title)
          (when (get-buffer buff-name)
            (with-current-buffer buff-name
              (save-excursion
                (goto-char (point-min))
                ;; This replacement works because when we insert a
                ;; conversation in the first place with
                ;; `eden-conversation-insert' we put the title
                ;; on the first line of the buffer.
                (when (search-forward old-title (line-end-position) t)
                  (replace-match new-title t t)))
              (rename-buffer (eden-conversation-buffer-name eden-conversation-id))))))
    (message "Cannot rename current conversation which is not set.  Switch to an existing conversation first.")))

(defun eden-conversation-update (info req)
  "...

See `eden-conversation' and `eden-conversations'."
  (let ((conversation-id (plist-get info :conversation-id))
        (req-uuid (plist-get req :uuid)))
    (when-let ((conversation-data
                (seq-copy
                 (cdr (assoc conversation-id eden-conversations)))))
      (let ((data (thread-first
                    conversation-data
                    (plist-put :action 'continue-from)
                    (plist-put :last-req-uuid req-uuid))))
        (setq eden-conversations
              (cons (cons conversation-id data)
                    (remove (assoc conversation-id eden-conversations)
                            eden-conversations)))))))

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

(defun eden-conversation-insert (req title &optional append start-from)
  ""
  (eden-request-check req)
  (when (and (null title) (null append))
    (error "`title' argument can be nil only when `append' argument is non-nil"))
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
       ;; If we change how we insert the title below, we may also
       ;; have to change how we rename conversation title in
       ;; `eden-conversation-rename-current' command.
       "** " (or title "Conversation") "\n"
       ":PROPERTIES:\n"
       ":" eden-org-property-date ": " (or (eden-request-date req) "")
       "\n"
       ":" eden-org-property-req ": " uuid "\n"
       ":END:\n"))
    (dolist (exchange conversation)
      (seq-let (prompt response) exchange
        (insert "*** prompt\n\n" prompt)
        (cond
         ((looking-back "\n\n" nil) nil)
         ((looking-back "\n" nil) (insert "\n"))
         (t (insert "\n\n")))
        (insert "*** response\n\n" response)
        (cond
         ((looking-back "\n\n" nil) nil)
         ((looking-back "\n" nil) (insert "\n"))
         (t (insert "\n\n")))))))

;;;; Sending Requests

(defvar eden-pending-requests nil "...")
(defvar eden-pending-timer nil "...")

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
                          `(:eval
                            ,(concat
                              "| "
                              (propertize
                               (concat "Eden." (make-string (mod idx 3) ?.))
                               'face '(:weight bold)))))
                    (force-mode-line-update 'all)
                    (cl-incf idx))))))))
    ('maybe-stop
     (when (and (not (eden-running-p)) eden-pending-timer)
       (cancel-timer eden-pending-timer)
       (setq eden-pending-timer nil)
       (setq global-mode-string nil)
       (force-mode-line-update 'all)))))

(defun eden-pending-conversation-p (conversation-id)
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
in the buffer \"*Eden*\":

    (lambda (req resp info)
        (with-current-buffer (get-buffer-create \"*Eden*\")
          (org-mode)
          (save-excursion
            (widen)
            (goto-char (point-max))
            (eden-conversation-insert req \"Request\")
            (save-buffer)))
        (eden-pending-remove req)
        (eden-conversation-update info req)
        (eden-mode-line-waiting \\='maybe-stop)
        (message \"Eden received a response\"))

If REQ is part of a conversation present in `eden-conversations',
the conversation id must be specified in INFO argument as value of
`:conversation-id' key.

For instance, if \"conversation-id-foo\" is the id of the some
conversation and REQ is part of that conversation (the next request
in the conversation), INFO argument must be a plist that looks
like this:

    (:conversation-id \"conversation-id-foo\" ...)"
  (let ((conversation-id (plist-get info :conversation-id)))
    (if (eden-pending-conversation-p conversation-id)
        (progn
          (message "Cannot send two concurrent requests in the same conversation.")
          (let ((inhibit-message t))
            (message "req: %S\nconversation: %s" req conversation-id)))
      (let ((callback-error (lambda (req _err _info)
                              (eden-pending-remove req)
                              (eden-mode-line-waiting 'maybe-stop))))
        (push (list :req req
                    :conversation-id conversation-id
                    :proc (eden-request-send req callback callback-error info))
              eden-pending-requests)
        (push (plist-get req :uuid) eden-request-history)
        (eden-prompt-history-state-set)
        (eden-mode-line-waiting 'maybe-start)))))

(cl-defun eden-request (&key prompt system-prompt exchanges
                             stream model temperature
                             api dir)
  (when (null prompt)
    (error "You must provide a prompt via `:prompt' key to build a request"))
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
      :dir ,(or dir
                eden-dir
                (concat (temporary-file-directory) "eden/"))
      :uuid ,(eden-uuid))))

(defun eden-send ()
  ""
  (interactive)
  (eden-send-request
   :req (eden-request
         :prompt (buffer-substring-no-properties (point-min) (point-max))
         :exchanges (eden-conversation-exchanges eden-conversation-id))
   :info `(:conversation-id ,eden-conversation-id)
   :callback (lambda (req _resp info)
               (let* ((conversation-id (plist-get info :conversation-id))
                      (title (or (eden-conversation-title conversation-id)
                                 "Request"))
                      (buff-name
                       (or (eden-conversation-buffer-name conversation-id)
                           (eden-buffer-name "requests")))
                      (buff-already-exist-p (get-buffer buff-name))
                      (append (and conversation-id buff-already-exist-p 'append))
                      (buff (get-buffer-create buff-name)))
                 (with-current-buffer buff
                   (save-excursion
                     (widen)
                     (org-mode)
                     (goto-char (point-max))
                     ;; If `buff' has been newly created we are at
                     ;; the beginning of buffer and `append' is nil such
                     ;; that we insert the conversation completly
                     (eden-conversation-insert req title append)))
                 (when (not (equal (window-buffer (selected-window)) buff))
                   (when-let ((w (or (and eden-pops-up-upon-receipt
                                          (display-buffer
                                           buff '(display-buffer-reuse-window)))
                                     (get-buffer-window buff))))
                     (with-selected-window w
                       (goto-char (point-max))
                       (when (re-search-backward "^\\*\\*\\* response" nil t)
                         (recenter-top-bottom 0)))))
                 (eden-pending-remove req)
                 (eden-conversation-update info req)
                 (eden-mode-line-waiting 'maybe-stop)
                 (message "Eden received a response from %s service"
                          (plist-get eden-api :service)))))
  (erase-buffer)
  (eden-maybe-delete-window-prompt-buffer)
  (message "Eden sent a request to %s service"
           (plist-get eden-api :service)))

;;;; Main menu

(defun eden-buffer-name (&optional title)
  (if title (format "*eden[%s]*" title) "*eden*"))

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
              (eden-conversation-insert
               last-req title nil (eq action 'start-from)))))
        (eden-maybe-delete-window-prompt-buffer)
        (select-window
         (display-buffer buff-name '(display-buffer-reuse-window)))))))

(defun eden-show-current-conversation-in-req-history ()
  (interactive)
  (if-let* ((req-uuid (eden-prompt-current-req-uuid))
            (req `(:dir ,eden-dir :uuid ,req-uuid)))
      (if (condition-case nil (eden-request-check req) (error nil))
          (let* ((title "current conversation in history")
                 (buff (get-buffer-create (eden-buffer-name title))))
            (with-current-buffer buff
              (save-excursion
                (erase-buffer)
                (org-mode)
                (eden-conversation-insert req title)))
            (eden-maybe-delete-window-prompt-buffer)
            (select-window
             (display-buffer buff '(display-buffer-reuse-window))))
        (message (concat "Current prompt is associated with a failed or missing request.  "
                         "Try navigating the prompt history with `M-p' and `M-n', "
                         "default binding of `eden-prompt-previous' and `eden-prompt-next'.")))
    (message (concat "Current prompt is not associated with a request.  "
                     "Try navigating the prompt history with `M-p' and `M-n', "
                     "default binding of `eden-prompt-previous' and `eden-prompt-next'."))))

(defun eden-last-paths (num-of-days)
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
                  (let ((req `(:dir ,eden-dir
                               :uuid ,(car r))))
                    (eden-request-conversation-path req)))))
      (delq nil))))

(defun eden-last-requests (num-of-days)
  (mapcar (lambda (p) (aref p (1- (length p))))
          (eden-last-paths num-of-days)))

(defun eden-last-conversations-keep (paths)
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

(defun eden-last-conversations (num-of-days)
  (eden-last-conversations-keep (eden-last-paths num-of-days)))

(defun eden-show-last-conversations ()
  (interactive)
  (let* ((num-of-days (read-number "Enter the number of days: "))
         (conversations (eden-last-conversations num-of-days))
         (buff (get-buffer-create (eden-buffer-name "last conversations"))))
    (with-current-buffer buff
      (save-excursion
        (erase-buffer)
        (org-mode)
        (dolist (req-uuid conversations)
          (eden-conversation-insert
           `(:dir ,eden-dir :uuid ,req-uuid) "Conversation"))))
    (eden-maybe-delete-window-prompt-buffer)
    (select-window
     (display-buffer buff '(display-buffer-reuse-window)))))

(defun eden-show-last-requests ()
  (interactive)
  (let* ((num-of-days (read-number "Enter the number of days: "))
         (requests (eden-last-requests num-of-days))
         (buff (get-buffer-create (eden-buffer-name "last requests"))))
    (with-current-buffer buff
      (save-excursion
        (erase-buffer)
        (org-mode)
        (dolist (req-uuid requests)
          (eden-conversation-insert
           `(:dir ,eden-dir :uuid ,req-uuid)
           "Request" nil 'start-from))))
    (eden-maybe-delete-window-prompt-buffer)
    (select-window
     (display-buffer buff '(display-buffer-reuse-window)))))

(defun eden-show-current-settings ()
  "..."
  (interactive)
  (let ((buff (get-buffer-create (eden-buffer-name "current settings")))
        (service (plist-get eden-api :service))
        (endpoint (plist-get eden-api :endpoint))
        (model eden-model)
        (temperature (or eden-temperature ""))
        (system-prompt (or eden-system-prompt ""))
        (conversation (or (assoc eden-conversation-id eden-conversations) "")))
    (with-current-buffer buff
      (save-excursion
        (erase-buffer)
        (insert
         (format
          (concat "      service: %s\n"
                  "     endpoint: %s\n"
                  "        model: %s\n"
                  "  temperature: %s\n"
                  " conversation: %s\n"
                  "system prompt: %s\n")
          service endpoint model temperature conversation system-prompt))))
    (eden-maybe-delete-window-prompt-buffer)
    (select-window
     (display-buffer buff '(display-buffer-reuse-window)))))

(defun eden-api-set ()
  "..."
  (interactive)
  (if-let* ((services
             (mapcar (lambda (api) (plist-get api :service)) eden-apis)))
      (let* ((service (completing-read
                       "Choose an API from the following services: "
                       services nil 'require-match))
             (api (seq-some (lambda (api)
                              (when (string= (plist-get api :service) service)
                                api))
                            eden-apis))
             (default-model (plist-get api :default-model)))
        (setq eden-api api)
        (when default-model (setq eden-model default-model)))
    (error
     (format
      (concat "`eden-apis' variable must be a list of API specifications like this\n\n"
              "((:service \"openai\"
  :endpoint \"https://api.openai.com/v1/chat/completions\"
  :default-model \"gpt-4o-mini\"
  :models (\"gpt-4o-mini\" \"gpt-4o\" \"o1-mini\" \"o1\"))
 (:service \"perplexity\"
  :endpoint \"https://api.perplexity.ai/chat/completions\"
  :default-model \"llama-3.1-sonar-small-128k-online\"
  :models (\"llama-3.1-sonar-small-128k-online\"
           \"llama-3.1-sonar-large-128k-online\"
           \"llama-3.1-sonar-huge-128k-online\")))\n\n"
              "not `%S'")
      eden-apis))))

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

(defun eden-system-prompt-set ()
  (interactive)
  (let ((err
         (format
          (concat "`eden-system-prompts' variable must be nil or an alist like this\n\n"
                  "((\"writer\" . \"You're a good writer who only writes in Italian.\")
 (\"programmer\" . \"You're a programmer who only answers with code snippets.\"))\n\n"
                  "not `%S'")
          eden-system-prompts)))
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
    ("r" "Rename current conversation" eden-conversation-rename-current)
    ("SPC" "Pause current conversation" eden-conversation-pause)
    ("TAB" "Switch conversation" eden-conversation-switch)]]
  [["Conversations and requests"
    ("v" "Show current conversation" eden-show-current-conversation)
    ("h" "Show current conversation in history" eden-show-current-conversation-in-req-history)
    ("l" "Show last conversations" eden-show-last-conversations)
    ("L" "Show last requests" eden-show-last-requests)
    ("k" "Kill last request" eden-kill-last-request)
    ("g" "Go to current request in history" eden-prompt-current-goto)]
   ["Model"
    ("a" "Set current API" eden-api-set)
    ("m" "Set model for current API" eden-model-set)
    ("t" "Set temperature" eden-temperature-set)
    ("p" "Set system prompt" eden-system-prompt-set)
    ("S" "Show current settings" eden-show-current-settings)]])

;;;; Request at point menu

(defun eden-req-at-point-uuid ()
  (if-let* ((req-uuid (org-entry-get nil eden-org-property-req))
            (req-dir (eden-request-dir
                      `(:dir ,eden-dir :uuid ,req-uuid))))
      (if (file-exists-p req-dir)
          req-uuid
        (error "Request `%s' doesn't exist" req-dir))
    (error "No request at point found")))

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
              (req `(:dir ,eden-dir :uuid ,req-uuid))
              (requests
               (mapcar
                (lambda (exchange)
                  `(:dir ,eden-dir :uuid ,(plist-get exchange :uuid)))
                (eden-request-conversation req)))
              (buff (get-buffer-create
                     (eden-buffer-name "requests of conversation at point"))))
    (with-current-buffer buff
      (erase-buffer)
      (org-mode)
      (save-excursion
        (dolist (req requests)
          (eden-conversation-insert req "Request" nil 'start-from))))
    (select-window
     (display-buffer buff '(display-buffer-reuse-window)))))

(defun eden-req-at-point-show-perplexity-citations ()
  (interactive)
  (when-let* ((req-uuid (eden-req-at-point-uuid))
              (req `(:dir ,eden-dir :uuid ,req-uuid)))
    (if-let ((citations (eden-request-perplexity-citations req)))
        (let ((buff (get-buffer-create
                     (eden-buffer-name "perplexity citations"))))
          (with-current-buffer buff
            (erase-buffer)
            (org-mode)
            (save-excursion
              (dolist (citation citations)
                (insert (format "- %s\n" citation)))))
          (select-window
           (display-buffer buff '(display-buffer-reuse-window))))
      (message "No citations for `%s' conversation"
               (eden-request-dir req)))))

(defun eden-req-at-point-goto ()
  (interactive)
  (when-let ((req-uuid (eden-req-at-point-uuid))
             (req-dir (eden-request-dir
                       `(:dir ,eden-dir :uuid ,req-uuid))))
    (dired req-dir)))

(transient-define-prefix eden-req-at-point-menu ()
  "Insert menu"
  [["Conversation/Request at point"
    ("s" "Start conversation from request at point" eden-req-at-point-start-conversation)
    ("c" "Continue conversation from request at point" eden-req-at-point-continue-conversation)
    ("R" "Show requests of conversation at point" eden-req-at-point-show-requests)
    ("C" "Show Perplexity citations of conversation at point" eden-req-at-point-show-perplexity-citations)
    ("g" "Go to directory of request at point" eden-req-at-point-goto)
    ]])

;;;; Main command

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
     (:propertize "%6b" face mode-line-buffer-id)
     (:eval (format " %s/%s"
                    (plist-get eden-api :service)
                    (truncate-string-to-width eden-model 16 nil nil t)))
     (:eval (when eden-temperature (format " <%s>" eden-temperature)))
     (:eval (when-let ((system-prompt-title (car-safe eden-system-prompt)))
              (format " %s"
                      (truncate-string-to-width
                       system-prompt-title 24 nil nil t))))
     (:eval (when eden-conversation-id
              (concat (propertize " *" 'face '(:weight bold))
                      (format " %s" (eden-conversation-title eden-conversation-id)))))
     " "
     mode-line-misc-info))
  (eden-request-history-set)
  (eden-prompt-history-state-set))

(defun eden (&optional arg)
  ""
  (interactive "P")
  (let ((error-fmt
         (concat
          "To use `eden' you must have `%s' utility installed.  "
          "Be sure it is in one of `exec-path' directories.")))
    (cond
     ((not (executable-find "curl")) (error (format error-fmt "curl")))
     ((not (executable-find "uuidgen")) (error (format error-fmt "uuidgen")))
     ((not (executable-find "pandoc")) (error (format error-fmt "pandoc")))))
  (cond
   (arg (call-interactively 'eden-req-at-point-menu))
   ((string= (buffer-name) eden-prompt-buffer-name)
    (call-interactively 'eden-menu))
   (t (let ((eden-buffer-p (get-buffer eden-prompt-buffer-name))
            (buff (get-buffer-create eden-prompt-buffer-name)))
        (select-window
         (display-buffer-at-bottom buff '(display-buffer-below-selected
                                          (window-height . 6))))
        (when (not eden-buffer-p)
          (eden-mode))))))

(provide 'eden)

;;; eden.el ends here

;; Local Variables:
;; byte-compile-warnings: (not docstrings)
;; End:
