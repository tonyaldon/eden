;;; eden.el --- A simple ChatGPT client for Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Tony Aldon
;;
;; Author: Tony Aldon <tony@tonyaldon.com>
;; Version: 1.33
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://tonyaldon.com
;;
;;; Commentary:
;;
;; Eden is a simple ChatGPT client for Emacs that focuses on conversations.
;;
;;; Code:

(require 'json)
(require 'ox-md)
(require 'transient)
(require 'cl-macs)

(defalias 'eden-get-in 'map-nested-elt)

;;; API to send asynchronous requests to OpenAI and Perplexity

(defun eden-plist-delete (plist property)
  "Delete PROPERTY from PLIST."
  ;; From org-macs.el.
  (let (p)
    (while plist
      (if (not (eq property (car plist)))
	        (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun eden-plist-merge (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting properties
from the other lists.  Settings in the last list are the most significant
ones and overrule settings in the other lists."
  ;; From org-macs.el.
  (let ((rtn (copy-sequence (pop plists)))
	      p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
	      (setq p (pop ls) v (pop ls))
	      (setq rtn (plist-put rtn p v))))
    rtn))

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
    (when (not (stringp dir))
      (signal 'eden-error-req
               (list (format "`:dir' key missing or not a string: %S" req))))
    (when (not (stringp uuid))
      (signal 'eden-error-req
               (list (format "`:uuid' key missing or not a string: %S" req))))
    (concat (file-name-as-directory (expand-file-name dir)) uuid "/")))

(defun eden-request-file (req file-type)
  "Return full path of file of FILE-TYPE of REQ request.

Signal an error if FILE-TYPE is not one of the following symbols:

    error, response, response-org, reasoning, request, api,
    prompt, system-message, exchanges, command.

For instance

    (eden-request-file \\='(:dir \"/tmp/eden/\" :uuid \"foo\") \\='request)
    ;; \"/tmp/eden/foo/request.json\"
    (eden-request-file \\='(:dir \"/tmp/eden/\" :uuid \"foo\" \\='prompt))
    ;; \"/tmp/eden/foo/prompt.org\""
  (let* ((filenames '((error          . "error.json")
                      (response       . "response.json")
                      (response-org   . "response.org")
                      (reasoning      . "reasoning.org")
                      (request        . "request.json")
                      (api            . "api.json")
                      (prompt         . "prompt.org")
                      (system-message . "system-message.org")
                      (exchanges      . "exchanges.json")
                      (command        . "command")))
         (filename (alist-get file-type filenames)))
    (if filename
        (concat (eden-request-dir req) filename)
      (error "`file-type' argument must be one of %s, not `%s'"
             (mapcar #'car filenames) file-type))))

(defun eden-request-read (req file-type)
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
      \"messages\": [
        {
          \"role\": \"user\",
          \"content\": \"foo bar baz\"
        }
      ]
    }

we have the following:

    (eden-request-read \\='(:dir \"/tmp/eden/\" :uuid \"foo\") \\='request)
    ;; (:stream :false
    ;;  :model \"gpt-4o-mini\"
    ;;  :messages [(:role \"user\" :content \"foo bar baz\")])

And for a non JSON file like the file \"prompt.org\" (assuming the
user prompt was \"foo bar baz\") we get something like this:

    (eden-request-read \\='(:dir \"/tmp/eden/\" :uuid \"foo\") \\='prompt)
    ;; \"foo bar baz\""
  (let* ((file (eden-request-file req file-type)))
    (if (not (file-exists-p file))
        (error "Missing `%s' file" file)
      (with-temp-buffer
        (insert-file-contents (eden-request-file req file-type))
        (if (string= (file-name-extension file) "json")
            (eden-json-read)
          (buffer-substring-no-properties (point-min) (point-max)))))))

(defun eden-request-assistant-content (resp)
  "Return the content of RESP response from OpenAI-compatible API.

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
    ;; \"foo assistant\"

Also works with Anthropic API which responses are like this

    (:id \"msg_011Cm7DW1Gz27innVYwhuWi9\"
     :type \"message\"
     :role \"assistant\"
     :model \"claude-3-5-haiku-20241022\"
     :content [(:type \"text\"
                :text \"foo assistant\")]
     :stop_reason \"end_turn\"
     :stop_sequence nil
     :usage (:input_tokens 11
             :cache_creation_input_tokens 0
             :cache_read_input_tokens 0
             :output_tokens 149
             :service_tier \"standard\"))

or this with reasoning models:

    (:id \"msg_011Cm7DW1Gz27innVYwhuWi9\"
     :type \"message\"
     :role \"assistant\"
     :model \"claude-3-7-sonnet-20250219\"
     :content [(:type \"thinking\" :thinking \"foo thinking\" :signature \"...\")
               (:type \"text\" :text \"foo assistant\")]
     :stop_reason \"end_turn\"
     :stop_sequence nil
     :usage (:input_tokens 11
             :cache_creation_input_tokens 0
             :cache_read_input_tokens 0
             :output_tokens 149
             :service_tier \"standard\"))"
  (or (eden-get-in resp [:choices 0 :message :content])
      ;; For Anthropic API
      (catch 'found
        (dolist (elt (append (plist-get resp :content) nil))
          (when (equal (plist-get elt :type) "text")
            (throw 'found (plist-get elt :text))))
        nil)))

(defun eden-request-assistant-reasoning (resp)
  "Return the reasoning content of RESP response from Deepseek-compatible API.

A Deepseek-compatible API is an OpenAI-compatible API in which
`:reasoning_content' key has been added at the same level of `:content'
key for reasoning models like \"deepseek-reasoner\".

For instance (with some keys omitted from a real response from
Deepseek API) we have:

    (let ((resp \\='(:id \"5b5178d0-9cca-4a8b-86f9-6971ce2c1788\"
                  :object \"chat.completion\"
                  :created 1738222989
                  :model \"deepseek-reasoner\"
                  :choices [(:index 0
                             :message (:role \"assistant\"
                                       :content \"foo assistant\"
                                       :reasoning_content \"foo reasoning\")
                             :logprobs nil
                             :finish_reason \"stop\")])))
      (eden-request-assistant-reasoning resp))
    ;; \"foo reasoning\"

We also support Perplexity and Anthropic even if they do it a bit differently."
  (or (eden-get-in resp [:choices 0 :message :reasoning_content])
      ;; Because Perplexity do it differently and I use Perplexity
      (when-let ((msg (eden-get-in resp [:choices 0 :message :content])))
        (when (string-match "\\`<think>\\(\\(.\\|\n\\)*?\\)</think>" msg)
          (match-string 1 msg)))
      ;; Because Anthropic do it differently
      (catch 'found
        (dolist (elt (append (plist-get resp :content) nil))
          (when (equal (plist-get elt :type) "thinking")
            (throw 'found (plist-get elt :thinking))))
        nil)))

(defun eden-request-check (req)
  "Return t if REQ did complete.

Signal an error in the following cases:

- the request doesn't exist,
- the request has failed in a prior attempt (so `error.json' file exists),
- the request is incomplete, specifically when at least one of the following
  files is missing:

  - api.json
  - prompt.org
  - request.json
  - response.json
  - response.org
  - exchanges.json
  - timestamp-<timestamp> where <timestamp> is a timestamp."
  (let ((req-dir (eden-request-dir req)))
    (cond
     ((not (file-exists-p (eden-request-dir req)))
      (error "Request `%s' doesn't exist" req-dir))
     ((file-exists-p (eden-request-file req 'error))
      (error "Request `%s' has failed in a prior attempt.  See why in `%s' file"
             req-dir (eden-request-file req 'error)))
     ((not (file-exists-p (eden-request-file req 'api)))
      (error "Missing `%s' file" (eden-request-file req 'api)))
     ((not (file-exists-p (eden-request-file req 'prompt)))
      (error "Missing `%s' file" (eden-request-file req 'prompt)))
     ((not (file-exists-p (eden-request-file req 'request)))
      (error "Missing `%s' file" (eden-request-file req 'request)))
     ((not (file-exists-p (eden-request-file req 'response)))
      (error "Missing `%s' file" (eden-request-file req 'response)))
     ((not (file-exists-p (eden-request-file req 'response-org)))
      (error "Missing `%s' file" (eden-request-file req 'response-org)))
     ((not (file-exists-p (eden-request-file req 'exchanges)))
      (error "Missing `%s' file" (eden-request-file req 'exchanges)))
     ((null (directory-files req-dir nil "timestamp-.*"))
      (error "Missing timestamp file in `%s' request" req-dir))
     (t t))))

(defun eden-request-conversation (req)
  "Return all exchanges of the conversation whose last request is REQ.

REQ is meant to be passed with only the keys `:dir' and `:uuid'.

The return value is a vector of plists (exchanges) containing the
following keys:

- :uuid                 - UUID of the request for that exchange
- :prompt               - prompt of that exchange (`org-mode' string)
- :response             - content message of the response of that exchange
                          received from OpenAI-compatible API converted to Org
                          (`org-mode' string)
- :reasoning            - (optional) reasoning content of the response of that exchange
                          received from Deepseek-compatible API converted to Org
                          (`org-mode' string)
- :context              - (optional) TODO

Signal an error if REQ doesn't pass `eden-request-check' check.

For instance, assuming \"uuid-baz\" is the UUID of the last request
of a conversation whose previous exchanges are the requests whose
UUIDs are \"uuid-foo\" and \"uuid-bar\" in that order, the following
function call

    (eden-request-conversation \\='(:dir \"/tmp/eden/\" :uuid \"uuid-baz\"))

gives use the following conversation:

    [(:uuid \"uuid-foo\"
      :prompt \"foo prompt\\n\"
      :response \"foo assistant\\n\")
     (:uuid \"uuid-bar\"
      :prompt \"bar prompt\\n\"
      :response \"bar assistant\\n\")
     (:uuid \"uuid-baz\"
      :prompt \"baz prompt\\n\"
      :response \"baz assistant\\n\"
      :context [(:role \"user\" :content \"foo user\")
                (:role \"assistant\" :content \"foo assistant\\n\")
                (:role \"user\" :content \"bar user\")
                (:role \"assistant\" :content \"bar assistant\\n\")
                (:role \"user\" :content \"baz user\")
                (:role \"assistant\" :content \"baz assistant\\n\")])]"
  (eden-request-check req)
  (let* ((api (eden-request-read req 'api))
         (request (eden-request-read req 'request))
         ;; Exchanges up to req excluded without :context which
         ;; will be attached to the current exchange.
         (prev-exchanges
          (mapcar (lambda (e) (eden-plist-delete e :context))
                  (eden-request-read req 'exchanges)))
         (resp (eden-request-read req 'response))
         (output
          (if (eden-api-is-responses-p api)
              nil ;; TODO (:output)
            (vector (eden-get-in resp [:choices 0 :message]))))
         (input
          (if (eden-api-is-responses-p api)
              nil ;; TODO (:output)
            (let ((msgs (plist-get request :messages)))
              ;; Don't include the system message in the context
              (if (string= (plist-get (aref msgs 0) :role) "system")
                  (seq-subseq msgs 1)
                msgs))))
         (reasoning (ignore-errors (eden-request-read req 'reasoning)))
         (exchange
          (delq nil
                `(:uuid ,(plist-get req :uuid)
                  :prompt ,(eden-request-read req 'prompt)
                  :response ,(eden-request-read req 'response-org)
                  ,@(when reasoning `(:reasoning ,reasoning))
                  :context ,(seq-concatenate 'vector input output)))))
    (apply 'vector (append prev-exchanges (list exchange)))))

(defun eden-request-conversation-path (req)
  "Return the path of the conversation whose last request is REQ.

Return nil if REQ doesn't pass `eden-request-check' check.

For instance, assuming \"uuid-baz\" is the UUID of the last request
of a conversation whose previous exchanges are the requests whose
UUIDs are \"uuid-foo\" and \"uuid-bar\" in that order, we have
the following:

    (eden-request-conversation-path \\='(:dir \"/tmp/eden/\" :uuid \"uuid-baz\"))
    ;; [\"uuid-foo\" \"uuid-bar\" \"uuid-baz\"]"
  (when (ignore-errors (eden-request-check req))
    (let* ((uuids (mapcar (lambda (exchange) (plist-get exchange :uuid))
                          (eden-request-read req 'exchanges)))
           (last-uuid (list (plist-get req :uuid))))
      (apply 'vector (append uuids last-uuid)))))

(defun eden-request-citations (req)
  "Return the list of citations of the conversation REQ.

More precisely of the conversation whose last request is REQ.

When using Perplexity API or OpenAI web search models, the JSON response
to a request may contain citations (urls).  These citations are accessible

- for Perplexity API under the key \"citations\" represented as an array.

      [\"https://foo-1.com\" \"https://foo-2.com\"]

- for OpenAI web search models under the key \"annotations\" of the
  message represented as an array:

      [(:type \"url_citation\"
        :url_citation (:end_index 559
                       :start_index 448
                       :title \"foo-1 title\"
                       :url \"https://foo-1.com\"))
       (:type \"url_citation\"
        :url_citation (:end_index 894
                       :start_index 818
                       :title \"foo-2\"
                       :url \"https://foo-2.com\"))]

The function `eden-request-citations' returns the
concatenation of all these citations for the conversation
whose last request is REQ.

For instance, assuming \"uuid-baz\" is the UUID of the third and
last request of a conversation whose first conversation has the
citations

    [\"https://foo-1.com\" \"https://foo-2.com\" \"https://foo-3.com\"]

the second request contains no citations at all and the third
request has the citations

    [\"https://baz-1.com\" \"https://baz-2.com\"]

then the following function call

    (eden-request-citations \\='(:dir \"/tmp/eden/\" :uuid \"uuid-baz\"))

gives use the following citations:

    (\"https://foo-1.com\" \"https://foo-2.com\" \"https://foo-3.com\"
     \"https://baz-1.com\" \"https://baz-2.com\")

See `eden-req-at-point-show-citations'."
  (let ((dir (plist-get req :dir)))
    (seq-reduce
     (lambda (acc exchange)
       (let* ((uuid-exchange (plist-get exchange :uuid))
              (req-exchange `(:dir ,dir :uuid ,uuid-exchange)))
         (let* ((resp (eden-request-read req-exchange 'response))
                (citations (or (plist-get resp :citations) ;; perplexity
                               ;; openai web search
                               (mapcar (lambda (elt)
                                         (eden-get-in elt [:url_citation :url]))
                                       (eden-get-in resp [:choices 0 :message :annotations])))))
           (append acc citations '()))))
     (eden-request-conversation req)
     '())))

(defun eden-request-timestamp (req)
  "Return the timestamp (a float) of the moment REQ request was sent to OpenAI-compatible API.

Just before we send a request to OpenAI-compatible API, we store
the information related to the request using `eden-write-request'
function.  One of the file we write to disk is a timestamp file
whose filename relative to the request's directory is of the form

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

For instance if the some request with \"foo\" UUID logged in
\"/tmp/eden/\" directory has the timestamp file
\"timestamp-1733921715.2331347\" in its directory, then we
have the following:

    (eden-request-timestamp (:dir \"/tmp/eden/\" :uuid \"foo\"))
    ;; [2024-12-11 Wed]

See `eden-request-timestamp'."
  (when-let ((timestamp (eden-request-timestamp req)))
    (format-time-string "[%Y-%m-%d %a]" (seconds-to-time (floor timestamp)))))

(defun eden-request-write (req file-type content)
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
                     (eden-request-file req file-type))))
    (make-directory (eden-request-dir req) 'parents)
    (with-temp-buffer
      (insert content)
      (write-file file-path))))

(defun eden-write-request (req)
  "Write REQ's information in several files in REQ's directory.

Specifically, 6 files are written to disk:

- a `timestamp' file      - see `eden-request-timestamp' for more details
                            on that file,
- a `request' file        - a JSON file with content being the value
                            under `:req-params' key of REQ plist,
- an `api' file           - a JSON file with content being the value
                            under `:api' key of REQ plist,
- a `prompt' file         - an `org-mode' file with content being the value
                            under `:prompt' key,
- a `system-message' file - an `org-mode' file with content being the value
                            under `:system-message' key,
- an `exchanges' file     - a JSON file with content being the value
                            under `:exchanges' key of REQ plist.  It
                            corresponds to all the exchanges with the
                            API up to REQ excluded.

Here's an example with a typical request (third of a conversation)
that we would send to OpenAI API.  Evaluating the following expression

    (let ((req \\='(:req-params (:stream :false
                              :model \"gpt-4o-mini\"
                              :messages [(:role \"system\" :content \"baz system message\")
                                         (:role \"user\" :content \"foo user\")
                                         (:role \"assistant\" :content \"foo assistant\")
                                         (:role \"user\" :content \"bar prompt\")
                                         (:role \"assistant\" :content \"bar assistant\")
                                         (:role \"user\" :content \"baz user prompt\")])
                 :api (:service \"openai\"
                       :endpoint \"https://api.openai.com/v1/chat/completions\")
                 :prompt \"baz user prompt\"
                 :system-message \"baz system message\"
                 :exchanges [(:uuid \"uuid-foo\"
                              :prompt \"foo prompt\"
                              :response \"foo response\")
                             (:uuid \"uuid-bar\"
                              :prompt \"bar prompt\"
                              :response \"bar response\"
                              :context [(:role \"user\" :content \"foo prompt\")
                                        (:role \"assistant\" :content \"foo response\")
                                        (:role \"user\" :content \"bar prompt\")
                                        (:role \"assistant\" :content \"bar response\")])]
                 :dir \"/tmp/eden/\"
                 :uuid \"uuid-baz\")))
      (eden-write-request req))

produces the following files (with timestamp file depending on
the moment we evaluate that expression):

- /tmp/eden/uuid-baz/api.json
- /tmp/eden/uuid-baz/exchanges.json
- /tmp/eden/uuid-baz/prompt.org
- /tmp/eden/uuid-baz/request.json
- /tmp/eden/uuid-baz/system-message.org
- /tmp/eden/uuid-baz/timestamp-1736137516.050655"
  (let ((request (eden-json-encode (plist-get req :req-params)))
        (api (eden-json-encode (plist-get req :api)))
        (prompt (plist-get req :prompt))
        (system-message (or (plist-get req :system-message) ""))
        (exchanges (eden-json-encode (plist-get req :exchanges))))
    (eden-request-write req 'timestamp "")
    (eden-request-write req 'request request)
    (eden-request-write req 'api api)
    (eden-request-write req 'prompt prompt)
    (eden-request-write req 'system-message system-message)
    (eden-request-write req 'exchanges exchanges)))

(defun eden-api-is-responses-p (api)
  "..."
  (string-suffix-p "/responses" (plist-get api :endpoint)))

(defun eden-api-key-symbol (service)
  "Return the symbol we use for holding api key for SERVICE service.

Precisely, return `eden-api-key-SERVICE' symbol as in the following
example:

    (eden-api-key-symbol \"openai\")
    ;; eden-api-key-openai

When we want to use `eden-request-send' programmatically without
asking the user (and so gpg) for the encrytped key in `~/.authinfo.gpg'
file we can use `eden-api-key-symbol' to set the api key like this
assumming SERVICE is \"openai\":

    (let ((api-key-symbol (eden-api-key-symbol \"openai\")))
      (defvar-1 api-key-symbol nil)
      (set api-key-symbol \"secret-api-key\")
      ;; call `eden-request-send' here
      nil)"
  (intern (format "eden-api-key-%s" service)))

(defun eden-request-command (req)
  "Return curl command we use to send REQ request to OpenAI-compatible API.

More precisely, return a list whose

1) first element is the command line with OpenAI-compatible API
   key that we actually use to send REQ and
2) second element is the same command line as in 1) but with
   the API key replaced with the placeholder <api-key> such
   that we can use it safely for logging.

REQ requires `:api', `:dir' and `:uuid' keys as in the following
example:

    (:api (:service \"openai\"
           :endpoint \"https://openai-endpoint\")
     :dir \"/tmp/eden/\"
     :uuid \"uuid-foo\")

The function `eden-request-command' not only produces these command
lines but also

1) retrieves OpenAI-compatible API key from `~/.authinfo.gpg'
   (encrypted with gpg) or `~/.authinfo' file looking for a line
   like this

       machine openai password <openai-api-key>

   where `openai' is found under [:api :service] path in REQ.

2) and keeps it in a variable determined by `eden-api-key-symbol'."
  (let* ((endpoint (eden-get-in req [:api :endpoint]))
         (service (eden-get-in req [:api :service]))
         (api-key-symbol (eden-api-key-symbol service))
         (request-file (eden-request-file req 'request))
         (command-fmt (if (string= service "anthropic")
                          (concat "curl -s -X POST %s "
                                  "-H 'x-api-key: %s' "
                                  (format "-H 'anthropic-version: %s' "
                                          (eden-get-in req [:api :anthropic-version]))
                                  "-H 'Content-Type: application/json' -d @%s")
                        (concat "curl -s -X POST %s "
                                "-H 'Authorization: Bearer %s' "
                                "-H 'Content-Type: application/json' -d @%s"))))
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
               (concat "Do you have a line in `~/.authinfo.gpg' file declaring "
                       "the API key of service `%s' like this: "
                       "machine %s password <api-key>")
               service service)))
    (list
     (format command-fmt endpoint (eval api-key-symbol) request-file)
     (format command-fmt endpoint "<api-key>" request-file))))

(defun eden-write-command (req command-no-api-key)
  "Write COMMAND-NO-API-KEY in file of type `command' in REQ's directory.

See `eden-request-file' and `eden-request-command'."
  (message "%s" command-no-api-key)
  (eden-request-write req 'command command-no-api-key))

(defun eden-markdown-to-org (markdown-str)
  "Return MARKDOWN-STR markdown string converted into `org-mode' string."
  (let* ((tmp-dir (concat (temporary-file-directory) "eden-tmp/"))
         (_ (make-directory tmp-dir 'parent))
         (file-markdown (make-temp-file tmp-dir nil ".md" markdown-str))
         (file-org (make-temp-file tmp-dir nil ".org"))
         (file-pandoc-filter (concat tmp-dir "pandoc-filter.lua"))
         (inhibit-message t))
    (when (not (file-exists-p file-pandoc-filter))
      (with-temp-buffer
        ;; Pandoc filter
        ;; 1) Remove org properties added by pandoc org backend.  Specifically,
        ;;    a CUSTOM_ID property is added to all headings which is annoying.
        ;; 2) Don't use example blocks, only source blocks with language name
        (insert "function Header(el)
    return pandoc.Header(el.level, el.content, pandoc.Attr())
end

function CodeBlock(block)
  if block.classes[1] ~= nil then
    return pandoc.RawBlock('org', string.format(\"#+BEGIN_SRC %s\\n%s\\n#+END_SRC\\n\\n\", block.classes[1], block.text))
  else
    return pandoc.RawBlock('org', string.format(\"#+BEGIN_SRC text\\n%s\\n#+END_SRC\\n\\n\", block.text))
  end
end")
        (write-file file-pandoc-filter)))
    (call-process "pandoc" nil nil nil file-markdown "-o" file-org
                  (format "--lua-filter=%s" file-pandoc-filter))
    (with-temp-buffer
      (insert-file-contents file-org)
      (buffer-string))))

(defun eden-org-replace-perplexity-citations (org-str citations)
  "Replace `[idx]' occurences in ORG-STR with corresponding urls in CITATIONS.

It's maybe clearer with an example:

    (eden-org-replace-perplexity-citations
     \"replace citation[1] and citations [1][2] but not inline code =arr[1]=\"
     [\"https://foo.com\" \"https://bar.com\"])
    ;; \"replace citation[[[https://foo.com][1]]] and citations [[[https://foo.com][1]]][[[https://bar.com][2]]] but not inline code =arr[1]=\""
  (let* ((citations-len (length citations)))
    (with-temp-buffer
      (org-mode)
      (save-excursion (insert org-str))
      (font-lock-ensure)
      (while (re-search-forward "\\[\\([0-9]+\\)\\]" nil t)
        (let* ((citation-number-str (match-string 1))
               (citation-idx (1- (string-to-number citation-number-str)))
               (face-or-faces (get-text-property (match-beginning 0) 'face))
               (faces (and face-or-faces
                           (if (consp face-or-faces)
                               face-or-faces
                             (list face-or-faces)))))
          (when (and (not (seq-contains-p faces 'org-code))
                     (not (seq-contains-p faces 'org-verbatim))
                     (not (seq-contains-p faces 'org-block)))
            (if (<= 0 citation-idx citations-len)
                (replace-match (format "[[%s][%s]]"
                                       (aref citations citation-idx)
                                       citation-number-str)
                               nil nil nil 1)))))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun eden-write-response (req resp-str resp)
  "Write response files of REQ request.

RESP-STR is used for the content of `response' file type.

The markdown content from RESP response is converted to `org-mode'
format, replacing citation references with actual citations when using
Perplexity API, and then saved as `response-org' file type.

If RESP includes reasoning content, it is converted to `org-mode'
format and saved as `reasoning' file type.

Note that RESP is just the object representation of the JSON string
RESP-STR.

See `eden-request-file', `eden-markdown-to-org' and
`eden-org-replace-perplexity-citations'."
  (eden-request-write req 'response resp-str)
  (let* ((assistant-content (eden-request-assistant-content resp))
         ;; Because Perplexity do it differently and I use Perplexity
         (assistant-content-filtered
          (if (string-match "\\`<think>\\(?:.\\|\n\\)*?</think>\\(\\(.\\|\n\\)*\\)"
                            assistant-content)
              (match-string 1 assistant-content)
            assistant-content))
         (response-org (eden-markdown-to-org assistant-content-filtered))
         (citations (plist-get resp :citations))
         (response-org
          (if (and citations (vectorp citations))
              (eden-org-replace-perplexity-citations response-org citations)
            response-org)))
    (eden-request-write req 'response-org response-org))
  (when-let* ((assistant-reasoning (eden-request-assistant-reasoning resp))
              (reasoning (eden-markdown-to-org assistant-reasoning)))
    (eden-request-write req 'reasoning reasoning)))

(defun eden-write-error (req err)
  "Write ERR error in REQ's directory.

ERR is non string object that we encode to JSON format
and saved as error file type in REQ's directory.

See `eden-request-file'."
  (eden-request-write req 'error (eden-json-encode err)))

(defvar eden-errors
  '((eden-error-req . "Bad request")
    (eden-error-api . "API error")
    (eden-error-api-key . "Error setting API key")
    (eden-error-callback . "Error while calling callback function in sentinel")
    (eden-error-callback-error . "Error while calling callback-error function when signaling an error in sentinel")
    (eden-error-json-read . "Error while parsing JSON in process buffer")
    (eden-error-process . "The process did not finished correctly")
    (eden-error-process-buffer . "The process buffer got killed while processing the request"))
  "Alist of error types and their messages.

See `eden-error-log-and-signal'.")

(dolist (err eden-errors)
  (define-error (car err) (cdr err)))

;; It's behavior is unit tested when we test `eden-sentinel'.
(cl-defun eden-error-log-and-signal (req type process
                                         &key error event process-stdout
                                         callback-error)
  "Signal error of TYPE type that happened when sending REQ request.

Before signaling the error:

1) PROCESS process is killed,
2) produce the error data,
2) CALLBACK-ERROR function is called with 2 arguments: REQ and
   the error data.
3) the error data (maybe modified by CALLBACK-ERROR) is logged
   to REQ's directory in an error file type.

The error data is a plist with the following keys:

- :type                   - TYPE (see `eden-errors')
- :message                - see `eden-errors'
- :directory              - REQ's directory
- :request                - the request we sent to OpenAI-compatible API
- :error                  - (optional) ERROR
- :process-event          - (optional) EVENT
- :process-buffer-content - (optional) stdout of the curl request
- :original-error         - (optional) the error data before calling
                            CALLBACK-ERROR function.  This happens only
                            when CALLBACK-ERROR signals an error.

For instance if we provide a wrong API key to OpenAI-compatible API the
error data looks like this:

    (:type \"eden-error-api\"
     :message \"API error\"
     :directory \"/tmp/eden/40e73d38-7cb9-4558-b11f-542f8a2d1f9c/\"
     :request (:stream :false
               :model \"gpt-4o-mini\"
               :messages [(:role \"user\" :content \"foo bar baz\")])
     :error (:message \"Incorrect API key provided: eesk-pro***WmEA. You can find your API key at https://platform.openai.com/account/api-keys.\"
             :type \"invalid_request_error\"
             :param nil
             :code \"invalid_api_key\"))"
  (let* ((error-function
          (lambda (type req error event process-stdout original-error)
            (delq nil
                  `(:type ,(symbol-name type)
                    :message ,(alist-get type eden-errors)
                    :directory ,(eden-request-dir req)
                    :request ,(plist-get req :req-params)
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
          (funcall callback-error req err)
        (error
         (setq type 'eden-error-callback-error)
         (setq err (funcall error-function type req -error nil nil err)))))
    (eden-write-error req err)
    (signal type err)))

(defmacro eden-sentinel (req callback callback-error)
  "Return a sentinel to be used in `eden-request-send'.

The return sentinel is a function that takes two arguments `process'
and `event' as described in `make-process'.

When no error occurs during execution of the sentinel, CALLBACK function
is called.  It takes 2 arguments

- REQ   - plist of information about the request where :req-params
          is an OpenAI-compatible API request.  For instance:

              (:req-params (:stream :false
                            :model \"gpt-4o-mini\"
                            :messages [(:role \"user\" :content \"foo bar baz\")])
               :api (:service \"openai\"
                     :endpoint \"https://api.openai.com/v1/chat/completions\")
               :prompt \"foo bar baz\"
               :dir \"/tmp/eden/\"
               :uuid \"40e73d38-7cb9-4558-b11f-542f8a2d1f9c\")

- resp  - plist of the response received from OpenAI-compatible API

and must be use for side effects.

When an error occurs, CALLBACK-ERROR function (if not nil) is called
just before signaling the error.  It takes 2 arguments:

- REQ  - the same as decscribed above,
- err  - plist describing the error which is also the data that is
         associated with error when signaled with `signal' function
         in `eden-error-log-and-signal'.  For instance if we provide
         OpenAI-compatible API with a wrong API key `err' looks like
         this:

             (:type \"eden-error-api\"
              :message \"API error\"
              :directory \"/tmp/eden/40e73d38-7cb9-4558-b11f-542f8a2d1f9c/\"
              :request (:stream :false
                        :model \"gpt-4o-mini\"
                        :messages [(:role \"user\" :content \"foo bar baz\")])
              :error (:message \"Incorrect API key provided: eesk-pro***WmEA. You can find your API key at https://platform.openai.com/account/api-keys.\"
                      :type \"invalid_request_error\"
                      :param nil
                      :code \"invalid_api_key\"))

         The keys `:type', `:message', `:directory', `:request' are
         always present while `:error', `:process-event',
         `:process-buffer-content' keys are optionals and depend of the
         type of error.

         See `eden-errors' and `eden-error-log-and-signal'."
  `(lambda (process event)
     (let ((stdout (lambda (process)
                     (with-current-buffer (process-buffer process)
                       (buffer-string)))))
       (cond
        ((not (buffer-name (process-buffer process)))
         (eden-error-log-and-signal
          ,req 'eden-error-process-buffer process
          :callback-error ,callback-error))
        ((string= event "finished\n")
         (let ((resp (condition-case err
                         (with-current-buffer (process-buffer process)
                           (goto-char (point-min))
                           (eden-json-read))
                       (error (eden-error-log-and-signal
                               ,req 'eden-error-json-read process
                               :error err
                               :process-stdout (funcall stdout process)
                               :callback-error ,callback-error)))))
           (if-let ((err (plist-get resp :error)))
               (eden-error-log-and-signal
                ,req 'eden-error-api process
                :error err
                :callback-error ,callback-error)
             (condition-case err
                 (progn
                   (eden-write-response ,req (funcall stdout process) resp)
                   (kill-buffer (process-buffer process))
                   (funcall ,callback ,req resp))
               (error (eden-error-log-and-signal
                       ,req 'eden-error-callback process
                       :error err
                       :callback-error ,callback-error))))))
        (t (eden-error-log-and-signal
            ,req 'eden-error-process process
            :process-stdout (funcall stdout process)
            :event event
            :callback-error ,callback-error))))))

(defun eden-request-send (req callback &optional callback-error)
  "Send REQ request asynchronously to OpenAI-compatible API using `make-process'.

Return the process handling the request.

If the request succeed, CALLBACK function is called in the sentinel.
If the request failed or there's an error in the sentinel, CALLBACK-ERROR
function is called.

For the request to have a chance to succeed, OpenAI-compatible API key
must be set correctly beforehand.  See `eden-request-command' and
`eden-api-key-symbol'.

Before sending REQ request:

1) Information about the request is saved in several files in
   REQ's directory.  See `eden-write-request'.
2) The curl command line issued to send the request is saved in
   REQ's directory.  See `eden-write-command'.

And upon receipt of OpenAI-compatible API response, we either save
the response or an error in REQ's directory.  See `eden-write-response'
and `eden-write-error'.

See also `eden-request-dir' and `eden-request-file'.

REQ request is a plist with the following keys:

- :api            - A plist describing the service and endpoint to use
                    for the creation of the curl command we use to send
                    the request, including where to find the API key.  Here's
                    an example

                        (:service \"openai\"
                         :endpoint \"https://api.openai.com/v1/chat/completions\")

                    which expects the API key to be in `~/.authinfo.gpg'
                    or `~/.authinfo' file on a line like this

                        machine openai password <openai-api-key>

                    See `eden-request-command'.

- :req-params     - The request that we send to OpenAI API or a compatible
                    API like Perplexity.  See:

                    - https://platform.openai.com/docs/guides/text-generation/,
                    - https://platform.openai.com/docs/api-reference/chat and
                    - https://docs.perplexity.ai/api-reference/chat-completions.

                    As we don't support streaming API, value for `:stream'
                    key must always be `:false'.  Here's an example:

                        (:stream :false
                         :model \"gpt-4o-mini\"
                         :messages [(:role \"user\" :content \"foo bar baz\")])

- :prompt         - The prompt of the request in `org-mode' format.
- :dir            - A directory (absolute path) where we log all the
                    requests.
- :uuid           - A unique ID (a string) which is the subdirectory of
                    `:dir' in which we kept information about REQ.  See
                    `eden-request-dir'.
- :system-message - (optional) The system message of the request in `org-mode'
                    format.
- :exchanges      - (optional) If REQ is the last exchange in a conversation,
                    this key must be a vector of the previous exchanges.  See
                    `eden-request-conversation' and `eden-conversation-exchanges'.
                    Here's an example:

                    [(:uuid \"uuid-foo\"
                      :prompt \"foo prompt\"
                      :response \"foo response\")
                     (:uuid \"uuid-bar\"
                      :prompt \"bar prompt\"
                      :response \"bar response\"
                      :context [(:role \"user\" :content \"foo prompt\")
                                (:role \"assistant\" :content \"foo response\")
                                (:role \"user\" :content \"bar prompt\")
                                (:role \"assistant\" :content \"bar response\")])]

Here's an example of a REQ request using OpenAI API, with no system
message and no previous exchanges:

    (:api (:service \"openai\"
           :endpoint \"https://api.openai.com/v1/chat/completions\")
     :req-params (:stream :false
                  :model \"gpt-4o-mini\"
                  :messages [(:role \"user\" :content \"foo bar baz\")])
     :prompt \"foo bar baz\"
     :dir \"/tmp/eden/\"
     :uuid \"40e73d38-7cb9-4558-b11f-542f8a2d1f9c\")

Here's an example of a REQ request (third of a conversation), using
Perplexity API and a system message:

    (:api (:service \"perplexity\"
           :endpoint \"https://api.perplexity.ai/chat/completions\")
     :req-params (:stream :false
                  :model \"gpt-4o-mini\"
                  :messages [(:role \"system\" :content \"baz system message\")
                             (:role \"user\" :content \"foo prompt\")
                             (:role \"assistant\" :content \"foo response\")
                             (:role \"user\" :content \"bar prompt\")
                             (:role \"assistant\" :content \"bar response\")
                             (:role \"user\" :content \"baz prompt\")])
     :prompt \"baz user prompt\"
     :system-message \"baz system message\"
     :exchanges [(:uuid \"uuid-foo\"
                  :prompt \"foo prompt\"
                  :response \"foo response\")
                 (:uuid \"uuid-bar\"
                  :prompt \"bar prompt\"
                  :response \"bar response\"
                  :context [(:role \"user\" :content \"foo prompt\")
                            (:role \"assistant\" :content \"foo response\")
                            (:role \"user\" :content \"bar prompt\")
                            (:role \"assistant\" :content \"bar response\")])]
     :dir \"/tmp/eden/\"
     :uuid \"uuid-baz\")"
  (seq-let (command command-no-api-key) (eden-request-command req)
    (eden-write-request req)
    (eden-write-command req command-no-api-key)
    (make-process
     :name "eden"
     :buffer (generate-new-buffer-name "eden")
     :command (list "sh" "-c" command)
     :connection-type 'pipe
     :sentinel (eden-sentinel req callback callback-error))))

;;; UI
;;;; User options

(defvar eden-api
  '(:service "openai"
    :endpoint "https://api.openai.com/v1/chat/completions"
    :default-model "gpt-5.1"
    :models ("gpt-5.1"
             "gpt-5"
             "gpt-5-mini"
             "gpt-5-nano"
             "gpt-5-chat-latest"
             "gpt-5-search-api"
             "gpt-4.1"
             "gpt-4.1-mini"
             "gpt-4.1-nano"
             "gpt-4o"
             "gpt-4o-mini"
             "chatgpt-4o-latest"
             "gpt-4o-search-preview"
             "gpt-4o-mini-search-preview"
             "o1"
             "o3"
             "o3-mini"
             "o4-mini"))
  "Plist describing the OpenAI-compatible API to use.

Accepted keys includes:

- :service       - The service name (string) that identifies the API key
                   in `~/.authinfo.gpg' (or `~/.authinfo') file used with
                   `:endpoint'.  See `eden-request-command'.
- :endpoint      - The URL for the API endpoint corresponding to `:service'.
- :default-model - (optional) The default model's name (string) for
                   `:service'.  While not actively utilized, it's
                   included for consistency with `eden-apis' list.
- :models        - (optional) A list of model names (strings) available
                   for selection when using `eden-model-set' command
                   to switch models for `:service'.

Example for OpenAI API configuration:

    (:service \"openai\"
     :endpoint \"https://api.openai.com/v1/chat/completions\"
     :default-model \"gpt-4.1\"
     :models (\"gpt-4.1\" \"gpt-4o\" \"o1\" \"o3-mini\"))

In that case, ensure that the API key, <openai-api-key>, for the
endpoint \"https://api.openai.com/v1/chat/completions\" is stored
in `~/.authinfo.gpg' (encrypted with gpg) or `~/.authinfo' file,
formatted as:

    machine openai password <openai-api-key>")

(defvar eden-apis
  '((:service "anthropic"
     :endpoint "https://api.anthropic.com/v1/messages"
     :anthropic-version "2023-06-01"
     :default-model "claude-sonnet-4-0"
     :models ("claude-opus-4-0"
              "claude-sonnet-4-0"
              "claude-3-7-sonnet-latest"
              "claude-3-5-sonnet-latest"
              "claude-3-5-haiku-latest"
              "claude-3-opus-latest"))
    (:service "deepseek"
     :endpoint "https://api.deepseek.com"
     :default-model "deepseek-reasoner"
     :models ("deepseek-chat"
              "deepseek-reasoner"))
    (:service "openai"
     :endpoint "https://api.openai.com/v1/chat/completions"
     :default-model "gpt-5.1"
     :models ("gpt-5.1"
              "gpt-5"
              "gpt-5-mini"
              "gpt-5-nano"
              "gpt-5-chat-latest"
              "gpt-5-search-api"
              "gpt-4.1"
              "gpt-4.1-mini"
              "gpt-4.1-nano"
              "gpt-4o"
              "gpt-4o-mini"
              "chatgpt-4o-latest"
              "gpt-4o-search-preview"
              "gpt-4o-mini-search-preview"
              "o1"
              "o3"
              "o3-mini"
              "o4-mini"))
    (:service "perplexity"
     :endpoint "https://api.perplexity.ai/chat/completions"
     :default-model "sonar"
     :models ("sonar-deep-research"
              "sonar-reasoning-pro"
              "sonar-reasoning"
              "sonar-pro"
              "sonar"
              "r1-1776"))
    (:service "x.ai"
     :endpoint "https://api.x.ai/v1/chat/completions"
     :default-model "grok-3"
     :models ("grok-3"
              "grok-3-fast"
              "grok-3-mini"
              "grok-3-mini-fast"
              "grok-2")))
  "List of OpenAI-compatible APIs available for selection when using `eden-api-set'.

See `eden-api', for detailed descriptions of each element.

Moreover, when we set `eden-api' with `eden-api-set', if API's
`:default-value' is non-nil, it becomes the value of `eden-model'.

Example listing OpenAI API and Perplexity configurations:

    ((:service \"openai\"
      :endpoint \"https://api.openai.com/v1/chat/completions\"
      :default-model \"gpt-4.1\"
      :models (\"gpt-4.1\" \"gpt-4o\" \"o1\" \"o3-mini\"))
     (:service \"perplexity\"
      :endpoint \"https://api.perplexity.ai/chat/completions\"
      :default-model \"sonar\"
      :models (\"sonar\" \"sonar-pro\")))

More information about the APIs and models:

- https://docs.x.ai/docs/models
- https://platform.openai.com/docs/models
- https://docs.anthropic.com/en/docs/about-claude/models/overview
- https://docs.perplexity.ai/models/model-cards
- https://api-docs.deepseek.com/quick_start/pricing
")

(defvar eden-model "gpt-5.1"
  "Model used by `eden-send' to send requests to `eden-api'.

Examples of valid model for OpenAI API: \"gpt-5.1\", \"gpt-5\", \"gpt-4.1\".

This variable can be modified via `eden-menu'.")

(defvar eden-system-message nil
  "System message used by `eden-send' to send requests to `eden-api'.

Don't set `eden-system-message' directly.  Use one of the following
command instead:

- `eden-system-message-set',
- `eden-system-message-reset',
- `eden-system-message-add' or
- `eden-system-message-update'.

It is a cons cell (\"title\" . \"system message\"), where \"system message\"
serves as `:content' of the first message in request's `:messages'.

Additionally,`eden-system-message' may be nil, in which case `:messages'
will omit the initial system message.

According to OpenAI API documentation, a system message consists of
\"Developer-provided instructions that the model should follow,
regardless of messages sent by the user.\"")

(defvar eden-system-message-append nil
  "Instructions (a string) appended to the system message used by `eden-send'.

Setting this variable doesn't modify `eden-system-message'.

See `eden-build-request'.")

(defvar eden-req-params-extra nil
  "Plist of parameters that overrides request body's parameters sent to the LLM API.

See `eden-build-request'.")

(defvar eden-dir (concat user-emacs-directory "eden/")
  "Directory where all requests sent by `eden-send' are stored.

Each request is organized in a unique subdirectory within `eden-dir',
containing comprehensive details such as the request itself, the
corresponding response, and any errors encountered, among other relevant
data.

See `eden-write-request', `eden-write-command', `eden-write-response'
and `eden-write-error'.")

(defvar eden-org-property-req "EDEN_REQ"
  "Org property used for request's UUID.

This is used when inserting requests or conversations into buffers
and is relevant for any command that operates on requests at point
such as `eden-req-at-point-goto'.

See `eden-conversation-insert' and `eden-req-at-point-uuid'.")

(defvar eden-pops-up-upon-receipt t
  "If t, the response's buffer pops up upon receipt from `eden-api'.

See `eden-send'.")

(defvar eden-include-reasoning nil
  "If t, include model's reasonings alongside the conversation's prompts and responses.

It only applies to models such as \"deepseek-reasoner\" from Deepseek
which generate responses based on their internal reasoning process while
providing access to that reasoning.

See `eden-request-assistant-reasoning' and `eden-conversation-insert'.")

(defvar eden-prompt-buffer-name "*eden*"
  "Name of the buffer used for user prompt input.")

;;;; Utils

(defun eden-uuid ()
  "Generate a random-based UUID using `uuidgen' linux utility."
  (string-remove-suffix "\n" (shell-command-to-string "uuidgen")))

(defun eden-buffer-name (&optional title)
  "Return a formatted buffer name optionally using TITLE."
  (if title (format "*eden[%s]*" title) "*eden*"))

(defun eden-maybe-delete-window-prompt-buffer ()
  "Delete prompt buffer window if selected and not the only live window."
  (when-let ((prompt-buffer-window (get-buffer-window eden-prompt-buffer-name)))
    (when (and (equal (selected-window) prompt-buffer-window)
               (> (length (window-list)) 1))
      (delete-window))))

(defun eden-org-to-markdown (org-str)
  "Convert ORG-STR `org-mode' string to a markdown string."
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

LEVEL must be 3 or 4.

For instance:

    (eden-org-demote \"* heading\" 3) ;; \"*** heading\""
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

(defun eden-assoc-in (map keys value)
  "Associate VALUE in nested MAP (a hash-table) at the path specified by KEYS."
  (let ((current-map map)
        (keys (append keys nil)))
    (while (cdr keys)
      (let* ((key (pop keys))
             (next-map (gethash key current-map)))
        (if (hash-table-p next-map)
            (setq current-map next-map)
          (puthash key (make-hash-table :test 'equal) current-map)
          (setq current-map (gethash key current-map)))))
    (puthash (car keys) value current-map)
    map))

(defun eden-dir-set (new-dir)
  "Set `eden-dir' to NEW-DIR.

This also set the variables `eden-request-history' and
`eden-prompt-history-state' accordingly.

If you do this interactively, do it via `eden-menu'.
See `eden-dir-set-suffix'."
  (cond
   ((equal new-dir eden-dir) nil) ;; Do nothing.
   ((eden-running-p)
    (error "Can't set `eden-dir' (`%s') to a different directory `%s' while Eden is running."
           eden-dir new-dir))
   ((not (stringp new-dir))
    (error "Wrong `new-dir' type: `%s'.  It should be a string." new-dir))
   (t (setq eden-dir (file-name-as-directory (expand-file-name new-dir)))
      ;; Reset current conversation when dirs don't match
      (when-let ((conversation-dir (eden-conversation-dir eden-conversation-id)))
        (message "foo")
        (when (not (string= conversation-dir new-dir))
          (message "bar")
          (setq eden-conversation-id nil)))
      (eden-history-update :dir eden-dir))))

;;;; Prompt and Request history

(defvar eden-request-history nil
  "List of request's UUID where the latest request is listed first.

When we call `eden' for the first time, `eden-request-history-set'
initializes `eden-request-history' with existing requests in `eden-dir'.

Then, each invocation of `eden-send' command updates `eden-request-history'
variable.

See `eden-send-request'.")

(defvar eden-prompt-history-state [nil nil nil]
  "State of the prompt history.

- Set by `eden-history-update' in `eden-mode',
- Reset with each call to `eden-send-request' and
- Updated on calling `eden-prompt-previous' and `eden-prompt-next'.

The variable is a vector of three elements:

1) The first element holds the list of previous prompts, which can be:

   - nil,
   - UUIDs of existing requests in `eden-dir' or
   - temporary prompts entered in `eden-prompt-buffer-name' buffer
     formatted as (:prompt \"foo bar baz\")

2) The second element holds the current prompt, which may be nil, a
   UUID or a temporary prompt as described in 1),
3) The Third element holds the list of next prompts which can also be
   nil, UUIDs or a temporary prompts as described in 1).")

(defun eden-request-history-build (dir)
  "Set `eden-request-history' with UUIDs of existing requests in `dir'.

They are sorted by their timestamp file with the latest request appearing
first.

See `eden-request-timestamp'."
  (when (file-exists-p dir)
    (message "Setting request history...")
    (prog1
        (thread-last
          (directory-files-recursively dir "timestamp-.*")
          (mapcar (lambda (f)
                    (string-match ".*/\\([^/]+\\)/timestamp-\\(.*\\)" f)
                    (cons (match-string 1 f)
                          (string-to-number (match-string 2 f)))))
          (seq-sort (lambda (t1 t2) (> (cdr t1) (cdr t2))))
          (mapcar 'car))
      (message "Setting request history...done"))))

(cl-defun eden-history-update (&key dir new-req-uuid)
  "Update `eden-request-history' and `eden-prompt-history-state'."

  (when dir (setq eden-request-history (eden-request-history-build dir)))
  (when new-req-uuid (push new-req-uuid eden-request-history))
  (setq eden-prompt-history-state
        (vector eden-request-history nil nil)))

(defun eden-prompt-current-buffer ()
  "Return current buffer content as string with no text properties.

This function should be called from `eden-prompt-buffer-name' buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun eden-prompt-current-req-uuid (prompt-history)
  "Return request's UUID of current prompt in PROMPT-HISTORY.

If the current prompt is temporary with no corresponding request, return nil.

See `eden-prompt-history-state'."
  (when-let* ((current (aref prompt-history 1))
              ((stringp current)))
    current))

(defun eden-prompt-current (dir prompt-history)
  "Return current prompt in PROMPT-HISTORY if any or nil.

The look up for the prompt is done in DIR.  See `eden-prompt-history-state'."
  (pcase (aref prompt-history 1)
    ('nil nil)
    ((and p (pred consp)) (plist-get p :prompt))
    (uuid (eden-request-read `(:dir ,dir :uuid ,uuid) 'prompt))))

(defun eden-prompt-history-previous (prompt-history &optional prompt discard-current)
  "Update in-place the PROMPT-HISTORY backward.

PROMPT-HISTORY follows the structure defined in `eden-prompt-history-state'.

If PROMPT is non-nil, both PROMPT and the current prompt are pushed onto
the stack of next prompts.

If DISCARD-CURRENT is non-nil, the current prompt will not be pushed
onto the stack of next prompts.

Signal an error if both PROMPT and DISCARD-CURRENT are non-nil."
  (when (and prompt discard-current)
    (error (format "`prompt' and `discard-current' arguments cannot be both non-nil: %S, %S"
                   prompt discard-current)))
  (when-let ((prev-items (aref prompt-history 0)))
    (let ((current (aref prompt-history 1))
          (next-items (aref prompt-history 2)))
      (aset prompt-history 0 (cdr prev-items))
      (aset prompt-history 1 (car prev-items))
      (aset prompt-history 2 (delq nil
                                   (append
                                    (list prompt (when (not discard-current) current))
                                    next-items))))))

(defun eden-prompt-history-next (prompt-history &optional prompt discard-current)
  "Update in-place the PROMPT-HISTORY forward.

Similar to `eden-prompt-history-previous'."
  (when (and prompt discard-current)
    (error (format "`prompt' and `discard-current' arguments cannot be both non-nil: %S, %S"
                   prompt discard-current)))
  (when-let ((next-items (aref prompt-history 2)))
    (let ((prev-items (aref prompt-history 0))
          (current (aref prompt-history 1)))
      (aset prompt-history 0 (delq nil (append
                                        (list prompt (when (not discard-current) current))
                                        prev-items)))
      (aset prompt-history 1 (car next-items))
      (aset prompt-history 2 (cdr next-items)))))

(defun eden-prompt-discard-current-p (dir prompt-history)
  "Return t if current prompt UUID is not associated to a request in `eden-dir'.

This can happens when we switch `eden-dir' during a session without
using `eden-dir-set' or `eden-dir-set-suffix'.

See `eden-prompt-history-state'."
  (when-let* ((current (aref prompt-history 1))
              ((stringp current)) ;; it's the UUID of a request
              (req `(:dir ,dir :uuid ,current)))
    (not (ignore-errors (eden-request-read req 'prompt)))))

;; This function is not unit tested.  When making changes to
;; it or any prompt history function, test it by starting a fresh
;; emacs and in the eden prompt buffer, play with M-n and M-p.
(defun eden-prompt-history-nav (dir direction prompt-history)
  "Replace current buffer content with previous or next prompt based on DIRECTION.

DIRECTION can be either `previous' or `next'.

This also updates in-place PROMPT-HISTORY accordingly.  See
`eden-prompt-history-previous' and `eden-prompt-history-next'.

If the content of `eden-prompt-buffer-name' buffer differs from
the current prompt in PROMPT-HISTORY, it is pushed
onto the respective stack of previous or next prompts based on
DIRECTION.

This function should be called from `eden-prompt-buffer-name' buffer."
  (let (prompts nav-fn)
    (pcase direction
      ('previous (setq prompts (aref prompt-history 0))
                 (setq nav-fn 'eden-prompt-history-previous))
      ('next (setq prompts (aref prompt-history 2))
             (setq nav-fn 'eden-prompt-history-next)))
    (cond
     ((null prompts)
      (message "No more requests or prompts in history."))
     ((eden-prompt-discard-current-p dir prompt-history)
      (funcall nav-fn prompt-history nil 'discard-current)
      (eden-prompt-history-nav direction))
     (t (let* ((pcb (eden-prompt-current-buffer))
               (pc (eden-prompt-current dir prompt-history))
               (prompt (when (or (null pc) (not (string= pcb pc)))
                         `(:prompt ,pcb))))
          (funcall nav-fn prompt-history prompt)
          (if (eden-prompt-discard-current-p dir prompt-history)
              ;; This can happens if the UUID stored in prompt history
              ;; doesn't match to a request in `dir'.  If none
              ;; of the UUIDs in prompt history correspond to an existing
              ;; request, we'll hit `max-lisp-eval-depth'.  But in practice,
              ;; this doesn't happens.
              (eden-prompt-history-nav direction)
            (erase-buffer)
            (save-excursion
              (insert (or (eden-prompt-current dir prompt-history) "")))))))))

(defun eden-prompt-previous ()
  "Replace current buffer content with previous prompt.

See `eden-prompt-history-state' and `eden-prompt-history-nav'.

This function should be called from `eden-prompt-buffer-name' buffer."
  (interactive)
  (eden-prompt-history-nav eden-dir 'previous eden-prompt-history-state))

(defun eden-prompt-next ()
  "Replace current buffer content with next prompt.

See `eden-prompt-history-state' and `eden-prompt-history-nav'.

This function should be called from `eden-prompt-buffer-name' buffer."
  (interactive)
  (eden-prompt-history-nav eden-dir 'next eden-prompt-history-state))

(defun eden-prompt-current-goto ()
  "Go to request's directory of current prompt in `eden-prompt-history-state'.

If the current prompt is temporary with no corresponding request, message
the user about it.

See `eden-request-dir'."
  (interactive)
  (if-let* ((req-uuid (eden-prompt-current-req-uuid eden-prompt-history-state))
            (req-dir (eden-request-dir `(:dir ,eden-dir :uuid ,req-uuid))))
      (progn
        (eden-maybe-delete-window-prompt-buffer)
        (dired-other-window req-dir))
    (message "Current prompt is not associated with a request.")))

;;;; Conversations

(defvar eden-conversations nil
  "Alist of conversations.

A conversation is a cons cells whose

- car is an ID and
- cdr is a plists with the following keys:

  - :dir           - The directory where the requests is stored.
  - :title         - The title of the conversation.
  - :last-req-uuid - The UUID of the last request in the conversation.
                     nil for a new conversation.

For instance `eden-conversations' can be:

    ((\"213940f6-fa87-4c27-9aa5-30d6ba3d2724\" .
      (:title \"foo title\" :dir \"/tmp/eden/\" :last-req-uuid nil))
     (\"bcb3f6ee-1b85-4c92-904a-f8ae8f536f7c\" .
      (:title \"bar title\"
       :dir \"/tmp/eden/\"
       :last-req-uuid \"04397cda-f623-425b-9a7d-c29caea3511f\")))")

(defvar eden-conversation-id nil
  "UUID of the current conversation if any.")

(defun eden-conversation-exists-p (conversation-id)
  "Return t if a conversation with CONVERSATION-ID exists in `eden-conversations'."
  (map-elt eden-conversations conversation-id))

(defun eden-conversation-with-title-exists-p (title)
  "Return t if a conversation with TITLE exists in `eden-conversations'."
  (seq-some (lambda (c) (equal title (plist-get (cdr c) :title)))
            eden-conversations))

(defun eden-conversation-add (dir title &optional req-uuid)
  "Add a conversation to `eden-conversations' with TITLE.

If REQ-UUID is the UUID of an existing request in DIR, resume a
conversation from that request.

Also set `eden-conversation-id' to the ID of the newly created conversation
making it the current conversation.

Signal an error if the conversation cannot be added."
  (when (eden-conversation-with-title-exists-p title)
    (error "Conversation with title `%s' already exists in `eden-conversations'"
           title))
  (when req-uuid
    (condition-case err
        (eden-request-check `(:dir ,dir :uuid ,req-uuid))
      (error
       (error "Cannot continue from that request.  %s"
              (error-message-string err)))))
  (let ((conversation-id (eden-uuid)))
    (push (cons conversation-id
                `(:title ,title :dir ,dir :last-req-uuid ,req-uuid))
          eden-conversations)
    (setq eden-conversation-id conversation-id)))

(defun eden-conversation-title (conversation-id)
  "Return title of conversation with CONVERSATION-ID in `eden-conversations'.

If no conversation found, return nil."
  (eden-get-in eden-conversations `(,conversation-id :title)))

(defun eden-conversation-dir (conversation-id)
  "Return directory of conversation with CONVERSATION-ID in `eden-conversations'.

If no conversation found, return nil."
  (eden-get-in eden-conversations `(,conversation-id :dir)))

(defun eden-conversation-last-req (conversation-id)
  "Return last request of conversation with CONVERSATION-ID in `eden-conversations'.

If no conversation with CONVERSATION-ID can be found in `eden-conversations',
return nil.

For instance:

    (let ((eden-conversations
           \\='((\"09b95117-ae13-41dc-aa76-53f63576b771\" .
              (:title \"baz title\"
               :dir \"/tmp/eden/\"
               :last-req-uuid \"2086eac6-61ff-4a44-993a-a928b7a29007\")))))
      (eden-conversation-last-req \"09b95117-ae13-41dc-aa76-53f63576b771\"))
    ;; (:uuid \"2086eac6-61ff-4a44-993a-a928b7a29007\"
    ;;  :dir \"/tmp/eden/\")"
  (when-let* ((conversation (map-elt eden-conversations conversation-id))
              (uuid (plist-get conversation :last-req-uuid))
              (dir (plist-get conversation :dir)))
    `(:uuid ,uuid :dir ,dir)))

(defun eden-conversation-last-req-uuid (conversation-id)
  "Return UUID of last request of conversation with CONVERSATION-ID in `eden-conversations'.

If no conversation found, return nil."
  (plist-get (eden-conversation-last-req conversation-id) :uuid))

(defun eden-conversation-buffer-name (conversation-id)
  "Return buffer name for conversation with CONVERSATION-ID."
  (when-let ((title (eden-conversation-title conversation-id)))
    (eden-buffer-name title)))

(defun eden-conversation-exchanges (conversation-id)
  "Return exchanges of the conversation with CONVERSATION-ID.

Return nil if the conversation is new or doesn't exist.

See `eden-conversations' and `eden-request-conversation'."
  (when-let ((last-req (eden-conversation-last-req conversation-id)))
    (eden-request-conversation last-req)))

(defun eden-conversation-rename (conversation-id new-title)
  "Rename conversation with CONVERSATION-ID to NEW-TITLE in `eden-conversations'.

Signal an error if NEW-TITLE is already used by another conversation."
  (when (eden-conversation-with-title-exists-p new-title)
    (error "Cannot rename conversation to `%s' which is already used by another conversation in `eden-conversations'"
           new-title))
  (when (string-empty-p new-title)
    (error "Cannot rename current conversation with an empty title."))
  (when-let ((cell (assoc conversation-id eden-conversations)))
    (setcdr cell (plist-put (copy-sequence (cdr cell))
                            :title new-title))))

;; This function is not unit tested.  When making changes to
;; it or any conversation function, test it by starting a fresh
;; emacs, continue a conversation from some request making it
;; current, display it in a buffer and finally edit the title.
;; The new title must change in the mode line and in the
;; conversation buffer.
;; Also try to rename a conversation with a title already taken.
;; This should diplay a message in the echo area.
(transient-define-suffix eden-conversation-edit-title ()
  "Edit title of current conversation and its associated buffer based on user's input.

See `eden-conversation-id' and `eden-conversation-rename'."
  :transient t
  (interactive)
  (when (null eden-conversation-id)
    (user-error "No current conversation to edit.  Start a conversation first."))
  ;; This should never happens, but who knows.
  (when (not (eden-conversation-exists-p eden-conversation-id))
    (let ((id eden-conversation-id))
      (setq eden-conversation-id nil)
      (error (concat "Current conversation `eden-conversation-id' not found in `eden-conversations'.\n"
                     "eden-conversation-id: %S\n"
                     "eden-conversations: %S\n"
                     "`eden-conversation-id' has been reset to nil.")
             id eden-conversations)))
  (let* ((old-buff-name (eden-conversation-buffer-name eden-conversation-id))
         (old-title (eden-conversation-title eden-conversation-id))
         (new-title (read-string (format "Edit conversation title `%s': " old-title)))
         (_ (eden-conversation-rename eden-conversation-id new-title))
         (new-buff-name (eden-conversation-buffer-name eden-conversation-id)))
    ;; Update conversation title in old-buff-name and rename buffer
    ;; to new-buff-name
    (when (get-buffer old-buff-name)
      (with-current-buffer old-buff-name
        (save-excursion
          (goto-char (point-min))
          ;; This replacement works because when we insert a
          ;; conversation in the first place with
          ;; `eden-conversation-insert' we put the title
          ;; on the first line of the buffer.
          (when (search-forward old-title (line-end-position) t)
            (replace-match new-title t t)))
        (rename-buffer new-buff-name)))))

(defun eden-conversation-update (req)
  "Set last request UUID of conversation REQ's `:conversation-id' to REQ's `:uuid' key.

This modifies `eden-conversations'.

If one of these keys is missing, do nothing.

This function is meant to be used in `eden-send-request''s callback.

For instance:

    (let ((eden-conversations
           \\='((\"conversation-id-foo\" . (:title \"foo title\"
                                       :dir \"/tmp/eden/\"
                                       :last-req-uuid nil)))))
      (eden-conversation-update \\='(:conversation-id \"conversation-id-foo\"
                                  :uuid \"new-foo-req-uuid\"))
      eden-conversations)
    ;; ((\"conversation-id-foo\" .
    ;;   (:title \"foo title\"
    ;;    :dir \"/tmp/eden/\"
    ;;    :last-req-uuid \"new-foo-req-uuid\")))"
  (when-let* ((conversation-id (plist-get req :conversation-id))
              (req-uuid (plist-get req :uuid))
              (cell (assoc conversation-id eden-conversations)))
    (setcdr cell (plist-put (copy-sequence (cdr cell))
                            :last-req-uuid req-uuid))))

(transient-define-suffix eden-conversation-continue-from-req-history ()
  "Continue a conversation from current request in history."
  :transient t
  (interactive)
  (if-let ((req-uuid (eden-prompt-current-req-uuid eden-prompt-history-state))
           (title (read-string "Continue conversation with title: ")))
      (progn (eden-conversation-add eden-dir title req-uuid)
             (message "Conversation `%s' initialized." title))
    (message "Current prompt is not associated with a request.")))

(transient-define-suffix eden-conversation-pause ()
  "Pause the current conversation by setting `eden-conversation-id' to nil."
  :transient t
  (interactive)
  (if-let ((title (eden-conversation-title eden-conversation-id)))
      (message "Conversation `%s' paused." title)
    (message "No current conversation."))
  (setq eden-conversation-id nil))

;; It's behavior is unit tested when we test `eden-conversation-insert'.
(defun eden-conversation-insert-or-update-top-heading (req title append)
  "Insert heading with TITLE for REQ conversation in current buffer.

If APPEND is t, don't insert the TITLE.  Only update the request UUID
of the conversation in current buffer.

In both cases, point is left at the end of current buffer.

This function depends on `eden-org-property-req'.

See `eden-conversation-insert'."
  (let ((req-uuid (plist-get req :uuid)))
    (if (and append
             (progn (goto-char (point-min))
                    (re-search-forward
                     (format "^:%s: \\(.*\\)" eden-org-property-req) nil t)))
        (progn
          (replace-match req-uuid nil nil nil 1)
          (goto-char (point-max)))
      (insert
       ;; If we change how we insert the title below, we may also
       ;; have to change how we rename conversation title in
       ;; `eden-conversation-edit-title' command.
       "** " (or title "Conversation") "\n"
       ":PROPERTIES:\n"
       ":" eden-org-property-req ": " req-uuid "\n"
       ":END:\n"))))

(defun eden-conversation-insert (req title &optional append only-last-req)
  "Format and insert the conversation whose last request is REQ into current buffer.

Set TITLE as the first heading.

If APPEND is non-nil, only append the last exchange of the conversation.

If ONLY-LAST-REQ is non-nil, do not include previous conversation
messages, only the last one being REQ itself.

Signal an error if REQ fails `eden-request-check'.
Signal an error if TITLE and APPEND are both non-nil.

The `org-mode' properties used the REQ's UUID is `eden-org-property-req'.

For instance, given a valid request with \"foo-uuid\" in `/tmp/eden/'
directory, with a prompt \"foo bar baz\", the response
\"foo bar baz assistant response\", dated Friday, December 20, 2024,
and that has been issued to \"openai\" service and the model
\"gpt-4o-mini-2024-07-18\", evaluating the following expression

    (eden-conversation-insert '(:dir \"/tmp/eden/\" :uuid \"foo-uuid\")
                              \"Foo Title\")

inserts the following in the current buffer

    ** Foo Title
    :PROPERTIES:
    :EDEN_REQ: foo-uuid
    :END:
    *** Prompt

    foo bar baz

    *** Response

    foo bar baz assistant response

See `eden-request-conversation'."
  (eden-request-check req)
  (when (and (null title) (null append))
    (error "`title' argument can be nil only when `append' argument is non-nil"))
  (let* ((format-exchange
          (lambda (exchange)
            (delq nil
                  (list (eden-org-demote (plist-get exchange :prompt) 4)
                        (eden-org-demote (plist-get exchange :response) 4)
                        (when-let ((reasoning (plist-get exchange :reasoning)))
                          (eden-org-demote reasoning 4))))))
         (conversation (mapcar format-exchange (eden-request-conversation req)))
         (conversation
          (if (or append only-last-req) (last conversation) conversation)))
    (eden-conversation-insert-or-update-top-heading req title append)
    (dolist (exchange conversation)
      (seq-let (prompt response reasoning) exchange
        (insert "*** Prompt\n\n" prompt)
        (cond
         ((looking-back "\n\n" nil) nil)
         ((looking-back "\n" nil) (insert "\n"))
         (t (insert "\n\n")))
        (when (and eden-include-reasoning reasoning)
          (insert "*** Reasoning\n\n" reasoning)
          (cond
           ((looking-back "\n\n" nil) nil)
           ((looking-back "\n" nil) (insert "\n"))
           (t (insert "\n\n"))))
        (insert "*** Response\n\n" response)
        (cond
         ((looking-back "\n\n" nil) nil)
         ((looking-back "\n" nil) (insert "\n"))
         (t (insert "\n\n")))))))

;;;; Profiles

(defvar eden-profile-ring (make-ring 32)
  "A ring containing the history of profiles.

Profiles are defined in `eden-profile-current'.

This ring is updated each time we modify the conversations,
the system messages or other settings using `eden-menu' command.
Specifically, this is done by `eden-profile-push' which we added
to `transient-exit-hook'.")

(defun eden-profile-current ()
  "Return current profile.

It is a plist with the following key/value pairs:

- :api                   - See `eden-api'
- :dir                   - See `eden-dir'
- :model                 - See `eden-model'
- :include-reasoning     - See `eden-include-reasoning'
- :conversation-id       - See `eden-conversation-id'
- :system-message        - See `eden-system-message'
- :system-message-append - See `eden-system-message-append'
- :req-params-extra      - See `eden-req-params-extra'

See `eden-profile-push'."
  (list :api eden-api
        :dir eden-dir
        :model eden-model
        :include-reasoning eden-include-reasoning
        :conversation-id eden-conversation-id
        :system-message eden-system-message
        :system-message-append eden-system-message-append
        :req-params-extra eden-req-params-extra))

(defun eden-profile-push ()
  "Push current profile into `eden-profile-ring'.

See `eden-profile-current'."
  (ring-remove+insert+extend eden-profile-ring (eden-profile-current) 'grow))

(defun eden-profile-apply (profile)
  "Set current profile to PROFILE.

Signal an error if PROFILE's `:dir' is different from the
current profile and Eden is running.

See `eden-profile-current'."
  (condition-case err
      (eden-dir-set (plist-get profile :dir))
    (error
     (error "Cannot apply profile:  %S\nError: %s"
            profile
            (error-message-string err))))

  (setq eden-api (plist-get profile :api))
  (setq eden-dir (plist-get profile :dir))
  (setq eden-model (plist-get profile :model))
  (setq eden-include-reasoning (plist-get profile :include-reasoning))
  (setq eden-conversation-id (plist-get profile :conversation-id))
  (setq eden-system-message (plist-get profile :system-message))
  (setq eden-system-message-append (plist-get profile :system-message-append))
  (setq eden-req-params-extra (plist-get profile :req-params-extra)))

(defun eden-profile-previous ()
  "Turn previous profile into the current one.

See `eden-profile-ring' and `eden-profile-current'."
  (interactive)
  (let* ((profile-current (eden-profile-current))
         (profile-prev
          (condition-case nil
              (ring-next eden-profile-ring profile-current)
            (error (eden-profile-push) profile-current))))
    (eden-profile-apply profile-prev)
    (when (= (ring-length eden-profile-ring) 1)
      (message "There's only one profile in the ring."))
    (force-mode-line-update)))

(defun eden-profile-next ()
  "Turn next profile into the current one.

See `eden-profile-ring' and `eden-profile-current'."
  (interactive)
  (let* ((profile-current (eden-profile-current))
         (profile-next
          (condition-case nil
              (ring-previous eden-profile-ring profile-current)
            (error (eden-profile-push) profile-current))))
    (eden-profile-apply profile-next)
    (when (= (ring-length eden-profile-ring) 1)
      (message "There's only one profile in the ring."))
    (force-mode-line-update)))

;;;; Sending Requests

(defvar eden-pending-requests nil
  "List of pending requests sent with `eden-send-request'.

Each element is a plist with the following keys:

- :req             - A request created with `eden-build-request'.
- :proc            - The process object handling `:req', as returned by
                     `eden-request-send'.

The latest submitted request is listed first.")

(defvar eden-pending-timer nil
  "Timer responsible to refresh waiting widget.

See `eden-mode-line-waiting'.")

(defun eden-pending-remove (req)
  "Remove REQ from `eden-pending-requests'."
  (setq eden-pending-requests
        (seq-remove (lambda (p)
                      (string=
                       (eden-get-in p [:req :uuid])
                       (plist-get req :uuid)))
                    eden-pending-requests)))

(defun eden-running-p ()
  "Return t if one of `eden-pending-requests' is still running."
  (when-let ((proc (plist-get (car-safe eden-pending-requests) :proc)))
    (and (processp proc) (buffer-name (process-buffer proc)) t)))

(transient-define-suffix eden-kill-last-request ()
  "Kill last request sent with `eden-send'.

See `eden-pending-requests' and `eden-send-request'."
  :transient t
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
  "Maybe start or stop a waiting widget in mode line.

Accepted values for ACTION includes:

- `maybe-start' - If no waiting widget is active, start `eden-pending-timer'
                  to display and refresh one such widget in all mode lines,
                  indicating that Eden is running (see `eden-running-p').
- `maybe-stop'  - If Eden has stopped, cancel `eden-pending-timer' to
                  stop displaying the waiting widget."
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
  "Return t if conversation with CONVERSATION-ID is pending.

A conversation is considered pending if an entry in `eden-pending-requests'
exists with a request being part of CONVERSATION-ID conversation.

See `eden-conversations'."
  (seq-some
   (lambda (p)
     (when-let ((id (eden-get-in p [:req :conversation-id])))
       (string= conversation-id id)))
   eden-pending-requests))

(cl-defun eden-send-request (&key req callback)
  "Send REQ request asynchronously to OpenAI-compatible API.

This function wraps around `eden-request-send', and performs the
following actions:

- Manages `eden-conversations',
- Updates `eden-prompt-history-state',
- Optionally activates the waiting widget (`eden-mode-line-waiting').

In case of an error during sentinel execution, the following callback-error
function is called:

    (lambda (req _err)
      (eden-pending-remove req)
      (eden-mode-line-waiting \\='maybe-stop))

If the request succeeds, the CALLBACK function is executed within the
sentinel and must call these three functions in this specific order:

1) `eden-pending-remove',
2) `eden-conversation-update' and
3) `eden-mode-line-waiting'.

Here's a valid CALLBACK function which appends responses in the
buffer \"*eden[requests]*\":

    (lambda (req resp)
      (with-current-buffer (get-buffer-create \"*eden[requests]*\")
        (org-mode)
        (save-excursion
          (widen)
          (goto-char (point-max))
          (eden-conversation-insert req \"Request\")
          (save-buffer)))
      (eden-pending-remove req)
      (eden-conversation-update req)
      (eden-mode-line-waiting \\='maybe-stop)
      (message \"Eden received a response\"))"
  (let ((conversation-id (plist-get req :conversation-id)))
    (if (eden-pending-conversation-p conversation-id)
        (progn
          (message "Cannot send two concurrent requests in the same conversation.")
          (let ((inhibit-message t))
            (message "conversation: %s\nreq: %S" conversation-id req)))
      (let ((callback-error (lambda (req _err)
                              (eden-pending-remove req)
                              (eden-mode-line-waiting 'maybe-stop))))
        (push (list :req req
                    :proc (eden-request-send req callback callback-error))
              eden-pending-requests)
        (eden-history-update :new-req-uuid (plist-get req :uuid))
        (eden-mode-line-waiting 'maybe-start)))))

(cl-defun eden-build-request (&key profile prompt prev-req-uuid info)
  "Return a request as defined in `eden-request-send'.

PROFILE is a plist as defined in `eden-profile-current'.

When PROMPT is nil, it isn't included in the request parameters.

When PREV-REQ-UUID is provided, the request built is chained to this
previous request, and all previous context is included.  See
`eden-request-conversation'.

INFO plist is pass as :info value in the return request.  This is useful
to pass data not in PROFILE that could then be used in the callback called
in the sentinel after receiving a response from the LLM API.
See `eden-send-request'.

This function is self-contained.  Its output doesn't depend on any
Eden global variable."
  (cl-destructuring-bind
      ;; All profile keys must be listed here even if we don't use them.
      (&key dir api model system-message system-message-append
            include-reasoning conversation-id req-params-extra)
      profile
    (let* ((uuid (eden-uuid))
           (_ (eden-request-dir `(:dir ,dir :uuid ,uuid))) ;; signal error if not ok)
           (-prompt (if (or (null prompt) (string-blank-p prompt)) "" prompt))
           (-system-message
            (cond
             ((and (null system-message) (null system-message-append)) nil)
             ((null system-message-append) system-message)
             ((null system-message) system-message-append)
             (t (format "%s\n\n%s" system-message system-message-append))))
           (exchanges
            (when prev-req-uuid
              (let ((prev-req `(:dir ,dir :uuid ,prev-req-uuid)))
                (condition-case err
                    (eden-request-conversation prev-req)
                  (error
                   (signal 'eden-error-req
                           (list (format "Cannot build request with this prev request:\n%S\nError: %s"
                                         prev-req
                                         (error-message-string err)))))))
              (eden-request-conversation `(:dir ,dir :uuid ,prev-req-uuid))))
           (-messages
            `(,(when -system-message
                 `(:role "system"
                   :content ,(eden-org-to-markdown -system-message)))
              ,@(when (not (null exchanges))
                  (let ((last-exchange (aref exchanges (1- (length exchanges)))))
                    (plist-get last-exchange :context)))
              ,(when (not (string-empty-p -prompt))
                 `(:role "user" :content ,(eden-org-to-markdown -prompt)))))
           (req-messages (apply 'vector (remq nil -messages)))
           (req-params `(:stream :false
                         :model ,model
                         :messages ,req-messages)))
      ;; Anthropic API
      (when (string= (plist-get api :service) "anthropic")
        (plist-put req-params :max_tokens 4096)
        (when include-reasoning
          (plist-put req-params :thinking '(:type "enabled" :budget_tokens 2048))))
      (setq req-params (eden-plist-merge req-params req-params-extra))
      ;; Done
      `(:req-params ,req-params
        :api ,api
        :prompt ,-prompt
        :system-message ,-system-message
        :exchanges ,exchanges
        :dir ,dir
        :uuid ,uuid
        :conversation-id ,conversation-id
        :info ,info))))

;; This function is not unit tested.  When making changes to
;; it or any code related to `eden-send', `eden-send-request'
;; and `eden-sentinel', test it by starting a fresh emacs,
;; call `eden' then in the prompt buffer call `eden-send'
;; with any prompt then with an active conversation.
(defun eden-callback (req _resp)
  "Default callback function used in `eden-send'."
  (let* ((conversation-id (plist-get req :conversation-id))
         (title (or (eden-conversation-title conversation-id)
                    "Request"))
         (buff-name
          (or (eden-conversation-buffer-name conversation-id)
              (eden-buffer-name "requests")))
         (buff-already-exist-p (get-buffer buff-name))
         (append (and conversation-id buff-already-exist-p 'append))
         (buff (get-buffer-create buff-name)))
    (push buff-name buffer-name-history)
    (with-current-buffer buff
      (save-excursion
        (widen)
        (when (not buff-already-exist-p) (org-mode))
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
          (re-search-backward "^\\*\\*\\* Response" nil t)
          (recenter-top-bottom 0))
        (when winner-mode (winner-save-old-configurations))))
    (eden-pending-remove req)
    (eden-conversation-update req)
    (eden-mode-line-waiting 'maybe-stop)
    (message "Eden received a response from %s after %.3fs.  See `%s' buffer."
             (plist-get eden-api :service)
             (- (float-time) (eden-get-in req [:info :created]))
             buff-name)))

;; This function is not unit tested.  When making changes to
;; it or any code related to `eden-send-request', `eden-callback'
;; and `eden-sentinel', test it by starting a fresh emacs,
;; call `eden' then in the prompt buffer call `eden-send'
;; with any prompt then with an active conversation.
(defun eden-send ()
  "Send current prompt to `eden-api' OpenAI-compatible API.

If the request succeeds, the response will be displayed in a buffer
named by `eden-buffer-name' and formatted with `eden-conversation-insert'.
To prevent automatic display, set `eden-pops-up-upon-receipt' to nil.

To modify or inspect Eden's settings, use `eden-menu' command.

This function should be called from `eden-prompt-buffer-name' buffer.

See `eden-send-request'."
  (interactive)
  (eden-send-request
   :req (eden-build-request
         :profile (eden-profile-current)
         :prompt (eden-prompt-current-buffer)
         :prev-req-uuid (eden-conversation-last-req-uuid eden-conversation-id)
         :info `(:created ,(float-time)))
   :callback 'eden-callback)
  ;; When we select a profile with `eden-profile-next' or
  ;; `eden-profile-previous', the profile, if not recently accessed,
  ;; might be deeper in the ring.  As we are interesting in using it now,
  ;; we reordered it to be the most recent, grouping it with other currently
  ;; engaged profiles.
  (eden-profile-push)
  (erase-buffer)
  (eden-maybe-delete-window-prompt-buffer)
  (message "Eden sent a request to %s."
           (plist-get eden-api :service)))

;;;; Conversation branches (paths)

(defun eden-paths-maximal (paths)
  "Return last entry of maximal paths in PATHS.

For instance:

    (let ((paths \\='([\"uuid-req-1\"]
                   [\"uuid-req-1\" \"uuid-req-2\"]
                   [\"uuid-req-1\" \"uuid-req-2\" \"uuid-req-3\"]
                   [\"uuid-req-1\" \"uuid-req-2\" \"uuid-req-4\"]
                   [\"uuid-req-2\" \"uuid-req-5\"])))
      (eden-paths-maximal paths))
    ;; ([\"uuid-req-1\" \"uuid-req-2\" \"uuid-req-3\"]
    ;;  [\"uuid-req-1\" \"uuid-req-2\" \"uuid-req-4\"]
    ;;  [\"uuid-req-2\" \"uuid-req-5\"])"
  (let ((tail (reverse paths))
        (maximals-map (make-hash-table :test 'equal))
        maximals)
    (while tail
      (let ((path-vec (pop tail)))
        (when (not (eden-get-in maximals-map path-vec))
          (push path-vec maximals)
          (eden-assoc-in maximals-map path-vec t))))
    (mapcar (lambda (p) (aref p (1- (length p))))
            maximals)))

(defun eden-paths-branches (uuid paths)
  "Return last entry of maximal paths in PATHS that contain UUID.

For instance:

    (let ((paths \\='([\"uuid-req-1\"]
                   [\"uuid-req-1\" \"uuid-req-2\"]
                   [\"uuid-req-1\" \"uuid-req-2\" \"uuid-req-3\"]
                   [\"uuid-req-1\" \"uuid-req-2\" \"uuid-req-4\"]
                   [\"uuid-req-2\" \"uuid-req-5\"]
                   [\"uuid-req-6\"])))
      (eden-paths-branches \"uuid-req-2\" paths))
    ;; (\"uuid-req-3\" \"uuid-req-4\" \"uuid-req-5\")"
  (let ((tail (reverse paths))
        (maximals-map (make-hash-table :test 'equal))
        branches)
    (while tail
      (let ((path-vec (pop tail)))
        (when (not (eden-get-in maximals-map path-vec))
          (eden-assoc-in maximals-map path-vec t)
          (when (seq-contains-p path-vec uuid)
            (push path-vec branches)))))
    (mapcar (lambda (p) (aref p (1- (length p))))
            branches)))

(defun eden-paths-since (dir timestamp)
  "Return the list of request paths in DIR since TIMESTAMP.

Request paths are ordered chronologically.

See `eden-request-conversation-path' and `eden-request-timestamp'."
  (let* ((timestamp-files (directory-files-recursively dir "timestamp-.*")))
    (thread-last
      timestamp-files
      (mapcar (lambda (f)
                (string-match ".*/\\([^/]+\\)/timestamp-\\(.*\\)" f)
                (cons (match-string 1 f)
                      (string-to-number (match-string 2 f)))))
      (seq-sort (lambda (t1 t2) (< (cdr t1) (cdr t2))))
      (mapcar (lambda (r)
                (when (<= timestamp (cdr r))
                  (let ((req `(:dir ,dir :uuid ,(car r))))
                    (eden-request-conversation-path req)))))
      (delq nil))))

(defun eden-paths-last (dir num-of-days)
  "Return the list of request paths in DIR for the last NUM-OF-DAYS days.

The range for NUM-OF-DAYS starts at 1 (indicating today), with 2
representing today and yesterday, and so on.

Request paths are ordered chronologically.

See `eden-paths-since' and `eden-request-timestamp'."
  (let* ((today (calendar-current-date))
         (midnight (encode-time `(0 0 0 ,(nth 1 today) ,(nth 0 today) ,(nth 2 today))))
         (timestamp (thread-last (days-to-time (1- num-of-days))
                                 (time-subtract midnight)
                                 (float-time)))
         (timestamp-files (directory-files-recursively dir "timestamp-.*")))
    (eden-paths-since dir timestamp)))

(defun eden-paths-last-requests (dir num-of-days)
  "Return the list of requests in DIR for the last NUM-OF-DAYS days.

The range for NUM-OF-DAYS starts at 1 (indicating today), with 2
representing today and yesterday, and so on.

Request are ordered chronologically (see `eden-request-timestamp')."
  (mapcar (lambda (p) (aref p (1- (length p))))
          (eden-paths-last dir num-of-days)))

(defun eden-paths-last-conversations (dir num-of-days)
  "Return the latest requests of conversations in DIR for the last NUM-OF-DAYS days.

The range for NUM-OF-DAYS starts at 1 (indicating today), with 2
representing today and yesterday, and so on.

Latest request of conversations are ordered chronologically."
  (eden-paths-maximal (eden-paths-last dir num-of-days)))

;;;; Main menu

(defun eden-show-current-conversation ()
  "Display current conversation.

See `eden-conversations' and `eden-conversation-id'."
  (interactive)
  (cond
   ((not (eden-conversation-exists-p eden-conversation-id))
    (message "No current conversation to display."))
   ((null (eden-conversation-last-req eden-conversation-id))
    (message "Current conversation is empty."))
   (t (let ((buff-name (eden-conversation-buffer-name eden-conversation-id))
            (title (eden-conversation-title eden-conversation-id))
            (last-req (eden-conversation-last-req eden-conversation-id)))
        (when (not (get-buffer buff-name))
          (with-current-buffer (get-buffer-create buff-name)
            (save-excursion
              (org-mode)
              (eden-conversation-insert last-req title))))
        (eden-maybe-delete-window-prompt-buffer)
        (select-window
         (display-buffer buff-name '(display-buffer-reuse-window)))))))

(defun eden-show-last-conversations ()
  "Show last conversations from `eden-dir' for a period of time entered in the minibuffer.

Timing can be 1 (indicating today), 2 representing today and yesterday,
and so on.

Conversations are ordered chronologically.

See `eden-paths-last-conversations'."
  (interactive)
  (let* ((num-of-days (read-number "Enter the number of days: "))
         (conversations (eden-paths-last-conversations eden-dir num-of-days))
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
  "Show last requests from `eden-dir' for a period of time entered in the minibuffer.

Timing can be 1 (indicating today), 2 representing today and yesterday,
and so on.

Requests are ordered chronologically.

See `eden-last-request'."
  (interactive)
  (let* ((num-of-days (read-number "Enter the number of days: "))
         (requests (eden-paths-last-requests eden-dir num-of-days))
         (buff (get-buffer-create (eden-buffer-name "last requests"))))
    (with-current-buffer buff
      (save-excursion
        (erase-buffer)
        (org-mode)
        (dolist (req-uuid requests)
          (eden-conversation-insert
           `(:dir ,eden-dir :uuid ,req-uuid)
           "Request" nil 'only-last-req))))
    (eden-maybe-delete-window-prompt-buffer)
    (select-window
     (display-buffer buff '(display-buffer-reuse-window)))))

(defun eden-show-current-configuration ()
  "Show current Configuration.

This includes informations about `eden-dir', `eden-api', `eden-model',
`eden-include-reasoning' `eden-system-message', `eden-system-message-append'
and the current conversation.

See `eden-conversation-id' and `eden-conversations'."
  (interactive)
  (let* ((buff (get-buffer-create (eden-buffer-name "current settings")))
         (service (plist-get eden-api :service))
         (conversation (if-let ((conversation (assoc eden-conversation-id eden-conversations)))
                           (format "%S" conversation)
                         ""))
         (system-message (format "%s" (or eden-system-message "")))
         (system-message-append (format "%s" (or eden-system-message-append "")))
         (options
          (delq nil
                `(("service" . ,service)
                  ("endpoint" . ,(plist-get eden-api :endpoint))
                  ("eden-dir" . ,eden-dir)
                  ("model" . ,eden-model)
                  ("include reasoning" . ,(format "%s" eden-include-reasoning))
                  ("conversation" . ,conversation)
                  ("system message" . ,system-message)
                  ("system message append" . ,system-message-append))))
         (opt-max-len (apply 'max (mapcar (lambda (opt) (length (car opt)))
                                          options))))
    (with-current-buffer buff
      (save-excursion
        (erase-buffer)
        (dolist (opt options)
          (insert (string-pad (car opt) opt-max-len nil 't) ": "
                  (cdr opt) "\n"))))
    (eden-maybe-delete-window-prompt-buffer)
    (select-window
     (display-buffer buff '(display-buffer-reuse-window)))))

(transient-define-suffix eden-api-set ()
  "Set `eden-api' selecting from `eden-apis' OpenAI-compatible APIs.

Moreover, if `:default-value' key of the selected API is non-nil,
it becomes the value of `eden-model'."
  :transient t
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
        (setq eden-model (or default-model eden-model))
        (message "Set current API to `%s' and current model to `%s'"
                 service eden-model))
    (error "`eden-apis' variable must be a list of API specifications, not `%S'.  See its documentation for an example.")))

(transient-define-suffix eden-model-set ()
  "Set `eden-model' selecting from `:models' of `eden-api'."
  :transient t
  (interactive)
  (let* ((service (plist-get eden-api :service))
         (models (plist-get eden-api :models))
         (model (completing-read
                 (format "Choose a model for the service `%s': " service)
                 models)))
    (setq eden-model model)
    (message "Current model has been set to `%s'." eden-model)))

(transient-define-suffix eden-dir-set-suffix ()
  "Set `eden-dir' interactively."
  :transient t
  (interactive)
  (let ((dir (read-directory-name "Set request directory to: ")))
    (eden-dir-set dir)
    (message "Next requests will be stored in `%s' directory." eden-dir)))

(transient-define-suffix eden-include-reasoning-toggle ()
  "Toggle `eden-include-reasoning' value."
  :transient t
  (interactive)
  (setq eden-include-reasoning (not eden-include-reasoning))
  (if eden-include-reasoning
      (message "Include reasoning information in conversations.")
    (message "Do not include reasoning information in conversations.")))

(transient-define-prefix eden-menu ()
  "Transient command to manage conversations, requests and Eden's settings.

- Conversations and requests
  - `eden-conversation-continue-from-req-history'
  - `eden-conversation-edit-title'
  - `eden-conversation-pause'
  - `eden-show-current-conversation'
  - `eden-show-last-conversations'
  - `eden-kill-last-request'
  - `eden-show-last-requests'
  - `eden-prompt-current-goto'
- Configuration
  - `eden-api-set'
  - `eden-model-set'
  - `eden-dir-set-suffix'
  - `eden-include-reasoning-toggle'
  - `eden-show-current-configuration'"
  [["Conversations and requests"
    ("c" "Continue conversation from current req" eden-conversation-continue-from-req-history)
    ("e" "Edit current conversation title" eden-conversation-edit-title)
    ("SPC" "Pause current conversation" eden-conversation-pause)
    ("v" "Show current conversation" eden-show-current-conversation)
    ("l" "Show last conversations" eden-show-last-conversations)
    ("L" "Show last requests" eden-show-last-requests)
    ("k" "Kill last request" eden-kill-last-request)
    ("g" "Go to current request in history" eden-prompt-current-goto)]
   ["Configuration"
    ("a" "Set API" eden-api-set)
    ("m" "Set model" eden-model-set)
    ("i" "Include reasoning information" eden-include-reasoning-toggle)
    ("d" "Set request directory" eden-dir-set-suffix)
    ("C" "Show current configuration" eden-show-current-configuration)]]
  (interactive)
  (transient-setup 'eden-menu)
  (define-key transient--transient-map (kbd "q") #'transient-quit-one)
  (define-key transient--transient-map (kbd "RET") #'transient-quit-one))

;;;; Request at point menu

(defun eden-req-at-point-uuid ()
  "Return the UUID of the request at point.

To find a request a point, the point must be on an `org-mode'
heading which includes the property `eden-org-property-req'.

Signal an error either if there is no request at point, or if
the request cannot be found in `eden-dir'."
  (if-let* ((req-uuid (org-entry-get nil eden-org-property-req))
            (req-dir (eden-request-dir `(:dir ,eden-dir :uuid ,req-uuid))))
      (if (file-exists-p req-dir)
          req-uuid
        (error "Request `%s' doesn't exist" req-dir))
    (error "No request at point found")))

(defun eden-req-at-point-continue-conversation ()
  "Continue conversation from request at point."
  (interactive)
  (when-let ((req-uuid (eden-req-at-point-uuid)))
    (eden-conversation-add
     eden-dir (read-string "Enter a conversation title: ") req-uuid))
  (eden))

(defun eden-req-at-point-show-requests ()
  "Show requests of conversation at point.

The conversation at point is the request at point as defined
in `eden-req-at-point-uuid' but considering all its exchanges.

Essentially, we look at the request at point as the last request
in a conversation (see `eden-request-conversation')."
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
          (eden-conversation-insert req "Request" nil 'only-last-req))))
    (select-window
     (display-buffer buff '(display-buffer-reuse-window)))))

(defun eden-req-at-point-show-branches ()
  "Show conversation branches containing request at point.

See `eden-req-at-point-uuid' and `eden-paths-branches'."
  (interactive)
  (when-let* ((req-uuid (eden-req-at-point-uuid))
              (req `(:dir ,eden-dir :uuid ,req-uuid))
              (timestamp (eden-request-timestamp req))
              (branches
               (eden-paths-branches req-uuid
                                    (eden-paths-since eden-dir timestamp)))
              (requests (mapcar
                         (lambda (uuid) `(:dir ,eden-dir :uuid ,uuid))
                         branches))
              (buff (get-buffer-create
                     (eden-buffer-name "branches of request at point"))))
    (with-current-buffer buff
      (erase-buffer)
      (org-mode)
      (save-excursion
        (dolist (req requests)
          (eden-conversation-insert req "Branch"))))
    (select-window
     (display-buffer buff '(display-buffer-reuse-window)))))

(defun eden-req-at-point-show-system-message ()
  "Show system message of the request at point.

See `eden-req-at-point-uuid' and `eden-system-message'."
  (interactive)
  (when-let* ((req-uuid (eden-req-at-point-uuid))
              (req `(:dir ,eden-dir :uuid ,req-uuid)))
    (let ((system-message (eden-request-read req 'system-message)))
      (if (string-empty-p system-message)
          (message "No system message for `%s' request." (eden-request-dir req))
        (let ((buff (get-buffer-create (eden-buffer-name "system message"))))
          (with-current-buffer buff
            (erase-buffer)
            (org-mode)
            (save-excursion (insert system-message)))
          (select-window
           (display-buffer buff '(display-buffer-reuse-window))))))))

(defun eden-req-at-point-show-citations ()
  "Show Perplexity citations of the request at point.

See `eden-req-at-point-uuid' and `eden-request-citations'."
  (interactive)
  (when-let* ((req-uuid (eden-req-at-point-uuid))
              (req `(:dir ,eden-dir :uuid ,req-uuid)))
    (if-let ((citations (eden-request-citations req)))
        (let ((buff (get-buffer-create
                     (eden-buffer-name "citations"))))
          (with-current-buffer buff
            (erase-buffer)
            (org-mode)
            (save-excursion
              (dolist (citation citations)
                (insert (format "- %s\n" citation)))))
          (select-window
           (display-buffer buff '(display-buffer-reuse-window))))
      (message "No citations for `%s' conversation" (eden-request-dir req)))))

(defun eden-req-at-point-show-reasoning ()
  "Show reasoning of the request at point.

See `eden-req-at-point-uuid' and `eden-request-assistant-reasoning'."
  (interactive)
  (when-let* ((req-uuid (eden-req-at-point-uuid))
              (req `(:dir ,eden-dir :uuid ,req-uuid)))
    (eden-request-check req)
    (if (file-exists-p (eden-request-file req 'reasoning))
        (let ((buff (get-buffer-create
                     (eden-buffer-name "reasoning of request at point"))))
          (with-current-buffer buff
            (erase-buffer)
            (org-mode)
            (insert-file-contents (eden-request-file req 'reasoning)))
          (select-window
           (display-buffer buff '(display-buffer-reuse-window))))
      (message "No reasoning for `%s' request" (eden-request-dir req)))))

(defun eden-req-at-point-goto ()
  "Go to the directory of the request at point.

See `eden-req-at-point-uuid' and `eden-request-dir'."
  (interactive)
  (when-let ((req-uuid (eden-req-at-point-uuid))
             (req-dir (eden-request-dir `(:dir ,eden-dir :uuid ,req-uuid))))
    (dired req-dir)))

(transient-define-prefix eden-req-at-point-menu ()
  "Transient command to manage conversations and requests at point.

- `eden-req-at-point-continue-conversation'
- `eden-req-at-point-show-requests'
- `eden-req-at-point-show-branches'
- `eden-req-at-point-show-system-message'
- `eden-req-at-point-show-citations'
- `eden-req-at-point-show-reasoning'
- `eden-req-at-point-goto'"
  [["Conversation/Request at point"
    ("c" "Continue conversation from request at point" eden-req-at-point-continue-conversation)
    ("r" "Show requests of conversation at point" eden-req-at-point-show-requests)
    ("b" "Show branches of request at point" eden-req-at-point-show-branches)
    ("s" "Show system message of request at point" eden-req-at-point-show-system-message)
    ("C" "Show citations of conversation at point" eden-req-at-point-show-citations)
    ("R" "Show reasoning of request at point" eden-req-at-point-show-reasoning)
    ("g" "Go to directory of request at point" eden-req-at-point-goto)
    ]]
  (interactive)
  (transient-setup 'eden-req-at-point-menu)
  (define-key transient--transient-map (kbd "q") #'transient-quit-one)
  (define-key transient--transient-map (kbd "RET") #'transient-quit-one))

;;;; Main command

(defvar eden-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-p") 'eden-prompt-previous)
    (define-key map (kbd "M-n") 'eden-prompt-next)
    (define-key map (kbd "C-M-p") 'eden-profile-previous)
    (define-key map (kbd "C-M-n") 'eden-profile-next)
    (define-key map (kbd "C-c C-c") #'eden-send)
    map)
  "Keymap for `eden-mode'.")

(define-derived-mode eden-mode org-mode "Eden"
  "Eden mode.

Upon activation, this mode sets `mode-line-format' and `eden-profile-ring'
variables, adds `eden-profile-push' to `transient-exit-hook' and calls
`eden-history-update' function.

It is used within `eden-prompt-buffer-name' for user prompt input.

Derived from `org-mode', this mode maintains most keybindings from
`org-mode', with the following exceptions in `eden-mode-map':

- `M-p'     - `eden-prompt-previous',
- `M-n'     - `eden-prompt-next',
- `C-M-p'   - `eden-profile-previous'
- `C-M-n'   - `eden-profile-next'
- `C-c C-c' - `eden-send'.

Overall, you can use most `org-mode' features alongside these
mode-specific capabilities."
  (setq
   mode-line-format
   '(" "
     (:propertize "%6b" face mode-line-buffer-id)
     (:eval (format " %s/%s"
                    (plist-get eden-api :service)
                    (truncate-string-to-width eden-model 16 nil nil t)))
     (:eval (format " %s" (file-name-base (directory-file-name eden-dir))))
     (:eval (when-let ((system-message-title (car-safe eden-system-message)))
              (concat (propertize " > " 'face '(:weight bold))
                      (format "%s"
                              (truncate-string-to-width
                               system-message-title 24 nil nil t)))))
     (:eval (when eden-conversation-id
              (format " [%s]" (eden-conversation-title eden-conversation-id))))
     " "
     mode-line-misc-info))
  (eden-profile-push)
  (add-hook 'transient-exit-hook 'eden-profile-push nil t)
  (eden-history-update :dir eden-dir))

(defun eden (&optional arg)
  "Command to access the prompt buffer and menus to manage conversations and settings.

If not in `eden-prompt-buffer-name' buffer, select a window displaying it
at the bottom of the frame.  Once in that buffer (in `eden-mode') you can
enter your prompt and send it to `eden-api' with `eden-send' command bound
by default to `C-c C-c'.

If not in `eden-prompt-buffer-name' buffer and called with a `\\[universal-argument]' prefix
argument, call `eden-req-at-point-menu' command.

If in `eden-prompt-buffer-name' buffer, call `eden-menu'.

Signal an error if the following utilities are not installed and available
in one of `exec-path' directories:

- curl
- uuidgen
- pandoc"
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
