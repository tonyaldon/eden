;;; eden.el --- The intuitive ChatGPT integration designed for Emacs users -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Tony Aldon
;;
;; Author: Tony Aldon <tony@tonyaldon.com>
;; Version: 1.9
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://tonyaldon.com
;;
;;; Commentary:
;;
;; Boost your productivity with Eden, the intuitive ChatGPT
;; integration designed for Emacs users craving a clean, text-focused
;; interface that adapts to various workflows.
;;
;; It seamlessly facilitates branching conversations making it the
;; perfect tool for effective thinking and brainstorming.
;;
;; Eden's interface is simple:
;;
;; - You want to ask something to ChatGPT, call `eden' command, enter your
;;   prompt, press C-c C-c and you're done.
;; - You want to integrate the response in your `org-mode' notes, just
;;   copy/paste it.
;; - You want to manage your configuration, call `eden' and explore the
;;   options in its transient menu.
;;
;; Eden strikes the perfect balance by focusing on conversations without
;; enforcing them; defaulting to independent requests, it makes starting
;; new conversations or continuing from previous ones easy!
;;
;; Another key feature of Eden is its ability to keep track of all your
;; interactions with ChatGPT.  Each request is stored in the `eden-dir'
;; directory, providing a range of benefits:
;;
;; 1) Requests are always preserved, ensuring you can retrieve them at
;;    any time.
;; 2) Should an error occur during processing, the corresponding
;;    error.json file can be consulted for troubleshooting.
;; 3) All data is stored in JSON (or text format), facilitating
;;    integration with other software for further analysis.
;;
;;;; Get started in minutes
;;
;; 1) Ensure the following utilities are installed and present in one
;;    of your `exec-path' directories:
;;
;;    - curl
;;    - uuidgen
;;    - pandoc
;;
;; 2) Add the directory containing eden.el to your `load-path' and
;;    require the Eden package by adding the following lines to your init
;;    file, ensuring to replace /path/to/eden/ with the appropriate
;;    directory:
;;
;;        (add-to-list 'load-path "/path/to/eden/")
;;        (require 'eden)
;;
;; 3) Store your OpenAI API key in either the ~/.authinfo.gpg file
;;    (encrypted with gpg) or the ~/.authinfo file (plaintext):
;;
;;    - After funding your OpenAI account (https://platform.openai.com)
;;      ($5.00 is enough to get started), create an OpenAI API key
;;      visiting https://platform.openai.com/api-keys.
;;    - Add the API key in the selected file as follows:
;;
;;          machine openai password <openai-api-key>
;;
;;      where <openai-api-key> is your API key.
;;
;;    - Restart Emacs to apply this change.
;;
;; 4) Call the command `eden' to switch to *eden* prompt buffer,
;; 5) Enter your prompt,
;; 6) Press C-c C-c to send your prompt to OpenAI API,
;; 7) Finally, the response will asynchronously show up in a dedicated
;;    buffer upon receipt.
;;
;;;; Utility requirements
;;
;; Ensure the following utilities are installed and present in one
;; of your `exec-path' directories:
;;
;; - curl
;; - uuidgen
;; - pandoc
;;
;;;; Eden's interface is simple
;;
;; 1) You want to ask something to ChatGPT, call `eden' command, enter your
;;    prompt, press `C-c C-c' and you're done.
;; 2) You want to integrate the response in your `org-mode' notes, just
;;    copy/paste it.
;; 3) You want to manage your configuration, call `eden' from the prompt
;;    buffer and explore the options in its transient menu.  For instance,
;;    you can:
;;
;;    - Show the current settings by pressing `C',
;;    - Set the current API by pressing `a' or
;;    - Pick a system message by pressing `p'.
;;
;; 4) You want to manage your conversations, call `eden' from the prompt
;;    buffer and explore the options in its transient menu.  For instance,
;;    you can:
;;
;;    - Start a new conversation by pressing `n',
;;    - Switch to an existing conversation by pressing `TAB' or
;;    - Paused a conversation by pressing `SPC'.
;;
;; 5) Finally, you can also continue or inspect a conversation from a
;;    request at point by calling `eden' with the prefix argument `C-u'.
;;    This will bring up `eden-req-at-point-menu', offering various actions
;;    related to the request at point.
;;
;;;; Eden keeps track of all your interactions with ChatGPT
;;
;; Another key feature of Eden is its ability to keep track of all your
;; interactions with ChatGPT or any OpenAI-compatible API.
;;
;; Each request is stored in the `eden-dir' directory, which defaults to
;; the `eden' subdirectory within your `user-emacs-directory', providing a
;; range of benefits:
;;
;; 1) Requests are always preserved, ensuring you can retrieve them at
;;    any time.
;; 2) With the request's UUID, you can track down the associated
;;    request and check details like the API, model, system prompt, and
;;    timestamp.
;; 3) Should an error occur during processing, the corresponding
;;    `error.json' file can be consulted for troubleshooting.
;; 4) You can start or continue a conversation from any existing request
;;    (a feature known as "branching"):
;;    - Either from a request at point in your notes,
;;    - Or navigating through history in the prompt buffer using `M-p' and
;;      `M-n' to find the desired request, opening the menu with `eden', and
;;      pressing `c' to continue the conversation or `s' to start a new
;;      conversation from the request.
;; 5) All data is stored in JSON (or text format), facilitating
;;    integration with other software for further analysis.
;;
;;;; Eden focuses on conversations without enforcing them
;;
;; Eden strikes the perfect balance by focusing on conversations without
;; enforcing them; defaulting to independent requests, it makes starting
;; new conversations or continuing from previous ones easy!
;;
;; There are several ways to engage in a conversation while in the prompt
;; buffer:
;;
;; 1) To start a new conversation, call the `eden' command, press `n',
;;    and enter a title.  This creates a new empty conversation, setting
;;    it as the current one for all new requests.
;;
;; 2) To start a conversation from the current request in history
;;    (excluding earlier exchanges), navigate through the history using
;;    `M-p' and `M-n' to find the desired request.  Then, call the `eden'
;;    command, press `s', and enter a title.  This creates a new
;;    conversation that already include one exchange.
;;
;; 3) To continue an existing conversation call the `eden' command, press
;;    `c', and enter a title.  This will include all previous exchanges of
;;    the current request in history.
;;
;; You can pause the current conversation by calling `eden' and pressing
;; `SPC'.  Subsequent requests sent to ChatGPT or any OpenAI-compatible API
;; will then be independent again.
;;
;; Note that conversation titles and IDs are not stored; they only exist
;; during your Emacs session.  However, you can retrieve any conversation
;; later either by saving its UUID in your notes or navigating the prompt
;; history with `M-p' and `M-n'.
;;
;;;; What are requests at point?
;;
;; Calling `eden' with the `C-u' prefix argument opens a menu with available
;; actions related to the request at point.
;;
;; If the point is on an `org-mode' heading which includes the
;; property `eden-org-property-req' (by default `EDEN_REQ'), indicating that
;; a request exists at that position, you can apply one of the action
;; listed in the menu.
;;
;; For instance:
;;
;; - Pressing `c' will continue a conversation whose last request is the
;;   request at point,
;; - Pressing `b' will show the conversation branches of the request at
;;   point (all the conversations containing the request at point).
;;
;;;; Including reasoning for Deepseek-compatible APIs
;;
;; Some models, such as `deepseek-reasoner' from https://www.deepseek.com,
;; generate responses based on their internal reasoning process and provide
;; access to this reasoning.
;;
;; To include this reasoning in Eden conversations for
;; Deepseek-compatible APIs, set the variable
;; `eden-conversation-include-reasoning' to `t'.
;;
;; Additionally, regardless of this setting, you can display the
;; reasoning of a request at point by calling `eden' with the `C-u' prefix
;; argument and pressing `R'.  A buffer will pop up with the corresponding
;; reasoning.
;;
;;;; Configuring Eden
;;
;; In the prompt buffer, you can call `eden' and press `C' to show the current
;; configuration.
;;
;; From the menu provided by `eden', you can modify the current configuration
;; doing the following
;;
;; - Press `a' to set the current API (`eden-api-set'),
;; - Press `m' to set the model for the current API (`eden-model-set'),
;; - Press `t' to set the temperature (`eden-temperature-set'),
;; - Press `d' to set the request directory (`eden-dir-set'),
;; - Press `i' to include reasoning information (`eden-conversation-include-reasoning-toggle'),
;;
;; or the following for modifying the system message:
;;
;; - Press `+' to add a system message (`eden-system-message-add'),
;; - Press `u' to update the system message (`eden-system-message-update'),
;; - Press `p' to pick a system message (`eden-system-message-set'),
;; - Press `_' to reset the system message (`eden-system-message-reset').
;;
;;;;; Navigating the profile history
;;
;; Each time you quit Eden's menu, the current profile which includes
;;
;; - the API,
;; - the request directory,
;; - the model,
;; - the temperature,
;; - the current conversation if any, and,
;; - the system message if any
;;
;; is pushed to an history that you can navigate from the prompt buffer
;; with:
;;
;; - `C-M-p' bound to `eden-profile-previous' and
;; - `C-M-n' bound to `eden-profile-next'.
;;
;;;;; The complete list of user variables
;;
;; The complete list of user variables you may want to adjust includes:
;;
;; - `eden-api'
;; - `eden-apis'
;; - `eden-model'
;; - `eden-temperature'
;; - `eden-system-message'
;; - `eden-system-messages'
;; - `eden-system-message->developer-for-models'
;; - `eden-dir'
;; - `eden-org-property-date'
;; - `eden-org-property-req'
;; - `eden-pops-up-upon-receipt'
;; - `eden-conversation-include-reasoning'
;; - `eden-prompt-buffer-name'
;;
;; For more information on these variables, consult their documentation
;; in the `*Help*' buffer using `describe-variable' command, bound by default
;; to `C-h v'.
;;
;;;; Adding Perplexity API key
;;
;; To use Perplexity API, you can either set `eden-api' to
;;
;;     (:service "perplexity"
;;      :endpoint "https://api.perplexity.ai/chat/completions")
;;
;; and `eden-model' to a model supported by Perplexity API like this
;;
;;     "sonar"
;;
;; or you can select Perplexity API by calling `eden' in the prompt buffer,
;; pressing the key `a' and selecting `perplexity'.
;;
;; In both cases you need to store your Perplexity API key in either the
;; ~/.authinfo.gpg file (encrypted with `gpg') or the ~/.authinfo file
;; (plaintext):
;;
;; - After funding your Perplexity account (https://www.perplexity.ai)
;;   ($5.00 is enough to get started), create a Perplexity API key
;;   visiting https://www.perplexity.ai/settings/api.
;; - Add the API key in the selected file as follows:
;;
;;       machine perplexity password <perplexity-api-key>
;;
;;   where `<perplexity-api-key>' is your API key.
;;
;;;; Adding Deepseek API key
;;
;; Store your Deepseek API key in either the ~/.authinfo.gpg file
;; (encrypted with ~gpg~) or the ~/.authinfo file (plaintext):
;;
;; - After funding your Deespeek account (https://platform.deepseek.com)
;;   ($5.00 is enough to get started), create a Deepseek API key
;;   visiting https://platform.deepseek.com/api_keys.
;; - Add the API key in the selected file as follows:
;;
;;       machine deepseek password <deepseek-api-key>
;;
;; where <deepseek-api-key> is your API key.
;;
;;;; Adding an OpenAI-compatible API to eden-apis
;;
;; Let's take an example with X.ai API, an OpenAI-compatible API.
;;
;; If you want to use X.ai API alongside other OpenAI-compatible APIs,
;; start by adding its description to the `eden-apis' variable as follows:
;;
;;     (add-to-list 'eden-apis
;;                  '(:service "x.ai"
;;                    :endpoint "https://api.x.ai/v1/chat/completions"
;;                    :default-model "grok-2"
;;                    :models ("grok-beta" "grok-2-latest" "grok-2" "grok-2-12-12")))
;;
;; Then you need to store your X.ai API key in either the ~/.authinfo.gpg
;; file (encrypted with `gpg') or the ~/.authinfo file (plaintext):
;;
;; - After funding your X.ai account (https://console.x.ai)
;;   ($5.00 is enough to get started), create an X.ai API key
;;   in that same console.
;; - Add the API key in the selected file as follows:
;;
;;       machine x.ai password <x.ai-api-key>
;;
;;   where `<x.ai-api-key>' is your API key.
;;
;; Finally, you can select X.ai API with `grok-2' default model by calling
;; `eden' in the prompt buffer, pressing the key `a' and selecting `x.ai'.
;;
;;; Code:

(require 'json)
(require 'ox-md)
(require 'transient)

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
    (concat (file-name-as-directory (expand-file-name dir)) uuid "/")))

(defun eden-request-file (file-type req)
  "Return full path of file of FILE-TYPE of REQ request.

Signal an error if FILE-TYPE is not one of the following symbols:

    error, response, response-org, reasoning, request, api,
    prompt, system-message, exchanges, command.

For instance

    (eden-request-file \\='request \\='(:dir \"/tmp/eden/\" :uuid \"foo\"))
    ;; \"/tmp/eden/foo/request.json\"
    (eden-request-file \\='prompt \\='(:dir \"/tmp/eden/\" :uuid \"foo\"))
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
  (let* ((file (eden-request-file file-type req)))
    (if (not (file-exists-p file))
        (error "Missing `%s' file" file)
      (with-temp-buffer
        (insert-file-contents (eden-request-file file-type req))
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
    ;; \"foo assistant\""
  (eden-get-in resp [:choices 0 :message :content]))

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
    ;; \"foo reasoning\""
  (eden-get-in resp [:choices 0 :message :reasoning_content]))

(defun eden-request-user-content (request)
  "Return the last message of REQUEST, an OpenAI-compatible API request.

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
  - exchanges.json
  - timestamp-<timestamp> where <timestamp> is a timestamp."
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
     ((null (directory-files req-dir nil "timestamp-.*"))
      (error "Missing timestamp file in `%s' request" req-dir))
     (t t))))

(defun eden-request-conversation (req)
  "Return all exchanges of the conversation whose last request is REQ.

The return value is a vector of plists (exchanges) containing the
following keys:

- :uuid                 - UUID of the request for that exchange
- :prompt               - prompt of that exchange (`org-mode' string)
- :user                 - prompt of that exchange sent to OpenAI-compatible API
                          (markdown string)
- :assistant            - content message of the response of that exchange
                          received from OpenAI-compatible API (markdown string)
- :response             - content message of the response of that exchange
                          received from OpenAI-compatible API converted to Org
                          (`org-mode' string)
- :assistant-reasoning  - (optional) reasoning content of the response of that exchange
                          received from Deepseek-compatible API (markdown string)
- :reasoning            - (optional) reasoning content of the response of that exchange
                          received from Deepseek-compatible API converted to Org
                          (`org-mode' string)

Signal an error if REQ doesn't pass `eden-request-check' check.

For instance, assuming \"uuid-baz\" is the UUID of the last request
of a conversation whose previous exchanges are the requests whose
UUIDs are \"uuid-foo\" and \"uuid-bar\" in that order, the following
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
         (resp (eden-request-read 'response req))
         (last-exchange
          (list
           (delq nil
                 `(:uuid ,(plist-get req :uuid)
                   :prompt ,(eden-request-read 'prompt req)
                   :user ,(eden-request-user-content
                           (eden-request-read 'request req))
                   :assistant ,(eden-request-assistant-content resp)
                   :response ,(eden-request-read 'response-org req)
                   ,@(when-let ((assistant-reasoning
                                 (eden-request-assistant-reasoning resp)))
                       `(:assistant-reasoning ,assistant-reasoning))
                   ,@(when (file-exists-p (eden-request-file 'reasoning req))
                       `(:reasoning ,(eden-request-read 'reasoning req))))))))
    (apply 'vector (append exchanges last-exchange))))

(defun eden-request-conversation-path (req)
  "Return the path of the conversation whose last request is REQ.

Return nil if REQ doesn't pass `eden-request-check' check.

For instance, assuming \"uuid-baz\" is the UUID of the last request
of a conversation whose previous exchanges are the requests whose
UUIDs are \"uuid-foo\" and \"uuid-bar\" in that order, we have
the following:

    (eden-request-conversation-path \\='(:dir \"/tmp/eden/\" :uuid \"uuid-baz\"))
    ;; [\"uuid-foo\" \"uuid-bar\" \"uuid-baz\"]"
  (when (condition-case nil (eden-request-check req) (error nil))
    (let* ((uuids (mapcar (lambda (exchange) (plist-get exchange :uuid))
                          (eden-request-read 'exchanges req)))
           (last-uuid (list (plist-get req :uuid))))
      (apply 'vector (append uuids last-uuid)))))

(defun eden-request-perplexity-citations (req)
  "Return the list of Perplexity citations of the conversation REQ.

More precisely of the conversation whose last request is REQ.

When using Perplexity API, the JSON response to a request may
contain citations (urls).  These citations are accessible
under the key \"citations\" represented as an array.

The function `eden-request-perplexity-citations' returns the
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

- a `timestamp' file      - see `eden-request-timestamp' for more details
                            on that file,
- a `request' file        - a JSON file with content being the value
                            under `:req' key of REQ plist,
- an `api' file           - a JSON file with content being the value
                            under `:api' key of REQ plist,
- a `prompt' file         - an `org-mode' file with content being the value
                            under `:prompt' key,
- a `system-message' file - an `org-mode' file with content being the value
                            under `:system-message' key,
- an `exchanges' file     - a JSON file with content being the value
                            under `:exchanges' key of REQ plist.

Here's an example with a typical request (third of a conversation)
that we would send to OpenAI API.  Evaluating the following expression

    (let ((req \\='(:req (:stream :false
                       :model \"gpt-4o-mini\"
                       :temperature 1
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
- /tmp/eden/uuid-baz/system-message.org
- /tmp/eden/uuid-baz/timestamp-1736137516.050655"
  (let ((request (eden-json-encode (plist-get req :req)))
        (api (eden-json-encode (plist-get req :api)))
        (prompt (plist-get req :prompt))
        (system-message (or (plist-get req :system-message) ""))
        (exchanges (eden-json-encode (plist-get req :exchanges))))
    (eden-request-write 'timestamp req "")
    (eden-request-write 'request req request)
    (eden-request-write 'api req api)
    (eden-request-write 'prompt req prompt)
    (eden-request-write 'system-message req system-message)
    (eden-request-write 'exchanges req exchanges)))

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
                (concat "Do you have a line in `~/.authinfo.gpg' file declaring "
                        "the API key of service `%s' like this: "
                        "machine %s password <api-key>")
                service service)))
    (list
     (format command-fmt endpoint (eval api-key-symbol) request-file)
     (format command-fmt endpoint "<api-key>" request-file))))

(defun eden-write-command (command-no-api-key req)
  "Write COMMAND-NO-API-KEY in file of type `command' in REQ's directory.

See `eden-request-file' and `eden-request-command'."
  (message "%s" command-no-api-key)
  (eden-request-write 'command req command-no-api-key))

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

(defun eden-write-response (resp-str resp req)
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
  (eden-request-write 'response req resp-str)
  (let* ((assistant-content (eden-request-assistant-content resp))
         (response-org (eden-markdown-to-org assistant-content))
         (citations (plist-get resp :citations))
         (response-org
          (if (and citations (vectorp citations))
              (eden-org-replace-perplexity-citations response-org citations)
            response-org)))
    (eden-request-write 'response-org req response-org))
  (when-let* ((assistant-reasoning (eden-request-assistant-reasoning resp))
              (reasoning (eden-markdown-to-org assistant-reasoning)))
    (eden-request-write 'reasoning req reasoning)))

(defun eden-write-error (err req)
  "Write ERR error in REQ's directory.

ERR is non string object that we encode to JSON format
and saved as error file type in REQ's directory.

See `eden-request-file'."
  (eden-request-write 'error req (eden-json-encode err)))

(defvar eden-errors
  '((eden-error-api . "API error")
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

(cl-defun eden-error-log-and-signal (type req process
                                          &key error event process-stdout
                                          callback-error info)
  "Signal error of TYPE type that happened when sending REQ request.

Before signaling the error:

1) PROCESS process is killed,
2) produce the error data,
2) CALLBACK-ERROR function is called with 3 arguments: REQ,
   the error data and INFO,
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
               :temperature nil
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

The return sentinel is a function that takes two arguments `process'
and `event' as described in `make-process'.

When no error occurs during execution of the sentinel, CALLBACK function
is called.  It takes 3 arguments

- REQ   - plist of information about the request where :req
          is an OpenAI-compatible API request.  For instance:

              (:req (:stream :false
                     :model \"gpt-4o-mini\"
                     :temperature 1
                     :messages [(:role \"user\" :content \"foo bar baz\")])
               :api (:service \"openai\"
                     :endpoint \"https://api.openai.com/v1/chat/completions\")
               :prompt \"foo bar baz\"
               :dir \"/tmp/eden/\"
               :uuid \"40e73d38-7cb9-4558-b11f-542f8a2d1f9c\")

- resp  - plist of the response received from OpenAI-compatible API
- INFO  - plist of additional data, can be nil

and must be use for side effects.

When an error occurs, CALLBACK-ERROR function (if not nil) is called
just before signaling the error.  It takes 3 arguments:

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
- INFO  - plist of additional data, can be nil"
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
  "Send REQ request asynchronously to OpenAI-compatible API using `make-process'.

Return the process handling the request.

If the request succeed, CALLBACK function is called in the sentinel.
If the request failed or there's an error in the sentinel, CALLBACK-ERROR
function is called.  In both cases, INFO, plist of additional data, is
passed as last argument to these functions.  See `eden-sentinel'.

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

- :req            - The request that we send to OpenAI API or a compatible
                    API like Perplexity.  See:

                    - https://platform.openai.com/docs/guides/text-generation/,
                    - https://platform.openai.com/docs/api-reference/chat and
                    - https://docs.perplexity.ai/api-reference/chat-completions.

                    As we don't support streaming API, value for `:stream'
                    key must always be `:false'.  Here's an example:

                        (:stream :false
                         :model \"gpt-4o-mini\"
                         :temperature 1
                         :messages [(:role \"user\" :content \"foo bar baz\")])

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

- :prompt         - The prompt of the request in `org-mode' format.
- :dir            - A directory (absolute path) where we log all the
                    requests.
- :uuid           - A unique ID (a string) which is the subdirectory of
                    `:dir' in which we kept information about REQ.  See
                    `eden-request-dir'.
- :system-message - (optional) The system message of the request in `org-mode'
                    format.
- :exchanges      - (optional) If REQ is the last exchange in a conversation,
                    this key must be a vector of the previous exchanges, where
                    each exchange is defined with a plist containing the
                    following keys: `:uuid', `:prompt', `:user', `:assistant'
                    and `:response'.  Here's an example:

                        [(:uuid \"uuid-foo\"
                          :prompt \"foo prompt org-mode\"
                          :user \"foo prompt markdown\"
                          :assistant \"foo response markdown\"
                          :response \"foo response org-mode\")
                         (:uuid \"uuid-bar\"
                          :prompt \"bar prompt org-mode\"
                          :user \"bar prompt markdown\"
                          :assistant \"bar response markdown\"
                          :response \"bar response org-mode\")]

Here's an example of a REQ request using OpenAI API, with no system
message and no previous exchanges:

    (:req (:stream :false
           :model \"gpt-4o-mini\"
           :temperature 1
           :messages [(:role \"user\" :content \"foo bar baz\")])
     :api (:service \"openai\"
           :endpoint \"https://api.openai.com/v1/chat/completions\")
     :prompt \"foo bar baz\"
     :dir \"/tmp/eden/\"
     :uuid \"40e73d38-7cb9-4558-b11f-542f8a2d1f9c\")

Here's an example of a REQ request (third of a conversation), using
Perplexity API and a system message:

    (:req (:stream :false
           :model \"gpt-4o-mini\"
           :temperature 1
           :messages [(:role \"system\" :content \"baz system message\")
                      (:role \"user\" :content \"foo user\")
                      (:role \"assistant\" :content \"foo assistant\")
                      (:role \"user\" :content \"bar prompt\")
                      (:role \"assistant\" :content \"bar assistant\")
                      (:role \"user\" :content \"baz user prompt\")])
     :api (:service \"perplexity\"
           :endpoint \"https://api.perplexity.ai/chat/completions\")
     :prompt \"baz user prompt\"
     :system-message \"baz system message\"
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
     :uuid \"uuid-baz\")"
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
     :default-model \"gpt-4o-mini\"
     :models (\"gpt-4o-mini\" \"gpt-4o\" \"o1-mini\" \"o1\"))

In that case, ensure that the API key, <openai-api-key>, for the
endpoint \"https://api.openai.com/v1/chat/completions\" is stored
in `~/.authinfo.gpg' (encrypted with gpg) or `~/.authinfo' file,
formatted as:

    machine openai password <openai-api-key>")

(defvar eden-apis
  '((:service "openai"
     :endpoint "https://api.openai.com/v1/chat/completions"
     :default-model "gpt-4o-mini"
     :models ("gpt-4o-mini" "gpt-4o" "o1-mini" "o1"))
    (:service "perplexity"
     :endpoint "https://api.perplexity.ai/chat/completions"
     :default-model "sonar"
     :models ("sonar" "sonar-pro" "sonar-reasoning"))
    (:service "deepseek"
     :endpoint "https://api.deepseek.com/chat/completions"
     :default-model "deepseek-reasoner"
     :models ("deepseek-chat" "deepseek-reasoner")))
  "List of OpenAI-compatible APIs available for selection when using `eden-api-set'.

See `eden-api', for detailed descriptions of each element.

Moreover, when we set `eden-api' with `eden-api-set', if API's
`:default-value' is non-nil, it becomes the value of `eden-model'.

Example listing OpenAI API and Perplexity configurations:

    ((:service \"openai\"
      :endpoint \"https://api.openai.com/v1/chat/completions\"
      :default-model \"gpt-4o-mini\"
      :models (\"gpt-4o-mini\" \"gpt-4o\" \"o1-mini\" \"o1\"))
     (:service \"perplexity\"
      :endpoint \"https://api.perplexity.ai/chat/completions\"
      :default-model \"sonar\"
      :models (\"sonar\" \"sonar-pro\")))")

(defvar eden-model "gpt-4o-mini"
  "Model used by `eden-send' to send requests to `eden-api'.

Examples of valid model for OpenAI API: \"gpt-4o-mini\", \"gpt-4o\",
\"o1-mini\", \"o1\".")

(defvar eden-temperature nil
  "Temperature used by `eden-send' to send requests to `eden-api'.

It can be a float between 0 and 2 or nil.")

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

And if `eden-model' belongs to `eden-system-message->developer-for-models',
`:role' of this first message is \"developer\"; otherwise it defaults
to \"system\".

Additionally,`eden-system-message' may be nil, in which case `:messages'
will omit the initial system message.

According to OpenAI API documentation, a system message consists of
\"Developer-provided instructions that the model should follow,
regardless of messages sent by the user.\"")

(defvar eden-system-messages nil
  "Alist of system messages available for selection when using `eden-system-message-set'.

See `eden-system-message', for detailed descriptions of system messages.

For instance we can set `eden-system-messages' to:

    ((\"writer\" . \"You\\='re a good writer who only writes in Italian.\")
     (\"programmer\" . \"You\\='re a programmer who only answers with code snippets.\"))")

(defvar eden-system-message->developer-for-models '("o1")
  "List of models that use \"developer\" message instead of \"system\" message.

According to OpenAI API documentation, \"With o1 models and newer,
developer messages replace the previous system messages.\"")

(defvar eden-dir (concat user-emacs-directory "eden/")
  "Directory where all requests sent by `eden-send' are stored.

Each request is organized in a unique subdirectory within `eden-dir',
containing comprehensive details such as the request itself, the
corresponding response, and any errors encountered, among other relevant
data.

See `eden-write-request', `eden-write-command', `eden-write-response'
and `eden-write-error'.")

(defvar eden-org-property-date "EDEN_DATE"
  "Org property used for the date a request has been issued.

This is used when inserting requests/conversations into buffers.

See `eden-conversation-insert'.")

(defvar eden-org-property-req "EDEN_REQ"
  "Org property used for request's UUID.

This is used when inserting requests or conversations into buffers
and is relevant for any command that operates on requests at point
such as `eden-req-at-point-goto'.

See `eden-conversation-insert' and `eden-req-at-point-uuid'.")

(defvar eden-pops-up-upon-receipt t
  "If t, the response's buffer pops up upon receipt from `eden-api'.

See `eden-send'.")

(defvar eden-conversation-include-reasoning nil
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

;;;; Prompt and Request history

(defvar eden-request-history nil
  "List of request's UUID where the latest request is listed first.

When we call `eden' for the first time, `eden-request-history-set'
initializes `eden-request-history' with existing requests in `eden-dir'.

Then, each invocation of `eden-send' command updates `eden-request-history'
variable.

See `eden-send-request'.")

(defun eden-request-history-set ()
  "Set `eden-request-history' with UUIDs of existing requests in `eden-dir'.

They are sorted by their timestamp file with the latest request appearing
first.

See `eden-request-timestamp'."
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

(defvar eden-prompt-history-state [nil nil nil]
  "State of the prompt history.

- Set by `eden-prompt-history-state-set' in `eden-mode',
- Reset with each call to `eden-send' and
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

(defun eden-prompt-history-state-set ()
  "Set `eden-prompt-history-state' with `eden-request-history'."
  (setq eden-prompt-history-state
        (vector eden-request-history nil nil)))

(defun eden-prompt-current-buffer ()
  "Return current buffer content as string with no text properties.

This function should be called from `eden-prompt-buffer-name' buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun eden-prompt-current-req-uuid ()
  "Return request's UUID of current prompt in `eden-prompt-history-state'.

If the current prompt is temporary with no corresponding request, return nil."
  (when-let ((current (aref eden-prompt-history-state 1)))
    (when (not (consp current)) current)))

(defun eden-prompt-current ()
  "Return current prompt in `eden-prompt-history-state' if any or nil."
  (let ((current (aref eden-prompt-history-state 1)))
    (cond
     ((null current) nil)
     ((consp current) (plist-get current :prompt))
     (t (eden-request-read 'prompt `(:dir ,eden-dir :uuid ,current))))))

(defun eden-prompt-current-goto ()
  "Go to request's directory of current prompt in `eden-prompt-history-state'.

If the current prompt is temporary with no corresponding request, message
the user about it.

See `eden-request-dir'."
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
  "Return a new state of STATE with previous prompt set as current prompt.

STATE follows the structure defined in `eden-prompt-history-state'.

If PROMPT is non-nil, both PROMPT and the current prompt are pushed onto
the stack of next prompts.

If DISCARD-CURRENT is non-nil, the current prompt will not be pushed
onto the stack of next prompts, accommodating requests in
`eden-request-history' that may fail `eden-request-check' and should
be omitted from `eden-prompt-history-state' while navigating it.

Signal an error if both PROMPT and DISCARD-CURRENT are non-nil.

For instance:

    (eden-prompt-history-previous [(\"bar\" \"baz\") \"foo\" nil])
    ;; [(\"baz\") \"bar\" (\"foo\")]

    (eden-prompt-history-previous
     [(\"bar\" \"baz\") \"foo\" nil] \\='(:prompt \"scratch prompt\"))
    ;; [(\"baz\") \"bar\" ((:prompt \"scratch prompt\") \"foo\")]

    (eden-prompt-history-previous
     [(\"foo\" \"bar\") \"to-be-discarded\" nil] nil \\='discard-current)
    ;; [(\"bar\") \"foo\" nil]"
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
  "Return a new state of STATE with next prompt set as current prompt.

Similar to `eden-prompt-history-previous'.

For instance:

    (eden-prompt-history-next [nil \"baz\" (\"bar\" \"foo\")])
    ;; [(\"baz\") \"bar\" (\"foo\")]

    (eden-prompt-history-next
     [(\"baz\") \"bar\" (\"foo\")] '(:prompt \"scratch prompt\"))
    ;; [((:prompt \"scratch prompt\") \"bar\" \"baz\") \"foo\" nil]

    (eden-prompt-history-next
     [nil \"to-be-discarded\" (\"bar\" \"foo\")] nil 'discard-current)
    ;; [nil \"bar\" (\"foo\")]"
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
  "Return t if current prompt is associated to a request failing `eden-request-check'.

See `eden-prompt-history-state'."
  (let ((current (aref eden-prompt-history-state 1)))
    (when (not (or (null current) (consp current)))
      (let ((req `(:dir ,eden-dir :uuid ,current)))
        (if (not (condition-case nil (eden-request-read 'prompt req) (error nil)))
            t)))))

(defun eden-prompt-history (direction)
  "Replace current buffer content with previous or next prompt based on DIRECTION.

DIRECTION can be either `previous' or `next'.

Also update `eden-prompt-history-state' accordingly.  See
`eden-prompt-history-previous' and `eden-prompt-history-next'.

If the content of `eden-prompt-buffer-name' buffer differs from
the current prompt in `eden-prompt-history-state', it is pushed
onto the respective stack of previous or next prompts based on
DIRECTION.

This function should be called from `eden-prompt-buffer-name' buffer."
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
            (save-excursion
              (insert (or (eden-prompt-current) "")))))))))

(defun eden-prompt-previous ()
  "Replace current buffer content with previous prompt.

See `eden-prompt-history-state' and `eden-prompt-history'.

This function should be called from `eden-prompt-buffer-name' buffer."
  (interactive)
  (eden-prompt-history 'previous))

(defun eden-prompt-next ()
  "Replace current buffer content with next prompt.

See `eden-prompt-history-state' and `eden-prompt-history'.

This function should be called from `eden-prompt-buffer-name' buffer."
  (interactive)
  (eden-prompt-history 'next))

;;;; Conversations

(defvar eden-conversations nil
  "Alist of conversations.

A conversation is a cons cells whose

- car is an ID and
- cdr is a plists with the following keys:

  - :title         - The title of the conversation
  - :action        - The symbol `start', `start-from' or `continue-from'
                     depending on the state of the conversation
  - :last-req-uuid - The UUID of the last request in the conversation
                     which can be nil if `:action' is `start'

For instance `eden-conversations' can be:

    ((\"213940f6-fa87-4c27-9aa5-30d6ba3d2724\" .
      (:title \"foo title\"
       :action start
       :last-req-uuid nil))
     (\"bcb3f6ee-1b85-4c92-904a-f8ae8f536f7c\" .
      (:title \"bar title\"
       :action start-from
       :last-req-uuid \"04397cda-f623-425b-9a7d-c29caea3511f\"))
     (\"09b95117-ae13-41dc-aa76-53f63576b771\" .
      (:title \"baz title\"
       :action continue-from
       :last-req-uuid \"2086eac6-61ff-4a44-993a-a928b7a29007\")))")

(defvar eden-conversation-id nil
  "UUID of the current conversation if any.")

(defun eden-conversation-with-title-exists-p (title)
  "Return t if a conversation with TITLE exists in `eden-conversations'."
  (seq-some (lambda (c) (equal title (plist-get (cdr c) :title)))
            eden-conversations))

(defun eden-conversation (action title &optional req-uuid)
  "Add a conversation to `eden-conversations' with a specied ACTION and TITLE.

Valid ACTION values include:

- `start'         - Initiates a new conversation.
- `start-from'    - Begins a conversation from a request requiring its
                    UUID specified by REQ-UUID.
- `continue-from' - Resumes a conversation from a request requiring its
                    UUID specified by REQ-UUID

Also set `eden-conversation-id' to the ID of the newly created conversation
making it the current conversation.

Signal an error if the conversation cannot be added."
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
  "Return title of conversation with CONVERSATION-ID in `eden-conversations'.

If no conversation with CONVERSATION-ID can be found in `eden-conversations',
return nil."
  (eden-get-in eden-conversations `(,conversation-id :title)))

(defun eden-conversation-action (conversation-id)
  "Return action of conversation with CONVERSATION-ID in `eden-conversations'.

If no conversation with CONVERSATION-ID can be found in `eden-conversations',
return nil."
  (eden-get-in eden-conversations `(,conversation-id :action)))

(defun eden-conversation-last-req (conversation-id)
  "Return last request of conversation with CONVERSATION-ID in `eden-conversations'.

If no conversation with CONVERSATION-ID can be found in `eden-conversations',
return nil.

For instance:

    (let ((eden-dir \"/tmp/eden/\")
          (eden-conversations
           \\='((\"09b95117-ae13-41dc-aa76-53f63576b771\" .
              (:title \"baz title\"
               :action continue-from
               :last-req-uuid \"2086eac6-61ff-4a44-993a-a928b7a29007\")))))
      (eden-conversation-last-req \"09b95117-ae13-41dc-aa76-53f63576b771\"))
    ;; (:uuid \"2086eac6-61ff-4a44-993a-a928b7a29007\"
    ;;  :dir \"/tmp/eden/\")"
  (when-let ((uuid (eden-get-in
                    eden-conversations `(,conversation-id :last-req-uuid))))
    `(:uuid ,uuid :dir ,eden-dir)))

(defun eden-conversation-buffer-name (conversation-id)
  "Return buffer name for conversation with CONVERSATION-ID."
  (when-let ((title (eden-conversation-title conversation-id)))
    (eden-buffer-name title)))

(defun eden-conversation-exchanges (conversation-id)
  "Return exchanges of the conversation with CONVERSATION-ID.

Return nil if conversation's `:action' is `start'.
Return only the last exchange if conversation's `:action' is `start-from'.
Return all the exchanges if conversation's `:action' is `start-continue'.

See `eden-conversations' and `eden-request-conversation'."
  (when-let ((last-req (eden-conversation-last-req
                        conversation-id))
             (conversation (eden-request-conversation last-req)))
    (pcase (eden-conversation-action conversation-id)
      ('start-from (vector (aref conversation (1- (length conversation)))))
      ('continue-from conversation))))

(defun eden-conversation-rename (conversation-id new-title)
  "Rename conversation with CONVERSATION-ID to NEW-TITLE in `eden-conversations'.

Signal an error if NEW-TITLE is already used by another conversation."
  (when (eden-conversation-with-title-exists-p new-title)
    (error "Cannot rename conversation with `%s' which is already used by another conversation in `eden-conversations'"
           new-title))
  (when-let ((conversation-data
              (seq-copy
               (cdr (assoc conversation-id eden-conversations)))))
    (setq eden-conversations
          (cons (cons conversation-id
                      (plist-put conversation-data :title new-title))
                (remove (assoc conversation-id eden-conversations)
                        eden-conversations)))))

(transient-define-suffix eden-conversation-edit-title ()
  "Edit title of current conversation and its associated buffer based on user's input.

See `eden-conversation-id' and `eden-conversation-rename'."
  :transient t
  (interactive)
  (if-let ((buff-name (eden-conversation-buffer-name eden-conversation-id)))
      (let* ((old-title (eden-conversation-title eden-conversation-id))
             (new-title (read-string (format "Edit conversation title `%s': " old-title))))
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
  "Update last request UUID of conversation specified by INFO to REQ's UUID.

INFO plist must include a `:conversation-id' key while REQ must contain
a `:uuid' key.

Additionally, conversation's `:action' key is set to `continue-from'.

For instance:

    (let ((eden-conversations
           \\='((\"conversation-id-foo\" .
              (:title \"foo title\" :action start :last-req-uuid nil)))))
      (eden-conversation-update \\='(:conversation-id \"conversation-id-foo\")
                                \\='(:uuid \"new-foo-req-uuid\"))
      eden-conversations)
    ;; ((\"conversation-id-foo\" .
    ;;   (:title \"foo title\" :action continue-from :last-req-uuid \"new-foo-req-uuid\")))

See `eden-conversations', `eden-send-request' and `eden-send'."
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

(transient-define-suffix eden-conversation-switch ()
  "Switch current conversation based on user's selection from `eden-conversations'.

See `eden-conversation-id'."
  :transient t
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
            (alist-get title conversations nil nil #'string=))
      (message "Switched to conversation `%s'." title))))

(transient-define-suffix eden-conversation-start ()
  "Start a new conversation with title based on user's input.

See `eden-conversation', `eden-conversations' and `eden-conversation-id'."
  :transient t
  (interactive)
  (let ((title (read-string "New conversation with title: ")))
    (eden-conversation 'start title)
    (message "Conversation `%s' initialized." title)))

(transient-define-suffix eden-conversation-start-from-req-history ()
  "Start a conversation from current request in history excluding previous exchanges.

See `eden-prompt-history-state', `eden-conversation', `eden-conversations' and
`eden-conversation-id'."
  :transient t
  (interactive)
  (if-let ((req-uuid (eden-prompt-current-req-uuid)))
      (let ((title (read-string "Start conversation with title: ")))
        (eden-conversation 'start-from title req-uuid)
        (message "Conversation `%s' initialized." title))
    (message (concat "Current prompt is not associated with a request.  "
                     "Try navigating the prompt history with `M-p' and `M-n', "
                     "default binding of `eden-prompt-previous' and `eden-prompt-next'."))))

(transient-define-suffix eden-conversation-continue-from-req-history ()
  "Start a conversation from current request in history including all previous exchanges.

See `eden-prompt-history-state', `eden-conversation', `eden-conversations' and
`eden-conversation-id'."
  :transient t
  (interactive)
  (if-let ((req-uuid (eden-prompt-current-req-uuid)))
      (let ((title (read-string "Continue conversation with title: ")))
        (eden-conversation 'continue-from title req-uuid)
        (message "Conversation `%s' initialized." title))
    (message (concat "Current prompt is not associated with a request.  "
                     "Try navigating the prompt history with `M-p' and `M-n', "
                     "default binding of `eden-prompt-previous' and `eden-prompt-next'."))))

(transient-define-suffix eden-conversation-pause ()
  "Pause the current conversation by setting `eden-conversation-id' to nil."
  :transient t
  (interactive)
  (if-let ((title (eden-conversation-title eden-conversation-id)))
      (message "Conversation `%s' paused." title)
    (message "No current conversation."))
  (setq eden-conversation-id nil))

(defun eden-conversation-insert (req title &optional append start-from)
  "Format and insert the conversation whose last request is REQ into current buffer.

Set TITLE as the first heading.

If APPEND is non-nil, only append the last exchange of the conversation.

If START-FROM is non-nil, do not include previous conversation messages,
only the last one being REQ itself.

Signal an error if REQ fails `eden-request-check'.
Signal an error if TITLE and APPEND are both non-nil.

The `org-mode' properties used for the date of the conversation and
REQ's UUID are defined respectively by the variables `eden-org-property-date'
and `eden-org-property-req'.

For instance, given a valid request with \"foo-uuid\" in `/tmp/eden/'
directory, with a prompt \"foo bar baz\" and the response
\"foo bar baz assistant response\", dated Friday, December 20, 2024,
evaluating the following expression

    (eden-conversation-insert '(:dir \"/tmp/eden/\" :uuid \"foo-uuid\")
                              \"Foo Title\")

inserts the following in the current buffer

    ** Foo Title
    :PROPERTIES:
    :EDEN_DATE: [2024-12-20 Fri]
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
  (let* ((uuid (plist-get req :uuid))
         (format-exchange
          (lambda (exchange)
            (delq nil
                  (list (eden-org-demote (plist-get exchange :prompt) 4)
                        (eden-org-demote (plist-get exchange :response) 4)
                        (when-let ((reasoning (plist-get exchange :reasoning)))
                          (eden-org-demote reasoning 4))))))
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
       ;; `eden-conversation-edit-title' command.
       "** " (or title "Conversation") "\n"
       ":PROPERTIES:\n"
       ":" eden-org-property-date ": " (or (eden-request-date req) "")
       "\n"
       ":" eden-org-property-req ": " uuid "\n"
       ":END:\n"))
    (dolist (exchange conversation)
      (seq-let (prompt response reasoning) exchange
        (insert "*** Prompt\n\n" prompt)
        (cond
         ((looking-back "\n\n" nil) nil)
         ((looking-back "\n" nil) (insert "\n"))
         (t (insert "\n\n")))
        (when (and eden-conversation-include-reasoning reasoning)
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

;;;; Sending Requests

(defvar eden-pending-requests nil
  "List of pending requests sent with `eden-send-request'.

Each element is a plist with the following keys:

- :req             - A request created with `eden-request'.
- :conversation-id - The ID for the conversation if `:req' is part of an
                     ongoing conversation in `eden-conversations';  nil
                     if not part of a conversation.
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
exists with `:conversation-id' key matching CONVERSATION-ID.

See `eden-conversations'."
  (seq-some
   (lambda (r)
     (when-let ((id (plist-get r :conversation-id)))
       (string= conversation-id id)))
   eden-pending-requests))

(cl-defun eden-send-request (&key req callback info)
  "Send REQ request asynchronously to OpenAI-compatible API.

This function wraps around `eden-request-send', and performs the
following actions:

- Manages `eden-conversations',
- Updates `eden-prompt-history-state',
- Optionally activates the waiting widget (`eden-mode-line-waiting').

In case of an error during sentinel execution, the following callback-error
function is called:

    (lambda (req _err _info)
      (eden-pending-remove req)
      (eden-mode-line-waiting \\='maybe-stop))

If the request succeeds, the CALLBACK function is executed within the
sentinel and must call these three functions in this specific order:

1) `eden-pending-remove',
2) `eden-conversation-update' and
3) `eden-mode-line-waiting'.

Here's a valid CALLBACK function which appends responses in the
buffer \"*eden[requests]*\":

    (lambda (req resp info)
      (with-current-buffer (get-buffer-create \"*eden[requests]*\")
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

If REQ belongs to a conversation present in `eden-conversations', we
must include its conversation ID in INFO argument using `:conversation-id'
key.

For instance, if \"conversation-id-foo\" is the ID for an ongoing
conversation, INFO argument must be structured as:

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

(cl-defun eden-request (&key prompt system-message exchanges
                             stream model temperature
                             api dir)
  "Return a request as defined in `eden-request-send'.

`:prompt' is mandatory.

If `:system-message' is missing, it is replaced by `eden-system-message'
or \"\".

If `:model' is missing, it is replaced by `eden-model'.
If the model picked is part of `eden-system-message->developer-for-models',
the role of the system message if any will be \"developer\" instead
of \"system\".

Both the prompt and the system message considered `org-mode' strings
are converted to markdown using `eden-org-to-markdown' function.

If `:temperature' is missing, it is replaced by `eden-temperature'.
If `:api' is missing, it is replaced by `eden-api'.
If `:dir' is missing, it is replaced by `eden-dir'
or a temporary directory."
  (when (null prompt)
    (error "You must provide a prompt via `:prompt' key to build a request"))
  (let* ((-model (or model eden-model))
         (-system-message
          (or system-message (cdr-safe eden-system-message) ""))
         (-messages
          `(,(when (not (string-empty-p -system-message))
               `(:role ,(if (seq-contains-p eden-system-message->developer-for-models
                                            -model)
                            "developer"
                          "system")
                 :content ,(eden-org-to-markdown -system-message)))
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
            :model ,-model
            :temperature ,(or temperature eden-temperature)
            :messages ,req-messages)
      :api ,(or api eden-api)
      :prompt ,prompt
      :system-message ,-system-message
      :exchanges ,exchanges
      :dir ,(or dir
                eden-dir
                (concat (temporary-file-directory) "eden/"))
      :uuid ,(eden-uuid))))

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
   :req (eden-request
         :prompt (eden-prompt-current-buffer)
         :exchanges (eden-conversation-exchanges eden-conversation-id))
   :info `(:conversation-id ,eden-conversation-id
           :created ,(float-time))
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
                 (eden-conversation-update info req)
                 (eden-mode-line-waiting 'maybe-stop)
                 (message "Eden received a response from %s after %.3fs.  See `%s' buffer."
                          (plist-get eden-api :service)
                          (- (float-time) (plist-get info :created))
                          buff-name))))
  (erase-buffer)
  (eden-maybe-delete-window-prompt-buffer)
  (message "Eden sent a request to %s."
           (plist-get eden-api :service)))

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

- :api             - See `eden-api'
- :dir             - See `eden-dir'
- :model           - See `eden-model'
- :temperature     - See `eden-temperature'
- :conversation-id - See `eden-conversation-id'
- :system-message  - See `eden-system-message'

See `eden-profile-push'."
  (list :api eden-api
        :dir eden-dir
        :model eden-model
        :temperature eden-temperature
        :conversation-id eden-conversation-id
        :system-message eden-system-message))

(defun eden-profile-push ()
  "Push current profile into `eden-profile-ring'.

See `eden-profile-current'."
  (ring-remove+insert+extend eden-profile-ring (eden-profile-current) 'grow))

(defun eden-profile-previous ()
  "Turn previous profile into the current one.

See `eden-profile-ring' and `eden-profile-current'."
  (interactive)
  (let ((profile-prev
         (condition-case nil
             (ring-next eden-profile-ring (eden-profile-current))
           (error
            (eden-profile-push)
            (ring-next eden-profile-ring (eden-profile-current))))))
    (setq eden-api (plist-get profile-prev :api))
    (setq eden-dir (plist-get profile-prev :dir))
    (setq eden-model (plist-get profile-prev :model))
    (setq eden-temperature (plist-get profile-prev :temperature))
    (setq eden-conversation-id (plist-get profile-prev :conversation-id))
    (setq eden-system-message (plist-get profile-prev :system-message))
    (when (= (ring-length eden-profile-ring) 1)
      (message "There's only one profile in the ring."))
    (force-mode-line-update)))

(defun eden-profile-next ()
  "Turn next profile into the current one.

See `eden-profile-ring' and `eden-profile-current'."
  (interactive)
  (let ((profile-next
         (condition-case nil
             (ring-previous eden-profile-ring (eden-profile-current))
           (error
            (eden-profile-push)
            (ring-previous eden-profile-ring (eden-profile-current))))))
    (setq eden-api (plist-get profile-next :api))
    (setq eden-dir (plist-get profile-next :dir))
    (setq eden-model (plist-get profile-next :model))
    (setq eden-temperature (plist-get profile-next :temperature))
    (setq eden-conversation-id (plist-get profile-next :conversation-id))
    (setq eden-system-message (plist-get profile-next :system-message))
    (when (= (ring-length eden-profile-ring) 1)
      (message "There's only one profile in the ring."))
    (force-mode-line-update)))

;;;; Main menu

(defun eden-show-current-conversation ()
  "Display current conversation.

See `eden-conversations' and `eden-conversation-id'."
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
  "Show conversation of current request in history.

See `eden-prompt-current-req-uuid' and `eden-prompt-history-state'."

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

(defun eden-paths-since (timestamp)
  "Return the list of request paths from `eden-dir' since TIMESTAMP.

Request paths are ordered chronologically.

See `eden-request-conversation-path' and `eden-request-timestamp'."
  (let* ((timestamp-files
          (directory-files-recursively eden-dir "timestamp-.*")))
    (thread-last
      timestamp-files
      (mapcar (lambda (f)
                (string-match ".*/\\([^/]+\\)/timestamp-\\(.*\\)" f)
                (cons (match-string 1 f)
                      (string-to-number (match-string 2 f)))))
      (seq-sort (lambda (t1 t2) (< (cdr t1) (cdr t2))))
      (mapcar (lambda (r)
                (when (<= timestamp (cdr r))
                  (let ((req `(:dir ,eden-dir
                               :uuid ,(car r))))
                    (eden-request-conversation-path req)))))
      (delq nil))))

(defun eden-last-paths (num-of-days)
  "Return the list of request paths from `eden-dir' for the last NUM-OF-DAYS days.

The range for NUM-OF-DAYS starts at 1 (indicating today), with 2
representing today and yesterday, and so on.

Request paths are ordered chronologically.

See `eden-paths-since' and `eden-request-timestamp'."
  (let* ((today (calendar-current-date))
         (midnight (encode-time `(0 0 0 ,(nth 1 today) ,(nth 0 today) ,(nth 2 today))))
         (timestamp (thread-last (days-to-time (1- num-of-days))
                                 (time-subtract midnight)
                                 (float-time)))
         (timestamp-files
          (directory-files-recursively eden-dir "timestamp-.*")))
    (eden-paths-since timestamp)))

(defun eden-last-requests (num-of-days)
  "Return the list of requests from `eden-dir' for the last NUM-OF-DAYS days.

The range for NUM-OF-DAYS starts at 1 (indicating today), with 2
representing today and yesterday, and so on.

Request are ordered chronologically (see `eden-request-timestamp')."
  (mapcar (lambda (p) (aref p (1- (length p))))
          (eden-last-paths num-of-days)))

(defun eden-last-conversations (num-of-days)
  "Return the latest requests of conversations from `eden-dir' for the last NUM-OF-DAYS days.

The range for NUM-OF-DAYS starts at 1 (indicating today), with 2
representing today and yesterday, and so on.

Latest request of conversations are ordered chronologically."
  (eden-paths-maximal (eden-last-paths num-of-days)))

(defun eden-show-last-conversations ()
  "Show last conversations from `eden-dir' for a period of time entered in the minibuffer.

Timing can be 1 (indicating today), 2 representing today and yesterday,
and so on.

Conversations are ordered chronologically.

See `eden-last-conversations'."
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
  "Show last requests from `eden-dir' for a period of time entered in the minibuffer.

Timing can be 1 (indicating today), 2 representing today and yesterday,
and so on.

Requests are ordered chronologically.

See `eden-last-request'."
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

(defun eden-show-current-configuration ()
  "Show current Configuration.

This incudes informations about `eden-dir', `eden-api', `eden-model',
`eden-temperature', `eden-system-message' and the current conversation.

See `eden-conversation-id' and `eden-conversations'."
  (interactive)
  (let ((buff (get-buffer-create (eden-buffer-name "current settings")))
        (service (plist-get eden-api :service))
        (endpoint (plist-get eden-api :endpoint))
        (model eden-model)
        (temperature (or eden-temperature ""))
        (system-message (or eden-system-message ""))
        (conversation (or (assoc eden-conversation-id eden-conversations) "")))
    (with-current-buffer buff
      (save-excursion
        (erase-buffer)
        (insert
         (format
          (concat "       service: %s\n"
                  "      endpoint: %s\n"
                  "      eden-dir: %s\n"
                  "         model: %s\n"
                  "   temperature: %s\n"
                  "  conversation: %s\n"
                  "system message: %s\n")
          service endpoint eden-dir model temperature conversation system-message))))
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

(transient-define-suffix eden-temperature-set ()
  "Set `eden-temperature' interactively."
  :transient t
  (interactive)
  (let ((temperature
         (read-string "Enter a float number [0-2] or (leave blank for none) to set model temperature: ")))
    (setq eden-temperature
          (when (not (string-empty-p temperature))
            (string-to-number temperature)))
    (message "Current temperature has been set to `%s'." eden-temperature)))

(transient-define-suffix eden-dir-set ()
  "Set `eden-dir' interactively."
  :transient t
  (interactive)
  (let ((dir (read-directory-name "Set request directory to: ")))
    (setq eden-dir (file-name-as-directory (expand-file-name dir)))
    (message "Next requests will be stored in `%s' directory." eden-dir)))

(transient-define-suffix eden-conversation-include-reasoning-toggle ()
  "Toggle `eden-conversation-include-reasoning' value."
  :transient t
  (interactive)
  (setq eden-conversation-include-reasoning (not eden-conversation-include-reasoning))
  (if eden-conversation-include-reasoning
      (message "Include reasoning information in conversations.")
    (message "Do not include reasoning information in conversations.")))

(transient-define-suffix eden-system-message-set ()
  "Set `eden-system-message' selecting from `eden-system-messages'."
  :transient t
  (interactive)
  (let ((err
         (format
          (concat "`eden-system-messages' variable must be nil or an alist like this\n\n"
                  "((\"writer\" . \"You're a good writer who only writes in Italian.\")
 (\"programmer\" . \"You're a programmer who only answers with code snippets.\"))\n\n"
                  "not `%S'")
          eden-system-messages)))
    (cond
     ((null eden-system-messages)
      (message "There's no system message to select from `eden-system-messages' variable which is nil."))
     ((not (listp eden-system-messages)) (error err))
     (t (if-let* ((system-message-titles
                   (delq nil (mapcar 'car-safe eden-system-messages))))
            (let ((title (completing-read
                          "System message title (leave blank for none): "
                          system-message-titles)))
              (setq eden-system-message (assoc title eden-system-messages))
              (message "System message `%s' selected." title))
          (error err))))))

(transient-define-suffix eden-system-message-reset ()
  "Set `eden-system-message' to nil."
  :transient t
  (interactive)
  (if eden-system-message
      (progn
        (message "Reset system message.")
        (setq eden-system-message nil))
    (message "System message is not set, no need to reset it.")))

(defun eden-system-message-update ()
  "Interactively update `eden-system-message'.

This also updates its value in `eden-system-messages' list."
  (interactive)
  (if-let ((title (car-safe eden-system-message))
           (message (cdr-safe eden-system-message)))
      (let ((_ (eden-maybe-delete-window-prompt-buffer))
            (new-message
             (read-string-from-buffer
              (format "Modifying \"%s\" system message" title)
              message)))
        (setf (cdr eden-system-message) new-message)
        (select-window
         (display-buffer-at-bottom
          (get-buffer-create eden-prompt-buffer-name)
          '(display-buffer-below-selected
            (window-height . 6))))
        (eden-menu)
        (message "System message `%s' has been updated." title))
    (message "Cannot update a system message not set.  Try instead to pick or add a system message via `eden-menu'.")
    (eden-menu)))

(defun eden-system-message-add ()
  "Interactively add a new system message to `eden-system-messages'.

This also sets `eden-system-message' with this new system message."
  (interactive)
  (let ((title (read-string "New system message title: ")))
    (if (seq-contains-p (mapcar 'car-safe eden-system-messages) title)
        (progn
          (message "Cannot use `%s' as system message title which is already taken." title)
          (eden-menu))
      (let ((_ (eden-maybe-delete-window-prompt-buffer))
            (message (read-string-from-buffer
                      (format "Enter \"%s\" system message" title) "")))
        (push (cons title message) eden-system-messages)
        (setq eden-system-message (assoc title eden-system-messages))
        (message "System message `%s' has been added." title)
        (eden-menu)))))

(defun eden-menu-quit ()
  "Quit `eden-menu'."
  (interactive)
  nil)

(transient-define-prefix eden-menu ()
  "Transient command to manage conversations, requests and Eden's settings.

- Conversations:
  - `eden-conversation-start'
  - `eden-conversation-start-from-req-history'
  - `eden-conversation-continue-from-req-history'
  - `eden-conversation-switch'
  - `eden-conversation-edit-title'
  - `eden-conversation-pause'
- Conversations and requests
  - `eden-show-current-conversation'
  - `eden-show-current-conversation-in-req-history'
  - `eden-show-last-conversations'
  - `eden-show-last-requests'
  - `eden-kill-last-request'
  - `eden-prompt-current-goto'
- Configuration
  - `eden-api-set'
  - `eden-model-set'
  - `eden-temperature-set'
  - `eden-dir-set'
  - `eden-conversation-include-reasoning-toggle'
  - `eden-show-current-configuration'
- System messages
  - `eden-system-message-add'
  - `eden-system-message-update'
  - `eden-system-message-set'
  - `eden-system-message-reset'"
  [["Conversation"
    ("n" "New conversation (cv)" eden-conversation-start)
    ("s" "Start cv from current request in history" eden-conversation-start-from-req-history)
    ("c" "Continue cv from current request in history" eden-conversation-continue-from-req-history)
    ("e" "Edit current conversation title" eden-conversation-edit-title)
    ("TAB" "Switch conversation" eden-conversation-switch)
    ("SPC" "Pause current conversation" eden-conversation-pause)]
   ["Configuration"
    ("a" "Set API" eden-api-set)
    ("m" "Set model" eden-model-set)
    ("t" "Set temperature" eden-temperature-set)
    ("d" "Set request directory" eden-dir-set)
    ("i" "Include reasoning information" eden-conversation-include-reasoning-toggle)
    ("C" "Show current configuration" eden-show-current-configuration)]]
  [["Conversations and requests"
    ("k" "Kill last request" eden-kill-last-request)
    ("v" "Show current conversation" eden-show-current-conversation)
    ("h" "Show current conversation in history" eden-show-current-conversation-in-req-history)
    ("l" "Show last conversations" eden-show-last-conversations)
    ("L" "Show last requests" eden-show-last-requests)
    ("g" "Go to current request in history" eden-prompt-current-goto)]
   ["System messages"
    ("+" "Add system message" eden-system-message-add)
    ("u" "Update system message" eden-system-message-update)
    ("p" "Pick system message" eden-system-message-set)
    ("_" "Reset system message" eden-system-message-reset)
    "..............."
    ("RET" "Quit menu" eden-menu-quit)]]
  )

;;;; Request at point menu

(defun eden-req-at-point-uuid ()
  "Return the UUID of the request at point.

To find a request a point, the point must be on an `org-mode'
heading which includes the property `eden-org-property-req'.

Signal an error either if there is no request at point, or if
the request cannot be found in `eden-dir'."
  (if-let* ((req-uuid (org-entry-get nil eden-org-property-req))
            (req-dir (eden-request-dir
                      `(:dir ,eden-dir :uuid ,req-uuid))))
      (if (file-exists-p req-dir)
          req-uuid
        (error "Request `%s' doesn't exist" req-dir))
    (error "No request at point found")))

(defun eden-req-at-point-start-conversation ()
  "Start a conversation from request at point including all previous exchanges.

See `eden-req-at-point-uuid', `eden-conversation', `eden-conversations',
and `eden-conversation-id'."
  (interactive)
  (when-let ((req-uuid (eden-req-at-point-uuid)))
    (eden-conversation
     'start-from (read-string "Enter a conversation title: ") req-uuid))
  (eden))

(defun eden-req-at-point-continue-conversation ()
  "Start a conversation from request at point excluding all previous exchanges.

See `eden-req-at-point-uuid', `eden-conversation', `eden-conversations',
and `eden-conversation-id'."
  (interactive)
  (when-let ((req-uuid (eden-req-at-point-uuid)))
    (eden-conversation
     'continue-from (read-string "Enter a conversation title: ") req-uuid))
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
          (eden-conversation-insert req "Request" nil 'start-from))))
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
               (eden-paths-branches req-uuid (eden-paths-since timestamp)))
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
    (let ((system-message (eden-request-read 'system-message req)))
      (if (string-empty-p system-message)
          (message "No system message for `%s' request." (eden-request-dir req))
        (let ((buff (get-buffer-create (eden-buffer-name "system message"))))
          (with-current-buffer buff
            (erase-buffer)
            (org-mode)
            (save-excursion (insert system-message)))
          (select-window
           (display-buffer buff '(display-buffer-reuse-window))))))))

(defun eden-req-at-point-show-perplexity-citations ()
  "Show Perplexity citations of the request at point.

See `eden-req-at-point-uuid' and `eden-request-perplexity-citations'."
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

(defun eden-req-at-point-show-reasoning ()
  "Show reasoning of the request at point.

See `eden-req-at-point-uuid' and `eden-request-assistant-reasoning'."
  (interactive)
  (when-let* ((req-uuid (eden-req-at-point-uuid))
              (req `(:dir ,eden-dir :uuid ,req-uuid)))
    (eden-request-check req)
    (if (file-exists-p (eden-request-file 'reasoning req))
        (let ((buff (get-buffer-create
                     (eden-buffer-name "reasoning of request at point"))))
          (with-current-buffer buff
            (erase-buffer)
            (org-mode)
            (insert-file-contents (eden-request-file 'reasoning req)))
          (select-window
           (display-buffer buff '(display-buffer-reuse-window))))
      (message "No reasoning for `%s' request" (eden-request-dir req)))))

(defun eden-req-at-point-goto ()
  "Go to the directory of the request at point.

See `eden-req-at-point-uuid' and `eden-request-dir'."
  (interactive)
  (when-let ((req-uuid (eden-req-at-point-uuid))
             (req-dir (eden-request-dir
                       `(:dir ,eden-dir :uuid ,req-uuid))))
    (dired req-dir)))

(transient-define-prefix eden-req-at-point-menu ()
  "Transient command to manage conversations and requests at point.

- `eden-req-at-point-start-conversation'
- `eden-req-at-point-continue-conversation'
- `eden-req-at-point-show-requests'
- `eden-req-at-point-show-branches'
- `eden-req-at-point-show-system-message'
- `eden-req-at-point-show-perplexity-citations'
- `eden-req-at-point-show-reasoning'
- `eden-req-at-point-goto'"
  [["Conversation/Request at point"
    ("s" "Start conversation from request at point" eden-req-at-point-start-conversation)
    ("c" "Continue conversation from request at point" eden-req-at-point-continue-conversation)
    ("r" "Show requests of conversation at point" eden-req-at-point-show-requests)
    ("b" "Show branches of request at point" eden-req-at-point-show-branches)
    ("S" "Show system message of request at point" eden-req-at-point-show-system-message)
    ("C" "Show Perplexity citations of conversation at point" eden-req-at-point-show-perplexity-citations)
    ("R" "Show reasoning of request at point" eden-req-at-point-show-reasoning)
    ("g" "Go to directory of request at point" eden-req-at-point-goto)
    ]])

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
`eden-request-history-set' and `eden-prompt-history-state-set'functions.

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
     (:eval (when eden-temperature (format " -%s-" eden-temperature)))
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
  (eden-request-history-set)
  (eden-prompt-history-state-set))

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
