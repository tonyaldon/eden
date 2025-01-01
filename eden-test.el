(defmacro comment (&rest body) "Ignores body and yield nil." nil)

(ert-deftest rich-ai-json-encoding/decoding-test ()
  (should
   (string=
    (rich-ai-json-encode '(:key :false))
    "{\n  \"key\": false\n}"))
  (should
   (equal (with-temp-buffer
            (save-excursion
              (insert "{\"key\":false,\"array\":[0,1,2]}"))
            (rich-ai-json-read))
          '(:key :false :array [0 1 2])))
  (should
   (equal
    (with-temp-buffer
      (save-excursion
        (insert (rich-ai-json-encode
                 '(:stream :false
                   :model "gpt-4o-mini"
                   :temperature 1
                   :messages [(:role "user" :content "foo bar baz")]))))
      (rich-ai-json-read))
    '(:stream :false
      :model "gpt-4o-mini"
      :temperature 1
      :messages [(:role "user" :content "foo bar baz")])))
  (should
   (equal
    (with-temp-buffer
      (save-excursion
        (insert "{
  \"id\": \"chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8\",
  \"object\": \"chat.completion\",
  \"created\": 1733030031,
  \"model\": \"gpt-4o-mini-2024-07-18\",
  \"choices\": [
    {
      \"index\": 0,
      \"message\": {
        \"role\": \"assistant\",
        \"content\": \"Here's a short example that illustrates the use of the terms \\\"foo,\\\" \\\"bar,\\\" and \\\"baz\\\":\\n\\n```python\\ndef foo():\\n    print(\\\"This is foo.\\\")\\n\\ndef bar():\\n    print(\\\"This is bar.\\\")\\n    foo()\\n\\ndef baz():\\n    print(\\\"This is baz.\\\")\\n    bar()\\n\\nbaz()\\n```\\n\\n### Output:\\n```\\nThis is baz.\\nThis is bar.\\nThis is foo.\\n```\\n\\nIn this example:\\n- `baz` calls `bar`.\\n- `bar` calls `foo`.\\n- The output reflects the order of the function calls. \\n\\nIn programming, \\\"foo,\\\" \\\"bar,\\\" and \\\"baz\\\" are often used as placeholder names for variables or functions when the specific name isn't important to the example.\",
        \"refusal\": null
      },
      \"logprobs\": null,
      \"finish_reason\": \"stop\"
    }
  ],
  \"usage\": {
    \"prompt_tokens\": 12,
    \"completion_tokens\": 148,
    \"total_tokens\": 160,
    \"prompt_tokens_details\": {
      \"cached_tokens\": 0,
      \"audio_tokens\": 0
    },
    \"completion_tokens_details\": {
      \"reasoning_tokens\": 0,
      \"audio_tokens\": 0,
      \"accepted_prediction_tokens\": 0,
      \"rejected_prediction_tokens\": 0
    }
  },
  \"system_fingerprint\": \"fp_0705bf87c0\"
}"))
      (rich-ai-json-read))
    '(:id "chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8"
      :object "chat.completion"
      :created 1733030031
      :model "gpt-4o-mini-2024-07-18"
      :choices [(:index 0
                 :message (:role "assistant"
                           :content "Here's a short example that illustrates the use of the terms \"foo,\" \"bar,\" and \"baz\":\n\n```python\ndef foo():\n    print(\"This is foo.\")\n\ndef bar():\n    print(\"This is bar.\")\n    foo()\n\ndef baz():\n    print(\"This is baz.\")\n    bar()\n\nbaz()\n```\n\n### Output:\n```\nThis is baz.\nThis is bar.\nThis is foo.\n```\n\nIn this example:\n- `baz` calls `bar`.\n- `bar` calls `foo`.\n- The output reflects the order of the function calls. \n\nIn programming, \"foo,\" \"bar,\" and \"baz\" are often used as placeholder names for variables or functions when the specific name isn't important to the example."
                           :refusal nil)
                 :logprobs nil
                 :finish_reason "stop")]
      :usage (:prompt_tokens 12
              :completion_tokens 148
              :total_tokens 160
              :prompt_tokens_details (:cached_tokens 0 :audio_tokens 0)
              :completion_tokens_details (:reasoning_tokens 0
                                          :audio_tokens 0
                                          :accepted_prediction_tokens 0
                                          :rejected_prediction_tokens 0))
      :system_fingerprint "fp_0705bf87c0"))))

(ert-deftest rich-ai-request-dir-test ()
  ;; signal error when one of the keys `:ai-dir' or `:uuid'
  ;; is missing or is not a string
  (should-error (rich-ai-request-dir '(:ai-dir "/tmp/ai/")))
  (should-error (rich-ai-request-dir '(:ai-dir "/tmp/ai/" :uuid 1)))
  (should-error (rich-ai-request-dir '(:uuid "foo-uuid")))
  (should-error (rich-ai-request-dir '(:uuid "foo-uuid" :ai-dir 1)))
  (should-error (rich-ai-request-dir nil))

  (should
   (string= (rich-ai-request-dir '(:ai-dir "/tmp/ai/" :uuid "foo-uuid"))
            "/tmp/ai/foo-uuid/"))
  (should
   (string= (rich-ai-request-dir '(:ai-dir "/tmp/ai" :uuid "foo-uuid"))
            "/tmp/ai/foo-uuid/")))

(ert-deftest rich-ai-request-assistant-content-test ()
  (let ((resp '(:id "chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8"
                :object "chat.completion"
                :created 1733030031
                :model "gpt-4o-mini-2024-07-18"
                :choices [(:index 0
                           :message (:role "assistant"
                                     :content "foo assistant"
                                     :refusal nil)
                           :logprobs nil
                           :finish_reason "stop")]
                :usage (:prompt_tokens 12
                        :completion_tokens 148
                        :total_tokens 160
                        :prompt_tokens_details (:cached_tokens 0 :audio_tokens 0)
                        :completion_tokens_details (:reasoning_tokens 0
                                                    :audio_tokens 0
                                                    :accepted_prediction_tokens 0
                                                    :rejected_prediction_tokens 0))
                :system_fingerprint "fp_0705bf87c0")))
    (should
     (string= (rich-ai-request-assistant-content resp) "foo assistant"))))

(ert-deftest rich-ai-request-user-content-test ()
  (let* ((request '(:stream :false
                    :model "gpt-4o-mini"
                    :temperature 1
                    :messages [(:role "user" :content "foo user")])))
    (should
     (string= (rich-ai-request-user-content request) "foo user")))

  ;; conversation with previous messages
  (let* ((request '(:stream :false
                    :model "gpt-4o-mini"
                    :temperature 1
                    :messages [(:role "system" :content "baz system")
                               (:role "user" :content "foo user")
                               (:role "assistant" :content "foo assistant")
                               (:role "user" :content "bar prompt")
                               (:role "assistant" :content "bar assistant")
                               (:role "user" :content "baz user")])))
    (should
     (string= (rich-ai-request-user-content request) "baz user"))))

(ert-deftest rich-ai-request-timestamp-test ()
  ;; `rich-ai-request-write' function uses `time-to-seconds' function
  ;; to write the timestamp file of a request so to be able to test
  ;; `rich-ai-request-timestamp', we temporary redefine `time-to-seconds'
  ;; to return a constant number similar to the one it would normally return.

  (cl-letf (((symbol-function 'time-to-seconds)
             (lambda () 1733921715.2331347)))
    (let ((req '(:ai-dir "/tmp/ai-dir/" :uuid "uuid-foo")))
      (rich-ai-request-write 'timestamp req "")
      (sleep-for 0.1)
      (should
       (equal (rich-ai-request-timestamp req) 1733921715.2331347)))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-request-date-test")))
(ert-deftest rich-ai-request-date-test ()
  ;; `rich-ai-request-write' function uses `time-to-seconds' function
  ;; to write the timestamp file of a request so to be able to test
  ;; `rich-ai-request-date', we temporary redefine `time-to-seconds'
  ;; to return a constant number similar to the one it would normally return.
  (cl-letf (((symbol-function 'time-to-seconds)
             (lambda () 1733921715.2331347)))
    (let ((req '(:ai-dir "/tmp/ai-dir/" :uuid "uuid-foo")))
      (rich-ai-request-write 'timestamp req "")
      (sleep-for 0.1)
      (should
       (equal (rich-ai-request-date req) "[2024-12-11 Wed]")))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-request-read-test")))
(ert-deftest rich-ai-request-read-test ()
  (let* ((request '(:stream :false
                    :model "gpt-4o-mini"
                    :temperature 1
                    :messages [(:role "user" :content "user prompt\n")]))
         (req `(:ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-foo")))
    (message "%s" (rich-ai-request-dir req))

    ;; json files
    (rich-ai-request-write 'request req (rich-ai-json-encode request))
    (should (equal (rich-ai-request-read 'request req) request))

    ;; non json files
    (rich-ai-request-write 'prompt req "user prompt\n")
    (should (equal (rich-ai-request-read 'prompt req) "user prompt\n"))

    ;; response.json doesn't exist
    (should-error (rich-ai-request-read 'response req))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-request-check-test")))
(ert-deftest rich-ai-request-check-test ()
  ;; Signal error if the request doesn't exist in `:ai-dir'
  (should-error
   (let* ((req `(:ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                 :uuid "uuid-foo")))
     (rich-ai-request-check req)))

  ;; Signal error when an error.json file exists in req directory
  (let* ((req `(:ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-foo")))
    (rich-ai-request-write 'prompt req "")
    (rich-ai-request-write 'request req "")
    (rich-ai-request-write 'exchanges req "")
    (rich-ai-request-write 'response req "")
    (rich-ai-request-write 'response-org req "")

    (rich-ai-request-write 'error req "")
    (should-error (rich-ai-request-check req)))

  ;; Signal error when the request in incomplete, specificaly
  ;; when the following files are missing:
  ;;
  ;; - prompt.org
  ;; - request.json
  ;; - response.json
  ;; - response.org
  ;; - exchanges.json
  (let* ((req `(:ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-foo")))
    (make-directory (rich-ai-request-dir req) 'parent)
    (should-error (rich-ai-request-check req)))

  ;; everything ok
  (let* ((req `(:ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-foo")))
    (rich-ai-request-write 'prompt req "")
    (rich-ai-request-write 'request req "")
    (rich-ai-request-write 'exchanges req "")
    (rich-ai-request-write 'response req "")
    (rich-ai-request-write 'response-org req "")
    (should (rich-ai-request-check req))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-request-conversation-test")))
(ert-deftest rich-ai-request-conversation-test ()
  ;; Signal error if the request doesn't exist in `:ai-dir'
  (should-error
   (let* ((req `(:ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                 :uuid "uuid-foo")))
     (rich-ai-request-conversation req)))

  ;; Signal error when an error.json file exists in req directory
  (let* ((req `(:ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-foo")))
    (rich-ai-request-write 'prompt req "")
    (rich-ai-request-write 'request req "")
    (rich-ai-request-write 'exchanges req "")
    (rich-ai-request-write 'response req "")
    (rich-ai-request-write 'response-org req "")

    (rich-ai-request-write 'error req "")
    (should-error (rich-ai-request-conversation req)))

  ;; Signal error when the request in incomplete, specificaly
  ;; when the following files are missing:
  ;;
  ;; - prompt.org
  ;; - request.json
  ;; - response.json
  ;; - response.org
  ;; - exchanges.json
  (let* ((req `(:ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-foo")))
    (make-directory (rich-ai-request-dir req) 'parent)
    (should-error (rich-ai-request-conversation req)))

  ;; conversation with no previous messages
  (let* ((req `(:req (:stream :false
                      :model "gpt-4o-mini"
                      :temperature 1
                      :messages [(:role "user" :content "foo user")])
                :api (:service "chatgpt"
                      :endpoint "https://api.openai.com/v1/chat/completions")
                :prompt "foo prompt\n"
                :ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-foo"))
         (ai-dir (plist-get req :ai-dir))
         (resp '(:id "chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8"
                 :object "chat.completion"
                 :created 1733030031
                 :model "gpt-4o-mini-2024-07-18"
                 :choices [(:index 0
                            :message (:role "assistant" :content "foo assistant\n" :refusal nil)
                            :logprobs nil :finish_reason "stop")]))
         (resp-str (rich-ai-json-encode resp)))
    (message "%s" (rich-ai-request-dir req))
    (rich-ai-write-request req)
    (rich-ai-write-response resp-str resp req)
    (should
     (equal
      (rich-ai-request-conversation `(:ai-dir ,ai-dir :uuid "uuid-foo"))
      [(:uuid "uuid-foo"
        :prompt "foo prompt\n"
        :user "foo user"
        :assistant "foo assistant\n"
        :response "foo assistant\n")])))

  ;; conversation with previous messages
  (let* ((req `(:req (:stream :false
                      :model "gpt-4o-mini"
                      :temperature 1
                      :messages [(:role "system" :content "baz system\n")
                                 (:role "user" :content "foo user")
                                 (:role "assistant" :content "foo assistant\n")
                                 (:role "user" :content "bar prompt")
                                 (:role "assistant" :content "bar assistant\n")
                                 (:role "user" :content "baz user")])
                :api (:service "chatgpt"
                      :endpoint "https://api.openai.com/v1/chat/completions")
                :prompt "baz user prompt\n"
                :system-prompt "baz system prompt\n"
                :exchanges [(:uuid "uuid-foo"
                             :prompt "foo prompt\n"
                             :user "foo user"
                             :assistant "foo assistant\n"
                             :response "foo assistant\n")
                            (:uuid "uuid-bar"
                             :prompt "bar prompt\n"
                             :user "bar user"
                             :assistant "bar assistant\n"
                             :response "bar assistant\n")]
                :ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-baz"))
         (ai-dir (plist-get req :ai-dir))
         (resp '(:id "chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8"
                 :object "chat.completion"
                 :created 1733030031
                 :model "gpt-4o-mini-2024-07-18"
                 :choices [(:index 0
                            :message (:role "assistant" :content "baz assistant\n" :refusal nil)
                            :logprobs nil :finish_reason "stop")]))
         (resp-str (rich-ai-json-encode resp)))
    (message "%s" (rich-ai-request-dir req))
    (rich-ai-write-request req)
    (rich-ai-write-response resp-str resp req)
    (should
     (equal
      (rich-ai-request-conversation `(:ai-dir ,ai-dir :uuid "uuid-baz"))
      [(:uuid "uuid-foo"
        :prompt "foo prompt\n"
        :user "foo user"
        :assistant "foo assistant\n"
        :response "foo assistant\n")
       (:uuid "uuid-bar"
        :prompt "bar prompt\n"
        :user "bar user"
        :assistant "bar assistant\n"
        :response "bar assistant\n")
       (:uuid "uuid-baz"
        :prompt "baz user prompt\n"
        :user "baz user"
        :assistant "baz assistant\n"
        :response "baz assistant\n")]))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-request-conversation-path-test")))
(ert-deftest rich-ai-request-conversation-path-test ()
  ;; nil if the request doesn't exist in `:ai-dir'
  (should-not
   (let* ((req `(:ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                 :uuid "uuid-foo")))
     (rich-ai-request-conversation-path req)))

  ;; nil when an error.json file exists in req directory
  (let* ((req `(:ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-foo")))
    (rich-ai-request-write 'prompt req "")
    (rich-ai-request-write 'request req "")
    (rich-ai-request-write 'exchanges req "")
    (rich-ai-request-write 'response req "")
    (rich-ai-request-write 'response-org req "")

    (rich-ai-request-write 'error req "")
    (should-not (rich-ai-request-conversation-path req)))

  ;; nil when the request in incomplete, specificaly
  ;; when the following files are missing:
  ;;
  ;; - prompt.org
  ;; - request.json
  ;; - response.json
  ;; - response.org
  ;; - exchanges.json
  (let* ((req `(:ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-foo")))
    (make-directory (rich-ai-request-dir req) 'parent)
    (should-not (rich-ai-request-conversation-path req)))

  ;; conversation with no previous messages
  (let* ((req `(:req (:stream :false
                      :model "gpt-4o-mini"
                      :temperature 1
                      :messages [(:role "user" :content "foo user")])

                :api (:service "chatgpt"
                      :endpoint "https://api.openai.com/v1/chat/completions")
                :prompt "foo prompt\n"
                :ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-foo"))
         (ai-dir (plist-get req :ai-dir))
         (resp '(:id "chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8"
                 :object "chat.completion"
                 :created 1733030031
                 :model "gpt-4o-mini-2024-07-18"
                 :choices [(:index 0
                            :message (:role "assistant" :content "foo assistant" :refusal nil)
                            :logprobs nil :finish_reason "stop")]))
         (resp-str (rich-ai-json-encode resp)))
    (rich-ai-write-request req)
    (rich-ai-write-response resp-str resp req)
    (should
     (equal
      (rich-ai-request-conversation-path `(:ai-dir ,ai-dir :uuid "uuid-foo"))
      ["uuid-foo"])))

  ;; conversation with previous messages
  (let* ((req `(:req (:stream :false
                      :model "gpt-4o-mini"
                      :temperature 1
                      :messages [(:role "system" :content "baz system")
                                 (:role "user" :content "foo user")
                                 (:role "assistant" :content "foo assistant")
                                 (:role "user" :content "bar prompt")
                                 (:role "assistant" :content "bar assistant")
                                 (:role "user" :content "baz user")])
                :api (:service "chatgpt"
                      :endpoint "https://api.openai.com/v1/chat/completions")
                :prompt "baz user prompt\n"
                :system-prompt "baz system prompt\n"
                :exchanges [(:uuid "uuid-foo"
                             :prompt "foo prompt"
                             :user "foo user"
                             :assistant "foo assistant"
                             :response "foo response")
                            (:uuid "uuid-bar"
                             :prompt "bar prompt"
                             :user "bar user"
                             :assistant "bar assistant"
                             :response "bar response")]
                :ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-baz"))
         (resp '(:id "chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8"
                 :object "chat.completion"
                 :created 1733030031
                 :model "gpt-4o-mini-2024-07-18"
                 :choices [(:index 0
                            :message (:role "assistant" :content "baz assistant" :refusal nil)
                            :logprobs nil :finish_reason "stop")]))
         (resp-str (rich-ai-json-encode resp))
         (ai-dir (plist-get req :ai-dir)))
    (rich-ai-write-request req)
    (rich-ai-write-response resp-str resp req)
    (should
     (equal
      (rich-ai-request-conversation-path `(:ai-dir ,ai-dir :uuid "uuid-baz"))
      ["uuid-foo" "uuid-bar" "uuid-baz"]))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-request-conversation-path-alist-test")))
(ert-deftest rich-ai-request-conversation-path-alist-test ()
  (should-not (rich-ai-request-conversation-path-alist nil))
  (should
   (equal
    (rich-ai-request-conversation-path-alist ["uuid-foo"])
    '(("uuid-foo" . t))))
  (should
   (equal
    (rich-ai-request-conversation-path-alist ["uuid-foo" "uuid-bar" "uuid-baz"])
    '(("uuid-foo" . (("uuid-bar" . (("uuid-baz" . t)))))))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-request-perplexity-citations-test")))
(ert-deftest rich-ai-request-perplexity-citations-test ()
  ;; Signal error if the request doesn't exist in `:ai-dir'
  (should-error
   (let* ((req `(:ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                 :uuid "uuid-foo")))
     (rich-ai-request-perplexity-citations req)))

  ;; Signal error when an error.json file exists in req directory
  (let* ((req `(:ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-foo")))
    (rich-ai-request-write 'prompt req "")
    (rich-ai-request-write 'request req "")
    (rich-ai-request-write 'exchanges req "")
    (rich-ai-request-write 'response req "")
    (rich-ai-request-write 'response-org req "")

    (rich-ai-request-write 'error req "")
    (should-error (rich-ai-request-perplexity-citations req)))

  ;; Signal error when the request in incomplete, specificaly
  ;; when the following files are missing:
  ;;
  ;; - prompt.org
  ;; - request.json
  ;; - response.json
  ;; - response.org
  ;; - exchanges.json
  (let* ((req `(:ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-foo")))
    (make-directory (rich-ai-request-dir req) 'parent)
    (should-error (rich-ai-request-perplexity-citations req)))

  ;; conversation with no previous messages
  (let* ((req `(:req (:stream :false
                      :model "llama-3.1-sonar-small-128k-online"
                      :temperature 1
                      :messages [(:role "user" :content "foo user")])
                :api (:service "perplexity"
                      :endpoint "https://api.perplexity.ai/chat/completions")
                :prompt "foo prompt\n"
                :ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-foo"))
         (ai-dir (plist-get req :ai-dir))
         (resp '(:id "022348c0-2af6-435f-8c60-01ef5b3b39dd"
                 :object "chat.completion"
                 :created 1735366499
                 :model "llama-3.1-sonar-small-128k-online"
                 :citations ["https://foo-1.com"
                             "https://foo-2.com"
                             "https://foo-3.com"]
                 :choices [(:index 0
                            :message (:role "assistant"
                                      :content "foo assistant[1][3]")
                            :finish_reason "stop")]))
         (resp-str (rich-ai-json-encode resp)))
    (message "%s" (rich-ai-request-dir req))
    (rich-ai-write-request req)
    (rich-ai-write-response resp-str resp req)
    (should
     (equal
      (rich-ai-request-perplexity-citations `(:ai-dir ,ai-dir :uuid "uuid-foo"))
      '("https://foo-1.com" "https://foo-2.com" "https://foo-3.com"))))
  ;; 1) `bar-req' request has no citations in its response
  ;; 2) `err-req' request has no response.json file, this can happen if
  ;;    it is removed inadvertently
  (let* ((ai-dir (concat (make-temp-file "rich-ai-" t) "/"))
         (foo-req `(:req (:stream :false
                          :model "llama-3.1-sonar-small-128k-online"
                          :temperature 1
                          :messages [(:role "user" :content "foo user")])
                    :api (:service "perplexity"
                          :endpoint "https://api.perplexity.ai/chat/completions")
                    :prompt "foo prompt\n"
                    :ai-dir ,ai-dir
                    :uuid "uuid-foo"))
         (foo-resp '(:id "022348c0-2af6-435f-8c60-01ef5b3b39dd"
                     :object "chat.completion"
                     :created 1735366499
                     :model "llama-3.1-sonar-small-128k-online"
                     :citations ["https://foo-1.com"
                                 "https://foo-2.com"
                                 "https://foo-3.com"]
                     :choices [(:index 0
                                :message (:role "assistant"
                                          :content "foo assistant[1][3]")
                                :finish_reason "stop")]))
         (foo-resp-str (rich-ai-json-encode foo-resp))
         (bar-req `(:req (:stream :false
                          :model "llama-3.1-sonar-small-128k-online"
                          :temperature 1
                          :messages [(:role "user" :content "foo user")
                                     (:role "assistant" :content "foo assistant[1][3]")
                                     (:role "user" :content "bar user")])
                    :exchanges [(:uuid "uuid-foo"
                                 :prompt "foo prompt"
                                 :user "foo user"
                                 :assistant "foo assistant[1][3]"
                                 :response "foo response[1][3]")]
                    :api (:service "perplexity"
                          :endpoint "https://api.perplexity.ai/chat/completions")
                    :prompt "bar prompt\n"
                    :ai-dir ,ai-dir
                    :uuid "uuid-bar"))
         ;; bar response with no citations
         (bar-resp '(:id "022348c0-2af6-435f-8c60-01ef5b3b39dd"
                     :object "chat.completion"
                     :created 1735366499
                     :model "llama-3.1-sonar-small-128k-online"
                     :choices [(:index 0
                                :message (:role "assistant"
                                          :content "bar assistant")
                                :finish_reason "stop")]))
         (bar-resp-str (rich-ai-json-encode bar-resp))
         (err-req `(:req (:stream :false
                          :model "llama-3.1-sonar-small-128k-online"
                          :temperature 1
                          :messages [(:role "user" :content "foo user")
                                     (:role "assistant" :content "foo assistant[1][3]")
                                     (:role "user" :content "bar user")
                                     (:role "assistant" :content "bar assistant")
                                     (:role "user" :content "err user")])
                    :api (:service "perplexity"
                          :endpoint "https://api.perplexity.ai/chat/completions")
                    :prompt "err prompt\n"
                    :exchanges [(:uuid "uuid-foo"
                                 :prompt "foo prompt"
                                 :user "foo user"
                                 :assistant "foo assistant[1][3]"
                                 :response "foo response[1][3]")
                                (:uuid "uuid-bar"
                                 :prompt "bar prompt"
                                 :user "bar user"
                                 :assistant "bar assistant"
                                 :response "bar response")]
                    :ai-dir ,ai-dir
                    :uuid "uuid-err"))
         (baz-req `(:req (:stream :false
                          :model "llama-3.1-sonar-small-128k-online"
                          :temperature 1
                          :messages [(:role "system" :content "baz system")
                                     (:role "user" :content "foo user")
                                     (:role "assistant" :content "foo assistant[1][3]")
                                     (:role "user" :content "bar user")
                                     (:role "assistant" :content "bar assistant")
                                     (:role "user" :content "err user")
                                     (:role "assistant" :content "err assistant")
                                     (:role "user" :content "baz user")])
                    :api (:service "perplexity"
                          :endpoint "https://api.perplexity.ai/chat/completions")
                    :prompt "baz user prompt\n"
                    :system-prompt "baz system prompt\n"
                    :exchanges [(:uuid "uuid-foo"
                                 :prompt "foo prompt"
                                 :user "foo user"
                                 :assistant "foo assistant[1][3]"
                                 :response "foo response[1][3]")
                                (:uuid "uuid-bar"
                                 :prompt "bar prompt"
                                 :user "bar user"
                                 :assistant "bar assistant"
                                 :response "bar response")
                                (:uuid "uuid-err"
                                 :prompt "err prompt"
                                 :user "err user"
                                 :assistant "err assistant"
                                 :response "err response")]
                    :ai-dir ,ai-dir
                    :uuid "uuid-baz"))
         (baz-resp '(:id "022348c0-2af6-435f-8c60-01ef5b3b39dd"
                     :object "chat.completion"
                     :created 1735366499
                     :model "llama-3.1-sonar-small-128k-online"
                     :citations ["https://baz-1.com" "https://baz-2.com"]
                     :choices [(:index 0
                                :message (:role "assistant"
                                          :content "baz assistant[1][2]")
                                :finish_reason "stop")]))
         (baz-resp-str (rich-ai-json-encode baz-resp)))
    (message "%s" (rich-ai-request-dir baz-req))
    (rich-ai-write-request foo-req)
    (rich-ai-write-response foo-resp-str foo-resp `(:ai-dir ,ai-dir :uuid "uuid-foo"))
    (rich-ai-write-request bar-req)
    (rich-ai-write-response bar-resp-str bar-resp `(:ai-dir ,ai-dir :uuid "uuid-bar"))
    (rich-ai-write-request err-req) ;; we don't write any response for `err-req'
    (rich-ai-write-request baz-req)
    (rich-ai-write-response baz-resp-str baz-resp baz-req)
    (should
     (equal
      (rich-ai-request-perplexity-citations `(:ai-dir ,ai-dir :uuid "uuid-baz"))
      '("https://foo-1.com"
        "https://foo-2.com"
        "https://foo-3.com"
        "https://baz-1.com"
        "https://baz-2.com")))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-write-request-test")))
(ert-deftest rich-ai-write-request-test ()
  (let* ((req `(:req (:stream :false
                      :model "gpt-4o-mini"
                      :temperature 1
                      :messages [(:role "user" :content "user prompt\n")])
                :api (:service "chatgpt"
                      :endpoint "https://api.openai.com/v1/chat/completions")
                :prompt "user prompt\n"
                :ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-foo")))
    (message "%s" (rich-ai-request-dir req))
    (rich-ai-write-request req)
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (rich-ai-request-file 'request req))
        (rich-ai-json-read))
      (plist-get req :req)))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (rich-ai-request-file 'api req))
        (rich-ai-json-read))
      (plist-get req :api)))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (rich-ai-request-file 'prompt req))
        (buffer-substring-no-properties (point-min) (point-max)))
      (plist-get req :prompt)))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (rich-ai-request-file 'system-prompt req))
        (buffer-substring-no-properties (point-min) (point-max)))
      ""))
    (should-not
     (with-temp-buffer
       (insert-file-contents (rich-ai-request-file 'exchanges req))
       (rich-ai-json-read)))
    (should
     (= (length (directory-files (rich-ai-request-dir req) nil "timestamp-"))
        1)))
  (let* ((req `(:req (:stream :false
                      :model "gpt-4o-mini"
                      :temperature 1
                      :messages [(:role "system" :content "system prompt\n")
                                 (:role "user" :content "bar user")
                                 (:role "assistant" :content "bar assistant")
                                 (:role "user" :content "baz prompt")
                                 (:role "assistant" :content "baz assistant")
                                 (:role "user" :content "user prompt\n")])
                :api (:service "chatgpt"
                      :endpoint "https://api.openai.com/v1/chat/completions")
                :prompt "user prompt\n"
                :system-prompt "system prompt\n"
                :exchanges [(:uuid "uuid-bar"
                             :prompt "bar prompt"
                             :user "bar user"
                             :assistant "bar assistant"
                             :response "bar response")
                            (:uuid "uuid-baz"
                             :prompt "baz prompt"
                             :user "baz user"
                             :assistant "baz assistant"
                             :response "baz response")]
                :ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-foo")))
    (message "%s" (rich-ai-request-dir req))
    (rich-ai-write-request req)
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (rich-ai-request-file 'request req))
        (rich-ai-json-read))
      (plist-get req :req)))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (rich-ai-request-file 'api req))
        (rich-ai-json-read))
      (plist-get req :api)))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (rich-ai-request-file 'prompt req))
        (buffer-substring-no-properties (point-min) (point-max)))
      (plist-get req :prompt)))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (rich-ai-request-file 'system-prompt req))
        (buffer-substring-no-properties (point-min) (point-max)))
      (plist-get req :system-prompt)))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (rich-ai-request-file 'exchanges req))
        (rich-ai-json-read))
      (plist-get req :exchanges)))
    (should
     (= (length (directory-files (rich-ai-request-dir req) nil "timestamp-"))
        1))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-org-replace-perplexity-citations-test")))
(ert-deftest rich-ai-org-replace-perplexity-citations-test ()
  (let ((org-str "Some content with a citation[1]. More citations [1][2].

We access an element in a python array ~arr[1][5]~ in inline verbatim or
=arr[1][5]= in an inline code.

Brackets with characters different from numbers like this [foo bar] or
this [foo-bar] are not citations.

- Another citation [3].
- Another citation[1][2][4].

[0] and [8] seem to be a valid citations but they are out of range of
the citation array we are using to replace citations.  Citation from
perplexity.ai starts at 1 in the response they return.

We access elements in a python array in a code block:

#+BEGIN_SRC python
arr[1][2]
arr[0]
#+END_SRC
")
        (citations ["https://foo.com/bar/baz.html"
                    "https://foo.bar.baz.ai/news"
                    "https://bar.org/today/1/2"
                    "https://baz.org"]))

    (should
     (string=
      (rich-ai-org-replace-perplexity-citations org-str citations)
      "Some content with a citation[[[https://foo.com/bar/baz.html][1]]]. More citations [[[https://foo.com/bar/baz.html][1]]][[[https://foo.bar.baz.ai/news][2]]].

We access an element in a python array ~arr[1][5]~ in inline verbatim or
=arr[1][5]= in an inline code.

Brackets with characters different from numbers like this [foo bar] or
this [foo-bar] are not citations.

- Another citation [[[https://bar.org/today/1/2][3]]].
- Another citation[[[https://foo.com/bar/baz.html][1]]][[[https://foo.bar.baz.ai/news][2]]][[[https://baz.org][4]]].

[0] and [8] seem to be a valid citations but they are out of range of
the citation array we are using to replace citations.  Citation from
perplexity.ai starts at 1 in the response they return.

We access elements in a python array in a code block:

#+BEGIN_SRC python
arr[1][2]
arr[0]
#+END_SRC
"))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-write-response-test")))
(ert-deftest rich-ai-write-response-test ()
  (let* ((req `(:ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-foo"))
         (resp '(:id "chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8"
                 :object "chat.completion"
                 :created 1733030031
                 :model "gpt-4o-mini-2024-07-18"
                 :choices [(:index 0
                            :message (:role "assistant" :content "### foo assistant\n" :refusal nil)
                            :logprobs nil :finish_reason "stop")]))
         (resp-str (rich-ai-json-encode resp)))
    (rich-ai-write-response resp-str resp req)
    (should (equal (rich-ai-request-read 'response req) resp))
    (should (equal (rich-ai-request-read 'response-org req) "*** foo assistant\n")))
  ;; response from perplexity.ai with citations array
  (let* ((req `(:ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-foo"))
         (resp '(:id "022348c0-2af6-435f-8c60-01ef5b3b39dd"
                 :object "chat.completion"
                 :created 1735366499
                 :model "llama-3.1-sonar-small-128k-online"
                 :citations ["https://foo.com"
                             "https://bar.com"
                             "https://baz.com"]
                 :choices [(:index 0
                            :message (:role "assistant"
                                      :content "### foo assistant\n\nfoo citation[1]\n\nbar baz citations[2][3]\n")
                            :finish_reason "stop")]))
         (resp-str (rich-ai-json-encode resp)))
    (rich-ai-write-response resp-str resp req)
    (should (equal (rich-ai-request-read 'response req) resp))
    (should (equal (rich-ai-request-read 'response-org req)
                   "*** foo assistant\n\nfoo citation[[[https://foo.com][1]]]\n\nbar baz citations[[[https://bar.com][2]]][[[https://baz.com][3]]]\n"))))

(defun rich-test-echo-resp (resp-str buffer-or-name sentinel &optional sleep)
  "..."
  (let ((command (format "%secho %s"
                         (if sleep (format "sleep %s | " sleep) "")
                         (shell-quote-argument resp-str))))
    (make-process
     :name "echo-resp"
     :buffer buffer-or-name
     :command (list "sh" "-c" command)
     :connection-type 'pipe
     :sentinel sentinel)))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-sentinel-test")))
(ert-deftest rich-ai-sentinel-test ()
  ;; we let-bind requests, callbacks and infos with the variables
  ;; `-req', `-callback' and `-info' to be sure that `rich-ai-sentinel'
  ;; macro doesn't rely on `req', `callback' and `info' (its argument names)
  ;; variables to be defined during its expansion.

  ;; Throw error when process buffer is killed
  (let* ((resp-str "")
         (-req `(:req (:foo "bar") ;; we don't use that request
                 :api (:service "openai"
                       :endpoint "https://api.openai.com/v1/chat/completions")
                 :ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                 :uuid "uuid-foo"))
         (-callback nil) ;; we don't call it
         (-callback-error nil) ;; we don't call it
         (-info nil) ;; we don't use it
         ;; we are testing that we throw an error if process has
         ;; been killed for some reason.  We wrapped the sentinel
         ;; generated by `rich-ai-sentinel' in order to kill
         ;; the process buffer when the sentinel is called.
         (sentinel-wrapped
          (lambda (process event)
            (kill-buffer (process-buffer process))
            (funcall
             (rich-ai-sentinel -req -callback -callback-error -info)
             process event)))
         (buff-name (generate-new-buffer-name "*ai-test*")))
    (should-error
     (let ((debug-on-error t))
       (rich-test-echo-resp resp-str buff-name sentinel-wrapped)
       (sleep-for 0.2)))
    (should-not (get-buffer buff-name))
    (message "%s" (rich-ai-request-file 'error -req))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (rich-ai-request-file 'error -req))
        (rich-ai-json-read))
      `(:type "rich-ai-error-process-buffer"
        :message "The process buffer got killed while processing the request"
        :directory ,(rich-ai-request-dir -req)
        :request ,(plist-get -req :req)))))


  ;; Throw error when process receives an event different from "finished\n"
  ;; Here we test it with "killed\n" event
  (let* ((resp-str "")
         (-req `(:req (:foo "bar") ;; we don't use that request
                 :api (:service "openai"
                       :endpoint "https://api.openai.com/v1/chat/completions")
                 :ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                 :uuid "uuid-foo"))
         (-callback nil)      ;; we don't call it
         (-callback-error nil) ;; we don't call it
         (-info nil) ;; we don't use it
         (buff-name (generate-new-buffer-name "*ai-test*")))
    ;; We insert text in `buff-name' to test that we log its content
    (with-current-buffer (get-buffer-create buff-name)
      (insert "foo bar baz"))
    (should-error
     (let ((debug-on-error t))
       (kill-process
        (rich-test-echo-resp
         resp-str buff-name
         (rich-ai-sentinel -req -callback -callback-error -info)
         1))
       (sleep-for 0.2)))
    (should-not (get-buffer buff-name))
    (message "%s" (rich-ai-request-file 'error -req))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (rich-ai-request-file 'error -req))
        (rich-ai-json-read))
      `(:type "rich-ai-error-process"
        :message "The process did not finished correctly"
        :directory ,(rich-ai-request-dir -req)
        :request ,(plist-get -req :req)
        :process-event "killed\n"
        :process-buffer-content "foo bar baz"))))

  ;; Here we test it with "interrupt\n" event
  (let* ((resp-str "")
         (-req `(:req (:foo "bar") ;; we don't use that request
                 :api (:service "openai"
                       :endpoint "https://api.openai.com/v1/chat/completions")
                 :ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                 :uuid "uuid-foo"))
         ;; we don't use that request
         (-callback nil)      ;; we don't call it
         (-callback-error nil) ;; we don't call it
         (-info nil) ;; we don't use it
         (buff-name (generate-new-buffer-name "*ai-test*")))
    ;; We insert text in `buff-name' to test that we log its content
    (with-current-buffer (get-buffer-create buff-name)
      (insert "foo bar baz"))
    (should-error
     (let ((debug-on-error t))
       (interrupt-process
        (rich-test-echo-resp
         resp-str buff-name
         (rich-ai-sentinel -req -callback -callback-error -info)
         1))
       (sleep-for 0.2)))
    (should-not (get-buffer buff-name))
    (message "%s" (rich-ai-request-file 'error -req))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (rich-ai-request-file 'error -req))
        (rich-ai-json-read))
      `(:type "rich-ai-error-process"
        :message "The process did not finished correctly"
        :directory ,(rich-ai-request-dir -req)
        :request ,(plist-get -req :req)
        :process-event "interrupt\n"
        :process-buffer-content "foo bar baz"))))

  ;; Throw error when reading json response in process buffer
  (let* ((resp-str "not valid json")
         (-req `(:req (:foo "bar") ;; we don't use that request
                 :api (:service "openai"
                       :endpoint "https://api.openai.com/v1/chat/completions")
                 :ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                 :uuid "uuid-foo"))
         (-callback nil)      ;; we don't call it
         (-callback-error nil) ;; we don't call it
         (-info nil) ;; we don't use it
         (buff-name (generate-new-buffer-name "*ai-test*")))
    (should-error
     (let ((debug-on-error t))
       (rich-test-echo-resp
        resp-str buff-name
        (rich-ai-sentinel -req -callback -callback-error -info))
       (sleep-for 0.2)))
    (should-not (get-buffer buff-name))
    (message "%s" (rich-ai-request-file 'error -req))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (rich-ai-request-file 'error -req))
        (rich-ai-json-read))
      `(:type "rich-ai-error-json-read"
        :message "Error while parsing JSON in process buffer"
        :directory ,(rich-ai-request-dir -req)
        :request ,(plist-get -req :req)
        :error ["json-unknown-keyword" "not"]
        :process-buffer-content "not valid json\n"))))


  ;; error in response from openai
  (let* ((resp-str "{
  \"error\":
  {
    \"message\": \"Incorrect API key provided: eesk-pro***WmEA. You can find your API key at https://platform.openai.com/account/api-keys.\",
    \"type\": \"invalid_request_error\",
    \"param\": null,
    \"code\": \"invalid_api_key\"
  }
}")
         (-req `(:req (:foo "bar") ;; we don't use that request
                 :api (:service "openai"
                       :endpoint "https://api.openai.com/v1/chat/completions")
                 :ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                 :uuid "uuid-foo"))
         (-callback nil)      ;; we don't call it
         (-callback-error nil) ;; we don't call it
         (-info nil) ;; we don't use it
         (buff-name (generate-new-buffer-name "*ai-test*")))
    (should-error
     (let ((debug-on-error t))
       (rich-test-echo-resp
        resp-str buff-name
        (rich-ai-sentinel -req -callback -callback-error -info))
       (sleep-for 0.2)))
    (should-not (get-buffer buff-name))
    (message "%s" (rich-ai-request-file 'error -req))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (rich-ai-request-file 'error -req))
        (rich-ai-json-read))
      `(:type "rich-ai-error-api"
        :message "API error"
        :directory ,(rich-ai-request-dir -req)
        :request ,(plist-get -req :req)
        :error (:message "Incorrect API key provided: eesk-pro***WmEA. You can find your API key at https://platform.openai.com/account/api-keys."
                :type "invalid_request_error"
                :param nil
                :code "invalid_api_key")))))


  ;; Wrong callback function
  (let* ((resp-str "{
    \"id\": \"chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8\",
    \"object\": \"chat.completion\",
    \"created\": 1733030031,
    \"model\": \"gpt-4o-mini-2024-07-18\",
    \"choices\": [
      {
        \"index\": 0,
        \"message\": {
          \"role\": \"assistant\",
          \"content\": \"foo bar baz\",
          \"refusal\": null
        },
        \"logprobs\": null,
        \"finish_reason\": \"stop\"
      }
    ]
  }")
         (-req `(:req (:foo "bar") ;; we don't use that request
                 :api (:service "openai"
                       :endpoint "https://api.openai.com/v1/chat/completions")
                 :ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                 :uuid "uuid-foo"))
         (-callback 'callback-not-a-function)
         (-callback-error nil) ;; we don't call it
         (-info nil) ;; we don't use it
         (buff-name (generate-new-buffer-name "*ai-test*")))
    (should-error
     (let ((debug-on-error t))
       (rich-test-echo-resp
        resp-str buff-name
        (rich-ai-sentinel -req -callback -callback-error -info))
       (sleep-for 0.2)))
    (should-not (get-buffer buff-name))
    (message "%s" (rich-ai-request-file 'error -req))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (rich-ai-request-file 'error -req))
        (rich-ai-json-read))
      `(:type "rich-ai-error-callback"
        :message "Error while calling callback function in sentinel"
        :directory ,(rich-ai-request-dir -req)
        :request ,(plist-get -req :req)
        :error ["void-function" "callback-not-a-function"]))))


  ;; Test that `callback-error' is called when an error occurs
  ;; in the sentinel call.  To do so we send the "killed\n" event
  ;; to the process which triggers an error.
  ;;
  ;; 1) First we test that we throw a 'rich-ai-error-callback-error error
  ;;    when `callback-error' itself throws an error
  (let* ((resp-str "")
         (-req `(:req (:foo "bar") ;; we don't use that request
                 :api (:service "openai"
                       :endpoint "https://api.openai.com/v1/chat/completions")
                 :ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                 :uuid "uuid-foo"))
         (-callback nil)      ;; we don't call it
         ;; Wrong callback-error function
         (-callback-error 'callback-error-not-a-function)
         (-info nil) ;; we don't use it
         (buff-name (generate-new-buffer-name "*ai-test*")))
    (should-error
     (let ((debug-on-error t))
       (kill-process
        (rich-test-echo-resp
         resp-str buff-name
         (rich-ai-sentinel -req -callback -callback-error -info)
         1))
       (sleep-for 0.2)))
    (let* ((error.json
            (with-temp-buffer
              (insert-file-contents (rich-ai-request-file 'error -req))
              (rich-ai-json-read)))
           (original-error (plist-get error.json :original-error)))
      (should (string= (plist-get original-error :type)
                       "rich-ai-error-process"))
      (should
       (equal
        (seq-subseq error.json 0 10)
        `(:type "rich-ai-error-callback-error"
          :message "Error while calling callback-error function when signaling an error in sentinel"
          :directory ,(rich-ai-request-dir -req)
          :request ,(plist-get -req :req)
          :error ["void-function" "callback-error-not-a-function"])))))
  ;; 2) Then we test that `callback-error' is called correctly
  (let* ((resp-str "")
         (-req `(:req (:foo "bar") ;; we don't use that request
                 :api (:service "openai"
                       :endpoint "https://api.openai.com/v1/chat/completions")
                 :ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                 :uuid "uuid-foo"))
         (-callback nil)      ;; we don't call it
         in-callback-error
         (-callback-error (lambda (req err info)
                            (setq in-callback-error
                                  (list :req req
                                        :error-type (plist-get err :type)
                                        :info info))))
         (-info '(:foo "bar")) ;; we don't use it
         (buff-name (generate-new-buffer-name "*ai-test*")))
    (should-error
     (let ((debug-on-error t))
       (kill-process
        (rich-test-echo-resp
         resp-str buff-name
         (rich-ai-sentinel -req -callback -callback-error -info)
         1))
       (sleep-for 0.2)))
    (should
     (string=
      (thread-first
        (with-temp-buffer
          (insert-file-contents (rich-ai-request-file 'error -req))
          (rich-ai-json-read))
        (plist-get :type))
      "rich-ai-error-process"))
    (should
     (equal in-callback-error
            (list :req -req
                  :error-type "rich-ai-error-process"
                  :info -info))))


  ;; everything's ok
  (let* ((resp-str "{
    \"id\": \"chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8\",
    \"object\": \"chat.completion\",
    \"created\": 1733030031,
    \"model\": \"gpt-4o-mini-2024-07-18\",
    \"choices\": [
      {
        \"index\": 0,
        \"message\": {
          \"role\": \"assistant\",
          \"content\": \"foo bar baz\",
          \"refusal\": null
        },
        \"logprobs\": null,
        \"finish_reason\": \"stop\"
      }
    ]
  }")
         (resp '(:id "chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8"
                 :object "chat.completion"
                 :created 1733030031
                 :model "gpt-4o-mini-2024-07-18"
                 :choices [(:index 0
                            :message (:role "assistant" :content "foo bar baz" :refusal nil)
                            :logprobs nil
                            :finish_reason "stop")]))
         (-req `(:req (:stream :false
                       :model "gpt-4o-mini"
                       :temperature 1
                       :messages [(:role "user" :content "foo bar baz")])
                 :api (:service "openai"
                       :endpoint "https://api.openai.com/v1/chat/completions")
                 :ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                 :uuid "uuid-foo"))
         (in-callback nil)
         (-callback (lambda (req resp info)
                      (setq in-callback (list :req req :resp resp :info info))))
         (-callback-error nil) ;; we don't call it
         (-info '(:foo "bar"))
         (buff-name (generate-new-buffer-name "*ai-test*")))
    (let ((debug-on-error t))
      (rich-test-echo-resp
       resp-str buff-name
       (rich-ai-sentinel -req -callback -callback-error -info))
      (sleep-for 0.2))
    (should-not (get-buffer buff-name))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (rich-ai-request-file 'response -req))
        (rich-ai-json-read))
      resp))
    (should
     (equal in-callback `(:req ,-req :resp ,resp :info ,-info)))))

;; Because we need it to be dynamic and declared before we
;; let-bind it and use it in `rich-ai-request-command'.
;; Normally this variable is defined in `rich-ai-request-command'
;; the first time we call it and ask the user to enter his
;; gpg passphrase to get the api key from ~/.authinfo.gpg file.
;; See `rich-ai-api-key-symbol'.
(defvar rich-ai-api-key-someservice-name)

(ert-deftest rich-ai-request-command-test ()
  (let* ((rich-ai-api-key-someservice-name "secret-api-key")
         (req '(:api (:service "someservice-name"
                      :endpoint "https://someservice-endpoint")
                :ai-dir "/tmp/ai-dir/"
                :uuid "uuid-foo"))
         (command-fmt (concat "curl -s -X POST https://someservice-endpoint "
                              "-H 'Authorization: Bearer %s' "
                              "-H 'Content-Type: application/json' -d @%s"))
         (request-file (rich-ai-request-file 'request req)))
    (should
     (equal (rich-ai-request-command req)
            (list
             (format command-fmt rich-ai-api-key-someservice-name request-file)
             (format command-fmt "<api-key>" request-file))))))

(ert-deftest rich-ai-history-requests-set-test ()

  (let* (rich-ai-history-requests
         (rich-ai-dir (concat (make-temp-file "rich-ai-" t) "/"))
         (req-0 (rich-ai-request :prompt "req-0"))
         (req-1 (rich-ai-request :prompt "req-1"))
         (req-2 (rich-ai-request :prompt "req-2"))
         (req-3 (rich-ai-request :prompt "req-3"))
         (uuid-0 (plist-get req-0 :uuid))
         (uuid-1 (plist-get req-1 :uuid))
         (uuid-2 (plist-get req-2 :uuid))
         (uuid-3 (plist-get req-3 :uuid)))
    (message "%s" rich-ai-dir)
    (rich-ai-write-request req-0)
    (rich-ai-write-request req-1)
    (rich-ai-write-request req-2)
    (rich-ai-write-request req-3)
    (rich-ai-history-requests-set)
    (should
     (equal rich-ai-history-requests (list uuid-3 uuid-2 uuid-1 uuid-0)))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-org-to-markdown-test")))
(ert-deftest rich-ai-org-to-markdown-test ()
  (should
   (string=
    (rich-ai-org-to-markdown "* h1

#+BEGIN_SRC emacs-lisp
(+ 1 2)
#+END_SRC

#+BEGIN_EXAMPLE
foo bar baz
#+END_EXAMPLE

** h2")
    "# h1

```emacs-lisp
(+ 1 2)

```

```
foo bar baz

```


## h2"))

  )

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-request-test")))
(ert-deftest rich-ai-request-test ()
  ;; Test :uuid
  (should
   (stringp
    (plist-get (rich-ai-request :prompt "foo prompt")
               :uuid)))

  ;; Test :ai-dir
  (should
   (string=
    (let ((rich-ai-dir nil))
      (plist-get (rich-ai-request :prompt "foo prompt")
                 :ai-dir))
    (concat (temporary-file-directory) "rich-ai/")))
  (should
   (string=
    (plist-get
     (rich-ai-request :prompt "foo prompt"
                      :ai-dir "/tmp/foo/")
     :ai-dir)
    "/tmp/foo/"))
  (should
   (string=
    (let ((rich-ai-dir "/tmp/bar/"))
      (plist-get (rich-ai-request :prompt "foo prompt")
                 :ai-dir))
    "/tmp/bar/"))

  ;; Check :prompt, :system-prompt, :exchanges
  ;;
  ;; :prompt argument is mandatory
  (should-error (rich-ai-request))
  ;; :prompt and :system-prompt
  (let* ((rich-ai-system-prompt nil)
         (req (rich-ai-request :prompt "foo prompt"))
         (req-messages (plist-get (plist-get req :req) :messages)))
    (should (string= (plist-get req :prompt) "foo prompt"))
    (should (string= (plist-get req :system-prompt) ""))
    (should (equal req-messages [(:role "user" :content "foo prompt")])))
  (let* ((req (rich-ai-request :prompt "foo prompt"
                               :system-prompt "foo system"))
         (req-messages (plist-get (plist-get req :req) :messages)))
    (should (string= (plist-get req :system-prompt) "foo system"))
    (should (equal req-messages
                   [(:role "system" :content "foo system")
                    (:role "user" :content "foo prompt")])))
  ;; :prompt, :system-prompt and :exchanges
  (let* ((exchanges [(:uuid "uuid-foo"
                      :prompt "foo prompt"
                      :user "foo user"
                      :assistant "foo assistant"
                      :response "foo response")
                     (:uuid "uuid-bar"
                      :prompt "bar prompt"
                      :user "bar user"
                      :assistant "bar assistant"
                      :response "bar response")])
         (req (rich-ai-request
               :prompt "baz prompt"
               :system-prompt "baz system"
               :exchanges exchanges))
         (req-messages (plist-get (plist-get req :req) :messages)))
    (should (string= (plist-get req :system-prompt) "baz system"))
    (should (equal (plist-get req :exchanges) exchanges))
    (should (equal req-messages
                   [(:role "system" :content "baz system")
                    (:role "user" :content "foo user")
                    (:role "assistant" :content "foo assistant")
                    (:role "user" :content "bar user")
                    (:role "assistant" :content "bar assistant")
                    (:role "user" :content "baz prompt")])))
  ;; :prompt and :system-prompt
  ;; both converted from org-mode to markdown
  (let* ((rich-ai-system-prompt nil)
         (req (rich-ai-request :prompt "* prompt h1\n** prompt h2"))
         (req-messages (plist-get (plist-get req :req) :messages)))
    (should (string= (plist-get req :prompt) "* prompt h1\n** prompt h2"))
    (should (string= (plist-get req :system-prompt) ""))
    (should (equal req-messages
                   [(:role "user" :content "# prompt h1\n\n\n## prompt h2")])))
  (let* ((req (rich-ai-request :prompt "* prompt h1\n** prompt h2"
                               :system-prompt "* system h1\n** system h2"))
         (req-messages (plist-get (plist-get req :req) :messages)))
    (should (string= (plist-get req :prompt) "* prompt h1\n** prompt h2"))
    (should (string= (plist-get req :system-prompt) "* system h1\n** system h2"))
    (should (equal req-messages
                   [(:role "system" :content "# system h1\n\n\n## system h2")
                    (:role "user" :content "# prompt h1\n\n\n## prompt h2")])))
  (let* ((exchanges [(:uuid "uuid-foo"
                      :prompt "foo prompt"
                      :user "foo user"
                      :assistant "foo assistant")
                     (:uuid "uuid-bar"
                      :prompt "bar prompt"
                      :user "bar user"
                      :assistant "bar assistant")])
         (req (rich-ai-request
               :prompt "* prompt h1\n** prompt h2"
               :system-prompt "* system h1\n** system h2"
               :exchanges exchanges))
         (req-messages (plist-get (plist-get req :req) :messages)))
    (should (equal req-messages
                   [(:role "system" :content "# system h1\n\n\n## system h2")
                    (:role "user" :content "foo user")
                    (:role "assistant" :content "foo assistant")
                    (:role "user" :content "bar user")
                    (:role "assistant" :content "bar assistant")
                    (:role "user" :content "# prompt h1\n\n\n## prompt h2")])))
  ;; :system-prompt and default `rich-ai-system-prompt'
  (should
   (string=
    (let ((rich-ai-system-prompt nil))
      (plist-get (rich-ai-request :prompt "foo prompt")
                 :system-prompt))
    ""))
  (should
   (string=
    (let ((rich-ai-system-prompt
           '("bar system prompt title" . "bar system prompt") ))
      (plist-get
       (rich-ai-request :prompt "foo prompt"
                        :system-prompt "foo system prompt")
       :system-prompt))
    "foo system prompt"))
  (should
   (string=
    (let ((rich-ai-system-prompt
           '("bar system prompt title" . "bar system prompt") ))
      (plist-get (rich-ai-request :prompt "foo prompt")
                 :system-prompt))
    "bar system prompt"))


  ;; Test :stream
  (let ((req (rich-ai-request :prompt "foo prompt")))
    (should (equal
             (plist-get (plist-get req :req) :stream)
             :false)))
  (let ((req (rich-ai-request :prompt "foo prompt"
                              :stream t)))
    (should (equal
             (plist-get (plist-get req :req) :stream)
             t)))

  ;; Test :model
  (let* ((rich-ai-model "gpt-4o")
         (req (rich-ai-request :prompt "foo prompt")))
    (should (equal
             (plist-get (plist-get req :req) :model)
             "gpt-4o")))
  (let* ((rich-ai-model "gpt-4o")
         (req (rich-ai-request :prompt "foo prompt"
                               :model "gpt-4o-mini")))
    (should (equal
             (plist-get (plist-get req :req) :model)
             "gpt-4o-mini")))

  ;; Test :temperature
  (let* ((rich-ai-temperature 0)
         (req (rich-ai-request :prompt "foo prompt")))
    (should (= (plist-get (plist-get req :req) :temperature)
               0)))
  (let* ((rich-ai-temperature 0)
         (req (rich-ai-request :prompt "foo prompt"
                               :temperature 1.1)))
    (should (= (plist-get (plist-get req :req) :temperature)
               1.1)))

  ;; Test :api
  (let* ((rich-ai-api '(:service "openai"
                        :endpoint "https://api.openai.com/v1/chat/completions"))
         (req (rich-ai-request :prompt "foo prompt")))
    (should (equal (plist-get req :api) rich-ai-api)))
  (let* ((rich-ai-api '(:service "openai"
                        :endpoint "https://api.openai.com/v1/chat/completions"))
         (req (rich-ai-request
               :prompt "foo prompt"
               :api '(:service "openai-service"
                      :endpoint "https://openai-endpoint"))))
    (should (equal (plist-get req :api)
                   '(:service "openai-service"
                     :endpoint "https://openai-endpoint")))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-org-demote-test")))
(ert-deftest rich-ai-org-demote-test ()
  ;; headline top level is 1
  (should
   (string=
    (rich-ai-org-demote "foo-1

** foo-2
* foo-3
*** foo-4
* foo-5
** foo-6
" 3)
    "foo-1

**** foo-2
*** foo-3
***** foo-4
*** foo-5
**** foo-6
"
    ))
  (should
   (string=
    (rich-ai-org-demote "foo-1

** foo-2
* foo-3
*** foo-4
* foo-5
** foo-6
" 4)
    "foo-1

***** foo-2
**** foo-3
****** foo-4
**** foo-5
***** foo-6
"))
  ;; headline top level is 2
  (should
   (string=
    (rich-ai-org-demote "foo-1

*** foo-2
** foo-3
**** foo-4
** foo-5
*** foo-6
" 3)
    "foo-1

**** foo-2
*** foo-3
***** foo-4
*** foo-5
**** foo-6
"))
  (should
   (string=
    (rich-ai-org-demote "foo-1

*** foo-2
** foo-3
**** foo-4
** foo-5
*** foo-6
" 4)
    "foo-1

***** foo-2
**** foo-3
****** foo-4
**** foo-5
***** foo-6
"))
  ;; headline top level is 3
  (should
   (string=
    (rich-ai-org-demote "foo-1

**** foo-2
*** foo-3
***** foo-4
*** foo-5
**** foo-6
" 3)
    "foo-1

**** foo-2
*** foo-3
***** foo-4
*** foo-5
**** foo-6
"))
  (should
   (string=
    (rich-ai-org-demote "foo-1

**** foo-2
*** foo-3
***** foo-4
*** foo-5
**** foo-6
" 4)
    "foo-1

***** foo-2
**** foo-3
****** foo-4
**** foo-5
***** foo-6
")))

(defun rich-ai-test-add-or-replace-timestamp-file (req &optional timestamp)
  ""
  ;; we replace (or add) the timestamp file with
  ;; timestamp-1734664733.06362 file which corresponds
  ;; to the date [2024-12-20 Fri]
  (make-directory (rich-ai-request-dir req) 'parents)
  (when-let ((timestamp-file-original
              (car
               (directory-files (rich-ai-request-dir req) t "timestamp-.*"))))
    (delete-file timestamp-file-original))
  (with-temp-buffer
    (write-file
     (format "%stimestamp-%s"
             (rich-ai-request-dir req)
             (or timestamp 1734664733.06362)))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-insert-conversation-test")))
(ert-deftest rich-ai-insert-conversation-test ()
  ;; Signal error if the request doesn't exist in `:ai-dir'
  (should-error
   (let* ((req `(:ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                 :uuid "uuid-foo")))
     (rich-ai-insert-conversation req)))

  ;; Signal error when an error.json file exists in req directory
  (let* ((req `(:ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-foo")))
    (rich-ai-request-write 'prompt req "")
    (rich-ai-request-write 'request req "")
    (rich-ai-request-write 'exchanges req "")
    (rich-ai-request-write 'response req "")
    (rich-ai-request-write 'response-org req "")

    (rich-ai-request-write 'error req "")
    (should-error (rich-ai-insert-conversation req)))

  ;; Signal error when the request in incomplete, specificaly
  ;; when the following files are missing:
  ;;
  ;; - prompt.org
  ;; - request.json
  ;; - response.json
  ;; - response.org
  ;; - exchanges.json
  (let* ((req `(:ai-dir ,(concat (make-temp-file "rich-ai-" t) "/")
                :uuid "uuid-foo")))
    (make-directory (rich-ai-request-dir req) 'parent)
    (should-error (rich-ai-insert-conversation req)))


  ;; We call `rich-ai-test-add-or-replace-timestamp-file' before
  ;; inserting the conversation.  This adds or replaces the timestamp
  ;; file that `rich-ai-insert-conversation' uses to determine the
  ;; date of the request

  ;; conversation with no previous exchanges
  (should
   (string=
    (with-temp-buffer
      (org-mode)
      (cl-letf (((symbol-function 'rich-ai-uuid)
                 (lambda nil "uuid")))
        (let ((rich-ai-org-property-date "AI_ASSISTANT_DATE")
              (rich-ai-org-property-req "AI_ASSISTANT_REQ")
              (req (rich-ai-request
                    :prompt "foo bar baz"
                    :ai-dir (concat (make-temp-file "rich-ai-" t) "/")))
              (resp '(:model "gpt-4o-mini-2024-07-18"
                      :choices [(:index 0
                                 :message (:role "assistant"
                                           :content "foo bar baz assistant response"))])))
          (rich-ai-write-request req)
          (rich-ai-write-response (rich-ai-json-encode resp) resp req)
          (rich-ai-test-add-or-replace-timestamp-file req)
          (rich-ai-insert-conversation req)))
      (buffer-substring-no-properties (point-min) (point-max)))
    "** Conversation
:PROPERTIES:
:AI_ASSISTANT_DATE: [2024-12-20 Fri]
:AI_ASSISTANT_REQ: uuid
:END:
*** prompt

foo bar baz

*** response

foo bar baz assistant response

"))


  ;; conversation with no previous exchanges to which we add a title
  ;; in the top level heading
  (should
   (string=
    (with-temp-buffer
      (org-mode)
      (cl-letf (((symbol-function 'rich-ai-uuid)
                 (lambda nil "uuid")))
        (let ((rich-ai-org-property-date "AI_ASSISTANT_DATE")
              (rich-ai-org-property-req "AI_ASSISTANT_REQ")
              (req (rich-ai-request
                    :prompt "* title-1\n** foo\n\nbar baz\n\n* title-2\n** foo\n\nbar baz"
                    :ai-dir (concat (make-temp-file "rich-ai-" t) "/")))
              (resp '(:model "o1-mini-2024-09-12"
                      :choices [(:index 0
                                 :message (:role "assistant"
                                           :content "### assistant title-1 \n#### foo\n\n bar baz\n\n### title-2\n#### foo\n\n bar baz"))]))
              (title "Title of the request"))
          (rich-ai-write-request req)
          (rich-ai-write-response (rich-ai-json-encode resp) resp req)
          (rich-ai-test-add-or-replace-timestamp-file req)
          (rich-ai-insert-conversation req title)))
      (buffer-substring-no-properties (point-min) (point-max)))
    "** Title of the request
:PROPERTIES:
:AI_ASSISTANT_DATE: [2024-12-20 Fri]
:AI_ASSISTANT_REQ: uuid
:END:
*** prompt

**** title-1
***** foo

bar baz

**** title-2
***** foo

bar baz

*** response

**** assistant title-1

***** foo

bar baz

**** title-2

***** foo

bar baz

"))

  ;; conversation with previous exchanges
  (should
   (string=
    (with-temp-buffer
      (org-mode)
      (cl-letf (((symbol-function 'rich-ai-uuid)
                 (lambda nil "uuid-baz")))
        (let ((rich-ai-org-property-date "AI_ASSISTANT_DATE")
              (rich-ai-org-property-req "AI_ASSISTANT_REQ")
              (req (rich-ai-request
                    :prompt "* baz-heading-1\n** baz-heading-2\n\nbaz-content"
                    :ai-dir (concat (make-temp-file "rich-ai-" t) "/")
                    :exchanges [(:uuid "uuid-foo"
                                 :prompt "* foo-heading-1\n** foo-heading-2\n\nfoo-content"
                                 :user "foo user"
                                 :assistant "### foo-assistant-heading-3\n#### foo-assistant-heading-4\n\nfoo-assistant-content"
                                 :response "*** foo-assistant-heading-3\n**** foo-assistant-heading-4\n\nfoo-assistant-content")
                                (:uuid "uuid-bar"
                                 :prompt "* bar-heading-1\n** bar-heading-2\n\nbar-content"
                                 :user "bar user"
                                 :assistant "### bar-assistant-heading-3\n#### bar-assistant-heading-4\n\nbar-assistant-content"
                                 :response "*** bar-assistant-heading-3\n**** bar-assistant-heading-4\n\nbar-assistant-content")]))
              (resp '(:model "o1-mini-2024-09-12"
                      :choices [(:index 0
                                 :message (:role "assistant"
                                           :content "### baz-assistant-heading-3\n\n#### baz-assistant-heading-4\n\nbaz-assistant-content"))])))
          (message "%s" (plist-get req :ai-dir))
          (rich-ai-write-request req)
          (rich-ai-write-response (rich-ai-json-encode resp) resp req)
          (rich-ai-test-add-or-replace-timestamp-file req)
          (rich-ai-insert-conversation req)))
      (buffer-substring-no-properties (point-min) (point-max)))
    "** Conversation
:PROPERTIES:
:AI_ASSISTANT_DATE: [2024-12-20 Fri]
:AI_ASSISTANT_REQ: uuid-baz
:END:
*** prompt

**** foo-heading-1
***** foo-heading-2

foo-content

*** response

**** foo-assistant-heading-3
***** foo-assistant-heading-4

foo-assistant-content

*** prompt

**** bar-heading-1
***** bar-heading-2

bar-content

*** response

**** bar-assistant-heading-3
***** bar-assistant-heading-4

bar-assistant-content

*** prompt

**** baz-heading-1
***** baz-heading-2

baz-content

*** response

**** baz-assistant-heading-3

***** baz-assistant-heading-4

baz-assistant-content

"))


  ;; Append last request to an existing conversation.
  ;; As we're appending, the date is not inserted again by
  ;; `rich-ai-insert-conversation', so we don't need to call
  ;; `rich-ai-test-add-or-replace-timestamp-file' to add a timestamp
  ;; file
  (should
   (string=
    (with-temp-buffer
      (org-mode)
      (cl-letf (((symbol-function 'rich-ai-uuid)
                 (lambda nil "uuid-baz")))
        (let ((rich-ai-org-property-date "AI_ASSISTANT_DATE")
              (rich-ai-org-property-req "AI_ASSISTANT_REQ")
              (req (rich-ai-request
                    :prompt "* baz-heading-1\n** baz-heading-2\n\nbaz-content"
                    :ai-dir (concat (make-temp-file "rich-ai-" t) "/")
                    :exchanges [(:uuid "uuid-foo"
                                 :prompt "* foo-heading-1\n** foo-heading-2\n\nfoo-content"
                                 :user "foo user"
                                 :assistant "### foo-assistant-heading-3\n#### foo-assistant-heading-4\n\nfoo-assistant-content"
                                 :response "*** foo-assistant-heading-3\n**** foo-assistant-heading-4\n\nfoo-assistant-content")
                                (:uuid "uuid-bar"
                                 :prompt "* bar-heading-1\n** bar-heading-2\n\nbar-content"
                                 :user "bar user"
                                 :assistant "### bar-assistant-heading-3\n#### bar-assistant-heading-4\n\nbar-assistant-content"
                                 :response "*** bar-assistant-heading-3\n**** bar-assistant-heading-4\n\nbar-assistant-content")]))
              (resp '(:model "o1-mini-2024-09-12"
                      :choices [(:index 0
                                 :message (:role "assistant"
                                           :content "### baz-assistant-heading-3\n\n#### baz-assistant-heading-4\n\nbaz-assistant-content"))])))
          (insert "** Title of the conversation
:PROPERTIES:
:AI_ASSISTANT_DATE: [date]
:AI_ASSISTANT_REQ: uuid-bar
:END:
*** prompt

**** foo-heading-1
***** foo-heading-2

foo-content

*** response

**** foo-assistant-heading-3

***** foo-assistant-heading-4

foo-assistant-content

*** prompt

**** bar-heading-1
***** bar-heading-2

bar-content

*** response

**** bar-assistant-heading-3

***** bar-assistant-heading-4

bar-assistant-content

")
          (rich-ai-write-request req)
          (rich-ai-write-response (rich-ai-json-encode resp) resp req)
          (rich-ai-insert-conversation req nil 'append)))
      (buffer-substring-no-properties (point-min) (point-max)))
    "** Title of the conversation
:PROPERTIES:
:AI_ASSISTANT_DATE: [date]
:AI_ASSISTANT_REQ: uuid-baz
:END:
*** prompt

**** foo-heading-1
***** foo-heading-2

foo-content

*** response

**** foo-assistant-heading-3

***** foo-assistant-heading-4

foo-assistant-content

*** prompt

**** bar-heading-1
***** bar-heading-2

bar-content

*** response

**** bar-assistant-heading-3

***** bar-assistant-heading-4

bar-assistant-content

*** prompt

**** baz-heading-1
***** baz-heading-2

baz-content

*** response

**** baz-assistant-heading-3

***** baz-assistant-heading-4

baz-assistant-content

"))

  ;; last argument `start-from' indicates we are starting a
  ;; conversation from `req' but we didn't send a new request
  ;; so far and we only want to insert the last exchange
  ;; user/assistant, not the whole conversation in `req' so far
  (should
   (string=
    (with-temp-buffer
      (org-mode)
      (cl-letf (((symbol-function 'rich-ai-uuid)
                 (lambda nil "uuid")))
        (let* ((rich-ai-org-property-date "AI_ASSISTANT_DATE")
               (rich-ai-org-property-req "AI_ASSISTANT_REQ")
               (rich-ai-dir (concat (make-temp-file "rich-ai-" t) "/"))
               (req (rich-ai-request
                     :prompt "* baz-heading-1\n** baz-heading-2\n\nbaz-content"
                     :exchanges [(:uuid "uuid-foo"
                                  :prompt "* foo-heading-1\n** foo-heading-2\n\nfoo-content"
                                  :user "foo user"
                                  :assistant "### foo-assistant-heading-3\n#### foo-assistant-heading-4\n\nfoo-assistant-content"
                                  :response "*** foo-assistant-heading-3\n**** foo-assistant-heading-4\n\nfoo-assistant-content")
                                 (:uuid "uuid-bar"
                                  :prompt "* bar-heading-1\n** bar-heading-2\n\nbar-content"
                                  :user "bar user"
                                  :assistant "### bar-assistant-heading-3\n#### bar-assistant-heading-4\n\nbar-assistant-content"
                                  :response "*** bar-assistant-heading-3\n**** bar-assistant-heading-4\n\nbar-assistant-content")]))
               (resp '(:model "o1-mini-2024-09-12"
                       :choices [(:index 0
                                  :message (:role "assistant"
                                            :content "### baz-assistant-heading-3\n\n#### baz-assistant-heading-4\n\nbaz-assistant-content"))])))
          (rich-ai-write-request req)
          (rich-ai-write-response (rich-ai-json-encode resp) resp req)
          (rich-ai-test-add-or-replace-timestamp-file req)
          (rich-ai-insert-conversation
           req "Title of the conversation" nil 'start-from)))
      (buffer-substring-no-properties (point-min) (point-max)))
    (concat "** Title of the conversation
:PROPERTIES:
:AI_ASSISTANT_DATE: [2024-12-20 Fri]
:AI_ASSISTANT_REQ: uuid
:END:
*** prompt

**** baz-heading-1
***** baz-heading-2

baz-content

*** response

**** baz-assistant-heading-3

***** baz-assistant-heading-4

baz-assistant-content

"))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-history-previous/next")))
(ert-deftest rich-ai-history-previous/next ()
  ;; default usage
  (should
   (equal (rich-ai-history-previous [("foo" "bar" "baz") nil nil])
          [("bar" "baz") "foo" nil]))
  (should
   (equal (rich-ai-history-previous [("bar" "baz") "foo" nil])
          [("baz") "bar" ("foo")]))
  (should
   (equal (rich-ai-history-previous [("baz") "bar" ("foo")])
          [nil "baz" ("bar" "foo")]))
  (should
   (equal (rich-ai-history-previous [nil "baz" ("bar" "foo")])
          [nil "baz" ("bar" "foo")]))
  (should
   (equal (rich-ai-history-next [nil "baz" ("bar" "foo")])
          [("baz") "bar" ("foo")]))
  (should
   (equal (rich-ai-history-next [("baz") "bar" ("foo")])
          [("bar" "baz") "foo" nil]))
  (should
   (equal (rich-ai-history-next [("bar" "baz") "foo" nil])
          [("bar" "baz") "foo" nil]))

  ;; signal error if both `prompt' and `discard-current' optional
  ;; arguments are non nil
  ;; todo
  (should-error
   (rich-ai-history-previous
    [("foo" "bar" "baz") nil nil] '(:prompt "scratch prompt") 'discard-current))

  (should-error
   (rich-ai-history-next
    [nil "baz" ("bar" "foo")] '(:prompt "scratch prompt") 'discard-current))

  ;; with `prompt' optional argument
  (should
   (equal (rich-ai-history-previous
           [("foo" "bar" "baz") nil nil] '(:prompt "scratch prompt"))
          [("bar" "baz") "foo" ((:prompt "scratch prompt"))]))
  (should
   (equal (rich-ai-history-previous
           [("bar" "baz") "foo" nil] '(:prompt "scratch prompt"))
          [("baz") "bar" ((:prompt "scratch prompt") "foo")]))
  (should
   (equal (rich-ai-history-previous
           [("baz") "bar" ("foo")] '(:prompt "scratch prompt"))
          [nil "baz" ((:prompt "scratch prompt") "bar" "foo")]))
  (should
   (equal (rich-ai-history-previous
           [nil "baz" ("bar" "foo")] '(:prompt "scratch prompt"))
          [nil "baz" ("bar" "foo")]))
  (should
   (equal (rich-ai-history-next
           [nil "baz" ("bar" "foo")] '(:prompt "scratch prompt"))
          [((:prompt "scratch prompt") "baz") "bar" ("foo")]))
  (should
   (equal (rich-ai-history-next
           [("baz") "bar" ("foo")] '(:prompt "scratch prompt"))
          [((:prompt "scratch prompt") "bar" "baz") "foo" nil]))
  (should
   (equal (rich-ai-history-next
           [("bar" "baz") "foo" nil] '(:prompt "scratch prompt"))
          [("bar" "baz") "foo" nil]))

  ;; with `discard-current' optional argument
  (should
   (equal (rich-ai-history-previous
           [("foo" "bar") "to-be-discarded" nil] nil 'discard-current)
          [("bar") "foo" nil]))
  (should
   (equal (rich-ai-history-previous
           [("bar") "to-be-discarded" ("foo")] nil 'discard-current)
          [nil "bar" ("foo")]))
  (should
   (equal (rich-ai-history-next
           [nil "to-be-discarded" ("bar" "foo")] nil 'discard-current)
          [nil "bar" ("foo")]))
  (should
   (equal (rich-ai-history-next
           [("bar") "to-be-discarded" ("foo")] nil 'discard-current)
          [("bar") "foo" nil])))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-prompt-current-test")))
(ert-deftest rich-ai-prompt-current-test ()
  (let ((rich-ai-prompt-history-state [("foo-uuid" "bar-uuid" "baz-uuid") nil nil]))
    (should-not (rich-ai-prompt-current)))
  (let ((rich-ai-prompt-history-state
         [("foo-uuid" "bar-uuid" "baz-uuid") (:prompt "scratch prompt") nil]))
    (should (string= (rich-ai-prompt-current) "scratch prompt")))
  (let* ((rich-ai-dir (concat (make-temp-file "rich-ai-" t) "/"))
         (req `(:ai-dir ,rich-ai-dir :uuid "foo-uuid"))
         (rich-ai-prompt-history-state [("bar-uuid" "baz-uuid") "foo-uuid" nil]))
    (message "%S" (rich-ai-request-dir req))
    (rich-ai-request-write 'prompt req "foo prompt\n")
    (should (string= (rich-ai-prompt-current) "foo prompt\n"))))

(ert-deftest rich-ai-conversation-locked-p-test ()
  ;; Values of :req and :proc keys are normally not string, but
  ;; as we don't use them `rich-ai-pending-requests', it's ok.
  (let ((rich-ai-pending-requests '((:conversation-id "conv-foo"
                                     :req "req-foo"
                                     :proc "proc-foo")
                                    (:req "req-bar"
                                     :proc "proc-bar"))))
    (should (rich-ai-conversation-locked-p "conv-foo"))
    (should-not (rich-ai-conversation-locked-p "conv-baz")))
  (let ((rich-ai-pending-requests nil))
    (should-not (rich-ai-conversation-locked-p "conv-foo"))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-conversation-test")))
(ert-deftest rich-ai-conversation-test ()

  ;; Titles must be unique
  (let ((rich-ai-conversations
         '(("conversation-id-foo" . (:title "foo title"
                                     :action start
                                     :last-req-uuid nil)))))
    (should-error
     (rich-ai-conversation 'start "foo title")))

  ;; actions must be start, start-from, continue-from
  (let ((rich-ai-conversations nil))
    (should-error
     (rich-ai-conversation 'wrong-action "foo title")))

  ;; action - start
  (let (rich-ai-conversations rich-ai-conversation-id)
    (cl-letf (((symbol-function 'rich-ai-uuid)
               (lambda nil "conversation-id-foo")))
      (rich-ai-conversation 'start "foo title")
      (should
       (equal rich-ai-conversations
              '(("conversation-id-foo" . (:title "foo title"
                                          :action start
                                          :last-req-uuid nil)))))
      (should
       (string= rich-ai-conversation-id "conversation-id-foo"))))
  (let ((rich-ai-conversations
         '(("conversation-id-foo" . (:title "foo title"
                                     :action start
                                     :last-req-uuid nil))))
        rich-ai-conversation-id)
    (cl-letf (((symbol-function 'rich-ai-uuid)
               (lambda nil "conversation-id-bar")))
      (rich-ai-conversation 'start "bar title")
      (should
       (seq-set-equal-p
        (mapcar #'car rich-ai-conversations)
        '("conversation-id-foo" "conversation-id-bar")))
      (should
       (equal (alist-get "conversation-id-bar" rich-ai-conversations nil nil 'string=)
              '(:title "bar title" :action start :last-req-uuid nil)))
      (should
       (string= rich-ai-conversation-id "conversation-id-bar"))))

  ;; action - start
  ;; error - req-uuid must not be specified
  (let ((rich-ai-conversations nil))
    (should-error
     (rich-ai-conversation 'start "foo title" "foo-req-uuid")))

  ;; actions - start-from, continue-from
  ;; error - req-uuid is mandatory
  (let ((rich-ai-conversations nil))
    (should-error (rich-ai-conversation 'start-from "foo title")))
  ;; error - req associated with req-uuid doesn't exist in `rich-ai-dir'
  (let ((rich-ai-conversations nil)
        (rich-ai-dir (concat (make-temp-file "rich-ai-" t) "/")))
    (should-error
     (rich-ai-conversation 'start-from "foo title" "foo-req-uuid")))

  ;; error -  when an error.json file exists in req directory
  (let* ((rich-ai-conversations nil)
         (rich-ai-dir (concat (make-temp-file "rich-ai-" t) "/"))
         (req `(:ai-dir ,rich-ai-dir :uuid "foo-req-uuid")))
    (rich-ai-request-write 'prompt req "")
    (rich-ai-request-write 'request req "")
    (rich-ai-request-write 'exchanges req "")
    (rich-ai-request-write 'response req "")
    (rich-ai-request-write 'response-org req "")

    (rich-ai-request-write 'error req "")
    (should-error
     (rich-ai-conversation 'start-from "foo title" "foo-req-uuid")))

  ;; error - when req associated with req-uuid is in incomplete,
  ;; specificaly when the following files are missing:
  ;;
  ;; - prompt.org
  ;; - request.json
  ;; - response.json
  ;; - response.org
  ;; - exchanges.json
  (let* ((rich-ai-conversations nil)
         (rich-ai-dir (concat (make-temp-file "rich-ai-" t) "/"))
         (req `(:ai-dir ,rich-ai-dir :uuid "foo-req-uuid")))
    (make-directory (rich-ai-request-dir req) 'parent)
    (should-error
     (rich-ai-conversation 'start-from "foo title" "foo-req-uuid")))

  ;; action - start-from, continue-from
  ;; we can start-from and continue-from multiple different conversations
  ;; from the same req-uuid
  (let* (rich-ai-conversations
         rich-ai-conversation-id
         (rich-ai-dir (concat (make-temp-file "rich-ai-" t) "/"))
         (foo-req (rich-ai-request :prompt "foo-req")) ;; depend on `rich-ai-dir'
         (foo-req-uuid (plist-get foo-req :uuid))
         (bar-req (rich-ai-request :prompt "bar-req")) ;; depend on `rich-ai-dir'
         (bar-req-uuid (plist-get bar-req :uuid))
         ;; we use the same response for two different requests
         ;; because we just need them to exist in the request directory
         (resp '(:id "chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8"
                 :object "chat.completion"
                 :created 1733030031
                 :model "gpt-4o-mini-2024-07-18"
                 :choices [(:index 0
                            :message (:role "assistant" :content "assistant" :refusal nil)
                            :logprobs nil :finish_reason "stop")]))
         (resp-str (rich-ai-json-encode resp)))
    (rich-ai-write-request foo-req)
    (rich-ai-write-response resp-str resp foo-req)
    (rich-ai-write-request bar-req)
    (rich-ai-write-response resp-str resp bar-req)

    (let ((n 0))
      (cl-letf (((symbol-function 'rich-ai-uuid)
                 (lambda nil (format "conversation-id-foo-%s" (cl-incf n)))))
        (rich-ai-conversation 'start-from "foo-1 title" foo-req-uuid)
        (rich-ai-conversation 'start-from "foo-2 title" foo-req-uuid)
        (rich-ai-conversation 'continue-from "foo-3 title" foo-req-uuid)))

    (let ((n 0))
      (cl-letf (((symbol-function 'rich-ai-uuid)
                 (lambda nil (format "conversation-id-bar-%s" (cl-incf n)))))
        (rich-ai-conversation 'start-from  "bar-1 title" bar-req-uuid)
        (rich-ai-conversation 'continue-from  "bar-2 title" bar-req-uuid)
        (rich-ai-conversation 'continue-from  "bar-3 title" bar-req-uuid)))

    (should
     (seq-set-equal-p
      (mapcar #'car rich-ai-conversations)
      '("conversation-id-foo-1" "conversation-id-foo-2" "conversation-id-foo-3"
        "conversation-id-bar-1" "conversation-id-bar-2" "conversation-id-bar-3")))
    (should
     (string= rich-ai-conversation-id "conversation-id-bar-3"))

    (should
     (equal
      (alist-get "conversation-id-foo-1" rich-ai-conversations nil nil 'string=)
      `(:title "foo-1 title" :action start-from  :last-req-uuid ,foo-req-uuid)))
    (should
     (equal
      (alist-get "conversation-id-foo-2" rich-ai-conversations nil nil 'string=)
      `(:title "foo-2 title" :action start-from  :last-req-uuid ,foo-req-uuid)))
    (should
     (equal
      (alist-get "conversation-id-foo-3" rich-ai-conversations nil nil 'string=)
      `(:title "foo-3 title" :action continue-from  :last-req-uuid ,foo-req-uuid)))
    (should
     (equal
      (alist-get "conversation-id-bar-1" rich-ai-conversations nil nil 'string=)
      `(:title "bar-1 title" :action start-from  :last-req-uuid ,bar-req-uuid)))
    (should
     (equal
      (alist-get "conversation-id-bar-2" rich-ai-conversations nil nil 'string=)
      `(:title "bar-2 title" :action continue-from  :last-req-uuid ,bar-req-uuid)))
    (should
     (equal
      (alist-get "conversation-id-bar-3" rich-ai-conversations nil nil 'string=)
      `(:title "bar-3 title" :action continue-from  :last-req-uuid ,bar-req-uuid))))

  (let* ((rich-ai-conversations
          '(("conversation-id-foo" . (:title "foo title"
                                      :action start
                                      :last-req-uuid nil))))
         rich-ai-conversation-id
         (rich-ai-dir (concat (make-temp-file "rich-ai-" t) "/"))
         (foo-req (rich-ai-request :prompt "foo-req")) ;; depend on `rich-ai-dir'
         (foo-req-uuid (plist-get foo-req :uuid))
         (resp '(:id "chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8"
                 :object "chat.completion"
                 :created 1733030031
                 :model "gpt-4o-mini-2024-07-18"
                 :choices [(:index 0
                            :message (:role "assistant" :content "assistant" :refusal nil)
                            :logprobs nil :finish_reason "stop")]))
         (resp-str (rich-ai-json-encode resp)))
    (rich-ai-write-request foo-req)
    (rich-ai-write-response resp-str resp foo-req)

    (cl-letf (((symbol-function 'rich-ai-uuid)
               (lambda nil "conversation-id-bar")))
      (rich-ai-conversation 'start-from "bar title" foo-req-uuid))
    (cl-letf (((symbol-function 'rich-ai-uuid)
               (lambda nil "conversation-id-baz")))
      (rich-ai-conversation 'continue-from "baz title" foo-req-uuid))

    (should
     (seq-set-equal-p
      (mapcar #'car rich-ai-conversations)
      '("conversation-id-foo" "conversation-id-bar" "conversation-id-baz")))
    (should
     (string= rich-ai-conversation-id "conversation-id-baz"))
    (should
     (equal
      (alist-get "conversation-id-bar" rich-ai-conversations nil nil 'string=)
      `(:title "bar title" :action start-from  :last-req-uuid ,foo-req-uuid)))
    (should
     (equal
      (alist-get "conversation-id-baz" rich-ai-conversations nil nil 'string=)
      `(:title "baz title" :action continue-from  :last-req-uuid ,foo-req-uuid)))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-conversation-update-test")))
(ert-deftest rich-ai-conversation-update-test ()
  (let ((rich-ai-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :action start :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :action start-from :last-req-uuid "bar-req-uuid"))
           ("conversation-id-baz" .
            (:title "baz title" :action continue-from :last-req-uuid "baz-req-uuid")))))
    (rich-ai-conversation-update '(:conversation-id "conversation-id-foo")
                                 '(:uuid "new-foo-req-uuid"))
    (rich-ai-conversation-update '(:conversation-id "conversation-id-bar")
                                 '(:uuid "new-bar-req-uuid"))
    (rich-ai-conversation-update '(:conversation-id "conversation-id-baz")
                                 '(:uuid "new-baz-req-uuid"))
    (rich-ai-conversation-update '(:conversation-id "not-in--rich-ai-conversations")
                                 '(:uuid "fake-req-uuid"))
    (should
     (equal
      (alist-get "conversation-id-foo" rich-ai-conversations nil nil 'string=)
      '(:title "foo title" :action continue-from :last-req-uuid "new-foo-req-uuid")))
    (should
     (equal
      (alist-get "conversation-id-bar" rich-ai-conversations nil nil 'string=)
      '(:title "bar title" :action continue-from :last-req-uuid "new-bar-req-uuid")))
    (should
     (equal
      (alist-get "conversation-id-baz" rich-ai-conversations nil nil 'string=)
      '(:title "baz title" :action continue-from :last-req-uuid "new-baz-req-uuid")))
    (should
     (seq-set-equal-p
      (mapcar #'car rich-ai-conversations)
      '("conversation-id-foo" "conversation-id-bar" "conversation-id-baz")))))

(ert-deftest rich-ai-conversation-title-test ()
  (let ((rich-ai-conversations nil))
    (should-not (rich-ai-conversation-title "conversation-id-bar")))
  (let ((rich-ai-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :action start :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :action start-from :last-req-uuid "bar-req-uuid"))
           ("conversation-id-baz" .
            (:title "baz title" :action continue-from :last-req-uuid "baz-req-uuid")))))
    (should
     (string=
      (rich-ai-conversation-title "conversation-id-bar")
      "bar title"))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-conversation-buffer-name-test")))
(ert-deftest rich-ai-conversation-buffer-name-test ()
  (let ((rich-ai-conversations nil))
    (should-not (rich-ai-conversation-buffer-name "conversation-id-foo"))
    (should-not (rich-ai-conversation-buffer-name nil)))
  (let ((rich-ai-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :action start :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :action start-from :last-req-uuid "bar-req-uuid"))
           ("conversation-id-baz" .
            (:title "baz title" :action continue-from :last-req-uuid "baz-req-uuid")))))
    (should-not (rich-ai-conversation-buffer-name nil))
    (should-not (rich-ai-conversation-buffer-name "wrong-id"))
    (should
     (string=
      (rich-ai-conversation-buffer-name "conversation-id-foo")
      "*ai foo title*"))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-conversation-action-test")))
(ert-deftest rich-ai-conversation-action-test ()
  (let ((rich-ai-conversations nil))
    (should-not (rich-ai-conversation-action "conversation-id-bar")))
  (let ((rich-ai-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :action start :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :action start-from :last-req-uuid "bar-req-uuid"))
           ("conversation-id-baz" .
            (:title "baz title" :action continue-from :last-req-uuid "baz-req-uuid")))))
    (should
     (eq (rich-ai-conversation-action "conversation-id-foo")
         'start))
    (should
     (eq (rich-ai-conversation-action "conversation-id-bar")
         'start-from))
    (should
     (eq (rich-ai-conversation-action "conversation-id-baz")
         'continue-from))))

(ert-deftest rich-ai-conversation-last-req-test ()
  (let ((rich-ai-conversations nil))
    (should-not (rich-ai-conversation-title "conversation-id-bar")))
  (let ((rich-ai-dir "/tmp/ai/")
        (rich-ai-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :action start :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :action start-from :last-req-uuid "bar-req-uuid"))
           ("conversation-id-baz" .
            (:title "baz title" :action continue-from :last-req-uuid "baz-req-uuid")))))
    (should-not (rich-ai-conversation-last-req "conversation-id-foo"))
    (should
     (equal
      (rich-ai-conversation-last-req "conversation-id-bar")
      `(:uuid "bar-req-uuid" :ai-dir "/tmp/ai/")))
    (should
     (equal
      (rich-ai-conversation-last-req "conversation-id-baz")
      `(:uuid "baz-req-uuid" :ai-dir "/tmp/ai/")))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-conversation-exchanges-test")))
(ert-deftest rich-ai-conversation-exchanges-test ()
  (let* ((rich-ai-dir (concat (make-temp-file "rich-ai-" t) "/"))
         (last-req
          `(:req (:stream :false
                  :model "gpt-4o-mini"
                  :temperature 1
                  :messages [(:role "system" :content "baz system")
                             (:role "user" :content "foo user")
                             (:role "assistant" :content "foo assistant\n")
                             (:role "user" :content "bar prompt")
                             (:role "assistant" :content "bar assistant\n")
                             (:role "user" :content "baz user")])
            :api (:service "chatgpt"
                  :endpoint "https://api.openai.com/v1/chat/completions")
            :prompt "baz prompt\n"
            :system-prompt "baz system prompt\n"
            :exchanges [(:uuid "uuid-foo"
                         :prompt "foo prompt\n"
                         :user "foo user"
                         :assistant "foo assistant\n"
                         :response "foo assistant\n")
                        (:uuid "uuid-bar"
                         :prompt "bar prompt\n"
                         :user "bar user"
                         :assistant "bar assistant\n"
                         :response "bar assistant\n")]
            :ai-dir ,rich-ai-dir
            :uuid "uuid-baz"))
         ;; #_(ai-dir (plist-get req :ai-dir))
         (last-req-uuid (plist-get last-req :uuid))
         (last-resp '(:id "chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8"
                      :object "chat.completion"
                      :created 1733030031
                      :model "gpt-4o-mini-2024-07-18"
                      :choices [(:index 0
                                 :message (:role "assistant" :content "baz assistant\n" :refusal nil)
                                 :logprobs nil :finish_reason "stop")]))
         (last-resp-str (rich-ai-json-encode last-resp))
         (rich-ai-conversations
          '(("conversation-id-start" .
             (:title "start title" :action start :last-req-uuid nil))
            ("conversation-id-start-from" .
             (:title "start-from title"
              :action start-from
              :last-req-uuid "uuid-baz"))
            ("conversation-id-continue-from" .
             (:title "continue-from title"
              :action continue-from
              :last-req-uuid "uuid-baz")))))
    (rich-ai-write-request last-req)
    (rich-ai-write-response last-resp-str last-resp last-req)
    (should-not (rich-ai-conversation-exchanges "conversation-id-start"))
    (should
     (equal
      (rich-ai-conversation-exchanges "conversation-id-start-from")
      [(:uuid "uuid-baz"
        :prompt "baz prompt\n"
        :user "baz user"
        :assistant "baz assistant\n"
        :response "baz assistant\n")]
      ))
    (should
     (equal
      (rich-ai-conversation-exchanges "conversation-id-continue-from")
      [(:uuid "uuid-foo"
        :prompt "foo prompt\n"
        :user "foo user"
        :assistant "foo assistant\n"
        :response "foo assistant\n")
       (:uuid "uuid-bar"
        :prompt "bar prompt\n"
        :user "bar user"
        :assistant "bar assistant\n"
        :response "bar assistant\n")
       (:uuid "uuid-baz"
        :prompt "baz prompt\n"
        :user "baz user"
        :assistant "baz assistant\n"
        :response "baz assistant\n")]
      ))
    ))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-send-request-test")))
(ert-deftest rich-ai-send-request-test ()
  ;; We want to test that concurrent requests work as expected:
  ;;
  ;; - observe mode-line during the test to be sure it doesn't stop
  ;;   after the first request done,
  ;; - test `rich-ai-pending-requests' value during all requests
  ;; - test that we can order responses according to their timestamp file
  ;; - test that we update `rich-ai-history-requests' variable

  ;; NOTE:
  ;;
  ;; 1) if `rich-ai-request-send' ever changes, we may have to change
  ;;    its binding definition below
  ;; 2) the way we redefine `rich-ai-request-send' is ugly as hell.
  ;;    we use `eval' and `macroexpand' and we repeat ourself
  ;;    3 times. I think we have to do this because `rich-ai-sentinel'
  ;;    is a macro.  Anyway we're able to test the behavior we expect
  ;;    from `rich-ai-send-request'.

  (let* (;; we modify these two variables in the callback function
         ;; which is called for each request when we received a response.
         ;; I don't know why but we need to define them globaly,
         ;; let defined doesn't work!  Anyway.
         (_ (progn
              (defvar responses nil "...")
              (setq responses nil)
              ))
         (callback (lambda (req resp info)
                     (rich-ai-pending-remove req)
                     (push (rich-ai-request-assistant-content resp) responses)
                     (rich-ai-conversation-update info req)
                     (rich-ai-mode-line-waiting 'maybe-stop)
                     (message "Response for request %s received"
                              (plist-get req :prompt))))
         (rich-ai-dir (concat (make-temp-file "rich-ai-" t) "/"))
         rich-ai-history-requests
         (conversation-id-foo "conversation-id-foo")
         (conversation-id-bar "conversation-id-bar")
         (rich-ai-conversations
          '(("conversation-id-foo" .
             (:title "foo title" :action start :last-req-uuid nil))
            ("conversation-id-bar" .
             (:title "bar title" :action start-from :last-req-uuid "last-bar-req-uuid"))
            ("conversation-id-baz" .
             (:title "baz title" :action continue-from :last-req-uuid "last-baz-req-uuid"))))
         (req-foo (rich-ai-request :prompt "foo"))
         (req-foo-foo (rich-ai-request :prompt "foo-foo"))
         ;; even if we don't use it (because we're not really sending
         ;; sending the request to openain) we pass :exchanges argument
         ;; to `rich-ai-request' as we should do it given that
         ;; below we send `req-baz' as part of the conversation
         ;; "conversation-id-bar" with :action being `start-from'
         (req-bar (rich-ai-request
                   :prompt "bar"
                   :exchanges [(:uuid "last-bar-req-uuid"
                                :prompt "last bar prompt"
                                :user "last bar user"
                                :assistant "last bar assistant"
                                :response "last bar response")]))
         (req-baz (rich-ai-request :prompt "baz"))
         (req-foo-uuid (plist-get req-foo :uuid))
         (req-bar-uuid (plist-get req-bar :uuid))
         (resp-fmt "{
    \"id\": \"chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8\",
    \"object\": \"chat.completion\",
    \"created\": 1733030031,
    \"model\": \"gpt-4o-mini-2024-07-18\",
    \"choices\": [
      {
        \"index\": 0,
        \"message\": {
          \"role\": \"assistant\",
          \"content\": \"%s\",
          \"refusal\": null
        },
        \"logprobs\": null,
        \"finish_reason\": \"stop\"
      }
    ]
  }"))
    (cl-letf (((symbol-function 'rich-ai-request-send)
               ;; Print response in stdout after 3 second
               ;; with sentinel constructed using `rich-ai-sentinel'
               (lambda (req callback callback-error info)
                 (rich-ai-write-request req)
                 (rich-test-echo-resp
                  (format resp-fmt "resp-foo") (generate-new-buffer-name "rich-ai")
                  (eval (macroexpand-all
                         `(rich-ai-sentinel
                           (quote ,req)
                           ;; no need for `quote' because car is `lambda'
                           ,callback
                           ;; works because ,callback-error returns (closure ...)
                           ;; because it is define with `lambda' in `rich-ai-send-request'
                           (quote ,callback-error)
                           (quote ,info))))
                  3))))
      (rich-ai-send-request
       :req req-foo
       :callback callback
       :info `(:conversation-id ,conversation-id-foo)))
    (cl-letf (((symbol-function 'rich-ai-request-send)
               ;; Print response in stdout after 3 second
               ;; with sentinel constructed using `rich-ai-sentinel'
               (lambda (req callback callback-error info)
                 (rich-ai-write-request req)
                 (rich-test-echo-resp
                  (format resp-fmt "resp-foo-foo") (generate-new-buffer-name "rich-ai")
                  (eval (macroexpand-all
                         `(rich-ai-sentinel
                           (quote ,req)
                           ;; no need for `quote' because car is `lambda'
                           ,callback
                           ;; works because ,callback-error returns (closure ...)
                           ;; because it is define with `lambda' in `rich-ai-send-request'
                           (quote ,callback-error)
                           (quote ,info))))
                  3))))
      ;; this request won't be sent because `req-foo' in the same
      ;; conversation `conversation-id-foo' is running at the same time
      (rich-ai-send-request
       :req req-foo-foo
       :callback callback
       :info `(:conversation-id ,conversation-id-foo)))
    (cl-letf (((symbol-function 'rich-ai-request-send)
               ;; Print response in stdout after 1 second
               ;; with sentinel constructed using `rich-ai-sentinel'
               (lambda (req callback callback-error info)
                 (rich-ai-write-request req)
                 (rich-test-echo-resp
                  (format resp-fmt "resp-bar") (generate-new-buffer-name "rich-ai")
                  (eval (macroexpand-all
                         `(rich-ai-sentinel
                           (quote ,req)
                           ;; no need for `quote' because car is `lambda'
                           ,callback
                           ;; works because ,callback-error returns (closure ...)
                           ;; because it is define with `lambda' in `rich-ai-send-request'
                           (quote ,callback-error)
                           (quote ,info))))
                  1))))
      (rich-ai-send-request
       :req req-bar
       :callback callback
       :info `(:conversation-id ,conversation-id-bar)))
    (cl-letf
        (((symbol-function 'rich-ai-request-send)
          ;; Print response in stdout after 1 second
          ;; with sentinel constructed using `rich-ai-sentinel'
          (lambda (req callback callback-error info)
            (rich-ai-write-request req)
            (rich-test-echo-resp
             (format resp-fmt "resp-baz") (generate-new-buffer-name "rich-ai")
             (eval (macroexpand-all
                    `(rich-ai-sentinel
                      (quote ,req)
                      ;; no need for `quote' because car is `lambda'
                      ,callback
                      ;; works because ,callback-error returns (closure ...)
                      ;; because it is define with `lambda' in `rich-ai-send-request'
                      (quote ,callback-error)
                      (quote ,info))))
             2))))
      (rich-ai-send-request
       :req req-baz
       :callback callback
       :info nil))

    ;; To manually test waiting widget in the mode line while we're
    ;; waiting for responses do the following:
    ;;
    ;; - comment the following 2 sexps
    ;; - run the test and observe the mode line and echo area
    ;;   during 5 seconds
    (let ((prompt-proc (lambda (r)
                         (cons (plist-get (plist-get r :req) :prompt)
                               (processp (plist-get r :proc)))))
          (pr-timer rich-ai-pending-timer))
      (sleep-for 0.2)
      (should
       (equal (mapcar prompt-proc rich-ai-pending-requests)
              '(("baz" . t)
                ("bar" . t)
                ("foo" . t))))
      (should (rich-ai-conversation-locked-p conversation-id-foo))
      (should (rich-ai-conversation-locked-p conversation-id-bar))
      ;; This means that the waiting widget still showing up
      ;; in the modeline
      (should (memq pr-timer timer-list))
      (sleep-for 0.9)
      (should
       (equal (mapcar prompt-proc rich-ai-pending-requests)
              '(("baz" . t)
                ("foo" . t))))
      (should (memq pr-timer timer-list))
      (sleep-for 1)
      (should
       (equal (mapcar prompt-proc rich-ai-pending-requests)
              '(("foo" . t))))
      (should (memq pr-timer timer-list))
      (sleep-for 1)
      (should (equal rich-ai-pending-requests nil))
      (should-not (memq pr-timer timer-list))
      (should-not global-mode-string) ;; waiting widget is gone
      (should (equal responses
                     '("resp-foo" "resp-baz" "resp-bar")))
      (should
       (equal rich-ai-history-requests
              (list
               (plist-get req-baz :uuid)
               (plist-get req-bar :uuid)
               (plist-get req-foo :uuid))))

      ;; state of `rich-ai-conversations'
      (should
       (equal
        (alist-get "conversation-id-foo" rich-ai-conversations nil nil 'string=)
        `(:title "foo title" :action continue-from :last-req-uuid ,req-foo-uuid)))
      (should
       (equal
        (alist-get "conversation-id-bar" rich-ai-conversations nil nil 'string=)
        `(:title "bar title" :action continue-from :last-req-uuid ,req-bar-uuid)))
      (should
       (seq-set-equal-p
        (mapcar #'car rich-ai-conversations)
        '("conversation-id-foo" "conversation-id-bar" "conversation-id-baz"))))

    ;; Test that the callback-error function remove waiting widget
    ;; in the modeline when an error occurs in the sentinel.  To
    ;; do so we send "killed\n" event to the process (waiting for
    ;; a response).  We use the same trick as above were we redefine
    ;; `rich-ai-request-send'.
    (let ((debug-on-error t)
          (proc-buff (generate-new-buffer-name "rich-ai"))
          pr-timer)
      (should-error
       (progn
         (cl-letf
             (((symbol-function 'rich-ai-request-send)
               ;; Print response in stdout after 1 second
               ;; with sentinel constructed using `rich-ai-sentinel'
               (lambda (req callback callback-error info)
                 (rich-ai-write-request req)
                 (rich-test-echo-resp
                  (format resp-fmt "resp-baz") proc-buff
                  (eval (macroexpand-all
                         `(rich-ai-sentinel
                           (quote ,req)
                           ;; no need for `quote' because car is `lambda'
                           ,callback
                           ;; works because ,callback-error returns (closure ...)
                           ;; because it is define with `lambda' in `rich-ai-send-request'
                           (quote ,callback-error)
                           (quote ,info))))
                  1))))
           (rich-ai-send-request :req (rich-ai-request :prompt "req")))
         (setq pr-timer rich-ai-pending-timer)
         (kill-process (get-buffer-process (get-buffer proc-buff)))
         (sleep-for 0.2)))
      (should (equal rich-ai-pending-requests nil))
      (should (equal rich-ai-pending-timer nil))
      ;; waiting widget is gone
      (should-not (memq pr-timer timer-list))
      (should-not global-mode-string))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-kill-last-request-test")))
(ert-deftest rich-ai-kill-last-request-test ()
  ;; See test `rich-ai-send-request-test' for explanation about
  ;; redefining `rich-ai-request-send'.

  (let ((debug-on-error t)
        (rich-ai-dir (concat (make-temp-file "rich-ai-" t) "/"))
        pr-timer)
    (cl-letf
        (((symbol-function 'rich-ai-request-send)
          ;; Print "resp-baz" in stdout after 1 second
          ;; with sentinel constructed using `rich-ai-sentinel'
          (lambda (req callback callback-error info)
            (rich-ai-write-request req)
            (rich-test-echo-resp
             "\"resp-baz\"" (generate-new-buffer-name "rich-ai")
             (eval (macroexpand-all
                    `(rich-ai-sentinel
                      (quote ,req)
                      ;; no need for `quote' because car is `lambda'
                      ,callback
                      ;; works because ,callback-error returns (closure ...)
                      ;; because it is define with `lambda' in `rich-ai-send-request'
                      (quote ,callback-error)
                      (quote ,info))))
             1))))
      (rich-ai-send-request :req (rich-ai-request :prompt "req")))
    (setq pr-timer rich-ai-pending-timer)
    ;; When we kill the last request, we actually kill its
    ;; associated process which signal an error.
    ;; But here we are not interested in that error but in checking
    ;; that global variables of interest are set back correctly after
    ;; the process is killed.
    (should-error
     (progn
       (rich-ai-kill-last-request)
       (sleep-for 0.2)))

    (should (equal rich-ai-pending-requests nil))
    (should (equal rich-ai-pending-timer nil))
    ;; waiting widget is gone
    (should-not (memq pr-timer timer-list))
    (should-not global-mode-string)))


(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "rich-ai-paths/conversations/requests-test")))
(ert-deftest rich-ai-paths/conversations/requests-test ()

  ;; ["uuid-req-1"]                           ;; not a conversation
  ;; ["uuid-req-1" "uuid-req-2"]              ;; not a conversation
  ;; ["uuid-req-1" "uuid-req-2" "uuid-req-3"]
  ;; ["uuid-req-1" "uuid-req-2" "uuid-req-4"]
  ;; ["uuid-req-5"]                           ;; with no response.json file
  ;; ["uuid-req-2" "uuid-req-6"]
  ;; ["uuid-req-7"]

  (let ((n 0))
    (cl-letf (((symbol-function 'rich-ai-uuid)
               (lambda nil (format "uuid-req-%s" (cl-incf n)))))
      (let* ((rich-ai-dir (concat (make-temp-file "rich-ai-" t) "/"))
             (req-1 (rich-ai-request :prompt "req-1 prompt"))
             (req-2 (rich-ai-request
                     :prompt "req-2 prompt"
                     :exchanges [(:uuid "uuid-req-1"
                                  :prompt "req-1 prompt"
                                  :user "req-1 user"
                                  :assistant "req-1 assistant"
                                  :response "req-1 response")]))
             (req-3 (rich-ai-request
                     :prompt "req-3 prompt"
                     :exchanges [(:uuid "uuid-req-1"
                                  :prompt "req-1 prompt"
                                  :user "req-1 user"
                                  :assistant "req-1 assistant"
                                  :response "req-1 response")
                                 (:uuid "uuid-req-2"
                                  :prompt "req-2 prompt"
                                  :user "req-2 user"
                                  :assistant "req-2 assistant"
                                  :response "req-2 response")]))
             (req-4 (rich-ai-request
                     :prompt "req-4 prompt"
                     :exchanges [(:uuid "uuid-req-1"
                                  :prompt "req-1 prompt"
                                  :user "req-1 user"
                                  :assistant "req-1 assistant"
                                  :response "req-1 response")
                                 (:uuid "uuid-req-2"
                                  :prompt "req-2 prompt"
                                  :user "req-2 user"
                                  :assistant "req-2 assistant"
                                  :response "req-2 response")]))
             (req-5 (rich-ai-request :prompt "req-5 prompt"))
             (req-6 (rich-ai-request
                     :prompt "req-6 prompt"
                     :exchanges [(:uuid "uuid-req-2"
                                  :prompt "req-2 prompt"
                                  :user "req-2 user"
                                  :assistant "req-2 assistant"
                                  :response "req-2 response")]))
             (req-7 (rich-ai-request :prompt "req-7 prompt"))
             ;; to be completly correct we should also modify
             ;; the value of `created' key.  But it doesn't matter here.
             (resp-fmt "{
    \"id\": \"chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8\",
    \"object\": \"chat.completion\",
    \"created\": 1733030031,
    \"model\": \"gpt-4o-mini-2024-07-18\",
    \"choices\": [
      {
        \"index\": 0,
        \"message\": {
          \"role\": \"assistant\",
          \"content\": \"%s\",
          \"refusal\": null
        },
        \"logprobs\": null,
        \"finish_reason\": \"stop\"
      }
    ]
  }")
             ;; Unix timestamps of the last 7 days including today
             (timestamps
              (mapcar (lambda (days)
                        (float-time
                         (time-subtract (current-time) (days-to-time days))))
                      '(6 5 4 3 2 1 0))))
        (message "%s" rich-ai-dir)
        (dotimes (idx 7)
          (let ((req (eval (intern (format "req-%s" (1+ idx))))))
            (rich-ai-write-request req)
            ;; We replace timestamp files in order to span requests
            ;; over 7 days including today.
            (rich-ai-test-add-or-replace-timestamp-file
             req (nth idx timestamps))
            ;; add response.json except for req-5 such that this request
            ;; is considered to be an error and so its path will not
            ;; be listed by `rich-ai-paths' function
            (when (not (= idx 4)) ;;
              (let* ((resp-str (format resp-fmt (format "req-%s assistant" (1+ idx))))
                     (resp (with-temp-buffer
                             (save-excursion (insert resp-str))
                             (rich-ai-json-read))))
                (rich-ai-write-response resp-str resp req)))))

        ;; Test `rich-ai-paths'
        (should (equal (rich-ai-paths 10)
                       '(["uuid-req-1"]
                         ["uuid-req-1" "uuid-req-2"]
                         ["uuid-req-1" "uuid-req-2" "uuid-req-3"]
                         ["uuid-req-1" "uuid-req-2" "uuid-req-4"]
                         ["uuid-req-2" "uuid-req-6"]
                         ["uuid-req-7"])))
        (should (equal (rich-ai-paths 1) '(["uuid-req-7"])))
        (should (equal (rich-ai-paths 4)
                       '(["uuid-req-1" "uuid-req-2" "uuid-req-4"]
                         ["uuid-req-2" "uuid-req-6"]
                         ["uuid-req-7"])))

        ;; Test `rich-ai-requests'
        (should (equal (rich-ai-requests 10)
                       '("uuid-req-1"
                         "uuid-req-2"
                         "uuid-req-3"
                         "uuid-req-4"
                         "uuid-req-6"
                         "uuid-req-7")))
        (should (equal (rich-ai-paths 1) '(["uuid-req-7"])))
        (should (equal (rich-ai-requests 4)
                       '("uuid-req-4" "uuid-req-6" "uuid-req-7")))

        ;; Test `rich-ai-conversations'
        (should (equal (rich-ai-conversations 10)
                       '("uuid-req-3" "uuid-req-4" "uuid-req-6" "uuid-req-7")))
        (should (equal (rich-ai-conversations 1) '("uuid-req-7")))
        (should (equal (rich-ai-conversations 4)
                       '("uuid-req-4" "uuid-req-6" "uuid-req-7")))))))

(ert-deftest rich-ai-conversations-keep-test ()
  (should-not (rich-ai-conversations-keep nil))
  (let ((paths '(["uuid-req-1"]
                 ["uuid-req-1" "uuid-req-2"]
                 ["uuid-req-1" "uuid-req-2" "uuid-req-3"]
                 ["uuid-req-1" "uuid-req-2" "uuid-req-4"]
                 ["uuid-req-2" "uuid-req-5"]
                 ["uuid-req-6"]
                 ["uuid-req-7"]
                 ["uuid-req-7" "uuid-req-8"]
                 ["uuid-req-5" "uuid-req-9"]
                 ["uuid-req-5" "uuid-req-9" "uuid-req-10"]
                 ["uuid-req-11"]
                 ["uuid-req-11" "uuid-req-12"]
                 ["uuid-req-13"])))
    (should
     (equal
      (rich-ai-conversations-keep paths)
      '("uuid-req-3"
        "uuid-req-4"
        "uuid-req-5"
        "uuid-req-6"
        "uuid-req-8"
        "uuid-req-10"
        "uuid-req-12"
        "uuid-req-13")))))
