(defmacro comment (&rest body) "Ignores body and yield nil." nil)

(defun eden-test-echo-resp (resp-str buffer-or-name sentinel &optional sleep)
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

(defun eden-test-add-or-replace-timestamp-file (req &optional timestamp)
  ""
  ;; we replace (or add) the timestamp file with
  ;; timestamp-1734664733.06362 file which corresponds
  ;; to the date [2024-12-20 Fri]
  (make-directory (eden-request-dir req) 'parents)
  (when-let ((timestamp-file-original
              (car
               (directory-files (eden-request-dir req) t "timestamp-.*"))))
    (delete-file timestamp-file-original))
  (with-temp-buffer
    (write-file
     (format "%stimestamp-%s"
             (eden-request-dir req)
             (or timestamp 1734664733.06362)))))

;;; API to send asynchronous requests to OpenAI and Perplexity

(ert-deftest eden-json-encoding/decoding-test ()
  (should
   (string=
    (eden-json-encode '(:key :false))
    "{\n  \"key\": false\n}"))
  (should
   (equal (with-temp-buffer
            (save-excursion
              (insert "{\"key\":false,\"array\":[0,1,2]}"))
            (eden-json-read))
          '(:key :false :array [0 1 2])))
  (should
   (equal
    (with-temp-buffer
      (save-excursion
        (insert (eden-json-encode
                 '(:stream :false
                   :model "gpt-4o-mini"
                   :messages [(:role "user" :content "foo bar baz")]))))
      (eden-json-read))
    '(:stream :false
      :model "gpt-4o-mini"
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
      (eden-json-read))
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

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-request-dir-test")))
(ert-deftest eden-request-dir-test ()
  ;; signal error when one of the keys `:dir' or `:uuid'
  ;; is missing or is not a string
  (should-error (eden-request-dir '(:dir "/tmp/eden/"))
                :type 'eden-error-req)
  (should-error (eden-request-dir '(:dir "/tmp/eden/" :uuid 1))
                :type 'eden-error-req)
  (should-error (eden-request-dir '(:uuid "foo-uuid"))
                :type 'eden-error-req)
  (should-error (eden-request-dir '(:uuid "foo-uuid" :dir 1))
                :type 'eden-error-req)
  (should-error (eden-request-dir nil)
                :type 'eden-error-req)

  (should
   (string= (eden-request-dir '(:dir "/tmp/eden/" :uuid "foo-uuid"))
            "/tmp/eden/foo-uuid/"))
  (should
   (string= (eden-request-dir '(:dir "/tmp/eden" :uuid "foo-uuid"))
            "/tmp/eden/foo-uuid/"))
  (should
   (string=
    (eden-request-dir '(:dir "~/eden" :uuid "foo-uuid"))
    (concat (file-name-as-directory
             (expand-file-name (getenv "HOME")))
            "eden/foo-uuid/"))))

(ert-deftest eden-request-read-test ()
  (let* ((req-params '(:stream :false
                       :model "gpt-4o-mini"
                       :messages [(:role "user" :content "user prompt\n")]))
         (req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (message "%s" (eden-request-dir req))

    ;; json files
    (eden-request-write 'request req (eden-json-encode req-params))
    (should (equal (eden-request-read 'request req) req-params))

    ;; non json files
    (eden-request-write 'prompt req "user prompt\n")
    (should (equal (eden-request-read 'prompt req) "user prompt\n"))

    ;; response.json doesn't exist
    (should-error (eden-request-read 'response req))))

(ert-deftest eden-request-assistant-content-test ()
  ;; OpenAI API
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
     (string= (eden-request-assistant-content resp) "foo assistant")))
  ;; Anthropic API (no reasoning)
  (let ((resp '(:id "msg_011Cm7DW1Gz27innVYwhuWi9"
                :type "message"
                :role "assistant"
                :model "claude-3-5-haiku-20241022"
                :content [(:type "text" :text "foo assistant")]
                :stop_reason "end_turn"
                :stop_sequence nil
                :usage (:input_tokens 11
                        :cache_creation_input_tokens 0
                        :cache_read_input_tokens 0
                        :output_tokens 149
                        :service_tier "standard"))))
    (should
     (string= (eden-request-assistant-content resp) "foo assistant")))
  ;; Anthropic API (with reasoning)
  (let ((resp '(:id "msg_011Cm7DW1Gz27innVYwhuWi9"
                :type "message"
                :role "assistant"
                :model "claude-3-7-sonnet-20250219"
                :content [(:type "thinking" :thinking "foo thinking" :signature "ErUBCkYIAxgCIkBsDgzxZPcAxTt6v8ZQGY4W+UZcPGDT/OBoV3MGcmF0Nyc05y5ZsWzsI8qhQAaLn/LT8UUP5jVtNnlpKOngUt04EgzkHx5Y+f6BcWE0MnYaDKTBoKEWUwDtyv3ksSIwRM8Yhp6o0VC9MQIzIUU0BxZKPg21gHUmLYhNtApTg6melJaahQQ2xz+h4QgD3HiTKh1y3ecKq4V0djcmYyxJ3WzUqBq/NHco5X1qHTPtzRgC")
                          (:type "text" :text "foo assistant")]
                :stop_reason "end_turn"
                :stop_sequence nil
                :usage (:input_tokens 11
                        :cache_creation_input_tokens 0
                        :cache_read_input_tokens 0
                        :output_tokens 149
                        :service_tier "standard"))))
    (should
     (string= (eden-request-assistant-content resp) "foo assistant"))))

(ert-deftest eden-request-assistant-reasoning-test ()
  (let ((resp '(:id "5b5178d0-9cca-4a8b-86f9-6971ce2c1788"
                :object "chat.completion"
                :created 1738222989
                :model "deepseek-reasoner"
                :choices [(:index 0
                           :message (:role "assistant"
                                     :content "foo assistant"
                                     :reasoning_content "foo reasoning")
                           :logprobs nil
                           :finish_reason "stop")])))
    (should
     (string= (eden-request-assistant-reasoning resp) "foo reasoning")))
  ;; Because Perplexity does it differently
  (let ((resp '(:id "35413197-0bfe-4ee3-a27b-61e6f8e0335e"
                :object "chat.completion"
                :created 1747915264
                :model "r1-1776"
                :choices [(:index 0
                           :message (:role "assistant"
                                     :content "<think>foo\nbar reasoning</think>foo assistant")
                           :finish_reason "stop")])))
    (should
     (string= (eden-request-assistant-reasoning resp) "foo\nbar reasoning")))
  ;; Because Anthropic does it differently
  (let ((resp '(:id "msg_01Lk1BzP7iZg5JSkQmSZWRhq"
                :type "message"
                :role "assistant"
                :model "claude-3-7-sonnet-20250219"
                :content
                [(:type "thinking" :thinking "foo reasoning" :signature "ErUBCkYIAxgCIkBsDgzxZPcAxTt6v8ZQGY4W+UZcPGDT/OBoV3MGcmF0Nyc05y5ZsWzsI8qhQAaLn/LT8UUP5jVtNnlpKOngUt04EgzkHx5Y+f6BcWE0MnYaDKTBoKEWUwDtyv3ksSIwRM8Yhp6o0VC9MQIzIUU0BxZKPg21gHUmLYhNtApTg6melJaahQQ2xz+h4QgD3HiTKh1y3ecKq4V0djcmYyxJ3WzUqBq/NHco5X1qHTPtzRgC")
                 (:type "text" :text "foo assistant")]
                :stop_reason "end_turn")))
    (should
     (string= (eden-request-assistant-reasoning resp) "foo reasoning"))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-request-check-test")))
(ert-deftest eden-request-check-test ()
  ;; Signal error if the request doesn't exist in `:dir'
  (should-error
   (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo")))
     (eden-request-check req)))

  ;; Signal error when an error.json file exists in req directory
  (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (eden-request-write 'error req "")
    (should-error (eden-request-check req)))

  ;; Signal error when the request is incomplete
  (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (make-directory (eden-request-dir req) 'parent)
    (should-error (eden-request-check req)))

  ;; everything ok
  (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (eden-request-write 'api req "")
    (eden-request-write 'prompt req "")
    (eden-request-write 'request req "")
    (eden-request-write 'exchanges req "")
    (eden-request-write 'response req "")
    (eden-request-write 'response-org req "")
    (with-temp-buffer
      (write-file (concat (eden-request-dir req) "timestamp-1234")))
    (should (eden-request-check req))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-request-conversation-test")))
(ert-deftest eden-request-conversation-test ()

  ;; Signal error if the request doesn't exist in `:dir'
  (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (should-error (eden-request-conversation req)))

  ;; Signal error when an error.json file exists in req directory
  (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (eden-request-write 'error req "")
    (should-error (eden-request-conversation req)))

  ;; Signal error when the request in incomplete
  (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (make-directory (eden-request-dir req) 'parent)
    (should-error (eden-request-conversation req)))

  ;; conversation with no previous messages
  (let* ((req `(:api (:service "openai"
                      :endpoint "https://api.openai.com/v1/chat/completions")
                :dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo"
                :req-params (:messages [(:role "user" :content "foo user")])
                :prompt "foo prompt\n"))
         (dir (plist-get req :dir))
         (resp '(:choices [(:message (:role "assistant"
                                      :content "foo assistant\n"
                                      :refusal nil))]))
         (resp-str (eden-json-encode resp)))
    (eden-write-request req)
    (eden-write-response resp-str resp req)
    (should
     (equal
      (eden-request-conversation `(:dir ,dir :uuid "uuid-foo"))
      [(:uuid "uuid-foo"
        :prompt "foo prompt\n"
        :response "foo assistant\n"
        :context [(:role "user" :content "foo user")
                  (:role "assistant" :content "foo assistant\n" :refusal nil)])])))

  ;; Conversation with a system message and no previous messages.
  ;; Make sure we don't include the system message in :context.
  (let* ((req `(:api (:service "openai"
                      :endpoint "https://api.openai.com/v1/chat/completions")
                :dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo"
                :req-params (:messages [(:role "system" :content "foo system\n")
                                        (:role "user" :content "foo user")])
                :prompt "foo prompt\n"
                :system-message "foo system message\n"))
         (dir (plist-get req :dir))
         (resp '(:choices [(:message (:role "assistant"
                                      :content "foo assistant\n"
                                      :refusal nil))]))
         (resp-str (eden-json-encode resp)))
    (eden-write-request req)
    (eden-write-response resp-str resp req)
    (should
     (equal
      (eden-request-conversation req)
      [(:uuid "uuid-foo"
        :prompt "foo prompt\n"
        :response "foo assistant\n"
        :context [(:role "user" :content "foo user")
                  (:role "assistant" :content "foo assistant\n" :refusal nil)])])))

  ;; conversation with previous messages
  (let* ((req `(:api (:service "openai"
                      :endpoint "https://api.openai.com/v1/chat/completions")
                :dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-baz"
                :req-params (:messages [(:role "system" :content "baz system\n")
                                        (:role "user" :content "foo user")
                                        (:role "assistant" :content "foo assistant\n")
                                        (:role "user" :content "bar user")
                                        (:role "assistant" :content "bar assistant\n")
                                        (:role "user" :content "baz user")])
                :prompt "baz prompt\n"
                :system-message "baz system message\n"
                :exchanges [(:uuid "uuid-foo"
                             :prompt "foo prompt\n"
                             :response "foo assistant\n")
                            (:uuid "uuid-bar"
                             :prompt "bar prompt\n"
                             :response "bar assistant\n"
                             :context [(:role "user" :content "foo user")
                                       (:role "assistant" :content "foo assistant\n")
                                       (:role "user" :content "bar user")
                                       (:role "assistant" :content "bar assistant\n")])]))
         (dir (plist-get req :dir))
         (resp '(:choices [(:message (:role "assistant" :content "baz assistant\n"))]))
         (resp-str (eden-json-encode resp)))
    (message "%s" (eden-request-dir req))
    (eden-write-request req)
    (eden-write-response resp-str resp req)
    (should
     (equal
      (eden-request-conversation `(:dir ,dir :uuid "uuid-baz"))
      [(:uuid "uuid-foo"
        :prompt "foo prompt\n"
        :response "foo assistant\n")
       (:uuid "uuid-bar"
        :prompt "bar prompt\n"
        :response "bar assistant\n")
       (:uuid "uuid-baz"
        :prompt "baz prompt\n"
        :response "baz assistant\n"
        :context [(:role "user" :content "foo user")
                  (:role "assistant" :content "foo assistant\n")
                  (:role "user" :content "bar user")
                  (:role "assistant" :content "bar assistant\n")
                  (:role "user" :content "baz user")
                  (:role "assistant" :content "baz assistant\n")])])))

  ;; With Deepseek reasoning content
  (let* ((req `(:api (:service "deepseek"
                      :endpoint "https://api.deepseek.com")
                :prompt "baz prompt\n"
                :dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-baz"
                :req-params (:messages [(:role "system" :content "baz system\n")
                                        (:role "user" :content "foo user")
                                        (:role "assistant"
                                         :content "foo assistant\n"
                                         :reasoning_content "foo reasoning\n")
                                        (:role "user" :content "bar user")
                                        (:role "assistant"
                                         :content "bar assistant\n"
                                         :reasoning_content "bar reasoning\n")
                                        (:role "user" :content "baz user")])
                :system-message "baz system message\n"
                :exchanges [(:uuid "uuid-foo"
                             :prompt "foo prompt\n"
                             :response "foo assistant\n"
                             :reasoning "foo reasoning\n")
                            (:uuid "uuid-bar"
                             :prompt "bar prompt\n"
                             :response "bar assistant\n"
                             :reasoning "bar reasoning\n"
                             :context [(:role "user" :content "foo user")
                                       (:role "assistant"
                                        :content "foo assistant\n"
                                        :reasoning_content "foo reasoning\n")
                                       (:role "user" :content "bar user")
                                       (:role "assistant"
                                        :content "bar assistant\n"
                                        :reasoning_content "bar reasoning\n")])]))
         (dir (plist-get req :dir))
         (resp '(:choices [(:message (:role "assistant"
                                      :content "baz assistant\n"
                                      :reasoning_content "baz reasoning\n"))]))
         (resp-str (eden-json-encode resp)))
    (eden-write-request req)
    (eden-write-response resp-str resp req)
    (should
     (equal
      (eden-request-conversation `(:dir ,dir :uuid "uuid-baz"))
      [(:uuid "uuid-foo"
        :prompt "foo prompt\n"
        :response "foo assistant\n"
        :reasoning "foo reasoning\n")
       (:uuid "uuid-bar"
        :prompt "bar prompt\n"
        :response "bar assistant\n"
        :reasoning "bar reasoning\n")
       (:uuid "uuid-baz"
        :prompt "baz prompt\n"
        :response "baz assistant\n"
        :reasoning "baz reasoning\n"
        :context [(:role "user" :content "foo user")
                  (:role "assistant"
                   :content "foo assistant\n"
                   :reasoning_content "foo reasoning\n")
                  (:role "user" :content "bar user")
                  (:role "assistant"
                   :content "bar assistant\n"
                   :reasoning_content "bar reasoning\n")
                  (:role "user" :content "baz user")
                  (:role "assistant"
                   :content "baz assistant\n"
                   :reasoning_content "baz reasoning\n")])]))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-request-conversation-path-test")))
(ert-deftest eden-request-conversation-path-test ()
  ;; nil if the request doesn't exist in `:dir'
  (should-not
   (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo")))
     (eden-request-conversation-path req)))

  ;; nil when an error.json file exists in req directory
  (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (eden-request-write 'error req "")
    (should-not (eden-request-conversation-path req)))

  ;; nil when the request in incomplete
  (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (make-directory (eden-request-dir req) 'parent)
    (should-not (eden-request-conversation-path req)))

  ;; conversation with no previous messages
  (let* ((req `(:req-params (:stream :false
                             :model "gpt-4o-mini"
                             :messages [(:role "user" :content "foo user")])
                :api (:service "openai"
                      :endpoint "https://api.openai.com/v1/chat/completions")
                :prompt "foo prompt\n"
                :dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo"))
         (dir (plist-get req :dir))
         (resp '(:id "chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8"
                 :object "chat.completion"
                 :created 1733030031
                 :model "gpt-4o-mini-2024-07-18"
                 :choices [(:index 0
                            :message (:role "assistant" :content "foo assistant" :refusal nil)
                            :logprobs nil :finish_reason "stop")]))
         (resp-str (eden-json-encode resp)))
    (eden-write-request req)
    (eden-write-response resp-str resp req)
    (should
     (equal
      (eden-request-conversation-path `(:dir ,dir :uuid "uuid-foo"))
      ["uuid-foo"])))

  ;; conversation with previous messages
  (let* ((req `(:req-params (:stream :false
                             :model "gpt-4o-mini"
                             :messages [(:role "system" :content "baz system")
                                        (:role "user" :content "foo prompt")
                                        (:role "assistant" :content "foo response")
                                        (:role "user" :content "bar prompt")
                                        (:role "assistant" :content "bar response")
                                        (:role "user" :content "baz user")])
                :api (:service "openai"
                      :endpoint "https://api.openai.com/v1/chat/completions")
                :prompt "baz user prompt\n"
                :system-message "baz system message\n"
                :exchanges [(:uuid "uuid-foo"
                             :prompt "foo prompt"
                             :response "foo response")
                            (:uuid "uuid-bar"
                             :prompt "bar prompt"
                             :response "bar response"
                             :context [(:role "user" :content "foo prompt")
                                       (:role "assistant" :content "foo response\n")
                                       (:role "user" :content "bar prompt")
                                       (:role "assistant" :content "bar response\n")])]
                :dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-baz"))
         (resp '(:id "chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8"
                 :object "chat.completion"
                 :created 1733030031
                 :model "gpt-4o-mini-2024-07-18"
                 :choices [(:index 0
                            :message (:role "assistant" :content "baz assistant" :refusal nil)
                            :logprobs nil :finish_reason "stop")]))
         (resp-str (eden-json-encode resp))
         (dir (plist-get req :dir)))
    (eden-write-request req)
    (eden-write-response resp-str resp req)
    (should
     (equal
      (eden-request-conversation-path `(:dir ,dir :uuid "uuid-baz"))
      ["uuid-foo" "uuid-bar" "uuid-baz"]))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-request-citations-test")))
(ert-deftest eden-request-citations-test ()
  ;; Signal error if the request doesn't exist in `:dir'
  (should-error
   (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo")))
     (eden-request-citations req)))

  ;; Signal error when an error.json file exists in req directory
  (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (eden-request-write 'error req "")
    (should-error (eden-request-citations req)))

  ;; Signal error when the request in incomplete
  (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (make-directory (eden-request-dir req) 'parent)
    (should-error (eden-request-citations req)))

  ;; conversation with no previous messages
  (let* ((req `(:req-params (:stream :false
                             :model "sonar"
                             :messages [(:role "user" :content "foo user")])
                :api (:service "perplexity"
                      :endpoint "https://api.perplexity.ai/chat/completions")
                :prompt "foo prompt\n"
                :dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo"))
         (dir (plist-get req :dir))
         (resp '(:id "022348c0-2af6-435f-8c60-01ef5b3b39dd"
                 :object "chat.completion"
                 :created 1735366499
                 :model "sonar"
                 :citations ["https://foo-1.com"
                             "https://foo-2.com"
                             "https://foo-3.com"]
                 :choices [(:index 0
                            :message (:role "assistant"
                                      :content "foo assistant[1][3]")
                            :finish_reason "stop")]))
         (resp-str (eden-json-encode resp)))
    (eden-write-request req)
    (eden-write-response resp-str resp req)
    (should
     (equal
      (eden-request-citations `(:dir ,dir :uuid "uuid-foo"))
      '("https://foo-1.com" "https://foo-2.com" "https://foo-3.com"))))

  ;; conversation with multiple requests mixing Perplexity and OpenAI APIs
  ;;
  ;; :exchanges we use here are not exactly what they are
  ;; in reality, when built with `eden-conversation-exchanges'
  ;; and `eden-request-conversation'.  But they conserve the important
  ;; properties and shape.  We do this because, if not the code here is
  ;; un-maintainable, especially the :context property in :exchanges
  (let* ((dir (concat (make-temp-file "eden-" t) "/"))
         ;; Perplexity request with citations in response
         (foo-req `(:req-params (:stream :false
                                 :model "sonar"
                                 :messages [(:role "user" :content "foo prompt")])
                    :api (:service "perplexity"
                          :endpoint "https://api.perplexity.ai/chat/completions")
                    :prompt "foo prompt\n"
                    :dir ,dir
                    :uuid "uuid-foo"))
         (foo-resp '(:id "022348c0-2af6-435f-8c60-01ef5b3b39dd"
                     :object "chat.completion"
                     :created 1735366499
                     :model "sonar"
                     :citations ["https://foo-1.com"
                                 "https://foo-2.com"
                                 "https://foo-3.com"]
                     :choices [(:message (:role "assistant"
                                          :content "foo response[1][3]"))]))
         (foo-resp-str (eden-json-encode foo-resp))
         ;; OpenAI web search request with citations in response
         (foo-foo-req `(:req-params (:stream :false
                                     :model "gpt-4o-search-preview"
                                     :messages [(:role "user" :content "foo-foo prompt")])
                        :exchanges [(:uuid "uuid-foo"
                                     :prompt "foo prompt"
                                     :response "foo response[1][3]"
                                     :context [(:role "user" :content "foo prompt")
                                               (:role "assistant" :content "foo response[1][3]")])]
                        :api (:service "openai"
                              :endpoint "https://api.openai.com/v1/chat/completions")
                        :prompt "foo-foo prompt\n"
                        :dir ,dir
                        :uuid "uuid-foo-foo"))
         (foo-foo-resp
          '(:id "chatcmpl-7b72b354-9256-4c2e-bacb-8fc390f91270"
            :object "chat.completion"
            :created 1748260920
            :model "gpt-4o-search-preview-2025-03-11"
            :choices
            [(:message
              (:role "assistant"
               :content "foo-foo response (https://foo-foo-1.com, https://foo-foo-2.com)"
               :refusal nil
               :annotations [(:type "url_citation"
                              :url_citation
                              (:end_index 19
                               :start_index 40
                               :title "foo-foo-1 title"
                               :url "https://foo-foo-1.com"))
                             (:type "url_citation"
                              :url_citation
                              (:end_index 42
                               :start_index 63
                               :title "foo-foo-2 title"
                               :url "https://foo-foo-2.com"))
                             ]))]))
         (foo-foo-resp-str (eden-json-encode foo-foo-resp))
         ;; bar response with no citations
         (bar-req `(:req-params (:stream :false
                                 :model "sonar"
                                 :messages [(:role "user" :content "foo prompt")
                                            (:role "assistant" :content "foo response[1][3]")
                                            (:role "user" :content "foo-foo prompt")
                                            (:role "assistant" :content "foo-foo response (https://foo-foo-1.com, https://foo-foo-2.com)")
                                            (:role "user" :content "bar prompt")])
                    :exchanges [(:uuid "uuid-foo"
                                 :prompt "foo prompt"
                                 :response "foo response[1][3]")
                                (:uuid "uuid-foo-foo"
                                 :prompt "foo-foo prompt"
                                 :response "foo-foo assistant (https://foo-foo-1.com, https://foo-foo-2.com)"
                                 :context [(:role "user" :content "foo prompt")
                                           (:role "assistant" :content "foo response[1][3]")
                                           (:role "user" :content "foo-foo prompt")
                                           (:role "assistant" :content "foo-foo response (https://foo-foo-1.com, https://foo-foo-2.com)")])]
                    :api (:service "perplexity"
                          :endpoint "https://api.perplexity.ai/chat/completions")
                    :prompt "bar prompt\n"
                    :dir ,dir
                    :uuid "uuid-bar"))
         (bar-resp '(:id "022348c0-2af6-435f-8c60-01ef5b3b39dd"
                     :object "chat.completion"
                     :created 1735366499
                     :model "sonar"
                     :choices [(:index 0
                                :message (:role "assistant"
                                          :content "bar response")
                                :finish_reason "stop")]))
         (bar-resp-str (eden-json-encode bar-resp))
         ;; Perplexity request with citations in response
         (baz-req `(:req-params (:stream :false
                                 :model "sonar"
                                 :messages [(:role "system" :content "baz system")
                                            (:role "user" :content "foo prompt")
                                            (:role "assistant" :content "foo response[1][3]")
                                            (:role "user" :content "foo-foo user")
                                            (:role "assistant" :content "foo-foo response (https://foo-foo-1.com, https://foo-foo-2.com)")
                                            (:role "user" :content "bar prompt")
                                            (:role "assistant" :content "bar response")
                                            (:role "user" :content "baz prompt")])
                    :api (:service "perplexity"
                          :endpoint "https://api.perplexity.ai/chat/completions")
                    :prompt "baz prompt\n"
                    :system-message "baz system\n"
                    :exchanges [(:uuid "uuid-foo"
                                 :prompt "foo prompt"
                                 :response "foo response[1][3]")
                                (:uuid "uuid-foo-foo"
                                 :prompt "foo-foo prompt"
                                 :response "foo-foo assistant (https://foo-foo-1.com, https://foo-foo-2.com)")
                                (:uuid "uuid-bar"
                                 :prompt "bar prompt"
                                 :response "bar response"
                                 :context [(:role "user" :content "foo prompt")
                                           (:role "assistant" :content "foo response[1][3]")
                                           (:role "user" :content "foo-foo prompt")
                                           (:role "assistant" :content "foo-foo response (https://foo-foo-1.com, https://foo-foo-2.com)")
                                           (:role "user" :content "bar prompt")
                                           (:role "assistant" :content "bar response")])]
                    :dir ,dir
                    :uuid "uuid-baz"))
         (baz-resp '(:id "022348c0-2af6-435f-8c60-01ef5b3b39dd"
                     :object "chat.completion"
                     :created 1735366499
                     :model "sonar"
                     :citations ["https://baz-1.com" "https://baz-2.com"]
                     :choices [(:index 0
                                :message (:role "assistant"
                                          :content "baz assistant[1][2]")
                                :finish_reason "stop")]))
         (baz-resp-str (eden-json-encode baz-resp)))
    (eden-write-request foo-req)
    (eden-write-response foo-resp-str foo-resp foo-req)
    (eden-write-request foo-foo-req)
    (eden-write-response foo-foo-resp-str foo-foo-resp foo-foo-req)
    (eden-write-request bar-req)
    (eden-write-response bar-resp-str bar-resp bar-req)
    (eden-write-request baz-req)
    (eden-write-response baz-resp-str baz-resp baz-req)
    (should
     (equal
      (eden-request-citations `(:dir ,dir :uuid "uuid-baz"))
      '("https://foo-1.com"
        "https://foo-2.com"
        "https://foo-3.com"
        "https://foo-foo-1.com"
        "https://foo-foo-2.com"
        "https://baz-1.com"
        "https://baz-2.com")))))

(ert-deftest eden-request-timestamp-test ()
  ;; `eden-request-write' function uses `time-to-seconds' function
  ;; to write the timestamp file of a request so to be able to test
  ;; `eden-request-timestamp', we temporary redefine `time-to-seconds'
  ;; to return a constant number similar to the one it would normally return.

  (cl-letf (((symbol-function 'time-to-seconds)
             (lambda () 1733921715.2331347)))
    (let ((req '(:dir "/tmp/eden/" :uuid "uuid-foo")))
      (eden-request-write 'timestamp req "")
      (sleep-for 0.1)
      (should
       (equal (eden-request-timestamp req) 1733921715.2331347)))))

(ert-deftest eden-request-date-test ()
  ;; `eden-request-write' function uses `time-to-seconds' function
  ;; to write the timestamp file of a request so to be able to test
  ;; `eden-request-date', we temporary redefine `time-to-seconds'
  ;; to return a constant number similar to the one it would normally return.
  (cl-letf (((symbol-function 'time-to-seconds)
             (lambda () 1733921715.2331347)))
    (let ((req '(:dir "/tmp/eden/" :uuid "uuid-foo")))
      (eden-request-write 'timestamp req "")
      (sleep-for 0.1)
      (should
       (equal (eden-request-date req) "[2024-12-11 Wed]")))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-write-request-test")))
(ert-deftest eden-write-request-test ()
  (let* ((req `(:req-params (:stream :false
                             :model "gpt-4o-mini"
                             :messages [(:role "user" :content "user prompt\n")])
                :api (:service "openai"
                      :endpoint "https://api.openai.com/v1/chat/completions")
                :prompt "user prompt\n"
                :dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (eden-write-request req)
    (should (equal (eden-request-read 'request req)
                   (plist-get req :req-params)))
    (should (equal (eden-request-read 'api req)
                   (plist-get req :api)))
    (should (equal (eden-request-read 'prompt req)
                   (plist-get req :prompt)))
    (should (equal (eden-request-read 'system-message req) ""))
    (should-not (eden-request-read 'exchanges req))
    (should
     (= (length (directory-files (eden-request-dir req) nil "timestamp-"))
        1)))
  (let* ((req `(:req-params (:stream :false
                             :model "gpt-4o-mini"
                             :messages [(:role "system" :content "baz system\n")
                                        (:role "user" :content "foo user")
                                        (:role "assistant" :content "foo response")
                                        (:role "user" :content "bar prompt")
                                        (:role "assistant" :content "bar response")
                                        (:role "user" :content "baz prompt\n")])
                :api (:service "openai"
                      :endpoint "https://api.openai.com/v1/chat/completions")
                :prompt "foo prompt\n"
                :system-message "foo system\n"
                :exchanges [(:uuid "uuid-foo"
                             :prompt "foo prompt"
                             :response "foo response")
                            (:uuid "uuid-bar"
                             :prompt "bar prompt"
                             :response "bar response"
                             :context [(:role "user" :content "foo prompt")
                                       (:role "assistant" :content "foo response")
                                       (:role "user" :content "bar prompt")
                                       (:role "assistant" :content "bar response")])]
                :dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (eden-write-request req)
    (should (equal (eden-request-read 'request req)
                   (plist-get req :req-params)))
    (should (equal (eden-request-read 'api req)
                   (plist-get req :api)))
    (should (equal (eden-request-read 'prompt req)
                   (plist-get req :prompt)))
    (should (equal (eden-request-read 'system-message req)
                   (plist-get req :system-message)))
    (should (equal (eden-request-read 'exchanges req)
                   (plist-get req :exchanges)))
    (should
     (= (length (directory-files (eden-request-dir req) nil "timestamp-"))
        1))))

(defvar eden-api-key-someservice-name)

(ert-deftest eden-request-command-test ()
  ;; OpenAI-compatible API
  (let* ((eden-api-key-someservice-name "secret-api-key")
         (req '(:api (:service "someservice-name"
                      :endpoint "https://someservice-endpoint")
                :dir "/tmp/eden/"
                :uuid "uuid-foo"))
         (command-fmt (concat "curl -s -X POST https://someservice-endpoint "
                              "-H 'Authorization: Bearer %s' "
                              "-H 'Content-Type: application/json' -d @%s"))
         (request-file (eden-request-file 'request req)))
    (should
     (equal (eden-request-command req)
            (list
             (format command-fmt eden-api-key-someservice-name request-file)
             (format command-fmt "<api-key>" request-file)))))
  ;; Anthropic API
  (let* ((eden-api-key-anthropic "secret-api-key")
         (req '(:api (:service "anthropic"
                      :endpoint "https://someservice-endpoint"
                      :anthropic-version "2023-06-01")
                :dir "/tmp/eden/"
                :uuid "uuid-bar"))
         (command-fmt (concat "curl -s -X POST https://someservice-endpoint "
                              "-H 'x-api-key: %s' "
                              "-H 'anthropic-version: 2023-06-01' "
                              "-H 'Content-Type: application/json' -d @%s"))
         (request-file (eden-request-file 'request req)))
    (should
     (equal (eden-request-command req)
            (list
             (format command-fmt eden-api-key-anthropic request-file)
             (format command-fmt "<api-key>" request-file))))))

(ert-deftest eden-org-replace-perplexity-citations-test ()
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

* citation on a heading [4] and another array ~arr[1]~

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
      (eden-org-replace-perplexity-citations org-str citations)
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

* citation on a heading [[[https://baz.org][4]]] and another array ~arr[1]~

We access elements in a python array in a code block:

#+BEGIN_SRC python
arr[1][2]
arr[0]
#+END_SRC
"))))

(ert-deftest eden-write-response-test ()
  (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo"))
         (resp '(:id "chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8"
                 :object "chat.completion"
                 :created 1733030031
                 :model "gpt-4o-mini-2024-07-18"
                 :choices [(:index 0
                            :message (:role "assistant" :content "### foo assistant\n" :refusal nil)
                            :logprobs nil :finish_reason "stop")]))
         (resp-str (eden-json-encode resp)))
    (eden-write-response resp-str resp req)
    (should (equal (eden-request-read 'response req) resp))
    (should (equal (eden-request-read 'response-org req) "*** foo assistant\n")))
  ;; response from perplexity.ai with citations array
  (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo"))
         (resp '(:id "022348c0-2af6-435f-8c60-01ef5b3b39dd"
                 :object "chat.completion"
                 :created 1735366499
                 :model "sonar"
                 :citations ["https://foo.com"
                             "https://bar.com"
                             "https://baz.com"]
                 :choices [(:index 0
                            :message (:role "assistant"
                                      :content "### foo assistant\n\nfoo citation[1]\n\nbar baz citations[2][3]\n")
                            :finish_reason "stop")]))
         (resp-str (eden-json-encode resp)))
    (eden-write-response resp-str resp req)
    (should (equal (eden-request-read 'response req) resp))
    (should (equal (eden-request-read 'response-org req)
                   "*** foo assistant\n\nfoo citation[[[https://foo.com][1]]]\n\nbar baz citations[[[https://bar.com][2]]][[[https://baz.com][3]]]\n")))
  ;; response from perplexity.ai with reasoning content
  (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo"))
         (resp '(:id "35413197-0bfe-4ee3-a27b-61e6f8e0335e"
                 :object "chat.completion"
                 :created 1747915264
                 :model "r1-1776"
                 :choices [(:index 0
                            :message (:role "assistant"
                                      :content "<think>### foo reasoning\n</think>\n### foo assistant\n")
                            :finish_reason "stop")]))
         (resp-str (eden-json-encode resp)))
    (eden-write-response resp-str resp req)
    (should (equal (eden-request-read 'response req) resp))
    (should (equal (eden-request-read 'response-org req) "*** foo assistant\n"))
    (should (equal (eden-request-read 'reasoning req) "*** foo reasoning\n")))
  ;; response from deepseek.com with reasoning content
  (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo"))
         (resp '(:id "5b5178d0-9cca-4a8b-86f9-6971ce2c1788"
                 :object "chat.completion"
                 :created 1738222989
                 :model "deepseek-reasoner"
                 :choices [(:index 0
                            :message (:role "assistant"
                                      :content "### foo assistant\n"
                                      :reasoning_content "### foo reasoning\n")
                            :logprobs nil
                            :finish_reason "stop")]))
         (resp-str (eden-json-encode resp)))
    (eden-write-response resp-str resp req)
    (should (equal (eden-request-read 'response req) resp))
    (should (equal (eden-request-read 'response-org req) "*** foo assistant\n"))
    (should (equal (eden-request-read 'reasoning req) "*** foo reasoning\n"))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-sentinel-test")))
(ert-deftest eden-sentinel-test ()
  ;; we let-bind requests, callbacks and infos with the variables
  ;; `-req', `-callback' to be sure that `eden-sentinel' macro
  ;; doesn't rely on `req', `callback' variables to be defined
  ;; during its expansion.

  ;; Throw error when process buffer is killed
  (let* ((-req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo"
                 :req-params (:foo "bar")))
         ;; we are testing that we throw an error if process has
         ;; been killed for some reason.  We wrapped the sentinel
         ;; generated by `eden-sentinel' in order to kill
         ;; the process buffer when the sentinel is called.
         (sentinel-wrapped
          (lambda (process event)
            (kill-buffer (process-buffer process))
            (funcall (eden-sentinel -req nil nil) process event)))
         (buff-name (generate-new-buffer-name "*ai-test*")))
    (should-error
     (let ((debug-on-error t))
       (eden-test-echo-resp "" buff-name sentinel-wrapped)
       (sleep-for 0.2))
     :type 'eden-error-process-buffer)
    (should-not (get-buffer buff-name))
    (message "%s" (eden-request-file 'error -req))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (eden-request-file 'error -req))
        (eden-json-read))
      `(:type "eden-error-process-buffer"
        :message "The process buffer got killed while processing the request"
        :directory ,(eden-request-dir -req)
        :request ,(plist-get -req :req-params)))))


  ;; Throw error when process receives an event different from "finished\n"
  ;; Here we test it with "killed\n" event
  (let* ((-req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo"
                 :req-params (:foo "bar")))
         (buff-name (generate-new-buffer-name "*ai-test*")))
    ;; We insert text in `buff-name' to test that we log its content
    (with-current-buffer (get-buffer-create buff-name)
      (insert "foo bar baz"))
    (should-error
     (let ((debug-on-error t))
       (kill-process
        (eden-test-echo-resp "" buff-name (eden-sentinel -req nil nil) 1))
       (sleep-for 0.2))
     :type 'eden-error-process)
    (should-not (get-buffer buff-name))
    (should
     (equal (eden-request-read 'error -req)
            `(:type "eden-error-process"
              :message "The process did not finished correctly"
              :directory ,(eden-request-dir -req)
              :request ,(plist-get -req :req-params)
              :process-event "killed\n"
              :process-buffer-content "foo bar baz"))))

  ;; Here we test it with "interrupt\n" event
  (let* ((-req `(:req-params (:foo "bar")
                 :dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo"))
         (buff-name (generate-new-buffer-name "*ai-test*")))
    ;; We insert text in `buff-name' to test that we log its content
    (with-current-buffer (get-buffer-create buff-name)
      (insert "foo bar baz"))
    (should-error
     (let ((debug-on-error t))
       (interrupt-process
        (eden-test-echo-resp "" buff-name (eden-sentinel -req nil nil) 1))
       (sleep-for 0.2))
     :type 'eden-error-process)
    (should-not (get-buffer buff-name))
    (should
     (equal (eden-request-read 'error -req)
            `(:type "eden-error-process"
              :message "The process did not finished correctly"
              :directory ,(eden-request-dir -req)
              :request ,(plist-get -req :req-params)
              :process-event "interrupt\n"
              :process-buffer-content "foo bar baz"))))

  ;; Throw error when reading json response in process buffer
  (let* ((-req `(:req-params (:foo "bar")
                 :dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo"))
         (buff-name (generate-new-buffer-name "*ai-test*")))
    (should-error
     (let ((debug-on-error t))
       (eden-test-echo-resp "not valid json" buff-name
                            (eden-sentinel -req nil nil))
       (sleep-for 0.2))
     :type 'eden-error-json-read)
    (should-not (get-buffer buff-name))
    (should
     (equal (eden-request-read 'error -req)
            `(:type "eden-error-json-read"
              :message "Error while parsing JSON in process buffer"
              :directory ,(eden-request-dir -req)
              :request ,(plist-get -req :req-params)
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
         (-req `(:req-params (:foo "bar")
                 :dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo"))
         (buff-name (generate-new-buffer-name "*ai-test*")))
    (should-error
     (let ((debug-on-error t))
       (eden-test-echo-resp resp-str buff-name (eden-sentinel -req nil nil))
       (sleep-for 0.2))
     :type 'eden-error-api)
    (should-not (get-buffer buff-name))
    (should
     (equal
      (eden-request-read 'error -req)
      `(:type "eden-error-api"
        :message "API error"
        :directory ,(eden-request-dir -req)
        :request ,(plist-get -req :req-params)
        :error (:message "Incorrect API key provided: eesk-pro***WmEA. You can find your API key at https://platform.openai.com/account/api-keys."
                :type "invalid_request_error"
                :param nil
                :code "invalid_api_key")))))


  ;; Wrong callback function
  (let* ((resp-str "{\"choices\": [{\"message\": {\"role\": \"assistant\", \"content\": \"foo bar baz\"}}]}")
         (-req `(:req-params (:foo "bar")
                 :dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo"))
         (buff-name (generate-new-buffer-name "*ai-test*")))
    (should-error
     (let ((debug-on-error t))
       (eden-test-echo-resp
        resp-str buff-name
        (eden-sentinel -req 'callback-not-a-function nil))
       (sleep-for 0.2))
     :type 'eden-error-callback)
    (should-not (get-buffer buff-name))
    (should
     (equal
      (eden-request-read 'error -req)
      `(:type "eden-error-callback"
        :message "Error while calling callback function in sentinel"
        :directory ,(eden-request-dir -req)
        :request ,(plist-get -req :req-params)
        :error ["void-function" "callback-not-a-function"]))))


  ;; Test that `callback-error' is called when an error occurs
  ;; in the sentinel call.  To do so we send the "killed\n" event
  ;; to the process which triggers an error.
  ;;
  ;; 1) First we test that we throw a 'eden-error-callback-error error
  ;;    when `callback-error' itself throws an error
  (let* ((-req `(:req-params (:foo "bar")
                 :dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo"))
         (buff-name (generate-new-buffer-name "*ai-test*")))
    (should-error
     (let ((debug-on-error t))
       (kill-process
        (eden-test-echo-resp
         "" buff-name
         (eden-sentinel -req nil 'callback-error-not-a-function)
         1))
       (sleep-for 0.2))
     :type 'eden-error-callback-error)
    (let* ((error.json (eden-request-read 'error -req))
           (original-error (plist-get error.json :original-error)))
      (should (string= (plist-get original-error :type)
                       "eden-error-process"))
      (should
       (equal
        (seq-subseq error.json 0 10)
        `(:type "eden-error-callback-error"
          :message "Error while calling callback-error function when signaling an error in sentinel"
          :directory ,(eden-request-dir -req)
          :request ,(plist-get -req :req-params)
          :error ["void-function" "callback-error-not-a-function"])))))
  ;; 2) Then we test that `callback-error' is called correctly
  (let* ((-req `(:req-params (:foo "bar")
                 :dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo"
                 :info (:foo (:bar "baz"))))
         (in-callback-error)
         (-callback-error (lambda (req err)
                            (setq in-callback-error
                                  (list :req req
                                        :error-type (plist-get err :type)
                                        :info (plist-get req :info)))))
         (buff-name (generate-new-buffer-name "*ai-test*")))
    (should-error
     (let ((debug-on-error t))
       (kill-process
        (eden-test-echo-resp
         "" buff-name (eden-sentinel -req nil -callback-error) 1))
       (sleep-for 0.2))
     :type 'eden-error-process)
    (should
     (string=
      (plist-get (eden-request-read 'error -req) :type)
      "eden-error-process"))
    (should
     (equal in-callback-error
            (list :req -req
                  :error-type "eden-error-process"
                  :info '(:foo (:bar "baz"))))))


  ;; everything's ok
  (let* ((resp '(:choices [(:message (:role "assistant" :content "foo bar baz"))]))
         (resp-str (eden-json-encode resp))
         (-req `(:req-params (:stream :false
                              :model "gpt-4o-mini"
                              :messages [(:role "user" :content "foo bar baz")])
                 :dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo"
                 :info (:foo (:bar "baz"))))
         (in-callback)
         (-callback (lambda (req resp)
                      (setq in-callback
                            (list :req req
                                  :resp resp
                                  :info (plist-get req :info)))))
         (buff-name (generate-new-buffer-name "*ai-test*")))
    (let ((debug-on-error t))
      (eden-test-echo-resp resp-str buff-name (eden-sentinel -req -callback nil))
      (sleep-for 0.2))
    (should-not (get-buffer buff-name))
    (should (equal (eden-request-read 'response -req) resp))
    (should (equal in-callback
                   `(:req ,-req :resp ,resp :info (:foo (:bar "baz")))))))

;; Because we need it to be dynamic and declared before we
;; let-bind it and use it in `eden-request-command'.
;; Normally this variable is defined in `eden-request-command'
;; the first time we call it and ask the user to enter his
;; gpg passphrase to get the api key from ~/.authinfo.gpg file.
;; See `eden-api-key-symbol'.

;;; AI Assistant UI
;;;; Utils

(ert-deftest eden-org-to-markdown-test ()
  (should
   (string=
    (eden-org-to-markdown "* h1

#+BEGIN_SRC python
foo = 1
#+END_SRC

#+BEGIN_EXAMPLE
foo bar baz
#+END_EXAMPLE

** h2")
    "# h1

```python
foo = 1

```

```
foo bar baz

```


## h2"))

  )

(ert-deftest eden-org-demote-test ()
  ;; headline top level is 1
  (should
   (string=
    (eden-org-demote "foo-1

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
    (eden-org-demote "foo-1

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
    (eden-org-demote "foo-1

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
    (eden-org-demote "foo-1

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
    (eden-org-demote "foo-1

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
    (eden-org-demote "foo-1

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

(ert-deftest eden-assoc-in-test ()
  (let ((map (make-hash-table :test 'equal)))
    (puthash "foo" (make-hash-table :test 'equal) map)
    (puthash "bar" (make-hash-table :test 'equal) (gethash "foo" map))
    (puthash "baz-1" 1 (gethash "bar" (gethash "foo" map)))

    (eden-assoc-in map '["foo" "bar" "baz-1"] 2)
    (eden-assoc-in map '["foo" "bar" "baz-2"] 3)
    (eden-assoc-in map '["baz-3" "baz-4"] 4)

    (should (= (gethash "baz-1" (gethash "bar" (gethash "foo" map))) 2))
    (should (= (gethash "baz-2" (gethash "bar" (gethash "foo" map))) 3))
    (should (= (gethash "baz-4" (gethash "baz-3" map)) 4))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-dir-set-test")))
(ert-deftest eden-dir-set-test ()
  ;; While Eden is already handling a request
  (cl-letf (((symbol-function 'eden-running-p)
             (lambda () t)))
    (let ((eden-dir "/tmp/foo/"))
      (should-error (eden-dir-set "/tmp/bar/"))

      (eden-dir-set "/tmp/foo/")
      (should (equal eden-dir "/tmp/foo/"))))

  ;; While Eden is idle
  (cl-letf (((symbol-function 'eden-running-p)
             (lambda () nil)))
    ;; The following expressions are order dependent, as we iteratively
    ;; update `eden-dir' value with `eden-dir-set'.
    (let ((eden-dir "/tmp/eden/"))
      (should-error (eden-dir-set 'not-a-dir))

      (eden-dir-set "/tmp/eden/")
      (should (string= eden-dir "/tmp/eden/"))

      (eden-dir-set "/tmp/new-eden")
      (should (string= eden-dir "/tmp/new-eden/"))

      ;; Check that if new dir is different from :dir of the current
      ;; conversation, we stop the current conversation by setting
      ;; `eden-conversation-id' to nil
      (let ((eden-conversations
             '(("conversation-id-foo" . (:title "foo title"
                                         :dir "/tmp/new-eden/"
                                         :last-req-uuid "foo-req-uuid"))))
            (eden-conversation-id "conversation-id-foo"))
        (eden-dir-set "/tmp/new-new-eden/")
        (should (string= eden-dir "/tmp/new-new-eden/"))
        ;; id has been reset to nil
        (should-not eden-conversation-id))
      (let ((eden-conversations
             '(("conversation-id-foo" . (:title "foo title"
                                         :dir "/tmp/new-eden/"
                                         :last-req-uuid "foo-req-uuid"))))
            (eden-conversation-id "conversation-id-foo"))
        (eden-dir-set "/tmp/new-eden/")
        (should (string= eden-dir "/tmp/new-eden/"))
        ;; id has been left unchanged
        (should (string= eden-conversation-id "conversation-id-foo")))

      ;; Check that we also update:
      ;; - request history and
      ;; - prompt history state.
      (let* ((new-dir (concat (make-temp-file "eden-" t) "/"))
             (req-foo `(:dir ,new-dir :uuid "foo"))
             (req-bar `(:dir ,new-dir :uuid "bar"))
             (req-baz `(:dir ,new-dir :uuid "baz"))
             eden-request-history
             eden-prompt-history-state)
        (message "new-dir: %S" new-dir)
        (eden-request-write 'timestamp req-foo "")
        (eden-request-write 'timestamp req-bar "")
        (eden-request-write 'timestamp req-baz "")

        (eden-dir-set new-dir)
        (should (equal eden-dir new-dir))

        (should (equal eden-request-history '("baz" "bar" "foo")))
        (should (equal eden-prompt-history-state
                       [("baz" "bar" "foo") nil nil]))))))

;;;; Prompt and Request history

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-request-history-build-test")))
(ert-deftest eden-request-history-build-test ()
  ;; Directory doesn't exist
  (let* ((dir (concat (make-temp-file "eden-" t) "/")))
    (delete-directory dir)
    (should-not (eden-request-history-build dir)))

  ;; Directory exists
  (let* ((dir (concat (make-temp-file "eden-" t) "/"))
         (req-0 `(:dir ,dir :uuid "uuid-0"))
         (req-1 `(:dir ,dir :uuid "uuid-1"))
         (req-2 `(:dir ,dir :uuid "uuid-2"))
         (req-3 `(:dir ,dir :uuid "uuid-3")))
    (eden-request-write 'timestamp req-0 "")
    (eden-request-write 'timestamp req-1 "")
    (eden-request-write 'timestamp req-2 "")
    (eden-request-write 'timestamp req-3 "")
    (should
     (equal (eden-request-history-build dir)
            '("uuid-3" "uuid-2" "uuid-1" "uuid-0")))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-history-update-test")))
(ert-deftest eden-history-update-test ()
  (let* ((eden-request-history)      ;; variable tested
         (eden-prompt-history-state) ;; variable tested
         (dir (concat (make-temp-file "eden-" t) "/"))
         (req-0 `(:dir ,dir :uuid "uuid-0"))
         (req-1 `(:dir ,dir :uuid "uuid-1"))
         (req-2 `(:dir ,dir :uuid "uuid-2")))
    (eden-request-write 'timestamp req-0 "")
    (eden-request-write 'timestamp req-1 "")
    (eden-request-write 'timestamp req-2 "")

    (eden-history-update :dir dir)

    (should
     (equal eden-request-history '("uuid-2" "uuid-1" "uuid-0")))
    (should
     (equal eden-prompt-history-state
            [("uuid-2" "uuid-1" "uuid-0") nil nil]))

    (eden-history-update :new-req-uuid "uuid-3")

    (should
     (equal eden-request-history '("uuid-3" "uuid-2" "uuid-1" "uuid-0")))
    (should
     (equal eden-prompt-history-state
            [("uuid-3" "uuid-2" "uuid-1" "uuid-0") nil nil]))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-prompt-current-req-uuid-test")))
(ert-deftest eden-prompt-current-req-uuid-test ()
  (let ((prompt-history [nil nil nil]))
    (should-not (eden-prompt-current-req-uuid prompt-history)))
  (let ((prompt-history [nil (:prompt "scratch prompt") nil]))
    (should-not (eden-prompt-current-req-uuid prompt-history)))
  (let* ((prompt-history [nil "foo-uuid" nil]))
    (should
     (string= (eden-prompt-current-req-uuid prompt-history)
              "foo-uuid"))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-prompt-current-test")))
(ert-deftest eden-prompt-current-test ()
  (let ((dir (concat (make-temp-file "eden-" t) "/"))
        (prompt-history [nil nil nil]))
    (should-not (eden-prompt-current dir prompt-history)))
  (let ((dir (concat (make-temp-file "eden-" t) "/"))
        (prompt-history [nil (:prompt "scratch prompt") nil]))
    (should (string= (eden-prompt-current dir prompt-history)
                     "scratch prompt")))
  (let* ((dir (concat (make-temp-file "eden-" t) "/"))
         (req `(:dir ,dir :uuid "foo-uuid"))
         (prompt-history [nil "foo-uuid" nil]))
    (eden-request-write 'prompt req "foo prompt\n")
    (should (string= (eden-prompt-current dir prompt-history)
                     "foo prompt\n"))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-prompt-history-previous/next")))
(ert-deftest eden-prompt-history-previous/next ()
  ;; For some reason, this test failed if run twice without
  ;; re-evaluating it.

  ;; default usage
  (let ((prompt-history [("foo" "bar" "baz") nil nil]))
    (eden-prompt-history-previous prompt-history)
    (should (equal prompt-history [("bar" "baz") "foo" nil])))
  (let ((prompt-history [("bar" "baz") "foo" nil]))
    (eden-prompt-history-previous prompt-history)
    (should (equal prompt-history [("baz") "bar" ("foo")])))
  (let ((prompt-history [("baz") "bar" ("foo")]))
    (eden-prompt-history-previous prompt-history)
    (should (equal prompt-history [nil "baz" ("bar" "foo")])))
  (let ((prompt-history [nil "baz" ("bar" "foo")]))
    (eden-prompt-history-previous prompt-history)
    (should (equal prompt-history [nil "baz" ("bar" "foo")])))

  (let ((prompt-history [nil "baz" ("bar" "foo")]))
    (eden-prompt-history-next prompt-history)
    (should (equal prompt-history [("baz") "bar" ("foo")])))
  (let ((prompt-history [("baz") "bar" ("foo")]))
    (eden-prompt-history-next prompt-history)
    (should (equal prompt-history [("bar" "baz") "foo" nil])))
  (let ((prompt-history [("bar" "baz") "foo" nil]))
    (eden-prompt-history-next prompt-history)
    (should (equal prompt-history [("bar" "baz") "foo" nil])))

  ;; signal error if both `prompt' and `discard-current' optional
  ;; arguments are non nil
  (let ((prompt-history [("foo" "bar" "baz") nil nil]))
    (eden-prompt-history-previous prompt-history )
    (should-error
     (eden-prompt-history-previous
      [("foo" "bar" "baz") nil nil] '(:prompt "scratch prompt") 'discard-current)))

  (let ((prompt-history [nil "baz" ("bar" "foo")]))
    (should-error
     (eden-prompt-history-next
      prompt-history '(:prompt "scratch prompt") 'discard-current)))

  ;; with `prompt' optional argument
  (let ((prompt-history [("foo" "bar" "baz") nil nil]))
    (eden-prompt-history-previous prompt-history '(:prompt "scratch prompt"))
    (should (equal prompt-history [("bar" "baz") "foo" ((:prompt "scratch prompt"))])))
  (let ((prompt-history [("bar" "baz") "foo" nil]))
    (eden-prompt-history-previous prompt-history '(:prompt "scratch prompt"))
    (should (equal prompt-history [("baz") "bar" ((:prompt "scratch prompt") "foo")])))
  (let ((prompt-history [("baz") "bar" ("foo")]))
    (eden-prompt-history-previous prompt-history '(:prompt "scratch prompt"))
    (should (equal prompt-history [nil "baz" ((:prompt "scratch prompt") "bar" "foo")])))
  (let ((prompt-history [nil "baz" ("bar" "foo")]))
    (eden-prompt-history-previous prompt-history '(:prompt "scratch prompt"))
    (should (equal prompt-history [nil "baz" ("bar" "foo")])))


  (let ((prompt-history [nil "baz" ("bar" "foo")]))
    (eden-prompt-history-next prompt-history '(:prompt "scratch prompt"))
    (should (equal prompt-history [((:prompt "scratch prompt") "baz") "bar" ("foo")])))
  (let ((prompt-history [("baz") "bar" ("foo")]))
    (eden-prompt-history-next prompt-history '(:prompt "scratch prompt"))
    (should (equal prompt-history [((:prompt "scratch prompt") "bar" "baz") "foo" nil])))
  (let ((prompt-history [("bar" "baz") "foo" nil]))
    (eden-prompt-history-next prompt-history '(:prompt "scratch prompt"))
    (should (equal prompt-history [("bar" "baz") "foo" nil])))


  ;; with `discard-current' optional argument)
  (let ((prompt-history [("foo" "bar") "to-be-discarded" nil]))
    (eden-prompt-history-previous prompt-history nil 'discard-current)
    (should (equal prompt-history [("bar") "foo" nil])))
  (let ((prompt-history [("bar") "to-be-discarded" ("foo")]))
    (eden-prompt-history-previous prompt-history nil 'discard-current)
    (should (equal prompt-history [nil "bar" ("foo")])))

  (let ((prompt-history [nil "to-be-discarded" ("bar" "foo")]))
    (eden-prompt-history-next prompt-history nil 'discard-current)
    (should (equal prompt-history [nil "bar" ("foo")])))

  (let ((prompt-history [("bar") "to-be-discarded" ("foo")]))
    (eden-prompt-history-next prompt-history nil 'discard-current)
    (should (equal prompt-history [("bar") "foo" nil]))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-prompt-discard-current-p-test")))
(ert-deftest eden-prompt-discard-current-p-test ()
  ;; We must discard a request's UUID if the request
  ;; can't be found in dir.
  (let* ((dir (concat (make-temp-file "eden-" t) "/"))
         (prompt-history [nil "foo-uuid" nil]))
    (should (eden-prompt-discard-current-p dir prompt-history)))

  ;; We don't discard current "prompt ref" if it's request's UUID
  ;; of an existing request in dir or if it's prompt not attached
  ;; to any request which we temporally keep in the history.
  (let* ((dir (concat (make-temp-file "eden-" t) "/"))
         (prompt-history [nil (:prompt "foo prompt") nil]))
    (should-not (eden-prompt-discard-current-p dir prompt-history)))
  (let* ((dir (concat (make-temp-file "eden-" t) "/"))
         (prompt-history [nil "bar-uuid" nil])
         (req-bar `(:dir ,dir :uuid "bar-uuid")))
    (eden-request-write 'prompt req-bar "bar prompt\n")
    (should-not (eden-prompt-discard-current-p dir prompt-history))))

;;;; Conversations

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-conversation-exists-p-test")))
(ert-deftest eden-conversation-exists-p-test ()
  (let ((eden-conversations nil))
    (should-not (eden-conversation-exists-p "not-a-conversation")))
  (let ((eden-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :dir "/tmp/eden/" :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :dir "/tmp/eden/" :last-req-uuid "bar-req-uuid")))))
    (should (eden-conversation-exists-p "conversation-id-foo"))
    (should (eden-conversation-exists-p "conversation-id-bar"))
    (should-not (eden-conversation-exists-p "not-a-conversation"))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-conversation-with-title-exists-p-test")))
(ert-deftest eden-conversation-with-title-exists-p-test ()
  (let ((eden-conversations nil))
    (should-not (eden-conversation-with-title-exists-p "bar title")))
  (let ((eden-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :dir "/tmp/eden/" :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :dir "/tmp/eden/" :last-req-uuid "bar-req-uuid"))
           ("conversation-id-baz" .
            (:title "baz title" :dir "/tmp/eden/" :last-req-uuid "baz-req-uuid")))))
    (should (eden-conversation-with-title-exists-p "bar title"))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-conversation-add-test")))
(ert-deftest eden-conversation-add-test ()

  ;; Titles must be unique
  (let* ((dir (concat (make-temp-file "eden-" t) "/"))
         (eden-conversations
          `(("conversation-id-foo" . (:title "foo title"
                                      :dir ,dir
                                      :last-req-uuid nil)))))
    (should-error (eden-conversation-add dir "foo title")))

  ;; add a new conversations
  (let ((eden-conversations)   ;; variable tested
        (eden-conversation-id) ;; variable tested
        (dir (concat (make-temp-file "eden-" t) "/")))
    (cl-letf (((symbol-function 'eden-uuid)
               (lambda nil "conversation-id-foo")))
      (eden-conversation-add dir "foo title")
      (should
       (equal eden-conversations
              `(("conversation-id-foo" . (:title "foo title"
                                          :dir ,dir
                                          :last-req-uuid nil)))))
      (should (string= eden-conversation-id "conversation-id-foo"))))
  (let* ((dir (concat (make-temp-file "eden-" t) "/"))
         (eden-conversation-id) ;; variable tested
         (eden-conversations    ;; variable tested
          `(("conversation-id-foo" . (:title "foo title"
                                      :dir ,dir
                                      :last-req-uuid nil)))))
    (cl-letf (((symbol-function 'eden-uuid)
               (lambda nil "conversation-id-bar")))
      (eden-conversation-add dir "bar title")
      (should
       (seq-set-equal-p
        (mapcar #'car eden-conversations)
        '("conversation-id-foo" "conversation-id-bar")))
      (should
       (equal (map-elt eden-conversations "conversation-id-bar")
              `(:title "bar title" :dir ,dir :last-req-uuid nil)))
      (should (string= eden-conversation-id "conversation-id-bar"))))

  ;; error - req associated with req-uuid doesn't exist in dir
  (let ((eden-conversations)
        (dir (concat (make-temp-file "eden-" t) "/")))
    (should-error (eden-conversation-add dir "foo title" "foo-req-uuid")))

  ;; error -  when an error.json file exists in req directory
  (let* ((eden-conversations)
         (dir (concat (make-temp-file "eden-" t) "/"))
         (req `(:dir ,dir :uuid "foo-req-uuid")))
    (eden-request-write 'error req "")
    (should-error (eden-conversation-add dir "foo title" "foo-req-uuid")))

  ;; error - when req associated with req-uuid is in incomplete
  (let* ((eden-conversations)
         (dir (concat (make-temp-file "eden-" t) "/"))
         (req `(:dir ,dir :uuid "foo-req-uuid")))
    (make-directory (eden-request-dir req) 'parent)
    (should-error (eden-conversation-add dir "foo title" "foo-req-uuid")))

  ;; we can continue multiple different conversations from the same req-uuid
  (let* ((eden-conversations)   ;; variable tested
         (eden-conversation-id) ;; variable tested
         (dir (concat (make-temp-file "eden-" t) "/"))
         (foo-req
          `(:req-params (:messages [(:role "user" :content "foo-req")])
            :prompt "foo-req"
            :dir ,dir
            :uuid "foo-req-uuid"))
         ;; we use the same response for two different requests
         ;; because we just need them to exist in the request directory
         (resp '(:choices [(:message (:role "assistant" :content "assistant"))]))
         (resp-str (eden-json-encode resp)))
    (eden-write-request foo-req)
    (eden-write-response resp-str resp foo-req)

    (let ((n 0))
      (cl-letf (((symbol-function 'eden-uuid)
                 (lambda nil (format "conversation-id-foo-%s" (cl-incf n)))))
        (eden-conversation-add dir "foo-1 title" "foo-req-uuid")
        (eden-conversation-add dir "foo-2 title" "foo-req-uuid")
        (eden-conversation-add dir "foo-3 title" "foo-req-uuid")))

    (should
     (seq-set-equal-p
      (mapcar #'car eden-conversations)
      '("conversation-id-foo-1" "conversation-id-foo-2" "conversation-id-foo-3")))
    (should (string= eden-conversation-id "conversation-id-foo-3"))

    (should
     (equal (map-elt eden-conversations "conversation-id-foo-1")
            `(:title "foo-1 title" :dir ,dir :last-req-uuid "foo-req-uuid")))
    (should
     (equal (map-elt eden-conversations "conversation-id-foo-2")
            `(:title "foo-2 title" :dir ,dir :last-req-uuid "foo-req-uuid")))
    (should
     (equal (map-elt eden-conversations "conversation-id-foo-3")
            `(:title "foo-3 title"  :dir ,dir :last-req-uuid "foo-req-uuid")))))


(ert-deftest eden-conversation-title-test ()
  (let ((eden-conversations nil))
    (should-not (eden-conversation-title "conversation-id-bar")))
  (let ((eden-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :dir "/tmp/eden/":last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :dir "/tmp/eden/":last-req-uuid "bar-req-uuid"))
           ("conversation-id-baz" .
            (:title "baz title" :dir "/tmp/eden/":last-req-uuid "baz-req-uuid")))))
    (should
     (string=
      (eden-conversation-title "conversation-id-bar")
      "bar title"))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-conversation-dir-test")))
(ert-deftest eden-conversation-dir-test ()
  (let ((eden-conversations nil))
    (should-not (eden-conversation-dir "conversation-id-bar")))
  (let ((eden-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :dir "/tmp/eden/":last-req-uuid nil)))))
    (should-not (eden-conversation-dir "conversation-id-bar")))
  (let ((eden-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :dir "/tmp/eden/":last-req-uuid nil)))))
    (should
     (string= (eden-conversation-dir "conversation-id-foo") "/tmp/eden/"))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-conversation-last-req-test")))
(ert-deftest eden-conversation-last-req-test ()
  (let ((eden-conversations nil))
    (should-not (eden-conversation-last-req "conversation-id-bar")))
  (let ((eden-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :dir "/tmp/eden/" :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :dir "/tmp/eden/" :last-req-uuid "bar-req-uuid")))))
    (should-not (eden-conversation-last-req "conversation-id-foo"))
    (should
     (equal (eden-conversation-last-req "conversation-id-bar")
            `(:uuid "bar-req-uuid" :dir "/tmp/eden/")))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-conversation-last-req-uuid-test")))
(ert-deftest eden-conversation-last-req-uuid-test ()
  (let ((eden-conversations nil))
    (should-not (eden-conversation-last-req-uuid "conversation-id-bar")))
  (let ((eden-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :dir "/tmp/eden/" :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :dir "/tmp/eden/" :last-req-uuid "bar-req-uuid")))))
    (should-not (eden-conversation-last-req-uuid "conversation-id-foo"))
    (should
     (string= (eden-conversation-last-req-uuid "conversation-id-bar")
              "bar-req-uuid"))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-conversation-buffer-name-test")))
(ert-deftest eden-conversation-buffer-name-test ()
  (let ((eden-conversations nil))
    (should-not (eden-conversation-buffer-name "conversation-id-foo"))
    (should-not (eden-conversation-buffer-name nil)))
  (let ((eden-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :dir "/tmp/eden/" :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :dir "/tmp/eden/" :last-req-uuid "bar-req-uuid"))
           ("conversation-id-baz" .
            (:title "baz title" :dir "/tmp/eden/" :last-req-uuid "baz-req-uuid")))))
    (should-not (eden-conversation-buffer-name nil))
    (should-not (eden-conversation-buffer-name "wrong-id"))
    (should
     (string=
      (eden-conversation-buffer-name "conversation-id-foo")
      (eden-buffer-name "foo title")))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-conversation-exchanges-test")))
(ert-deftest eden-conversation-exchanges-test ()
  (let* ((dir (concat (make-temp-file "eden-" t) "/"))
         (last-req `(:api (:service "openai"
                           :endpoint "https://api.openai.com/v1/chat/completions")
                     :dir ,dir
                     :uuid "uuid-baz"
                     :req-params (:messages [(:role "system" :content "baz system\n")
                                             (:role "user" :content "foo user")
                                             (:role "assistant" :content "foo assistant\n")
                                             (:role "user" :content "bar user")
                                             (:role "assistant" :content "bar assistant\n")
                                             (:role "user" :content "baz user")])
                     :prompt "baz prompt\n"
                     :system-message "baz system message\n"
                     :exchanges [(:uuid "uuid-foo"
                                  :prompt "foo prompt\n"
                                  :response "foo assistant\n")
                                 (:uuid "uuid-bar"
                                  :prompt "bar prompt\n"
                                  :response "bar assistant\n"
                                  :context [(:role "user" :content "foo user")
                                            (:role "assistant" :content "foo assistant\n")
                                            (:role "user" :content "bar user")
                                            (:role "assistant" :content "bar assistant\n")])]))
         (last-req-uuid (plist-get last-req :uuid))
         (last-resp '(:choices [(:message (:role "assistant" :content "baz assistant\n"))]))
         (last-resp-str (eden-json-encode last-resp))
         (eden-conversations
          `(("conversation-id-new" .
             (:title "new title" :dir ,dir :last-req-uuid nil))
            ("conversation-id-continue-from" .
             (:title "continue-from title" :dir ,dir :last-req-uuid "uuid-baz")))))
    (eden-write-request last-req)
    (eden-write-response last-resp-str last-resp last-req)
    (should-not (eden-conversation-exchanges "conversation-id-new"))
    (should
     (equal
      (eden-conversation-exchanges "conversation-id-continue-from")
      [(:uuid "uuid-foo"
        :prompt "foo prompt\n"
        :response "foo assistant\n")
       (:uuid "uuid-bar"
        :prompt "bar prompt\n"
        :response "bar assistant\n")
       (:uuid "uuid-baz"
        :prompt "baz prompt\n"
        :response "baz assistant\n"
        :context [(:role "user" :content "foo user")
                  (:role "assistant" :content "foo assistant\n")
                  (:role "user" :content "bar user")
                  (:role "assistant" :content "bar assistant\n")
                  (:role "user" :content "baz user")
                  (:role "assistant" :content "baz assistant\n")])]))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-conversation-rename-test")))
(ert-deftest eden-conversation-rename-test ()
  ;; Signal error:
  ;; - if try to rename to a taken title or
  ;; - if try to rename to empty title
  (let ((eden-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :dir "/tmp/eden/" :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :dir "/tmp/eden/" :last-req-uuid "bar-req-uuid")))))
    (should-error (eden-conversation-rename "conversation-id-foo" "bar title"))
    (should-error (eden-conversation-rename "conversation-id-foo" "")))
  ;; Rename or ignore if conversation doesn't exist
  (let ((eden-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :dir "/tmp/eden/" :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :dir "/tmp/eden/" :last-req-uuid "bar-req-uuid"))
           ("conversation-id-baz" .
            (:title "baz title" :dir "/tmp/eden/" :last-req-uuid "baz-req-uuid")))))
    (eden-conversation-rename "conversation-id-foo" "FOO")
    (eden-conversation-rename "conversation-id-bar" "BAR")
    (eden-conversation-rename "conversation-id-baz" "BAZ")
    (eden-conversation-rename "not-in--eden-conversations" "foo bar baz")
    (should
     (equal (map-elt eden-conversations "conversation-id-foo")
            '(:title "FOO" :dir "/tmp/eden/" :last-req-uuid nil)))
    (should
     (equal (map-elt eden-conversations "conversation-id-bar")
            '(:title "BAR" :dir "/tmp/eden/" :last-req-uuid "bar-req-uuid")))
    (should
     (equal (map-elt eden-conversations "conversation-id-baz")
            '(:title "BAZ" :dir "/tmp/eden/" :last-req-uuid "baz-req-uuid")))
    (should
     (seq-set-equal-p
      (mapcar #'car eden-conversations)
      '("conversation-id-foo" "conversation-id-bar" "conversation-id-baz")))
    (should (= (length eden-conversations) 3))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-conversation-update-test")))
(ert-deftest eden-conversation-update-test ()
  (let ((eden-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :dir "/tmp/eden/" :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :dir "/tmp/eden/" :last-req-uuid "bar-req-uuid")))))
    ;; Do not modify `eden-conversations'
    (eden-conversation-update '(:conversation-id "not-in--eden-conversations"
                                :uuid "fake-req-uuid"))
    (eden-conversation-update '(:conversation-id "conversation-id-foo"))
    (eden-conversation-update '(:uuid "new-foo-req-uuid"))

    ;; Modify `eden-conversations'
    (eden-conversation-update '(:conversation-id "conversation-id-foo"
                                :uuid "new-foo-req-uuid"))
    (eden-conversation-update '(:conversation-id "conversation-id-bar"
                                :uuid "new-bar-req-uuid"))

    (should
     (equal
      (map-elt eden-conversations "conversation-id-foo")
      '(:title "foo title" :dir "/tmp/eden/" :last-req-uuid "new-foo-req-uuid")))
    (should
     (equal
      (map-elt eden-conversations "conversation-id-bar")
      '(:title "bar title" :dir "/tmp/eden/" :last-req-uuid "new-bar-req-uuid")))
    (should
     (seq-set-equal-p
      (mapcar #'car eden-conversations)
      '("conversation-id-foo" "conversation-id-bar")))
    (should (= (length eden-conversations) 2))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-conversation-insert-test")))
(ert-deftest eden-conversation-insert-test ()
  ;; Signal error if the request doesn't exist in `:dir'
  (should-error
   (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo")))
     (eden-conversation-insert req "title")))

  ;; Signal error when an error.json file exists in req directory
  (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (eden-request-write 'error req "")
    (should-error (eden-conversation-insert req "title")))

  ;; Signal error when the request in incomplete
  (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (make-directory (eden-request-dir req) 'parent)
    (should-error (eden-conversation-insert req "title")))


  ;; Signal error when both `title' and `append' argument are nil
  (let ((req `(:req-params (:messages [(:role "user" :content "foo")])
               :prompt "foo"
               :dir ,(concat (make-temp-file "eden-" t) "/")
               :uuid "req-foo-uuid"))
        (resp '(:choices [(:message (:role "assistant" :content "foo response"))])))
    (eden-write-request req)
    (eden-write-response (eden-json-encode resp) resp req)
    (should-error (eden-conversation-insert req nil)))

  ;; conversation with no previous exchanges
  (should
   (string=
    (with-temp-buffer
      (org-mode)
      (let ((eden-org-property-req "EDEN_REQ")
            (req `(:req-params (:messages [(:role "user" :content "foo bar baz")])
                   :prompt "foo bar baz"
                   :dir ,(concat (make-temp-file "eden-" t) "/")
                   :uuid "uuid"))
            (resp '(:choices [(:message (:role "assistant"
                                         :content "foo bar baz assistant response"))])))
        (eden-write-request req)
        (eden-write-response (eden-json-encode resp) resp req)
        (eden-conversation-insert req "Conversation"))
      (buffer-substring-no-properties (point-min) (point-max)))
    "** Conversation
:PROPERTIES:
:EDEN_REQ: uuid
:END:
*** Prompt

foo bar baz

*** Response

foo bar baz assistant response

"))

  ;; conversation:
  ;; - with no previous exchanges
  ;; - with reasoning
  ;;   - we use Deepseek reasoning model to generate the response
  ;;   - the reasoning is inserted in the buffer
  ;;     because we set `eden-include-reasoning' to t
  ;;   - Note: this should work for any reasoning model given the reasoning
  ;;     is written in a reasoning.org file in the req directory
  (should
   (string=
    (with-temp-buffer
      (org-mode)
      (let ((eden-include-reasoning t)
            (eden-org-property-req "EDEN_REQ")
            (req `(:req-params (:messages [(:role "user" :content "foo bar baz")])
                   :prompt "foo bar baz"
                   :dir ,(concat (make-temp-file "eden-" t) "/")
                   :uuid "uuid"
                   :api (:service "deepseek"
                         :endpoint "https://api.deepseek.com")))
            (resp '(:choices [(:message (:role "assistant"
                                         :content "foo bar baz assistant response"
                                         :reasoning_content "foo bar baz assistant reasoning"))])))
        (eden-write-request req)
        (eden-write-response (eden-json-encode resp) resp req)
        (eden-conversation-insert req "Conversation"))
      (buffer-substring-no-properties (point-min) (point-max)))
    "** Conversation
:PROPERTIES:
:EDEN_REQ: uuid
:END:
*** Prompt

foo bar baz

*** Reasoning

foo bar baz assistant reasoning

*** Response

foo bar baz assistant response

"))

  ;; conversation:
  ;; - with no previous exchanges
  ;; - with reasoning
  ;;   - we use Deepseek reasoning model to generate the response
  ;;   - we don't inserted the reasoning in the buffer
  ;;     by setting `eden-include-reasoning' to nil
  ;;   - Note: this should work for any reasoning model given the reasoning
  ;;     is written in a reasoning.org file in the req directory
  (should
   (string=
    (with-temp-buffer
      (org-mode)
      (let ((eden-include-reasoning nil) ;; variable tested
            (eden-org-property-req "EDEN_REQ")
            (req `(:req-params (:messages [(:role "user" :content "foo bar baz")])
                   :prompt "foo bar baz"
                   :dir ,(concat (make-temp-file "eden-" t) "/")
                   :uuid "uuid"
                   :api (:service "deepseek"
                         :endpoint "https://api.deepseek.com")))
            (resp '(:model "deepseek-reasoner"
                    :choices [(:message (:role "assistant"
                                         :content "foo bar baz assistant response"
                                         :reasoning_content "foo bar baz assistant reasoning"))])))
        (eden-write-request req)
        (eden-write-response (eden-json-encode resp) resp req)
        (eden-conversation-insert req "Conversation"))
      (buffer-substring-no-properties (point-min) (point-max)))
    "** Conversation
:PROPERTIES:
:EDEN_REQ: uuid
:END:
*** Prompt

foo bar baz

*** Response

foo bar baz assistant response

"))

  ;; conversation with no previous exchanges to which we add a title
  ;; in the top level heading
  (should
   (string=
    (with-temp-buffer
      (org-mode)
      (let ((eden-org-property-req "EDEN_REQ")
            (req `(:req-params (:messages [(:role "user" :content "* title-1\n** foo\n\nbar baz\n\n* title-2\n** foo\n\nbar baz")])
                   :prompt "* title-1\n** foo\n\nbar baz\n\n* title-2\n** foo\n\nbar baz"
                   :dir ,(concat (make-temp-file "eden-" t) "/")
                   :uuid "uuid"))
            (resp '(:choices [(:message (:role "assistant"
                                         :content "### assistant title-1 \n#### foo\n\n bar baz\n\n### title-2\n#### foo\n\n bar baz"))]))
            (title "Title of the request"))
        (eden-write-request req)
        (eden-write-response (eden-json-encode resp) resp req)
        (eden-conversation-insert req title))
      (buffer-substring-no-properties (point-min) (point-max)))
    "** Title of the request
:PROPERTIES:
:EDEN_REQ: uuid
:END:
*** Prompt

**** title-1
***** foo

bar baz

**** title-2
***** foo

bar baz

*** Response

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
      (let ((eden-org-property-req "EDEN_REQ")
            (req
             `(:req-params (:messages [(:role "user" :content "* baz-heading-1\n** baz-heading-2\n\nbaz-content")])
               :prompt "* baz-heading-1\n** baz-heading-2\n\nbaz-content"
               :dir ,(concat (make-temp-file "eden-" t) "/")
               :uuid "uuid-baz"
               :api (:service "openai"
                     :endpoint "https://api.openai.com/v1/chat/completions")
               :exchanges [(:uuid "uuid-foo"
                            :prompt "* foo-heading-1\n** foo-heading-2\n\nfoo-content"
                            :response "*** foo-assistant-heading-3\n**** foo-assistant-heading-4\n\nfoo-assistant-content")
                           (:uuid "uuid-bar"
                            :prompt "* bar-heading-1\n** bar-heading-2\n\nbar-content"
                            :response "*** bar-assistant-heading-3\n**** bar-assistant-heading-4\n\nbar-assistant-content"
                            :context [(:role "user" :content "* foo-heading-1\n** foo-heading-2\n\nfoo-content")
                                      (:role "assistant" :content "*** foo-assistant-heading-3\n**** foo-assistant-heading-4\n\nfoo-assistant-content")
                                      (:role "user" :content "* bar-heading-1\n** bar-heading-2\n\nbar-content")
                                      (:role "assistant" :content "*** bar-assistant-heading-3\n**** bar-assistant-heading-4\n\nbar-assistant-content")])]))
            (resp '(:choices [(:message (:role "assistant"
                                         :content "### baz-assistant-heading-3\n\n#### baz-assistant-heading-4\n\nbaz-assistant-content"))])))
        (message "%s" (plist-get req :dir))
        (eden-write-request req)
        (eden-write-response (eden-json-encode resp) resp req)
        (eden-conversation-insert req "Conversation"))
      (buffer-substring-no-properties (point-min) (point-max)))
    "** Conversation
:PROPERTIES:
:EDEN_REQ: uuid-baz
:END:
*** Prompt

**** foo-heading-1
***** foo-heading-2

foo-content

*** Response

**** foo-assistant-heading-3
***** foo-assistant-heading-4

foo-assistant-content

*** Prompt

**** bar-heading-1
***** bar-heading-2

bar-content

*** Response

**** bar-assistant-heading-3
***** bar-assistant-heading-4

bar-assistant-content

*** Prompt

**** baz-heading-1
***** baz-heading-2

baz-content

*** Response

**** baz-assistant-heading-3

***** baz-assistant-heading-4

baz-assistant-content

"))


  ;; Append last request to an existing conversation.
  (should
   (string=
    (with-temp-buffer
      (org-mode)
      (let ((eden-org-property-req "EDEN_REQ")
            (req `(:req-params (:messages [(:role "user" :content "* baz-heading-1\n** baz-heading-2\n\nbaz-content")])
                   :prompt "* baz-heading-1\n** baz-heading-2\n\nbaz-content"
                   :dir ,(concat (make-temp-file "eden-" t) "/")
                   :uuid "uuid-baz"
                   :api (:service "openai"
                         :endpoint "https://api.openai.com/v1/chat/completions")
                   ;; We keep :exchanges thought modifying it doesn't affect
                   ;; `eden-conversation-insert' as we are only appending
                   ;; the last request (i.e. this request) not the previous
                   ;; exchanges
                   :exchanges [(:uuid "uuid-foo"
                                :prompt "* foo-heading-1\n** foo-heading-2\n\nfoo-content"
                                :response "*** foo-assistant-heading-3\n**** foo-assistant-heading-4\n\nfoo-assistant-content")
                               (:uuid "uuid-bar"
                                :prompt "* bar-heading-1\n** bar-heading-2\n\nbar-content"
                                :response "*** bar-assistant-heading-3\n**** bar-assistant-heading-4\n\nbar-assistant-content"
                                :context [(:role "user" :content "* foo-heading-1\n** foo-heading-2\n\nfoo-content")
                                          (:role "assistant" :content "*** foo-assistant-heading-3\n**** foo-assistant-heading-4\n\nfoo-assistant-content")
                                          (:role "user" :content "* bar-heading-1\n** bar-heading-2\n\nbar-content")
                                          (:role "assistant" :content "*** bar-assistant-heading-3\n**** bar-assistant-heading-4\n\nbar-assistant-content")])]
                   ))
            (resp '(:choices [(:message (:role "assistant"
                                         :content "### baz-assistant-heading-3\n\n#### baz-assistant-heading-4\n\nbaz-assistant-content"))])))
        (insert "** Title of the conversation
:PROPERTIES:
:EDEN_REQ: uuid-bar
:END:
*** Prompt

**** foo-heading-1
***** foo-heading-2

foo-content

*** Response

**** foo-assistant-heading-3

***** foo-assistant-heading-4

foo-assistant-content

*** Prompt

**** bar-heading-1
***** bar-heading-2

bar-content

*** Response

**** bar-assistant-heading-3

***** bar-assistant-heading-4

bar-assistant-content

")
        (eden-write-request req)
        (eden-write-response (eden-json-encode resp) resp req)
        (eden-conversation-insert req nil 'append))
      (buffer-substring-no-properties (point-min) (point-max)))
    "** Title of the conversation
:PROPERTIES:
:EDEN_REQ: uuid-baz
:END:
*** Prompt

**** foo-heading-1
***** foo-heading-2

foo-content

*** Response

**** foo-assistant-heading-3

***** foo-assistant-heading-4

foo-assistant-content

*** Prompt

**** bar-heading-1
***** bar-heading-2

bar-content

*** Response

**** bar-assistant-heading-3

***** bar-assistant-heading-4

bar-assistant-content

*** Prompt

**** baz-heading-1
***** baz-heading-2

baz-content

*** Response

**** baz-assistant-heading-3

***** baz-assistant-heading-4

baz-assistant-content

"))

  ;; Insert only insert the last request of the conversation
  ;; by passing 'only-last-req argument to `eden-conversation-insert'
  (should
   (string=
    (with-temp-buffer
      (org-mode)
      (let* ((eden-org-property-req "EDEN_REQ")
             (dir (concat (make-temp-file "eden-" t) "/"))
             (req `(:req-params (:messages [(:role "user" :content "* baz-heading-1\n** baz-heading-2\n\nbaz-content")])
                    :prompt "* baz-heading-1\n** baz-heading-2\n\nbaz-content"
                    :dir ,(concat (make-temp-file "eden-" t) "/")
                    :uuid "uuid"
                    :api (:service "openai"
                          :endpoint "https://api.openai.com/v1/chat/completions")
                    ;; We keep :exchanges thought modifying it doesn't affect
                    ;; `eden-conversation-insert' as we are only inserting
                    ;; the last request (i.e. this request) not the previous
                    ;; exchanges
                    :exchanges [(:uuid "uuid-foo"
                                 :prompt "* foo-heading-1\n** foo-heading-2\n\nfoo-content"
                                 :response "*** foo-assistant-heading-3\n**** foo-assistant-heading-4\n\nfoo-assistant-content")
                                (:uuid "uuid-bar"
                                 :prompt "* bar-heading-1\n** bar-heading-2\n\nbar-content"
                                 :response "*** bar-assistant-heading-3\n**** bar-assistant-heading-4\n\nbar-assistant-content"
                                 :context [(:role "user" :content "* foo-heading-1\n** foo-heading-2\n\nfoo-content")
                                           (:role "assistant" :content "*** foo-assistant-heading-3\n**** foo-assistant-heading-4\n\nfoo-assistant-content")
                                           (:role "user" :content "* bar-heading-1\n** bar-heading-2\n\nbar-content")
                                           (:role "assistant" :content "*** bar-assistant-heading-3\n**** bar-assistant-heading-4\n\nbar-assistant-content")])]))
             (resp '(:choices [(:message (:role "assistant"
                                          :content "### baz-assistant-heading-3\n\n#### baz-assistant-heading-4\n\nbaz-assistant-content"))])))
        (eden-write-request req)
        (eden-write-response (eden-json-encode resp) resp req)
        (eden-conversation-insert
         req "Only last request of the conversation" nil 'only-last-req))
      (buffer-substring-no-properties (point-min) (point-max)))
    (concat "** Only last request of the conversation
:PROPERTIES:
:EDEN_REQ: uuid
:END:
*** Prompt

**** baz-heading-1
***** baz-heading-2

baz-content

*** Response

**** baz-assistant-heading-3

***** baz-assistant-heading-4

baz-assistant-content

"))))

;;;; Sending Requests

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-kill-last-request-test")))
(ert-deftest eden-kill-last-request-test ()
  ;; We kill the last request sent with `eden-send-request' by killing
  ;; its associated process.  This event is handle by the process sentinel
  ;; which:
  ;;
  ;; 1) Logs the error on disk (we don't test this here)
  ;; 2) Signals an `eden-error-process' error (we check this)
  (let ((timer-list)            ;; to not interfer with global state
        (global-mode-string)    ;; to not interfer with global state
        (eden-pending-timer)    ;; to not interfer with global state
        (eden-pending-requests) ;; to not interfer with global state
        ;; Don't catch errors in sentinel, so that we can catch
        ;; and test them with `should-error' macro
        (debug-on-error t)
        (dir (concat (make-temp-file "eden-" t) "/")))
    (cl-letf (((symbol-function 'eden-request-command)
               ;; Print "resp-foo" in stdout after 1 second
               (lambda (req)
                 (let* ((resp-fmt "{\"choices\": [{\"message\": {\"role\": \"assistant\", \"content\": \"%s\"}}]}")
                        (resp-str-quoted
                         (shell-quote-argument (format resp-fmt "resp-foo"))))
                   (list
                    (format "sleep 1 | echo %s" resp-str-quoted)
                    "[redacted command]")))))
      (eden-send-request
       :req `(:req-params (:messages [(:role "user" :content "foo")])
              :prompt "foo"
              :dir ,dir
              :uuid "req-foo-uuid")))
    (should-error (progn (eden-kill-last-request) (sleep-for 0.2))
                  :type 'eden-error-process)))


(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-pending-conversation-p-test")))
(ert-deftest eden-pending-conversation-p-test ()
  ;; :req and :proc are different in practice but here
  ;; to test `eden-pending-conversation-p' we just need
  ;; :conversation-id to be in :req.
  (let ((eden-pending-requests '((:req (:conversation-id "conv-foo")
                                  :proc "proc-foo")
                                 (:req "req-bar-not-part-of-a-conversation"
                                  :proc "proc-bar"))))
    (should (eden-pending-conversation-p "conv-foo"))
    (should-not (eden-pending-conversation-p "conv-baz")))
  (let ((eden-pending-requests nil))
    (should-not (eden-pending-conversation-p "conv-foo"))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-send-request-test")))
(ert-deftest eden-send-request-test ()
  ;; We want to test that concurrent requests work as expected:
  ;;
  ;; - Observe mode-line during the test to be sure it doesn't stop
  ;;   after the first request done,
  ;; - Test `eden-pending-requests' value during all requests
  ;; - Test that two requests can't run simultaneously in the same
  ;;   conversation (`eden-conversations')
  ;; - Test that :last-req-uuid is updated correctly in `eden-conversations'
  ;; - Test that the request's callback function is called in the order the
  ;;   responses arrive.
  ;; - Test that request are push onto `eden-request-history' stack
  (let* ((print-to-stdout-after-delay
          ;; Print RESP-CONTENT after DELAY seconds in stdout wrapped in
          ;; JSON response like OpenAI API (choices[0].message.content)
          (lambda (resp-content delay)
            `(lambda (req)
               (let* ((resp-fmt "{\"choices\": [{\"message\": {\"role\": \"assistant\", \"content\": \"%s\"}}]}")
                      (resp-str-quoted
                       (shell-quote-argument (format resp-fmt ,resp-content))))
                 (list
                  (format "sleep %s | echo %s" ,delay resp-str-quoted)
                  "[redacted command]")))))
         (_ (progn
              ;; It must be a global dynamic variable so that we can
              ;; access it and modify it in the callback function bellow.
              (defvar -callback-acc nil "...")
              (setq -callback-acc nil)))
         (callback (lambda (req resp)
                     (push (eden-get-in resp [:choices 0 :message :content])
                           -callback-acc)
                     ;; These functions must be called here to update
                     ;; Eden state.  This is the only requirement imposed
                     ;; on the callback function passed to `eden-send-request'.
                     (eden-pending-remove req)
                     (eden-conversation-update req)
                     (eden-mode-line-waiting 'maybe-stop)))
         (timer-list)            ;; variable tested
         (global-mode-string)    ;; variable tested
         (eden-pending-timer)    ;; variable tested
         (eden-pending-requests) ;; variable tested
         (eden-request-history)  ;; variable tested
         (dir (concat (make-temp-file "eden-" t) "/"))
         (eden-conversations     ;; variable tested
          `(("conversation-id-foo" . (:title "foo title"
                                      :dir ,dir
                                      :last-req-uuid nil))
            ("conversation-id-bar" . (:title "bar title"
                                      :dir ,dir
                                      :last-req-uuid "last-bar-req-uuid"))
            ("conversation-id-baz" . (:title "baz title"
                                      :dir ,dir
                                      :last-req-uuid "last-baz-req-uuid")))))
    (cl-letf (((symbol-function 'eden-request-command)
               (funcall print-to-stdout-after-delay "resp-foo" 3)))
      (eden-send-request
       :req `(:req-params (:messages [(:role "user" :content "foo")])
              :prompt "foo"
              :dir ,dir
              :uuid "req-foo-uuid"
              :conversation-id "conversation-id-foo")
       :callback callback))
    (cl-letf (((symbol-function 'eden-request-command)
               (funcall print-to-stdout-after-delay "resp-foo-foo" 3)))
      ;; This request won't be sent because `req-foo', next request
      ;; in "conversation-id-foo" conversation is already running
      (eden-send-request
       :req `(:req-params (:messages [(:role "user" :content "foo-foo")])
              :prompt "foo-foo"
              :dir ,dir
              :uuid "req-foo-foo-uuid"
              :conversation-id "conversation-id-foo")
       :callback callback))
    (cl-letf (((symbol-function 'eden-request-command)
               (funcall print-to-stdout-after-delay "resp-bar" 1)))
      (eden-send-request
       :req `(:req-params (:messages [(:role "user" :content "bar")])
              :prompt "bar"
              :dir ,dir
              :uuid "req-bar-uuid"
              :conversation-id "conversation-id-bar")
       :callback callback))
    (cl-letf (((symbol-function 'eden-request-command)
               (funcall print-to-stdout-after-delay "resp-baz" 2)))
      (eden-send-request
       :req `(:req-params (:messages [(:role "user" :content "baz")])
              :prompt "baz"
              :dir ,dir
              :uuid "req-baz-uuid")
       :callback callback))

    ;; To manually test waiting widget in the mode line while we're
    ;; waiting for responses do the following:
    ;;
    ;; - comment the following 2 sexps
    ;; - run the test and observe the mode line and echo area
    ;;   during 5 seconds
    (let ((prompt-proc (lambda (p)
                         (cons (plist-get (plist-get p :req) :prompt)
                               (processp (plist-get p :proc)))))
          (pr-timer eden-pending-timer))
      (sleep-for 0.2)
      (should
       (equal (mapcar prompt-proc eden-pending-requests)
              '(("baz" . t)
                ("bar" . t)
                ("foo" . t))))
      (should (eden-pending-conversation-p "conversation-id-foo"))
      (should (eden-pending-conversation-p "conversation-id-bar"))
      ;; This means that the waiting widget still showing up
      ;; in the modeline
      (should (memq pr-timer timer-list))
      (sleep-for 0.9)
      (should
       (equal (mapcar prompt-proc eden-pending-requests)
              '(("baz" . t)
                ("foo" . t))))
      (should (memq pr-timer timer-list))
      (sleep-for 1)
      (should
       (equal (mapcar prompt-proc eden-pending-requests)
              '(("foo" . t))))
      (should (memq pr-timer timer-list))
      (sleep-for 1)
      (should (equal eden-pending-requests nil))
      (should-not (memq pr-timer timer-list))
      (should-not global-mode-string) ;; waiting widget is gone
      (should (equal -callback-acc '("resp-foo" "resp-baz" "resp-bar")))
      (should (equal eden-request-history
                     '("req-baz-uuid" "req-bar-uuid" "req-foo-uuid")))

      ;; state of `eden-conversations'
      (should
       (equal (map-elt eden-conversations "conversation-id-foo")
              `(:title "foo title" :dir ,dir :last-req-uuid "req-foo-uuid")))
      (should
       (equal (map-elt eden-conversations "conversation-id-bar")
              `(:title "bar title" :dir ,dir :last-req-uuid "req-bar-uuid")))
      (should
       (seq-set-equal-p
        (mapcar #'car eden-conversations)
        '("conversation-id-foo" "conversation-id-bar" "conversation-id-baz")))))

  ;; Test that `eden-send-request''s default callback-error function
  ;; removes waiting widget in the modeline when an error occurs in
  ;; the sentinel.  To do so we trigger an JSON read error by filling
  ;; Eden process buffer with non JSON content.
  (let ((eden-pending-requests) ;; variable tested
        (eden-pending-timer)    ;; variable tested
        (timer-list)            ;; variable tested
        (global-mode-string)    ;; variable tested
        (dir (concat (make-temp-file "eden-" t) "/"))
        (pr-timer)
        ;; Don't catch errors in sentinel, so that we can catch
        ;; and test them with `should-error' macro
        (debug-on-error t))
    (should-error
     (cl-letf (((symbol-function 'eden-request-command)
                (lambda (req)
                  (list (format "echo not-json")
                        "[redacted command]"))))
       (eden-send-request
        :req `(:req-params (:messages [(:role "user" :content "req")])
               :prompt "req"
               :dir ,dir
               :uuid "req-uuid")
        :callback (lambda (req resp) nil))
       (setq pr-timer eden-pending-timer)
       ;; Wait for process to terminate
       (let ((proc (plist-get (car eden-pending-requests) :proc)))
         (while (accept-process-output proc))))
     :type 'eden-error-json-read)
    (should (equal eden-pending-requests nil))
    (should (equal eden-pending-timer nil))
    ;; waiting widget is gone
    (should-not (memq pr-timer timer-list))
    (should-not global-mode-string)))


(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-build-request-test")))
(ert-deftest eden-build-request-test ()
  ;; Errors
  (should-error (eden-build-request :profile '(:dir nil))
                :type 'eden-error-req)

  ;; :prompt
  ;;
  ;; If blank string, we don't pass it as input/messages in the API request
  (let* ((dir (concat (make-temp-file "eden-" t) "/"))
         (req-prompt-blank
          (eden-build-request
           :profile `(:dir ,dir
                      :api (:service "openai"
                            :endpoint "https://api.openai.com/v1/chat/completions"))
           :prompt " \n \t "))
         (req-prompt-nil
          (eden-build-request
           :profile `(:dir ,dir
                      :api (:service "openai"
                            :endpoint "https://api.openai.com/v1/chat/completions"))
           :prompt nil))
         (req-prompt-not-blank
          (eden-build-request
           :profile `(:dir ,dir
                      :api (:service "openai"
                            :endpoint "https://api.openai.com/v1/chat/completions"))
           ;; :prompt is expected to use org-mode syntax
           ;; and is converted to markdown in request body
           :prompt "* foo prompt")))
    (should (string-empty-p (plist-get req-prompt-blank :prompt)))
    (should (equal (eden-get-in req-prompt-blank [:req-params :messages]) []))
    (should (string-empty-p (plist-get req-prompt-nil :prompt)))
    (should (equal (eden-get-in req-prompt-nil [:req-params :messages]) []))
    (should (string= (plist-get req-prompt-not-blank :prompt)
                     "* foo prompt"))
    (should (equal (eden-get-in req-prompt-not-blank [:req-params :messages])
                   [(:role "user" :content "# foo prompt")])))


  ;; :uuid
  ;;
  ;; TODO: add case where we pass :uuid
  (cl-letf (((symbol-function 'eden-uuid)
             (lambda nil "foo-uuid")))
    (let* ((dir (concat (make-temp-file "eden-" t) "/"))
           (req (eden-build-request
                 :profile `(:dir ,dir
                            :api (:service "openai"
                                  :endpoint "https://api.openai.com/v1/chat/completions"))
                 :prompt "foo prompt")))
      (should (string= (plist-get req :uuid) "foo-uuid"))))


  ;; :api
  (let* ((dir (concat (make-temp-file "eden-" t) "/"))
         (api '(:service "openai"
                :endpoint "https://api.openai.com/v1/chat/completions"))
         (req (eden-build-request :profile `(:dir ,dir :api ,api)
                                  :prompt "foo prompt")))
    (should (equal (plist-get req :api) api)))

  ;; :model and stream false
  (let* ((dir (concat (make-temp-file "eden-" t) "/"))
         (req (eden-build-request
               :profile `(:dir ,dir
                          :api (:service "openai"
                                :endpoint "https://api.openai.com/v1/chat/completions")
                          :model "gpt-5.4")
               :prompt "foo prompt")))
    (should (equal (eden-get-in req [:req-params :stream]) :false))
    (should (equal (eden-get-in req [:req-params :model]) "gpt-5.4")))

  ;; :system-message and :system-message-append
  ;;
  ;; :system-message and :system-message-append are expected to use
  ;; org-mode syntax and are converted to markdown in request body
  (let* ((dir (concat (make-temp-file "eden-" t) "/"))
         (req-no-system-msg-no-append
          (eden-build-request
           :profile `(:dir ,dir
                      :api (:service "openai"
                            :endpoint "https://api.openai.com/v1/chat/completions")
                      :model "gpt-5.4")
           :prompt "foo prompt"))
         (req-no-system-with-append
          (eden-build-request
           :profile `(:dir ,dir
                      :api (:service "openai"
                            :endpoint "https://api.openai.com/v1/chat/completions")
                      :model "gpt-5.4"
                      :system-message-append "* foo system append")
           :prompt "foo prompt"))
         (req-with-system-msg-no-append
          (eden-build-request
           :profile `(:dir ,dir
                      :api (:service "openai"
                            :endpoint "https://api.openai.com/v1/chat/completions")
                      :model "gpt-5.4"
                      :system-message "* foo system")
           :prompt "foo prompt"))
         (req-with-system-msg-with-append
          (eden-build-request
           :profile `(:dir ,dir
                      :api (:service "openai"
                            :endpoint "https://api.openai.com/v1/chat/completions")
                      :model "gpt-5.4"
                      :system-message "* foo system"
                      :system-message-append "* foo system append")
           :prompt "foo prompt")))
    (should-not (plist-get req-no-system-msg-no-append :system-message))
    (should (equal (eden-get-in req-no-system-msg-no-append [:req-params :messages])
                   [(:role "user" :content "foo prompt")]))
    (should (string= (plist-get req-no-system-with-append :system-message)
                     "* foo system append"))
    (should (equal (eden-get-in req-no-system-with-append [:req-params :messages])
                   [(:role "system" :content "# foo system append")
                    (:role "user" :content "foo prompt")]))
    (should (string= (plist-get req-with-system-msg-no-append :system-message)
                     "* foo system"))
    (should (equal (eden-get-in req-with-system-msg-no-append [:req-params :messages])
                   [(:role "system" :content "# foo system")
                    (:role "user" :content "foo prompt")]))
    (should (string= (plist-get req-with-system-msg-with-append :system-message)
                     "* foo system\n\n* foo system append"))
    (should (equal (eden-get-in req-with-system-msg-with-append [:req-params :messages])
                   [(:role "system" :content "# foo system\n\n\n# foo system append")
                    (:role "user" :content "foo prompt")])))

  ;; :prev-req-uuid
  ;;
  ;; Error when :prev-req-uuid refer to bad/missing request
  (let* ((dir (concat (make-temp-file "eden-" t) "/")))
    (should-error
     (eden-build-request
      :profile `(:dir ,dir
                 :api (:service "openai"
                       :endpoint "https://api.openai.com/v1/chat/completions")
                 :model "gpt-5.4")
      :prev-req-uuid "request-missing-so-cannot-be-used"
      :prompt "foo prompt")
     :type 'eden-error-req))
  ;; Note that (eden-org-to-markdown "foo\n") ;; "foo" (last newline trimmed)
  ;; and that org content are save to file with last newline at
  ;; the end (maybe due to some Emacs config - I'm not sure)
  (let* ((dir (concat (make-temp-file "eden-" t) "/"))
         (api '(:service "openai"
                :endpoint "https://api.openai.com/v1/chat/completions"))
         (model "gpt-5.4")
         (req-foo (eden-build-request
                   :profile `(:dir ,dir :api ,api :model ,model)
                   :prompt "foo prompt\n"))
         (req-foo-uuid (plist-get req-foo :uuid))
         ;; We must save this request with a response in `dir'
         ;; before referencing it as previous request for the
         ;; new request `req-bar'
         (_ (eden-write-request req-foo))
         (resp-foo '(:choices
                     [(:message (:role "assistant" :content "foo response\n"))]))
         (_ (eden-write-response (eden-json-encode resp-foo) resp-foo req-foo))
         (req-bar (eden-build-request
                   :profile `(:dir ,dir :api ,api :model ,model)
                   :prompt "bar prompt\n"
                   :prev-req-uuid req-foo-uuid))
         (req-bar-uuid (plist-get req-bar :uuid))
         ;; Same here
         (_ (eden-write-request req-bar))
         (resp-bar '(:choices
                     [(:message (:role "assistant" :content "bar response\n"))]))
         (_ (eden-write-response (eden-json-encode resp-bar) resp-bar req-bar))
         (exchanges (vector
                     `(:uuid ,req-foo-uuid
                       :prompt "foo prompt\n"
                       :response "foo response\n")
                     `(:uuid ,req-bar-uuid
                       :prompt "bar prompt\n"
                       :response "bar response\n"
                       :context [(:role "user" :content "foo prompt")
                                 (:role "assistant" :content "foo response\n")
                                 (:role "user" :content "bar prompt")
                                 (:role "assistant" :content "bar response\n")])))
         (req-with-system-msg-with-prompt
          (eden-build-request
           :profile `(:dir ,dir :api ,api :model model
                      :system-message "baz system\n")
           :prompt "baz prompt\n"
           :prev-req-uuid req-bar-uuid))
         (req-with-system-msg-no-prompt
          (eden-build-request
           :profile `(:dir ,dir :api ,api :model ,model
                      :system-message "baz system\n")
           :prev-req-uuid req-bar-uuid))
         (req-no-system-msg-no-prompt
          (eden-build-request
           :profile `(:dir ,dir :api ,api :model ,model)
           :prev-req-uuid req-bar-uuid)))
    (should (equal (plist-get req-with-system-msg-with-prompt :exchanges)
                   exchanges))
    (should (equal (eden-get-in req-with-system-msg-with-prompt
                                [:req-params :messages])
                   [(:role "system" :content "baz system")
                    (:role "user" :content "foo prompt")
                    (:role "assistant" :content "foo response\n")
                    (:role "user" :content "bar prompt")
                    (:role "assistant" :content "bar response\n")
                    (:role "user" :content "baz prompt")]))
    (should (equal (plist-get req-with-system-msg-no-prompt :exchanges)
                   exchanges))
    (should (equal (eden-get-in req-with-system-msg-no-prompt
                                [:req-params :messages])
                   [(:role "system" :content "baz system")
                    (:role "user" :content "foo prompt")
                    (:role "assistant" :content "foo response\n")
                    (:role "user" :content "bar prompt")
                    (:role "assistant" :content "bar response\n")]))
    (should (equal (plist-get req-no-system-msg-no-prompt :exchanges)
                   exchanges))
    (should (equal (eden-get-in req-no-system-msg-no-prompt
                                [:req-params :messages])
                   [(:role "user" :content "foo prompt")
                    (:role "assistant" :content "foo response\n")
                    (:role "user" :content "bar prompt")
                    (:role "assistant" :content "bar response\n")])))

  ;; :conversation-id
  ;;
  ;; We include :conversation-id key in the output request.
  (let* ((dir (concat (make-temp-file "eden-" t) "/"))
         (req (eden-build-request
               :profile `(:dir ,dir
                          :api (:service "openai"
                                :endpoint "https://api.openai.com/v1/chat/completions")
                          :model "gpt-5.4"
                          ;; In practice, when this id in non-nil, we should
                          ;; pass the corresponding :last-req-uuid as
                          ;; :prev-req-uuid of `eden-build-request'.
                          ;; We don't do any lookup in `eden-conversations'
                          ;; so that `eden-build-request' keep being
                          ;; self-contained.  We just pass the conversation
                          ;; id to be used in callback of the sentinel
                          ;; after receiving a response from OpenAI API.
                          :conversation-id "foo-conversation-id")
               :prompt "foo prompt")))
    (should (string= (plist-get req :conversation-id) "foo-conversation-id")))

  ;; :info
  ;;
  ;; We include :conversation-id key in the output request.
  (let* ((dir (concat (make-temp-file "eden-" t) "/"))
         (req (eden-build-request
               :profile `(:dir ,dir
                          :api (:service "openai"
                                :endpoint "https://api.openai.com/v1/chat/completions")
                          :model "gpt-5.4")
               :prompt "foo prompt"
               :info '(:foo (:bar "baz")))))
    (should (equal (plist-get req :info) '(:foo (:bar "baz")))))

  ;; :include-reasoning
  ;;
  ;; Anthopic API specific
  (let* ((dir (concat (make-temp-file "eden-" t) "/"))
         (req (eden-build-request
               :profile `(:dir ,dir
                          :api (:service "anthropic"
                                :endpoint "https://api.anthropic.com/v1/messages")
                          :model "claude-opus-4-7"
                          :include-reasoning t)
               :prompt "foo prompt")))
    (should (equal (eden-get-in req [:req-params :thinking])
                   '(:type "enabled" :budget_tokens 2048))))

  ;; Anthropic API specific
  ;;
  ;; :max_tokens sets by default in request body
  (let* ((dir (concat (make-temp-file "eden-" t) "/"))
         (req (eden-build-request
               :profile `(:dir ,dir
                          :api (:service "anthropic"
                                :endpoint "https://api.anthropic.com/v1/messages")
                          :model "claude-opus-4-7")
               :prompt "foo prompt")))
    (should (equal (eden-get-in req [:req-params :max_tokens]) 4096))))

;;;; Conversation branches (paths)

(ert-deftest eden-paths-maximal-test ()
  (should-not (eden-paths-maximal nil))
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
      (eden-paths-maximal paths)
      '("uuid-req-3"
        "uuid-req-4"
        "uuid-req-5"
        "uuid-req-6"
        "uuid-req-8"
        "uuid-req-10"
        "uuid-req-12"
        "uuid-req-13")))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-paths-branches-test")))
(ert-deftest eden-paths-branches-test ()
  (should-not (eden-paths-maximal nil))
  (let ((paths '(["uuid-req-1"]
                 ["uuid-req-1" "uuid-req-2"]
                 ["uuid-req-1" "uuid-req-2" "uuid-req-3"]
                 ["uuid-req-1" "uuid-req-2" "uuid-req-4"]
                 ["uuid-req-2" "uuid-req-5"]
                 ["uuid-req-6"])))
    (should
     (equal
      (eden-paths-branches "uuid-req-2" paths)
      '("uuid-req-3" "uuid-req-4" "uuid-req-5")))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-paths-last-test")))
(ert-deftest eden-paths-last-test ()

  ;; ["uuid-req-1"]                           ;; not a conversation
  ;; ["uuid-req-1" "uuid-req-2"]              ;; not a conversation
  ;; ["uuid-req-1" "uuid-req-2" "uuid-req-3"]
  ;; ["uuid-req-1" "uuid-req-2" "uuid-req-4"]
  ;; ["uuid-req-5"]                           ;; with no response.json file
  ;; ["uuid-req-2" "uuid-req-6"]
  ;; ["uuid-req-7"]
  (let* ((dir (concat (make-temp-file "eden-" t) "/"))
         (req-1 `(:req-params (:messages [(:role "user" :content "req-1 prompt")])
                  :prompt "req-1 prompt"
                  :dir ,dir
                  :uuid "uuid-req-1"))
         (req-2 `(:req-params (:messages [(:role "user" :content "req-2 prompt")])
                  :prompt "req-2 prompt"
                  :dir ,dir
                  :uuid "uuid-req-2"
                  :exchanges [(:uuid "uuid-req-1"
                               :prompt "req-1 prompt"
                               :response "req-1 response"
                               :context [(:role "user" :content "req-1 prompt")
                                         (:role "assistant" :content "req-1 response")])]))
         (req-3 `(:req-params (:messages [(:role "user" :content "req-3 prompt")])
                  :prompt "req-3 prompt"
                  :dir ,dir
                  :uuid "uuid-req-3"
                  :exchanges [(:uuid "uuid-req-1"
                               :prompt "req-1 prompt"
                               :response "req-1 response")
                              (:uuid "uuid-req-2"
                               :prompt "req-2 prompt"
                               :response "req-2 response"
                               :context [(:role "user" :content "req-1 prompt")
                                         (:role "assistant" :content "req-1 response")
                                         (:role "user" :content "req-2 prompt")
                                         (:role "assistant" :content "req-2 response")])]))
         (req-4 `(:req-params (:messages [(:role "user" :content "req-4 prompt")])
                  :prompt "req-4 prompt"
                  :dir ,dir
                  :uuid "uuid-req-4"
                  :exchanges [(:uuid "uuid-req-1"
                               :prompt "req-1 prompt"
                               :response "req-1 response")
                              (:uuid "uuid-req-2"
                               :prompt "req-2 prompt"
                               :response "req-2 response"
                               :context [(:role "user" :content "req-1 prompt")
                                         (:role "assistant" :content "req-1 response")
                                         (:role "user" :content "req-2 prompt")
                                         (:role "assistant" :content "req-2 response")])]))
         (req-5 `(:req-params (:messages [(:role "user" :content "req-5 prompt")])
                  :prompt "req-5 prompt"
                  :dir ,dir
                  :uuid "uuid-req-5"))
         (req-6 `(:req-params (:messages [(:role "user" :content "req-6 prompt")])
                  :prompt "req-6 prompt"
                  :dir ,dir
                  :uuid "uuid-req-6"
                  :exchanges [(:uuid "uuid-req-2"
                               :prompt "req-2 prompt"
                               :response "req-2 response"
                               :context [(:role "user" :content "req-2 prompt")
                                         (:role "assistant" :content "req-2 response")])]))
         (req-7 `(:req-params (:messages [(:role "user" :content "req-7 prompt")])
                  :prompt "req-7 prompt"
                  :dir ,dir
                  :uuid "uuid-req-7"))
         ;; Unix timestamps of the last 7 days including today
         (timestamps
          (mapcar (lambda (days)
                    (float-time
                     (time-subtract (current-time) (days-to-time days))))
                  '(6 5 4 3 2 1 0))))
    (dotimes (idx 7)
      (let ((req (eval (intern (format "req-%s" (1+ idx))))))
        (eden-write-request req)
        ;; We replace timestamp files in order to span requests
        ;; over 7 days including today.
        (eden-test-add-or-replace-timestamp-file
         req (nth idx timestamps))
        ;; add response.json except for req-5 such that this request
        ;; is considered to be an error and so its path will not
        ;; be listed by `eden-paths-last' function
        (when (not (= idx 4)) ;;
          (let* ((resp `(:choices
                         [(:message
                           (:role "assistant"
                            :content ,(format "req-%s assistant" (1+ idx))))]))
                 (resp-str (eden-json-encode resp)))
            (eden-write-response resp-str resp req)))))

    ;; Test `eden-paths-since'
    (should (equal (eden-paths-since dir (nth 0 timestamps))
                   '(["uuid-req-1"]
                     ["uuid-req-1" "uuid-req-2"]
                     ["uuid-req-1" "uuid-req-2" "uuid-req-3"]
                     ["uuid-req-1" "uuid-req-2" "uuid-req-4"]
                     ["uuid-req-2" "uuid-req-6"]
                     ["uuid-req-7"])))
    (should (equal (eden-paths-since dir (nth 6 timestamps)) '(["uuid-req-7"])))
    (should (equal (eden-paths-since dir (nth 3 timestamps))
                   '(["uuid-req-1" "uuid-req-2" "uuid-req-4"]
                     ["uuid-req-2" "uuid-req-6"]
                     ["uuid-req-7"])))

    ;; Test `eden-paths-last'
    (should (equal (eden-paths-last dir 10)
                   '(["uuid-req-1"]
                     ["uuid-req-1" "uuid-req-2"]
                     ["uuid-req-1" "uuid-req-2" "uuid-req-3"]
                     ["uuid-req-1" "uuid-req-2" "uuid-req-4"]
                     ["uuid-req-2" "uuid-req-6"]
                     ["uuid-req-7"])))
    (should (equal (eden-paths-last dir 1) '(["uuid-req-7"])))
    (should (equal (eden-paths-last dir 4)
                   '(["uuid-req-1" "uuid-req-2" "uuid-req-4"]
                     ["uuid-req-2" "uuid-req-6"]
                     ["uuid-req-7"])))

    ;; Test `eden-paths-last-requests'
    (should (equal (eden-paths-last-requests dir 10)
                   '("uuid-req-1"
                     "uuid-req-2"
                     "uuid-req-3"
                     "uuid-req-4"
                     "uuid-req-6"
                     "uuid-req-7")))
    (should (equal (eden-paths-last-requests dir 1) '("uuid-req-7")))
    (should (equal (eden-paths-last-requests dir 4)
                   '("uuid-req-4" "uuid-req-6" "uuid-req-7")))

    ;; Test `eden-paths-last-conversations'
    (should (equal (eden-paths-last-conversations dir 10)
                   '("uuid-req-3" "uuid-req-4" "uuid-req-6" "uuid-req-7")))
    (should (equal (eden-paths-last-conversations dir 1) '("uuid-req-7")))
    (should (equal (eden-paths-last-conversations dir 4)
                   '("uuid-req-4" "uuid-req-6" "uuid-req-7")))))
