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
                   :temperature 1
                   :messages [(:role "user" :content "foo bar baz")]))))
      (eden-json-read))
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

(ert-deftest eden-request-dir-test ()
  ;; signal error when one of the keys `:dir' or `:uuid'
  ;; is missing or is not a string
  (should-error (eden-request-dir '(:dir "/tmp/eden/")))
  (should-error (eden-request-dir '(:dir "/tmp/eden/" :uuid 1)))
  (should-error (eden-request-dir '(:uuid "foo-uuid")))
  (should-error (eden-request-dir '(:uuid "foo-uuid" :dir 1)))
  (should-error (eden-request-dir nil))

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
  (let* ((request '(:stream :false
                    :model "gpt-4o-mini"
                    :temperature 1
                    :messages [(:role "user" :content "user prompt\n")]))
         (req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (message "%s" (eden-request-dir req))

    ;; json files
    (eden-request-write 'request req (eden-json-encode request))
    (should (equal (eden-request-read 'request req) request))

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

(ert-deftest eden-request-user-content-test ()
  (let* ((request '(:stream :false
                    :model "gpt-4o-mini"
                    :temperature 1
                    :messages [(:role "user" :content "foo user")])))
    (should
     (string= (eden-request-user-content request) "foo user")))

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
     (string= (eden-request-user-content request) "baz user"))))

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

  ;; Signal error when the request in incomplete
  (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (make-directory (eden-request-dir req) 'parent)
    (should-error (eden-request-check req)))

  ;; everything ok
  (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (eden-request-write 'prompt req "")
    (eden-request-write 'request req "")
    (eden-request-write 'exchanges req "")
    (eden-request-write 'response req "")
    (eden-request-write 'response-org req "")
    (with-temp-buffer
      (write-file (concat (eden-request-dir req) "timestamp-1234")))
    (should (eden-request-check req))))

(ert-deftest eden-request-conversation-test ()
  ;; Signal error if the request doesn't exist in `:dir'
  (should-error
   (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo")))
     (eden-request-conversation req)))

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
  (let* ((req `(:req (:stream :false
                      :model "gpt-4o-mini"
                      :temperature 1
                      :messages [(:role "user" :content "foo user")])
                :api (:service "chatgpt"
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
                            :message (:role "assistant" :content "foo assistant\n" :refusal nil)
                            :logprobs nil :finish_reason "stop")]))
         (resp-str (eden-json-encode resp)))
    (message "%s" (eden-request-dir req))
    (eden-write-request req)
    (eden-write-response resp-str resp req)
    (should
     (equal
      (eden-request-conversation `(:dir ,dir :uuid "uuid-foo"))
      [(:uuid "uuid-foo"
        :prompt "foo prompt\n"
        :user "foo user"
        :assistant "foo assistant\n"
        :response "foo assistant\n")])))

  ;; conversation with no previous messages and reasoning content
  (let* ((req `(:req (:stream :false
                      :model "gpt-4o-mini"
                      :temperature 1
                      :messages [(:role "user" :content "foo user")])
                :api (:service "chatgpt"
                      :endpoint "https://api.openai.com/v1/chat/completions")
                :prompt "foo prompt\n"
                :dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo"))
         (dir (plist-get req :dir))
         (resp '(:id "5b5178d0-9cca-4a8b-86f9-6971ce2c1788"
                 :object "chat.completion"
                 :created 1738222989
                 :model "deepseek-reasoner"
                 :choices [(:index 0
                            :message (:role "assistant"
                                      :content "foo assistant\n"
                                      :reasoning_content "foo reasoning\n")
                            :logprobs nil
                            :finish_reason "stop")]))
         (resp-str (eden-json-encode resp)))
    (eden-write-request req)
    (eden-write-response resp-str resp req)
    (should
     (equal
      (eden-request-conversation `(:dir ,dir :uuid "uuid-foo"))
      [(:uuid "uuid-foo"
        :prompt "foo prompt\n"
        :user "foo user"
        :assistant "foo assistant\n"
        :response "foo assistant\n"
        :assistant-reasoning "foo reasoning\n"
        :reasoning "foo reasoning\n")])))

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
                :system-message "baz system message\n"
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
                :dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-baz"))
         (dir (plist-get req :dir))
         (resp '(:id "chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8"
                 :object "chat.completion"
                 :created 1733030031
                 :model "gpt-4o-mini-2024-07-18"
                 :choices [(:index 0
                            :message (:role "assistant" :content "baz assistant\n" :refusal nil)
                            :logprobs nil :finish_reason "stop")]))
         (resp-str (eden-json-encode resp)))
    (message "%s" (eden-request-dir req))
    (eden-write-request req)
    (eden-write-response resp-str resp req)
    (should
     (equal
      (eden-request-conversation `(:dir ,dir :uuid "uuid-baz"))
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
  (let* ((req `(:req (:stream :false
                      :model "gpt-4o-mini"
                      :temperature 1
                      :messages [(:role "user" :content "foo user")])

                :api (:service "chatgpt"
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
                :system-message "baz system message\n"
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

(ert-deftest eden-request-perplexity-citations-test ()
  ;; Signal error if the request doesn't exist in `:dir'
  (should-error
   (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo")))
     (eden-request-perplexity-citations req)))

  ;; Signal error when an error.json file exists in req directory
  (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (eden-request-write 'error req "")
    (should-error (eden-request-perplexity-citations req)))

  ;; Signal error when the request in incomplete
  (let* ((req `(:dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (make-directory (eden-request-dir req) 'parent)
    (should-error (eden-request-perplexity-citations req)))

  ;; conversation with no previous messages
  (let* ((req `(:req (:stream :false
                      :model "sonar"
                      :temperature 1
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
    (message "%s" (eden-request-dir req))
    (eden-write-request req)
    (eden-write-response resp-str resp req)
    (should
     (equal
      (eden-request-perplexity-citations `(:dir ,dir :uuid "uuid-foo"))
      '("https://foo-1.com" "https://foo-2.com" "https://foo-3.com"))))
  ;; 1) `bar-req' request has no citations in its response
  ;; 2) `err-req' request has no response.json file, this can happen if
  ;;    it is removed inadvertently
  (let* ((dir (concat (make-temp-file "eden-" t) "/"))
         (foo-req `(:req (:stream :false
                          :model "sonar"
                          :temperature 1
                          :messages [(:role "user" :content "foo user")])
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
                     :choices [(:index 0
                                :message (:role "assistant"
                                          :content "foo assistant[1][3]")
                                :finish_reason "stop")]))
         (foo-resp-str (eden-json-encode foo-resp))
         (bar-req `(:req (:stream :false
                          :model "sonar"
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
                    :dir ,dir
                    :uuid "uuid-bar"))
         ;; bar response with no citations
         (bar-resp '(:id "022348c0-2af6-435f-8c60-01ef5b3b39dd"
                     :object "chat.completion"
                     :created 1735366499
                     :model "sonar"
                     :choices [(:index 0
                                :message (:role "assistant"
                                          :content "bar assistant")
                                :finish_reason "stop")]))
         (bar-resp-str (eden-json-encode bar-resp))
         (err-req `(:req (:stream :false
                          :model "sonar"
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
                    :dir ,dir
                    :uuid "uuid-err"))
         (baz-req `(:req (:stream :false
                          :model "sonar"
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
                    :system-message "baz system message\n"
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
    (message "%s" (eden-request-dir baz-req))
    (eden-write-request foo-req)
    (eden-write-response foo-resp-str foo-resp `(:dir ,dir :uuid "uuid-foo"))
    (eden-write-request bar-req)
    (eden-write-response bar-resp-str bar-resp `(:dir ,dir :uuid "uuid-bar"))
    (eden-write-request err-req) ;; we don't write any response for `err-req'
    (eden-write-request baz-req)
    (eden-write-response baz-resp-str baz-resp baz-req)
    (should
     (equal
      (eden-request-perplexity-citations `(:dir ,dir :uuid "uuid-baz"))
      '("https://foo-1.com"
        "https://foo-2.com"
        "https://foo-3.com"
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

(ert-deftest eden-write-request-test ()
  (let* ((req `(:req (:stream :false
                      :model "gpt-4o-mini"
                      :temperature 1
                      :messages [(:role "user" :content "user prompt\n")])
                :api (:service "chatgpt"
                      :endpoint "https://api.openai.com/v1/chat/completions")
                :prompt "user prompt\n"
                :dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (message "%s" (eden-request-dir req))
    (eden-write-request req)
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (eden-request-file 'request req))
        (eden-json-read))
      (plist-get req :req)))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (eden-request-file 'api req))
        (eden-json-read))
      (plist-get req :api)))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (eden-request-file 'prompt req))
        (buffer-substring-no-properties (point-min) (point-max)))
      (plist-get req :prompt)))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (eden-request-file 'system-message req))
        (buffer-substring-no-properties (point-min) (point-max)))
      ""))
    (should-not
     (with-temp-buffer
       (insert-file-contents (eden-request-file 'exchanges req))
       (eden-json-read)))
    (should
     (= (length (directory-files (eden-request-dir req) nil "timestamp-"))
        1)))
  (let* ((req `(:req (:stream :false
                      :model "gpt-4o-mini"
                      :temperature 1
                      :messages [(:role "system" :content "system message\n")
                                 (:role "user" :content "bar user")
                                 (:role "assistant" :content "bar assistant")
                                 (:role "user" :content "baz prompt")
                                 (:role "assistant" :content "baz assistant")
                                 (:role "user" :content "user prompt\n")])
                :api (:service "chatgpt"
                      :endpoint "https://api.openai.com/v1/chat/completions")
                :prompt "user prompt\n"
                :system-message "system message\n"
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
                :dir ,(concat (make-temp-file "eden-" t) "/")
                :uuid "uuid-foo")))
    (message "%s" (eden-request-dir req))
    (eden-write-request req)
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (eden-request-file 'request req))
        (eden-json-read))
      (plist-get req :req)))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (eden-request-file 'api req))
        (eden-json-read))
      (plist-get req :api)))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (eden-request-file 'prompt req))
        (buffer-substring-no-properties (point-min) (point-max)))
      (plist-get req :prompt)))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (eden-request-file 'system-message req))
        (buffer-substring-no-properties (point-min) (point-max)))
      (plist-get req :system-message)))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (eden-request-file 'exchanges req))
        (eden-json-read))
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

(ert-deftest eden-sentinel-test ()
  ;; we let-bind requests, callbacks and infos with the variables
  ;; `-req', `-callback' and `-info' to be sure that `eden-sentinel'
  ;; macro doesn't rely on `req', `callback' and `info' (its argument names)
  ;; variables to be defined during its expansion.

  ;; Throw error when process buffer is killed
  (let* ((resp-str "")
         (-req `(:req (:foo "bar") ;; we don't use that request
                 :api (:service "openai"
                       :endpoint "https://api.openai.com/v1/chat/completions")
                 :dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo"))
         (-callback nil) ;; we don't call it
         (-callback-error nil) ;; we don't call it
         (-info nil) ;; we don't use it
         ;; we are testing that we throw an error if process has
         ;; been killed for some reason.  We wrapped the sentinel
         ;; generated by `eden-sentinel' in order to kill
         ;; the process buffer when the sentinel is called.
         (sentinel-wrapped
          (lambda (process event)
            (kill-buffer (process-buffer process))
            (funcall
             (eden-sentinel -req -callback -callback-error -info)
             process event)))
         (buff-name (generate-new-buffer-name "*ai-test*")))
    (should-error
     (let ((debug-on-error t))
       (eden-test-echo-resp resp-str buff-name sentinel-wrapped)
       (sleep-for 0.2)))
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
        :request ,(plist-get -req :req)))))


  ;; Throw error when process receives an event different from "finished\n"
  ;; Here we test it with "killed\n" event
  (let* ((resp-str "")
         (-req `(:req (:foo "bar") ;; we don't use that request
                 :api (:service "openai"
                       :endpoint "https://api.openai.com/v1/chat/completions")
                 :dir ,(concat (make-temp-file "eden-" t) "/")
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
        (eden-test-echo-resp
         resp-str buff-name
         (eden-sentinel -req -callback -callback-error -info)
         1))
       (sleep-for 0.2)))
    (should-not (get-buffer buff-name))
    (message "%s" (eden-request-file 'error -req))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (eden-request-file 'error -req))
        (eden-json-read))
      `(:type "eden-error-process"
        :message "The process did not finished correctly"
        :directory ,(eden-request-dir -req)
        :request ,(plist-get -req :req)
        :process-event "killed\n"
        :process-buffer-content "foo bar baz"))))

  ;; Here we test it with "interrupt\n" event
  (let* ((resp-str "")
         (-req `(:req (:foo "bar") ;; we don't use that request
                 :api (:service "openai"
                       :endpoint "https://api.openai.com/v1/chat/completions")
                 :dir ,(concat (make-temp-file "eden-" t) "/")
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
        (eden-test-echo-resp
         resp-str buff-name
         (eden-sentinel -req -callback -callback-error -info)
         1))
       (sleep-for 0.2)))
    (should-not (get-buffer buff-name))
    (message "%s" (eden-request-file 'error -req))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (eden-request-file 'error -req))
        (eden-json-read))
      `(:type "eden-error-process"
        :message "The process did not finished correctly"
        :directory ,(eden-request-dir -req)
        :request ,(plist-get -req :req)
        :process-event "interrupt\n"
        :process-buffer-content "foo bar baz"))))

  ;; Throw error when reading json response in process buffer
  (let* ((resp-str "not valid json")
         (-req `(:req (:foo "bar") ;; we don't use that request
                 :api (:service "openai"
                       :endpoint "https://api.openai.com/v1/chat/completions")
                 :dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo"))
         (-callback nil)      ;; we don't call it
         (-callback-error nil) ;; we don't call it
         (-info nil) ;; we don't use it
         (buff-name (generate-new-buffer-name "*ai-test*")))
    (should-error
     (let ((debug-on-error t))
       (eden-test-echo-resp
        resp-str buff-name
        (eden-sentinel -req -callback -callback-error -info))
       (sleep-for 0.2)))
    (should-not (get-buffer buff-name))
    (message "%s" (eden-request-file 'error -req))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (eden-request-file 'error -req))
        (eden-json-read))
      `(:type "eden-error-json-read"
        :message "Error while parsing JSON in process buffer"
        :directory ,(eden-request-dir -req)
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
                 :dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo"))
         (-callback nil)      ;; we don't call it
         (-callback-error nil) ;; we don't call it
         (-info nil) ;; we don't use it
         (buff-name (generate-new-buffer-name "*ai-test*")))
    (should-error
     (let ((debug-on-error t))
       (eden-test-echo-resp
        resp-str buff-name
        (eden-sentinel -req -callback -callback-error -info))
       (sleep-for 0.2)))
    (should-not (get-buffer buff-name))
    (message "%s" (eden-request-file 'error -req))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (eden-request-file 'error -req))
        (eden-json-read))
      `(:type "eden-error-api"
        :message "API error"
        :directory ,(eden-request-dir -req)
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
                 :dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo"))
         (-callback 'callback-not-a-function)
         (-callback-error nil) ;; we don't call it
         (-info nil) ;; we don't use it
         (buff-name (generate-new-buffer-name "*ai-test*")))
    (should-error
     (let ((debug-on-error t))
       (eden-test-echo-resp
        resp-str buff-name
        (eden-sentinel -req -callback -callback-error -info))
       (sleep-for 0.2)))
    (should-not (get-buffer buff-name))
    (message "%s" (eden-request-file 'error -req))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (eden-request-file 'error -req))
        (eden-json-read))
      `(:type "eden-error-callback"
        :message "Error while calling callback function in sentinel"
        :directory ,(eden-request-dir -req)
        :request ,(plist-get -req :req)
        :error ["void-function" "callback-not-a-function"]))))


  ;; Test that `callback-error' is called when an error occurs
  ;; in the sentinel call.  To do so we send the "killed\n" event
  ;; to the process which triggers an error.
  ;;
  ;; 1) First we test that we throw a 'eden-error-callback-error error
  ;;    when `callback-error' itself throws an error
  (let* ((resp-str "")
         (-req `(:req (:foo "bar") ;; we don't use that request
                 :api (:service "openai"
                       :endpoint "https://api.openai.com/v1/chat/completions")
                 :dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo"))
         (-callback nil)      ;; we don't call it
         ;; Wrong callback-error function
         (-callback-error 'callback-error-not-a-function)
         (-info nil) ;; we don't use it
         (buff-name (generate-new-buffer-name "*ai-test*")))
    (should-error
     (let ((debug-on-error t))
       (kill-process
        (eden-test-echo-resp
         resp-str buff-name
         (eden-sentinel -req -callback -callback-error -info)
         1))
       (sleep-for 0.2)))
    (let* ((error.json
            (with-temp-buffer
              (insert-file-contents (eden-request-file 'error -req))
              (eden-json-read)))
           (original-error (plist-get error.json :original-error)))
      (should (string= (plist-get original-error :type)
                       "eden-error-process"))
      (should
       (equal
        (seq-subseq error.json 0 10)
        `(:type "eden-error-callback-error"
          :message "Error while calling callback-error function when signaling an error in sentinel"
          :directory ,(eden-request-dir -req)
          :request ,(plist-get -req :req)
          :error ["void-function" "callback-error-not-a-function"])))))
  ;; 2) Then we test that `callback-error' is called correctly
  (let* ((resp-str "")
         (-req `(:req (:foo "bar") ;; we don't use that request
                 :api (:service "openai"
                       :endpoint "https://api.openai.com/v1/chat/completions")
                 :dir ,(concat (make-temp-file "eden-" t) "/")
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
        (eden-test-echo-resp
         resp-str buff-name
         (eden-sentinel -req -callback -callback-error -info)
         1))
       (sleep-for 0.2)))
    (should
     (string=
      (thread-first
        (with-temp-buffer
          (insert-file-contents (eden-request-file 'error -req))
          (eden-json-read))
        (plist-get :type))
      "eden-error-process"))
    (should
     (equal in-callback-error
            (list :req -req
                  :error-type "eden-error-process"
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
                 :dir ,(concat (make-temp-file "eden-" t) "/")
                 :uuid "uuid-foo"))
         (in-callback nil)
         (-callback (lambda (req resp info)
                      (setq in-callback (list :req req :resp resp :info info))))
         (-callback-error nil) ;; we don't call it
         (-info '(:foo "bar"))
         (buff-name (generate-new-buffer-name "*ai-test*")))
    (let ((debug-on-error t))
      (eden-test-echo-resp
       resp-str buff-name
       (eden-sentinel -req -callback -callback-error -info))
      (sleep-for 0.2))
    (should-not (get-buffer buff-name))
    (should
     (equal
      (with-temp-buffer
        (insert-file-contents (eden-request-file 'response -req))
        (eden-json-read))
      resp))
    (should
     (equal in-callback `(:req ,-req :resp ,resp :info ,-info)))))

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

;;;; Prompt and Request history

(ert-deftest eden-request-history-set-test ()
  ;; `eden-dir' doesn't exist
  (let* (eden-request-history
         (eden-dir (concat (make-temp-file "eden-" t) "/")))
    (delete-directory eden-dir)
    (eden-request-history-set)
    (should-not eden-request-history))
  ;; `eden-dir' exist with request and timestamp files
  (let* (eden-request-history
         (eden-dir (concat (make-temp-file "eden-" t) "/"))
         (req-0 (eden-request :prompt "req-0"))
         (req-1 (eden-request :prompt "req-1"))
         (req-2 (eden-request :prompt "req-2"))
         (req-3 (eden-request :prompt "req-3"))
         (uuid-0 (plist-get req-0 :uuid))
         (uuid-1 (plist-get req-1 :uuid))
         (uuid-2 (plist-get req-2 :uuid))
         (uuid-3 (plist-get req-3 :uuid)))
    (message "%s" eden-dir)
    (eden-write-request req-0)
    (eden-write-request req-1)
    (eden-write-request req-2)
    (eden-write-request req-3)
    (eden-request-history-set)
    (should
     (equal eden-request-history (list uuid-3 uuid-2 uuid-1 uuid-0)))))

(ert-deftest eden-prompt-history-previous/next ()
  ;; default usage
  (should
   (equal (eden-prompt-history-previous [("foo" "bar" "baz") nil nil])
          [("bar" "baz") "foo" nil]))
  (should
   (equal (eden-prompt-history-previous [("bar" "baz") "foo" nil])
          [("baz") "bar" ("foo")]))
  (should
   (equal (eden-prompt-history-previous [("baz") "bar" ("foo")])
          [nil "baz" ("bar" "foo")]))
  (should
   (equal (eden-prompt-history-previous [nil "baz" ("bar" "foo")])
          [nil "baz" ("bar" "foo")]))
  (should
   (equal (eden-prompt-history-next [nil "baz" ("bar" "foo")])
          [("baz") "bar" ("foo")]))
  (should
   (equal (eden-prompt-history-next [("baz") "bar" ("foo")])
          [("bar" "baz") "foo" nil]))
  (should
   (equal (eden-prompt-history-next [("bar" "baz") "foo" nil])
          [("bar" "baz") "foo" nil]))

  ;; signal error if both `prompt' and `discard-current' optional
  ;; arguments are non nil
  ;; todo
  (should-error
   (eden-prompt-history-previous
    [("foo" "bar" "baz") nil nil] '(:prompt "scratch prompt") 'discard-current))

  (should-error
   (eden-prompt-history-next
    [nil "baz" ("bar" "foo")] '(:prompt "scratch prompt") 'discard-current))

  ;; with `prompt' optional argument
  (should
   (equal (eden-prompt-history-previous
           [("foo" "bar" "baz") nil nil] '(:prompt "scratch prompt"))
          [("bar" "baz") "foo" ((:prompt "scratch prompt"))]))
  (should
   (equal (eden-prompt-history-previous
           [("bar" "baz") "foo" nil] '(:prompt "scratch prompt"))
          [("baz") "bar" ((:prompt "scratch prompt") "foo")]))
  (should
   (equal (eden-prompt-history-previous
           [("baz") "bar" ("foo")] '(:prompt "scratch prompt"))
          [nil "baz" ((:prompt "scratch prompt") "bar" "foo")]))
  (should
   (equal (eden-prompt-history-previous
           [nil "baz" ("bar" "foo")] '(:prompt "scratch prompt"))
          [nil "baz" ("bar" "foo")]))
  (should
   (equal (eden-prompt-history-next
           [nil "baz" ("bar" "foo")] '(:prompt "scratch prompt"))
          [((:prompt "scratch prompt") "baz") "bar" ("foo")]))
  (should
   (equal (eden-prompt-history-next
           [("baz") "bar" ("foo")] '(:prompt "scratch prompt"))
          [((:prompt "scratch prompt") "bar" "baz") "foo" nil]))
  (should
   (equal (eden-prompt-history-next
           [("bar" "baz") "foo" nil] '(:prompt "scratch prompt"))
          [("bar" "baz") "foo" nil]))

  ;; with `discard-current' optional argument
  (should
   (equal (eden-prompt-history-previous
           [("foo" "bar") "to-be-discarded" nil] nil 'discard-current)
          [("bar") "foo" nil]))
  (should
   (equal (eden-prompt-history-previous
           [("bar") "to-be-discarded" ("foo")] nil 'discard-current)
          [nil "bar" ("foo")]))
  (should
   (equal (eden-prompt-history-next
           [nil "to-be-discarded" ("bar" "foo")] nil 'discard-current)
          [nil "bar" ("foo")]))
  (should
   (equal (eden-prompt-history-next
           [("bar") "to-be-discarded" ("foo")] nil 'discard-current)
          [("bar") "foo" nil])))

(ert-deftest eden-prompt-current-test ()
  (let ((eden-prompt-history-state [("foo-uuid" "bar-uuid" "baz-uuid") nil nil]))
    (should-not (eden-prompt-current)))
  (let ((eden-prompt-history-state
         [("foo-uuid" "bar-uuid" "baz-uuid") (:prompt "scratch prompt") nil]))
    (should (string= (eden-prompt-current) "scratch prompt")))
  (let* ((eden-dir (concat (make-temp-file "eden-" t) "/"))
         (req `(:dir ,eden-dir :uuid "foo-uuid"))
         (eden-prompt-history-state [("bar-uuid" "baz-uuid") "foo-uuid" nil]))
    (message "%S" (eden-request-dir req))
    (eden-request-write 'prompt req "foo prompt\n")
    (should (string= (eden-prompt-current) "foo prompt\n"))))

;;;; Conversations

(ert-deftest eden-conversation-with-title-exists-p-test ()
  (let ((eden-conversations nil))
    (should-not (eden-conversation-with-title-exists-p "bar title")))
  (let ((eden-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :action start :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :action start-from :last-req-uuid "bar-req-uuid"))
           ("conversation-id-baz" .
            (:title "baz title" :action continue-from :last-req-uuid "baz-req-uuid")))))
    (should (eden-conversation-with-title-exists-p "bar title"))))

(ert-deftest eden-conversation-test ()

  ;; Titles must be unique
  (let ((eden-conversations
         '(("conversation-id-foo" . (:title "foo title"
                                     :action start
                                     :last-req-uuid nil)))))
    (should-error
     (eden-conversation 'start "foo title")))

  ;; actions must be start, start-from, continue-from
  (let ((eden-conversations nil))
    (should-error
     (eden-conversation 'wrong-action "foo title")))

  ;; action - start
  (let (eden-conversations eden-conversation-id)
    (cl-letf (((symbol-function 'eden-uuid)
               (lambda nil "conversation-id-foo")))
      (eden-conversation 'start "foo title")
      (should
       (equal eden-conversations
              '(("conversation-id-foo" . (:title "foo title"
                                          :action start
                                          :last-req-uuid nil)))))
      (should
       (string= eden-conversation-id "conversation-id-foo"))))
  (let ((eden-conversations
         '(("conversation-id-foo" . (:title "foo title"
                                     :action start
                                     :last-req-uuid nil))))
        eden-conversation-id)
    (cl-letf (((symbol-function 'eden-uuid)
               (lambda nil "conversation-id-bar")))
      (eden-conversation 'start "bar title")
      (should
       (seq-set-equal-p
        (mapcar #'car eden-conversations)
        '("conversation-id-foo" "conversation-id-bar")))
      (should
       (equal (alist-get "conversation-id-bar" eden-conversations nil nil 'string=)
              '(:title "bar title" :action start :last-req-uuid nil)))
      (should
       (string= eden-conversation-id "conversation-id-bar"))))

  ;; action - start
  ;; error - req-uuid must not be specified
  (let ((eden-conversations nil))
    (should-error
     (eden-conversation 'start "foo title" "foo-req-uuid")))

  ;; actions - start-from, continue-from
  ;; error - req-uuid is mandatory
  (let ((eden-conversations nil))
    (should-error (eden-conversation 'start-from "foo title")))
  ;; error - req associated with req-uuid doesn't exist in `eden-dir'
  (let ((eden-conversations nil)
        (eden-dir (concat (make-temp-file "eden-" t) "/")))
    (should-error
     (eden-conversation 'start-from "foo title" "foo-req-uuid")))

  ;; error -  when an error.json file exists in req directory
  (let* ((eden-conversations nil)
         (eden-dir (concat (make-temp-file "eden-" t) "/"))
         (req `(:dir ,eden-dir :uuid "foo-req-uuid")))
    (eden-request-write 'error req "")
    (should-error
     (eden-conversation 'start-from "foo title" "foo-req-uuid")))

  ;; error - when req associated with req-uuid is in incomplete
  (let* ((eden-conversations nil)
         (eden-dir (concat (make-temp-file "eden-" t) "/"))
         (req `(:dir ,eden-dir :uuid "foo-req-uuid")))
    (make-directory (eden-request-dir req) 'parent)
    (should-error
     (eden-conversation 'start-from "foo title" "foo-req-uuid")))

  ;; action - start-from, continue-from
  ;; we can start-from and continue-from multiple different conversations
  ;; from the same req-uuid
  (let* (eden-conversations
         eden-conversation-id
         (eden-dir (concat (make-temp-file "eden-" t) "/"))
         (foo-req (eden-request :prompt "foo-req")) ;; depend on `eden-dir'
         (foo-req-uuid (plist-get foo-req :uuid))
         (bar-req (eden-request :prompt "bar-req")) ;; depend on `eden-dir'
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
         (resp-str (eden-json-encode resp)))
    (eden-write-request foo-req)
    (eden-write-response resp-str resp foo-req)
    (eden-write-request bar-req)
    (eden-write-response resp-str resp bar-req)

    (let ((n 0))
      (cl-letf (((symbol-function 'eden-uuid)
                 (lambda nil (format "conversation-id-foo-%s" (cl-incf n)))))
        (eden-conversation 'start-from "foo-1 title" foo-req-uuid)
        (eden-conversation 'start-from "foo-2 title" foo-req-uuid)
        (eden-conversation 'continue-from "foo-3 title" foo-req-uuid)))

    (let ((n 0))
      (cl-letf (((symbol-function 'eden-uuid)
                 (lambda nil (format "conversation-id-bar-%s" (cl-incf n)))))
        (eden-conversation 'start-from  "bar-1 title" bar-req-uuid)
        (eden-conversation 'continue-from  "bar-2 title" bar-req-uuid)
        (eden-conversation 'continue-from  "bar-3 title" bar-req-uuid)))

    (should
     (seq-set-equal-p
      (mapcar #'car eden-conversations)
      '("conversation-id-foo-1" "conversation-id-foo-2" "conversation-id-foo-3"
        "conversation-id-bar-1" "conversation-id-bar-2" "conversation-id-bar-3")))
    (should
     (string= eden-conversation-id "conversation-id-bar-3"))

    (should
     (equal
      (alist-get "conversation-id-foo-1" eden-conversations nil nil 'string=)
      `(:title "foo-1 title" :action start-from  :last-req-uuid ,foo-req-uuid)))
    (should
     (equal
      (alist-get "conversation-id-foo-2" eden-conversations nil nil 'string=)
      `(:title "foo-2 title" :action start-from  :last-req-uuid ,foo-req-uuid)))
    (should
     (equal
      (alist-get "conversation-id-foo-3" eden-conversations nil nil 'string=)
      `(:title "foo-3 title" :action continue-from  :last-req-uuid ,foo-req-uuid)))
    (should
     (equal
      (alist-get "conversation-id-bar-1" eden-conversations nil nil 'string=)
      `(:title "bar-1 title" :action start-from  :last-req-uuid ,bar-req-uuid)))
    (should
     (equal
      (alist-get "conversation-id-bar-2" eden-conversations nil nil 'string=)
      `(:title "bar-2 title" :action continue-from  :last-req-uuid ,bar-req-uuid)))
    (should
     (equal
      (alist-get "conversation-id-bar-3" eden-conversations nil nil 'string=)
      `(:title "bar-3 title" :action continue-from  :last-req-uuid ,bar-req-uuid))))

  (let* ((eden-conversations
          '(("conversation-id-foo" . (:title "foo title"
                                      :action start
                                      :last-req-uuid nil))))
         eden-conversation-id
         (eden-dir (concat (make-temp-file "eden-" t) "/"))
         (foo-req (eden-request :prompt "foo-req")) ;; depend on `eden-dir'
         (foo-req-uuid (plist-get foo-req :uuid))
         (resp '(:id "chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8"
                 :object "chat.completion"
                 :created 1733030031
                 :model "gpt-4o-mini-2024-07-18"
                 :choices [(:index 0
                            :message (:role "assistant" :content "assistant" :refusal nil)
                            :logprobs nil :finish_reason "stop")]))
         (resp-str (eden-json-encode resp)))
    (eden-write-request foo-req)
    (eden-write-response resp-str resp foo-req)

    (cl-letf (((symbol-function 'eden-uuid)
               (lambda nil "conversation-id-bar")))
      (eden-conversation 'start-from "bar title" foo-req-uuid))
    (cl-letf (((symbol-function 'eden-uuid)
               (lambda nil "conversation-id-baz")))
      (eden-conversation 'continue-from "baz title" foo-req-uuid))

    (should
     (seq-set-equal-p
      (mapcar #'car eden-conversations)
      '("conversation-id-foo" "conversation-id-bar" "conversation-id-baz")))
    (should
     (string= eden-conversation-id "conversation-id-baz"))
    (should
     (equal
      (alist-get "conversation-id-bar" eden-conversations nil nil 'string=)
      `(:title "bar title" :action start-from  :last-req-uuid ,foo-req-uuid)))
    (should
     (equal
      (alist-get "conversation-id-baz" eden-conversations nil nil 'string=)
      `(:title "baz title" :action continue-from  :last-req-uuid ,foo-req-uuid)))))


(ert-deftest eden-conversation-title-test ()
  (let ((eden-conversations nil))
    (should-not (eden-conversation-title "conversation-id-bar")))
  (let ((eden-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :action start :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :action start-from :last-req-uuid "bar-req-uuid"))
           ("conversation-id-baz" .
            (:title "baz title" :action continue-from :last-req-uuid "baz-req-uuid")))))
    (should
     (string=
      (eden-conversation-title "conversation-id-bar")
      "bar title"))))

(ert-deftest eden-conversation-action-test ()
  (let ((eden-conversations nil))
    (should-not (eden-conversation-action "conversation-id-bar")))
  (let ((eden-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :action start :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :action start-from :last-req-uuid "bar-req-uuid"))
           ("conversation-id-baz" .
            (:title "baz title" :action continue-from :last-req-uuid "baz-req-uuid")))))
    (should
     (eq (eden-conversation-action "conversation-id-foo")
         'start))
    (should
     (eq (eden-conversation-action "conversation-id-bar")
         'start-from))
    (should
     (eq (eden-conversation-action "conversation-id-baz")
         'continue-from))))

(ert-deftest eden-conversation-last-req-test ()
  (let ((eden-conversations nil))
    (should-not (eden-conversation-title "conversation-id-bar")))
  (let ((eden-dir "/tmp/eden/")
        (eden-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :action start :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :action start-from :last-req-uuid "bar-req-uuid"))
           ("conversation-id-baz" .
            (:title "baz title" :action continue-from :last-req-uuid "baz-req-uuid")))))
    (should-not (eden-conversation-last-req "conversation-id-foo"))
    (should
     (equal
      (eden-conversation-last-req "conversation-id-bar")
      `(:uuid "bar-req-uuid" :dir "/tmp/eden/")))
    (should
     (equal
      (eden-conversation-last-req "conversation-id-baz")
      `(:uuid "baz-req-uuid" :dir "/tmp/eden/")))))

(ert-deftest eden-conversation-buffer-name-test ()
  (let ((eden-conversations nil))
    (should-not (eden-conversation-buffer-name "conversation-id-foo"))
    (should-not (eden-conversation-buffer-name nil)))
  (let ((eden-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :action start :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :action start-from :last-req-uuid "bar-req-uuid"))
           ("conversation-id-baz" .
            (:title "baz title" :action continue-from :last-req-uuid "baz-req-uuid")))))
    (should-not (eden-conversation-buffer-name nil))
    (should-not (eden-conversation-buffer-name "wrong-id"))
    (should
     (string=
      (eden-conversation-buffer-name "conversation-id-foo")
      (eden-buffer-name "foo title")))))

(ert-deftest eden-conversation-exchanges-test ()
  (let* ((eden-dir (concat (make-temp-file "eden-" t) "/"))
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
            :system-message "baz system message\n"
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
            :dir ,eden-dir
            :uuid "uuid-baz"))
         ;; #_(dir (plist-get req :dir))
         (last-req-uuid (plist-get last-req :uuid))
         (last-resp '(:id "chatcmpl-AZWZDflWKlARNWTUJu7bAorpW5KF8"
                      :object "chat.completion"
                      :created 1733030031
                      :model "gpt-4o-mini-2024-07-18"
                      :choices [(:index 0
                                 :message (:role "assistant" :content "baz assistant\n" :refusal nil)
                                 :logprobs nil :finish_reason "stop")]))
         (last-resp-str (eden-json-encode last-resp))
         (eden-conversations
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
    (eden-write-request last-req)
    (eden-write-response last-resp-str last-resp last-req)
    (should-not (eden-conversation-exchanges "conversation-id-start"))
    (should
     (equal
      (eden-conversation-exchanges "conversation-id-start-from")
      [(:uuid "uuid-baz"
        :prompt "baz prompt\n"
        :user "baz user"
        :assistant "baz assistant\n"
        :response "baz assistant\n")]
      ))
    (should
     (equal
      (eden-conversation-exchanges "conversation-id-continue-from")
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

(ert-deftest eden-conversation-rename-test ()
  (let ((eden-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :action start :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :action start-from :last-req-uuid "bar-req-uuid")))))
    (should-error (eden-conversation-rename "conversation-id-foo" "bar title")))

  (let ((eden-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :action start :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :action start-from :last-req-uuid "bar-req-uuid"))
           ("conversation-id-baz" .
            (:title "baz title" :action continue-from :last-req-uuid "baz-req-uuid")))))
    (eden-conversation-rename "conversation-id-foo" "FOO")
    (eden-conversation-rename "conversation-id-bar" "BAR")
    (eden-conversation-rename "conversation-id-baz" "BAZ")
    (eden-conversation-rename "not-in--eden-conversations" "foo bar baz")
    (should
     (equal
      (alist-get "conversation-id-foo" eden-conversations nil nil 'string=)
      '(:title "FOO" :action start :last-req-uuid nil)))
    (should
     (equal
      (alist-get "conversation-id-bar" eden-conversations nil nil 'string=)
      '(:title "BAR" :action start-from :last-req-uuid "bar-req-uuid")))
    (should
     (equal
      (alist-get "conversation-id-baz" eden-conversations nil nil 'string=)
      '(:title "BAZ" :action continue-from :last-req-uuid "baz-req-uuid")))
    (should
     (seq-set-equal-p
      (mapcar #'car eden-conversations)
      '("conversation-id-foo" "conversation-id-bar" "conversation-id-baz")))
    (should (= (length eden-conversations) 3))))

(ert-deftest eden-conversation-update-test ()
  (let ((eden-conversations
         '(("conversation-id-foo" .
            (:title "foo title" :action start :last-req-uuid nil))
           ("conversation-id-bar" .
            (:title "bar title" :action start-from :last-req-uuid "bar-req-uuid"))
           ("conversation-id-baz" .
            (:title "baz title" :action continue-from :last-req-uuid "baz-req-uuid")))))
    (eden-conversation-update '(:conversation-id "conversation-id-foo")
                              '(:uuid "new-foo-req-uuid"))
    (eden-conversation-update '(:conversation-id "conversation-id-bar")
                              '(:uuid "new-bar-req-uuid"))
    (eden-conversation-update '(:conversation-id "conversation-id-baz")
                              '(:uuid "new-baz-req-uuid"))
    (eden-conversation-update '(:conversation-id "not-in--eden-conversations")
                              '(:uuid "fake-req-uuid"))
    (should
     (equal
      (alist-get "conversation-id-foo" eden-conversations nil nil 'string=)
      '(:title "foo title" :action continue-from :last-req-uuid "new-foo-req-uuid")))
    (should
     (equal
      (alist-get "conversation-id-bar" eden-conversations nil nil 'string=)
      '(:title "bar title" :action continue-from :last-req-uuid "new-bar-req-uuid")))
    (should
     (equal
      (alist-get "conversation-id-baz" eden-conversations nil nil 'string=)
      '(:title "baz title" :action continue-from :last-req-uuid "new-baz-req-uuid")))
    (should
     (seq-set-equal-p
      (mapcar #'car eden-conversations)
      '("conversation-id-foo" "conversation-id-bar" "conversation-id-baz")))
    (should (= (length eden-conversations) 3))))

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
  (let ((req (eden-request
              :prompt "foo bar baz"
              :dir (concat (make-temp-file "eden-" t) "/")))
        (resp '(:model "gpt-4o-mini-2024-07-18"
                :choices [(:index 0
                           :message (:role "assistant"
                                     :content "foo bar baz assistant response"))])))
    (eden-write-request req)
    (eden-write-response (eden-json-encode resp) resp req)
    (should-error (eden-conversation-insert req nil)))

  ;; We call `eden-test-add-or-replace-timestamp-file' before
  ;; inserting the conversation.  This adds or replaces the timestamp
  ;; file that `eden-conversation-insert' uses to determine the
  ;; date of the request

  ;; conversation with no previous exchanges
  (should
   (string=
    (with-temp-buffer
      (org-mode)
      (cl-letf (((symbol-function 'eden-uuid)
                 (lambda nil "uuid")))
        (let ((eden-org-property-date "EDEN_DATE")
              (eden-org-property-model "EDEN_MODEL")
              (eden-org-property-req "EDEN_REQ")
              (req (eden-request
                    :prompt "foo bar baz"
                    :dir (concat (make-temp-file "eden-" t) "/")
                    :api '(:service "openai")))
              (resp '(:model "gpt-4o-mini-2024-07-18"
                      :choices [(:index 0
                                 :message (:role "assistant"
                                           :content "foo bar baz assistant response"))])))
          (eden-write-request req)
          (eden-write-response (eden-json-encode resp) resp req)
          (eden-test-add-or-replace-timestamp-file req)
          (eden-conversation-insert req "Conversation")))
      (buffer-substring-no-properties (point-min) (point-max)))
    "** Conversation
:PROPERTIES:
:EDEN_DATE: [2024-12-20 Fri]
:EDEN_MODEL: openai/gpt-4o-mini-2024-07-18
:EDEN_REQ: uuid
:END:
*** Prompt

foo bar baz

*** Response

foo bar baz assistant response

"))

  ;; conversation:
  ;; - with no previous exchanges
  ;; - with reasoning content
  ;; - with `eden-include-reasoning' sets to t
  (should
   (string=
    (with-temp-buffer
      (org-mode)
      (cl-letf (((symbol-function 'eden-uuid)
                 (lambda nil "uuid")))
        (let ((eden-include-reasoning t)
              (eden-org-property-date "EDEN_DATE")
              (eden-org-property-req "EDEN_REQ")
              (eden-org-property-model "EDEN_MODEL")
              (req (eden-request
                    :prompt "foo bar baz"
                    :dir (concat (make-temp-file "eden-" t) "/")
                    :api '(:service "deepseek")))
              (resp '(:model "deepseek-reasoner"
                      :choices [(:index 0
                                 :message (:role "assistant"
                                           :content "foo bar baz assistant response"
                                           :reasoning_content "foo bar baz assistant reasoning"))])))
          (eden-write-request req)
          (eden-write-response (eden-json-encode resp) resp req)
          (eden-test-add-or-replace-timestamp-file req)
          (eden-conversation-insert req "Conversation")))
      (buffer-substring-no-properties (point-min) (point-max)))
    "** Conversation
:PROPERTIES:
:EDEN_DATE: [2024-12-20 Fri]
:EDEN_MODEL: deepseek/deepseek-reasoner
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
  ;; - with reasoning content
  ;; - with `eden-include-reasoning' sets to nil
  (should
   (string=
    (with-temp-buffer
      (org-mode)
      (cl-letf (((symbol-function 'eden-uuid)
                 (lambda nil "uuid")))
        (let ((eden-include-reasoning nil)
              (eden-org-property-date "EDEN_DATE")
              (eden-org-property-model "EDEN_MODEL")
              (eden-org-property-req "EDEN_REQ")
              (req (eden-request
                    :prompt "foo bar baz"
                    :dir (concat (make-temp-file "eden-" t) "/")
                    :api '(:service "deepseek")))
              (resp '(:model "deepseek-reasoner"
                      :choices [(:index 0
                                 :message (:role "assistant"
                                           :content "foo bar baz assistant response"
                                           :reasoning_content "foo bar baz assistant reasoning"))])))
          (eden-write-request req)
          (eden-write-response (eden-json-encode resp) resp req)
          (eden-test-add-or-replace-timestamp-file req)
          (eden-conversation-insert req "Conversation")))
      (buffer-substring-no-properties (point-min) (point-max)))
    "** Conversation
:PROPERTIES:
:EDEN_DATE: [2024-12-20 Fri]
:EDEN_MODEL: deepseek/deepseek-reasoner
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
      (cl-letf (((symbol-function 'eden-uuid)
                 (lambda nil "uuid")))
        (let ((eden-org-property-date "EDEN_DATE")
              (eden-org-property-model "EDEN_MODEL")
              (eden-org-property-req "EDEN_REQ")
              (req (eden-request
                    :prompt "* title-1\n** foo\n\nbar baz\n\n* title-2\n** foo\n\nbar baz"
                    :dir (concat (make-temp-file "eden-" t) "/")
                    :api '(:service "openai")))
              (resp '(:model "o1-mini-2024-09-12"
                      :choices [(:index 0
                                 :message (:role "assistant"
                                           :content "### assistant title-1 \n#### foo\n\n bar baz\n\n### title-2\n#### foo\n\n bar baz"))]))
              (title "Title of the request"))
          (eden-write-request req)
          (eden-write-response (eden-json-encode resp) resp req)
          (eden-test-add-or-replace-timestamp-file req)
          (eden-conversation-insert req title)))
      (buffer-substring-no-properties (point-min) (point-max)))
    "** Title of the request
:PROPERTIES:
:EDEN_DATE: [2024-12-20 Fri]
:EDEN_MODEL: openai/o1-mini-2024-09-12
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
      (cl-letf (((symbol-function 'eden-uuid)
                 (lambda nil "uuid-baz")))
        (let ((eden-org-property-date "EDEN_DATE")
              (eden-org-property-model "EDEN_MODEL")
              (eden-org-property-req "EDEN_REQ")
              (req (eden-request
                    :prompt "* baz-heading-1\n** baz-heading-2\n\nbaz-content"
                    :dir (concat (make-temp-file "eden-" t) "/")
                    :api '(:service "openai")
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
          (message "%s" (plist-get req :dir))
          (eden-write-request req)
          (eden-write-response (eden-json-encode resp) resp req)
          (eden-test-add-or-replace-timestamp-file req)
          (eden-conversation-insert req "Conversation")))
      (buffer-substring-no-properties (point-min) (point-max)))
    "** Conversation
:PROPERTIES:
:EDEN_DATE: [2024-12-20 Fri]
:EDEN_MODEL: openai/o1-mini-2024-09-12
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
  ;; As we're appending, the date is not inserted again by
  ;; `eden-conversation-insert', so we don't need to call
  ;; `eden-test-add-or-replace-timestamp-file' to add a timestamp
  ;; file
  (should
   (string=
    (with-temp-buffer
      (org-mode)
      (cl-letf (((symbol-function 'eden-uuid)
                 (lambda nil "uuid-baz")))
        (let ((eden-org-property-date "EDEN_DATE")
              (eden-org-property-model "EDEN_MODEL")
              (eden-org-property-req "EDEN_REQ")
              (req (eden-request
                    :prompt "* baz-heading-1\n** baz-heading-2\n\nbaz-content"
                    :dir (concat (make-temp-file "eden-" t) "/")
                    :api '(:service "openai")
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
:EDEN_DATE: [date]
:EDEN_MODEL: openai/gpt-4o-mini-2024-07-18
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
          (eden-conversation-insert req nil 'append)))
      (buffer-substring-no-properties (point-min) (point-max)))
    "** Title of the conversation
:PROPERTIES:
:EDEN_DATE: [date]
:EDEN_MODEL: openai/o1-mini-2024-09-12
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

  ;; last argument `start-from' indicates we are starting a
  ;; conversation from `req' but we didn't send a new request
  ;; so far and we only want to insert the last exchange
  ;; user/assistant, not the whole conversation in `req' so far
  (should
   (string=
    (with-temp-buffer
      (org-mode)
      (cl-letf (((symbol-function 'eden-uuid)
                 (lambda nil "uuid")))
        (let* ((eden-org-property-date "EDEN_DATE")
               (eden-org-property-model "EDEN_MODEL")
               (eden-org-property-req "EDEN_REQ")
               (eden-dir (concat (make-temp-file "eden-" t) "/"))
               (req (eden-request
                     :prompt "* baz-heading-1\n** baz-heading-2\n\nbaz-content"
                     :api '(:service "openai")
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
          (eden-write-request req)
          (eden-write-response (eden-json-encode resp) resp req)
          (eden-test-add-or-replace-timestamp-file req)
          (eden-conversation-insert
           req "Title of the conversation" nil 'start-from)))
      (buffer-substring-no-properties (point-min) (point-max)))
    (concat "** Title of the conversation
:PROPERTIES:
:EDEN_DATE: [2024-12-20 Fri]
:EDEN_MODEL: openai/o1-mini-2024-09-12
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

")))
  )

;;;; Sending Requests

(ert-deftest eden-kill-last-request-test ()
  ;; See test `eden-send-request-test' for explanation about
  ;; redefining `eden-request-send'.

  (let ((debug-on-error t)
        (eden-dir (concat (make-temp-file "eden-" t) "/"))
        pr-timer)
    (cl-letf
        (((symbol-function 'eden-request-send)
          ;; Print "resp-baz" in stdout after 1 second
          ;; with sentinel constructed using `eden-sentinel'
          (lambda (req callback callback-error info)
            (eden-write-request req)
            (eden-test-echo-resp
             "\"resp-baz\"" (generate-new-buffer-name "eden")
             (eval (macroexpand-all
                    `(eden-sentinel
                      (quote ,req)
                      ;; no need for `quote' because car is `lambda'
                      ,callback
                      ;; works because ,callback-error returns (closure ...)
                      ;; because it is define with `lambda' in `eden-send-request'
                      (quote ,callback-error)
                      (quote ,info))))
             1))))
      (eden-send-request :req (eden-request :prompt "req")))
    (setq pr-timer eden-pending-timer)
    ;; When we kill the last request, we actually kill its
    ;; associated process which signal an error.
    ;; But here we are not interested in that error but in checking
    ;; that global variables of interest are set back correctly after
    ;; the process is killed.
    (should-error
     (progn
       (eden-kill-last-request)
       (sleep-for 0.2)))

    (should (equal eden-pending-requests nil))
    (should (equal eden-pending-timer nil))
    ;; waiting widget is gone
    (should-not (memq pr-timer timer-list))
    (should-not global-mode-string)))


(ert-deftest eden-pending-conversation-p-test ()
  ;; Values of :req and :proc keys are normally not string, but
  ;; as we don't use them `eden-pending-requests', it's ok.
  (let ((eden-pending-requests '((:conversation-id "conv-foo"
                                  :req "req-foo"
                                  :proc "proc-foo")
                                 (:req "req-bar"
                                  :proc "proc-bar"))))
    (should (eden-pending-conversation-p "conv-foo"))
    (should-not (eden-pending-conversation-p "conv-baz")))
  (let ((eden-pending-requests nil))
    (should-not (eden-pending-conversation-p "conv-foo"))))

(ert-deftest eden-send-request-test ()
  ;; We want to test that concurrent requests work as expected:
  ;;
  ;; - observe mode-line during the test to be sure it doesn't stop
  ;;   after the first request done,
  ;; - test `eden-pending-requests' value during all requests
  ;; - test that we can order responses according to their timestamp file
  ;; - test that we update `eden-request-history' variable

  ;; NOTE:
  ;;
  ;; 1) if `eden-request-send' ever changes, we may have to change
  ;;    its binding definition below
  ;; 2) the way we redefine `eden-request-send' is ugly as hell.
  ;;    we use `eval' and `macroexpand' and we repeat ourself
  ;;    3 times. I think we have to do this because `eden-sentinel'
  ;;    is a macro.  Anyway we're able to test the behavior we expect
  ;;    from `eden-send-request'.

  (let* (;; we modify these two variables in the callback function
         ;; which is called for each request when we received a response.
         ;; I don't know why but we need to define them globaly,
         ;; let defined doesn't work!  Anyway.
         (_ (progn
              (defvar responses nil "...")
              (setq responses nil)
              ))
         (callback (lambda (req resp info)
                     (eden-pending-remove req)
                     (push (eden-request-assistant-content resp) responses)
                     (eden-conversation-update info req)
                     (eden-mode-line-waiting 'maybe-stop)
                     (message "Response for request %s received"
                              (plist-get req :prompt))))
         (eden-dir (concat (make-temp-file "eden-" t) "/"))
         eden-request-history
         (conversation-id-foo "conversation-id-foo")
         (conversation-id-bar "conversation-id-bar")
         (eden-conversations
          '(("conversation-id-foo" .
             (:title "foo title" :action start :last-req-uuid nil))
            ("conversation-id-bar" .
             (:title "bar title" :action start-from :last-req-uuid "last-bar-req-uuid"))
            ("conversation-id-baz" .
             (:title "baz title" :action continue-from :last-req-uuid "last-baz-req-uuid"))))
         (req-foo (eden-request :prompt "foo"))
         (req-foo-foo (eden-request :prompt "foo-foo"))
         ;; even if we don't use it (because we're not really sending
         ;; sending the request to openain) we pass :exchanges argument
         ;; to `eden-request' as we should do it given that
         ;; below we send `req-baz' as part of the conversation
         ;; "conversation-id-bar" with :action being `start-from'
         (req-bar (eden-request
                   :prompt "bar"
                   :exchanges [(:uuid "last-bar-req-uuid"
                                :prompt "last bar prompt"
                                :user "last bar user"
                                :assistant "last bar assistant"
                                :response "last bar response")]))
         (req-baz (eden-request :prompt "baz"))
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
    (cl-letf (((symbol-function 'eden-request-send)
               ;; Print response in stdout after 3 second
               ;; with sentinel constructed using `eden-sentinel'
               (lambda (req callback callback-error info)
                 (eden-write-request req)
                 (eden-test-echo-resp
                  (format resp-fmt "resp-foo") (generate-new-buffer-name "eden")
                  (eval (macroexpand-all
                         `(eden-sentinel
                           (quote ,req)
                           ;; no need for `quote' because car is `lambda'
                           ,callback
                           ;; works because ,callback-error returns (closure ...)
                           ;; because it is define with `lambda' in `eden-send-request'
                           (quote ,callback-error)
                           (quote ,info))))
                  3))))
      (eden-send-request
       :req req-foo
       :callback callback
       :info `(:conversation-id ,conversation-id-foo)))
    (cl-letf (((symbol-function 'eden-request-send)
               ;; Print response in stdout after 3 second
               ;; with sentinel constructed using `eden-sentinel'
               (lambda (req callback callback-error info)
                 (eden-write-request req)
                 (eden-test-echo-resp
                  (format resp-fmt "resp-foo-foo") (generate-new-buffer-name "eden")
                  (eval (macroexpand-all
                         `(eden-sentinel
                           (quote ,req)
                           ;; no need for `quote' because car is `lambda'
                           ,callback
                           ;; works because ,callback-error returns (closure ...)
                           ;; because it is define with `lambda' in `eden-send-request'
                           (quote ,callback-error)
                           (quote ,info))))
                  3))))
      ;; this request won't be sent because `req-foo' in the same
      ;; conversation `conversation-id-foo' is running at the same time
      (eden-send-request
       :req req-foo-foo
       :callback callback
       :info `(:conversation-id ,conversation-id-foo)))
    (cl-letf (((symbol-function 'eden-request-send)
               ;; Print response in stdout after 1 second
               ;; with sentinel constructed using `eden-sentinel'
               (lambda (req callback callback-error info)
                 (eden-write-request req)
                 (eden-test-echo-resp
                  (format resp-fmt "resp-bar") (generate-new-buffer-name "eden")
                  (eval (macroexpand-all
                         `(eden-sentinel
                           (quote ,req)
                           ;; no need for `quote' because car is `lambda'
                           ,callback
                           ;; works because ,callback-error returns (closure ...)
                           ;; because it is define with `lambda' in `eden-send-request'
                           (quote ,callback-error)
                           (quote ,info))))
                  1))))
      (eden-send-request
       :req req-bar
       :callback callback
       :info `(:conversation-id ,conversation-id-bar)))
    (cl-letf
        (((symbol-function 'eden-request-send)
          ;; Print response in stdout after 1 second
          ;; with sentinel constructed using `eden-sentinel'
          (lambda (req callback callback-error info)
            (eden-write-request req)
            (eden-test-echo-resp
             (format resp-fmt "resp-baz") (generate-new-buffer-name "eden")
             (eval (macroexpand-all
                    `(eden-sentinel
                      (quote ,req)
                      ;; no need for `quote' because car is `lambda'
                      ,callback
                      ;; works because ,callback-error returns (closure ...)
                      ;; because it is define with `lambda' in `eden-send-request'
                      (quote ,callback-error)
                      (quote ,info))))
             2))))
      (eden-send-request
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
          (pr-timer eden-pending-timer))
      (sleep-for 0.2)
      (should
       (equal (mapcar prompt-proc eden-pending-requests)
              '(("baz" . t)
                ("bar" . t)
                ("foo" . t))))
      (should (eden-pending-conversation-p conversation-id-foo))
      (should (eden-pending-conversation-p conversation-id-bar))
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
      (should (equal responses
                     '("resp-foo" "resp-baz" "resp-bar")))
      (should
       (equal eden-request-history
              (list
               (plist-get req-baz :uuid)
               (plist-get req-bar :uuid)
               (plist-get req-foo :uuid))))

      ;; state of `eden-conversations'
      (should
       (equal
        (alist-get "conversation-id-foo" eden-conversations nil nil 'string=)
        `(:title "foo title" :action continue-from :last-req-uuid ,req-foo-uuid)))
      (should
       (equal
        (alist-get "conversation-id-bar" eden-conversations nil nil 'string=)
        `(:title "bar title" :action continue-from :last-req-uuid ,req-bar-uuid)))
      (should
       (seq-set-equal-p
        (mapcar #'car eden-conversations)
        '("conversation-id-foo" "conversation-id-bar" "conversation-id-baz"))))

    ;; Test that the callback-error function remove waiting widget
    ;; in the modeline when an error occurs in the sentinel.  To
    ;; do so we send "killed\n" event to the process (waiting for
    ;; a response).  We use the same trick as above were we redefine
    ;; `eden-request-send'.
    (let ((debug-on-error t)
          (proc-buff (generate-new-buffer-name "eden"))
          pr-timer)
      (should-error
       (progn
         (cl-letf
             (((symbol-function 'eden-request-send)
               ;; Print response in stdout after 1 second
               ;; with sentinel constructed using `eden-sentinel'
               (lambda (req callback callback-error info)
                 (eden-write-request req)
                 (eden-test-echo-resp
                  (format resp-fmt "resp-baz") proc-buff
                  (eval (macroexpand-all
                         `(eden-sentinel
                           (quote ,req)
                           ;; no need for `quote' because car is `lambda'
                           ,callback
                           ;; works because ,callback-error returns (closure ...)
                           ;; because it is define with `lambda' in `eden-send-request'
                           (quote ,callback-error)
                           (quote ,info))))
                  1))))
           (eden-send-request :req (eden-request :prompt "req")))
         (setq pr-timer eden-pending-timer)
         (kill-process (get-buffer-process (get-buffer proc-buff)))
         (sleep-for 0.2)))
      (should (equal eden-pending-requests nil))
      (should (equal eden-pending-timer nil))
      ;; waiting widget is gone
      (should-not (memq pr-timer timer-list))
      (should-not global-mode-string))))

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-request-test")))
(ert-deftest eden-request-test ()
  ;; Test :uuid
  (should
   (stringp
    (plist-get (eden-request :prompt "foo prompt")
               :uuid)))

  ;; Test :dir
  (should
   (string=
    (let ((eden-dir nil))
      (plist-get (eden-request :prompt "foo prompt")
                 :dir))
    (concat (temporary-file-directory) "eden/")))
  (should
   (string=
    (plist-get
     (eden-request :prompt "foo prompt" :dir "/tmp/foo/")
     :dir)
    "/tmp/foo/"))
  (should
   (string=
    (let ((eden-dir "/tmp/bar/"))
      (plist-get (eden-request :prompt "foo prompt")
                 :dir))
    "/tmp/bar/"))

  ;; Check :prompt, :system-message, :exchanges
  ;;
  ;; :prompt argument is mandatory
  (should-error (eden-request))
  ;; :prompt and :system-message
  (let* ((eden-system-message nil)
         (req (eden-request :prompt "foo prompt")))
    (should (string= (plist-get req :prompt) "foo prompt"))
    (should (string= (plist-get req :system-message) ""))
    (should (equal (eden-get-in req [:req :messages])
                   [(:role "user" :content "foo prompt")])))
  (let* ((eden-system-message->developer-for-models '("o1" "o1-mini"))
         (eden-model "gpt-4o-mini")
         (req (eden-request :prompt "foo prompt"
                            :system-message "foo system")))
    (should (string= (plist-get req :system-message) "foo system"))
    (should (equal (eden-get-in req [:req :messages])
                   [(:role "system" :content "foo system")
                    (:role "user" :content "foo prompt")])))
  ;; :prompt, :system-message and :exchanges
  (let* ((eden-system-message->developer-for-models '("o1" "o1-mini"))
         (eden-model "gpt-4o-mini")
         (exchanges [(:uuid "uuid-foo"
                      :prompt "foo prompt"
                      :user "foo user"
                      :assistant "foo assistant"
                      :response "foo response")
                     (:uuid "uuid-bar"
                      :prompt "bar prompt"
                      :user "bar user"
                      :assistant "bar assistant"
                      :response "bar response")])
         (req (eden-request
               :prompt "baz prompt"
               :system-message "baz system"
               :exchanges exchanges)))
    (should (string= (plist-get req :system-message) "baz system"))
    (should (equal (plist-get req :exchanges) exchanges))
    (should (equal (eden-get-in req [:req :messages])
                   [(:role "system" :content "baz system")
                    (:role "user" :content "foo user")
                    (:role "assistant" :content "foo assistant")
                    (:role "user" :content "bar user")
                    (:role "assistant" :content "bar assistant")
                    (:role "user" :content "baz prompt")])))
  ;; :prompt and :system-message
  ;; both converted from org-mode to markdown
  (let* ((eden-system-message nil)
         (req (eden-request :prompt "* prompt h1\n** prompt h2")))
    (should (string= (plist-get req :prompt) "* prompt h1\n** prompt h2"))
    (should (string= (plist-get req :system-message) ""))
    (should (equal (eden-get-in req [:req :messages])
                   [(:role "user" :content "# prompt h1\n\n\n## prompt h2")])))
  (let* ((eden-system-message->developer-for-models '("o1" "o1-mini"))
         (eden-model "gpt-4o-mini")
         (req (eden-request :prompt "* prompt h1\n** prompt h2"
                            :system-message "* system h1\n** system h2")))
    (should (string= (plist-get req :prompt) "* prompt h1\n** prompt h2"))
    (should (string= (plist-get req :system-message) "* system h1\n** system h2"))
    (should (equal (eden-get-in req [:req :messages])
                   [(:role "system" :content "# system h1\n\n\n## system h2")
                    (:role "user" :content "# prompt h1\n\n\n## prompt h2")])))
  (let* ((eden-system-message->developer-for-models '("o1" "o1-mini"))
         (eden-model "gpt-4o-mini")
         (exchanges [(:uuid "uuid-foo"
                      :prompt "foo prompt"
                      :user "foo user"
                      :assistant "foo assistant")
                     (:uuid "uuid-bar"
                      :prompt "bar prompt"
                      :user "bar user"
                      :assistant "bar assistant")])
         (req (eden-request
               :prompt "* prompt h1\n** prompt h2"
               :system-message "* system h1\n** system h2"
               :exchanges exchanges)))
    (should (equal (eden-get-in req [:req :messages])
                   [(:role "system" :content "# system h1\n\n\n## system h2")
                    (:role "user" :content "foo user")
                    (:role "assistant" :content "foo assistant")
                    (:role "user" :content "bar user")
                    (:role "assistant" :content "bar assistant")
                    (:role "user" :content "# prompt h1\n\n\n## prompt h2")])))
  ;; :system-message and default `eden-system-message'
  (should
   (string=
    (let ((eden-system-message nil))
      (plist-get (eden-request :prompt "foo prompt")
                 :system-message))
    ""))
  (should
   (string=
    (let ((eden-system-message
           '("bar system message title" . "bar system message") ))
      (plist-get
       (eden-request :prompt "foo prompt"
                     :system-message "foo system message")
       :system-message))
    "foo system message"))
  (should
   (string=
    (let ((eden-system-message
           '("bar system message title" . "bar system message") ))
      (plist-get (eden-request :prompt "foo prompt")
                 :system-message))
    "bar system message"))
  ;; :system-message and :model and the variables
  ;; `eden-model' and `eden-system-message->developer-for-models'
  ;; in that case :role of first message in messages must be "developer"
  (let* ((eden-system-message->developer-for-models '("o1" "o1-mini"))
         (req (eden-request :prompt "foo prompt"
                            :system-message "foo system"
                            :model "o1")))
    (should (equal (eden-get-in req [:req :messages])
                   [(:role "developer" :content "foo system")
                    (:role "user" :content "foo prompt")])))
  (let* ((eden-system-message->developer-for-models '("o1" "o1-mini"))
         (eden-model "o1")
         (req (eden-request :prompt "foo prompt"
                            :system-message "foo system")))
    (should (equal (eden-get-in req [:req :messages])
                   [(:role "developer" :content "foo system")
                    (:role "user" :content "foo prompt")])))

  ;; Test :model
  (let* ((eden-model "gpt-4o")
         (req (eden-request :prompt "foo prompt")))
    (should (equal
             (plist-get (plist-get req :req) :model)
             "gpt-4o")))
  (let* ((eden-model "gpt-4o")
         (req (eden-request :prompt "foo prompt"
                            :model "gpt-4o-mini")))
    (should (equal
             (plist-get (plist-get req :req) :model)
             "gpt-4o-mini")))

  ;; Test :temperature
  (let* ((eden-temperature 0)
         (req (eden-request :prompt "foo prompt")))
    (should (= (plist-get (plist-get req :req) :temperature)
               0)))
  (let* ((eden-temperature 0)
         (req (eden-request :prompt "foo prompt"
                            :temperature 1.1)))
    (should (= (plist-get (plist-get req :req) :temperature)
               1.1)))

  ;; Test :api
  (let* ((eden-api '(:service "openai"
                     :endpoint "https://api.openai.com/v1/chat/completions"))
         (req (eden-request :prompt "foo prompt")))
    (should (equal (plist-get req :api) eden-api)))
  (let* ((eden-api '(:service "openai"
                     :endpoint "https://api.openai.com/v1/chat/completions"))
         (req (eden-request
               :prompt "foo prompt"
               :api '(:service "openai-service"
                      :endpoint "https://openai-endpoint"))))
    (should (equal (plist-get req :api)
                   '(:service "openai-service"
                     :endpoint "https://openai-endpoint"))))

  ;; Test :api when service is anthropic
  (let* ((eden-anthropic-max-tokens 4096)
         (eden-api '(:service "anthropic"
                     :endpoint "https://api.anthropic.com/v1/messages"))
         (req (eden-request :prompt "foo prompt")))
    (should (equal (eden-get-in req [:req :max_tokens]) 4096)))

  ;; Test :api when service is anthropic with reasoning
  ;; `eden-include-reasoning' is t
  (let* ((eden-anthropic-max-tokens 4096)
         (eden-anthropic-thinking-budget-tokens 2048)
         (eden-api '(:service "anthropic"
                     :endpoint "https://api.anthropic.com/v1/messages"))
         (eden-include-reasoning t)
         (req (eden-request :prompt "foo prompt")))
    (should (equal (eden-get-in req [:req :thinking :type]) "enabled"))
    (should (equal (eden-get-in req [:req :thinking :budget_tokens]) 2048)))

  ;; Test web_search_options for Perplexity and OpenAI web search models
  (let* ((eden-web-search-context-size "low")
         (eden-api '(:service "perplexity"
                     :endpoint "https://api.perplexity.ai/chat/completions"))
         (req (eden-request :prompt "foo prompt")))
    (should (equal (eden-get-in req [:req :web_search_options :search_context_size])
                   "low")))
  (let* ((eden-web-search-context-size "low")
         (eden-api '(:service "openai"
                     :endpoint "https://api.openai.com/v1/chat/completions"))
         (req (eden-request
               :prompt "foo prompt"
               :model "gpt-4o-search-preview")))
    (should (equal (eden-get-in req [:req :web_search_options :search_context_size])
                   "low"))))

;;;; Main menu

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

(global-set-key (kbd "C-<f1>") (lambda () (interactive) (ert "eden-last-paths/conversations/requests-test")))
(ert-deftest eden-last-paths/conversations/requests-test ()

  ;; ["uuid-req-1"]                           ;; not a conversation
  ;; ["uuid-req-1" "uuid-req-2"]              ;; not a conversation
  ;; ["uuid-req-1" "uuid-req-2" "uuid-req-3"]
  ;; ["uuid-req-1" "uuid-req-2" "uuid-req-4"]
  ;; ["uuid-req-5"]                           ;; with no response.json file
  ;; ["uuid-req-2" "uuid-req-6"]
  ;; ["uuid-req-7"]

  (let ((n 0))
    (cl-letf (((symbol-function 'eden-uuid)
               (lambda nil (format "uuid-req-%s" (cl-incf n)))))
      (let* ((eden-dir (concat (make-temp-file "eden-" t) "/"))
             (req-1 (eden-request :prompt "req-1 prompt"))
             (req-2 (eden-request
                     :prompt "req-2 prompt"
                     :exchanges [(:uuid "uuid-req-1"
                                  :prompt "req-1 prompt"
                                  :user "req-1 user"
                                  :assistant "req-1 assistant"
                                  :response "req-1 response")]))
             (req-3 (eden-request
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
             (req-4 (eden-request
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
             (req-5 (eden-request :prompt "req-5 prompt"))
             (req-6 (eden-request
                     :prompt "req-6 prompt"
                     :exchanges [(:uuid "uuid-req-2"
                                  :prompt "req-2 prompt"
                                  :user "req-2 user"
                                  :assistant "req-2 assistant"
                                  :response "req-2 response")]))
             (req-7 (eden-request :prompt "req-7 prompt"))
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
        (message "%s" eden-dir)
        (dotimes (idx 7)
          (let ((req (eval (intern (format "req-%s" (1+ idx))))))
            (eden-write-request req)
            ;; We replace timestamp files in order to span requests
            ;; over 7 days including today.
            (eden-test-add-or-replace-timestamp-file
             req (nth idx timestamps))
            ;; add response.json except for req-5 such that this request
            ;; is considered to be an error and so its path will not
            ;; be listed by `eden-last-paths' function
            (when (not (= idx 4)) ;;
              (let* ((resp-str (format resp-fmt (format "req-%s assistant" (1+ idx))))
                     (resp (with-temp-buffer
                             (save-excursion (insert resp-str))
                             (eden-json-read))))
                (eden-write-response resp-str resp req)))))

        ;; Test `eden-paths-since'
        (should (equal (eden-paths-since (nth 0 timestamps))
                       '(["uuid-req-1"]
                         ["uuid-req-1" "uuid-req-2"]
                         ["uuid-req-1" "uuid-req-2" "uuid-req-3"]
                         ["uuid-req-1" "uuid-req-2" "uuid-req-4"]
                         ["uuid-req-2" "uuid-req-6"]
                         ["uuid-req-7"])))
        (should (equal (eden-paths-since (nth 6 timestamps)) '(["uuid-req-7"])))
        (should (equal (eden-paths-since (nth 3 timestamps))
                       '(["uuid-req-1" "uuid-req-2" "uuid-req-4"]
                         ["uuid-req-2" "uuid-req-6"]
                         ["uuid-req-7"])))

        ;; Test `eden-last-paths'
        (should (equal (eden-last-paths 10)
                       '(["uuid-req-1"]
                         ["uuid-req-1" "uuid-req-2"]
                         ["uuid-req-1" "uuid-req-2" "uuid-req-3"]
                         ["uuid-req-1" "uuid-req-2" "uuid-req-4"]
                         ["uuid-req-2" "uuid-req-6"]
                         ["uuid-req-7"])))
        (should (equal (eden-last-paths 1) '(["uuid-req-7"])))
        (should (equal (eden-last-paths 4)
                       '(["uuid-req-1" "uuid-req-2" "uuid-req-4"]
                         ["uuid-req-2" "uuid-req-6"]
                         ["uuid-req-7"])))

        ;; Test `eden-last-requests'
        (should (equal (eden-last-requests 10)
                       '("uuid-req-1"
                         "uuid-req-2"
                         "uuid-req-3"
                         "uuid-req-4"
                         "uuid-req-6"
                         "uuid-req-7")))
        (should (equal (eden-last-requests 1) '("uuid-req-7")))
        (should (equal (eden-last-requests 4)
                       '("uuid-req-4" "uuid-req-6" "uuid-req-7")))

        ;; Test `eden-last-conversations'
        (should (equal (eden-last-conversations 10)
                       '("uuid-req-3" "uuid-req-4" "uuid-req-6" "uuid-req-7")))
        (should (equal (eden-last-conversations 1) '("uuid-req-7")))
        (should (equal (eden-last-conversations 4)
                       '("uuid-req-4" "uuid-req-6" "uuid-req-7")))))))
