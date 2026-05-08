.PHONY: test
test:
	emacs -batch -l eden.el -l eden-test.el -f ert-run-tests-batch-and-exit

test-ui:
	emacs -q -l eden.el --eval "(progn (define-key input-decode-map \"\C-i\" [C-i]) (global-set-key [C-i] #'eden) (define-key key-translation-map (kbd \"M-q\") (kbd \"C-g\")) (define-key key-translation-map (kbd \"C-<return>\") (kbd \"C-c\")))"
