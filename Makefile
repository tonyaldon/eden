.PHONY: test
test:
	emacs -batch -l eden.el -l eden-test.el -f ert-run-tests-batch-and-exit
