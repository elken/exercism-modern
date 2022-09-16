SHELL=/usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

build:
	$(EASK) package
	$(EASK) install

ci: clean build compile checkdoc lint test

compile:
	@echo "Compiling..."
	$(EASK) compile

checkdoc:
	$(EASK) lint checkdoc

lint:
	@echo "package linting..."
	$(EASK) lint package

test:
	$(EASK) install-deps --dev
	$(EASK) test ert ./test/*.el

clean:
	$(EASK) clean-all

# Allow args to make commands
%:
	@:

.PHONY : ci compile checkdoc lint test clean tag
