#!/bin/sh

cask exec emacs -Q --batch --directory $PWD --directory $PWD/test $(for i in test/*-test.el ; do echo "--load $i" ; done) --eval "(progn (require 'emamux-ruby-test) (ert-run-tests-batch-and-exit t))"
