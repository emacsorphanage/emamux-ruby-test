#!/bin/sh

rm -f emamux-ruby-test.elc

cask exec emacs -Q -nw --directory $PWD --eval "(progn (require 'emamux-ruby-test) (global-emamux-ruby-test-mode))" $@
