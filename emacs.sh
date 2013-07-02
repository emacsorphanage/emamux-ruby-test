#!/bin/sh

carton exec emacs -nw -Q --directory $PWD --eval "(progn (require 'emamux-ruby-test) (add-hook 'ruby-mode-hook 'emamux-ruby-test-mode))" $@
