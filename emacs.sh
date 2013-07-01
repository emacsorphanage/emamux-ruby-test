#!/bin/sh

carton exec emacs -Q --directory $PWD --eval "(require 'emamux-ruby-test)" $@
