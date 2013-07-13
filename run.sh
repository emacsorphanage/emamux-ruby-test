#!/bin/sh

case $1 in
    emacs)
        shift
        rm -f emamux-ruby-test.elc
        carton exec emacs -Q -nw --directory $PWD --eval "(progn (require 'emamux-ruby-test) (global-emamux-ruby-test-mode))" $@
        ;;
    test)
        carton exec emacs -Q --batch --directory $PWD --directory $PWD/test $(for i in test/*-test.el ; do echo "--load $i" ; done) --eval "(progn (require 'emamux-ruby-test) (ert-run-tests-batch-and-exit t))"
        ;;
    *)
        echo "Aveilable options are:"
        echo " * emacs --- Load GNU/Emacs without any modules but that."
        echo " * test  --- Run all tests defined in test directory."
esac
