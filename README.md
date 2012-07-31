emamux-ruby-test
==================
Ruby test with [emamux](https://github.com/syohex/emacs-emamux).


Requirements
------------
* Emacs 22.1 or higher.
* emamux
* tmux 1.5


Basic Usage
-----------

Run test of this file within runner pane

    M-x emamux-ruby-test:run-all

Run test of focused test(current posion method or test/should block)

    M-x emamux-ruby-test:run-focused-test

Run test of focused contest(current posion context/describe block)

    M-x emamux-ruby-test:run-focused-context
