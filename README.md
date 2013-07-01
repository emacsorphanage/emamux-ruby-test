emamux-ruby-test
==================
Ruby test with [emamux](https://github.com/syohex/emacs-emamux).


Requirements
------------
* Emacs 22.1 or higher.
* emamux
* tmux 1.5


Installation
------------
Add following lines to your emacs config file

    (require 'emamux-ruby-test)
    (add-hook 'ruby-mode-hook 'emamux-ruby-test-mode)


Basic Usage
-----------

Run test of this project within runner pane

    M-x emamux-ruby-test:run-all

Run test of this file within runner pane

    M-x emamux-ruby-test:run-current-test
