emamux-ruby-test
==================
Ruby test with [emamux](https://github.com/syohex/emacs-emamux).


Requirements
------------
* Emacs 22.1 or higher.
* emamux
* tmux 1.5


Features
--------
* Detect test framework and project scope automatically
* Run all test from any project buffer
* Run current test from test and implementation buffer
* Open project console


Future work
-----------
* Send focused test/class into [tconsole](https://github.com/commondream/tconsole)
* Send focused spec/context into [spork](https://github.com/sporkrb/spork) subprocess

Usage
-----
For more info please visit comment section in `emamux-ruby-test.el`.


Contribution
------------
Test you improvements in clean emacs configuration without any other packages.
You cat simply to that with our console wrapper.

    carton install
    ./emacs.sh ../path/to/your/ruby/project/lib/project.rb
