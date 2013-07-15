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
* Send focused test/class into [tconsole](https://github.com/commondream/tconsole)
* Open project specific console


Future work
-----------
* Send focused spec/context into [spork](https://github.com/sporkrb/spork) subprocess

Usage
-----
Please visit comment section in `emamux-ruby-test.el`.


Contribution
------------
Test you improvements in clean emacs configuration without any other packages.
You cat simply to that with our console wrapper.

    carton install
    ./run.sh emacs ../path/to/your/ruby/project/lib/project.rb

When you implement some feature, please write all necessary tests for it.
Run it as simple as that.

    ./run.sh test
