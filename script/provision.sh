#!/bin/sh

# Verify aptitude tools.
sudo apt-get update -qq
sudo apt-get install -qq python-software-properties
sudo add-apt-repository -y ppa:cassou/emacs
sudo apt-get update -qq

# Install development tools.
PACKAGES="make git emacs24 emacs24-el emacs24-common-non-dfsg"
sudo apt-get install -qq $PACKAGES

# Install Carton.
CARTON_DIR=/tmp/carton
rm -rf $CARTON_DIR
git clone git://github.com/rejeep/carton.git $CARTON_DIR
sudo ln -sf $CARTON_DIR/bin/carton /usr/local/bin/
