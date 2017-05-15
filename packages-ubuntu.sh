#!/bin/zsh

#install apt-fast to speed-up downloads
sudo add-apt-repository ppa:saiarcot895/myppa
sudo apt-get update
sudo apt-get -y install apt-fast

#basic installs for every system
sudo apt-fast install vim \
                      ghc cabal-install \
                      git zsh build-essential curl \
                      keychain checkinstall tree texlive-full \
                      font-manager ros-desktop-full

#install latest emacs
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt-get update
sudo apt-get install emacs25