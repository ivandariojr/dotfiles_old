#!/bin/zsh

###################################################
############## configure antigen-hs ###############
###################################################
cabal install shelly
source "$HOME/.zsh/antigen-hs/init.zsh"
antigen-hs-compile
source "$HOME/.zsh/antigen-hs/init.zsh"
