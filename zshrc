###############################################################################
######################### oh-my-zsh Configuration #############################
###############################################################################
# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="saulrh"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git python svn debian)

source $ZSH/oh-my-zsh.sh

###############################################################################
########################## Environment Variables ##############################
###############################################################################

# zsh might default my keybindings to vi based on my EDITOR, so
# explicitly state otherwise.
bindkey -e

# set up editor variables - use vim for quick stuff, basically
export EDITOR="vim"
export VISUAL="vim"

# Don't duplicate history lines.
setopt hist_ignore_all_dups

# Can now prevent something from being added to history by prepending a space
setopt hist_ignore_space

# more powerful globbing
setopt extendedglob

# autocmoplete switches even on aliases
setopt completealiases

# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000

# don't change into a directory unless I tell you to!
unsetopt autocd

###############################################################################
########################### Colors and Display ################################
###############################################################################

# make ls more friendly with lesspipe
#  note that we're using the pipe option, so we don't get nice %-progress
#  indicators.
eval "$(lesspipe)"

# random aliases
alias la='ls -a'
alias ll='ls -l'
alias lal='ls -al'
alias lla='ls -al'
compdef _ls la=ls
compdef _ls ll=ls
compdef _ls lal=ls
compdef _ls lla=ls

# working with ACLs
alias ga='getfacl'
alias sa='setfacl'
compdef _getfacl ga=getfacl
compdef _setfacl sa=setfacl

###############################################################################
########################## emacs editing things ###############################
###############################################################################

# Local Variables:
# mode: sh
# End:
