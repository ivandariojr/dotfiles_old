#!/bin/zsh -xv
###############################################################################
######################### oh-my-zsh Configuration #############################
###############################################################################
# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="saulrh"

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

# colors!
export TERM='xterm-256color'

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

# don't autocorrect. just autcomplete.
unsetopt correct_all

# for gitolite
PATH=/home/saul/bin:$PATH

###############################################################################
########################### Colors and Display ################################
###############################################################################

# make ls more friendly with lesspipe
#  note that we're using the pipe option, so we don't get nice %-progress
#  indicators.
export LESSOPEN="|lesspipe %s"

# make ls easier to type
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

# call this to update the git vars in our command line
alias gup='chpwd'

###############################################################################
#################################### prompt ###################################
###############################################################################

# turn on command substitution in the prompt!
# Also parameter expansion and artihmetic expansion, but we don't use those.
setopt promptsubst

###############################################################################

# normal:
# [user@host dir]$
# {red}[{green}user@host {blue}dir{red}]$

# with clean git repo:
# [user@host dir (green git)]$
# {red}[{green}user@host {blue}dir {blue}({green}git{blue}){red}]$

# with dirty git repo:
# [user@host dir (yellow git)]$
# {red}[{green}user@host {blue}dir {blue}({yellow}git{blue}){red}]$

###############################################################################

# gets the name of the current branch
# saves result as a var
git_branch()
{
    git_branch_string="$(git symbolic-ref HEAD 2>/dev/null)"
    git_branch_string="${git_branch_string##*/}"
    git_branch_string="${git_branch_string:-no branch}"
}

# gets whether the current worktree has changes
# changes color of the branch name
git_dirty()
{
    if [[ -n "$(git status -s --ignore-submodules=dirty --porcelain 2> /dev/null)" ]]; then
        git_dirty_string="%{$fg[yellow]%}"
    else
        git_dirty_string="%{$fg[green]%}"
    fi
}

git_prompt() {
    if [[ -n "$(git symbolic-ref HEAD 2> /dev/null)" ]];
    then
        git_prompt_string="%{$fg_bold[blue]%} ("$git_dirty_string$git_branch_string"%{$fg_bold[blue]%})"
    else
        unset git_prompt_string
    fi
}

# now, update our variables whenever we change directory. 
function chpwd()
{
    git_branch
    git_dirty
    git_prompt
}
chpwd

#              command                    # color   part
PROMPT="%{$fg_bold[red]%}["               # red     [
PROMPT=$PROMPT"%{$fg_bold[green]%}%n"     # green   user
PROMPT=$PROMPT"%{$fg_bold[green]%}@"      # green   @
PROMPT=$PROMPT"%{$fg_bold[green]%}%m"     # green   host
PROMPT=$PROMPT"%{$fg_bold[blue]%} %1~"    # blue     dir
PROMPT=$PROMPT'$git_prompt_string'        # *        (git)
PROMPT=$PROMPT"%{$fg_bold[red]%}]$ "      # red     ]$
PROMPT=$PROMPT"%{$reset_color%}"          # reset   _


###############################################################################
########################## emacs editing things ###############################
###############################################################################

# Local Variables:
# mode: sh
# End:
