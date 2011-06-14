############################################################################
############################# Bash RC file #################################
############################################################################
# Mostly by whoever wrote the default debian/ubuntu bashrc, but a few tweaks
# and additions by Saul Reynolds-Haertle.


# If not running interactively, don't do anything
[ -z "$PS1" ] && return





############################################################################
####################### Environment Variable setup #########################
############################################################################
# setup editor variable
export EDITOR="vim"

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# add the android sdk to my path
export PATH=${PATH}:/usr/share/android-sdk-linux_86/tools








############################################################################
################################ Colors ####################################
############################################################################

# If we're in an xterm, change the colors and stuff. Otherwise leave them
# as is.
case "$TERM" in
xterm*)
	# Color the primary prompt, and set it to something useful and short.
	export PS1="\[\e[31m\][\u@\h \W]\$\[\e[0m\] "
	# attempt to change the xterm title.
	export PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}\007"'
	
	export TERM=xterm-256color
;;
screen*)
	# Color the primary prompt, and set it to something useful and short.
	export PS1="\[\e[31m\][\u@\h \W]\$\[\e[0m\] "
	# attempt to change the xterm title.
	export PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}\007"'

    export TERM=screen-256color
;;
gnome*)
	#leave the gnome terminal alone. It does its own stuff.
    #If, that is, gnome-terminal identified itself as something other than
    #an xterm...
;;
esac






############################################################################
################################ Aliases ###################################
############################################################################

# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable color support for ls some other programs where color is useful.
if [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# A few very random shortcut aliases I use
alias more='less'
alias la='ls -a'
alias ll='ls -l'
alias lal='ls -al'
alias lla='ls -al'

# Some these are just plain weird, but I like them.
alias irssi='su -c vebyast irssi'
alias octave='octave -q'



############################################################################
### Clojure

export CLOJURE_EXT=~/.clojure
export PATH=${PATH}:~/opt/clojure-contrib/launchers/bash
alias clj=clj-env-dir
export PATH=${PATH}:~/opt/leiningen


############################################################################
######## Uhhhh, I'm not really sure, but it was in the default rc. #########
############################################################################

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi
