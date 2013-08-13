#!/bin/zsh -xv

###############################################################################
############################# dumb terminals ##################################
###############################################################################

if [[ "$TERM" == "dumb" ]]; then
    return
fi
if [[ "$TERM" != "screen-256color" && "$TERM" != "screen" ]]; then
    tmux -2
fi

###############################################################################
############################ zsh Configuration ################################
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

# hopefully disable completion
DISABLE_CORRECTION="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git python svn debian safe-paste rsync)

source $ZSH/oh-my-zsh.sh

###############################################################################
########################## Environment Variables ##############################
###############################################################################

# zsh might default my keybindings to vi based on my EDITOR, so
# explicitly state otherwise.
bindkey -e

# set up editor variables
export EDITOR="emacsclient -a \"\""
export VISUAL="emacsclient -a \"\""
export ALTERNATE_EDITOR="vim"

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
unsetopt correct

# for gitolite and things from pip
# note that we put in our local bin files before anything else, which means
# that we prefer local versions of things
PATH=/home/saul/bin:/home/saul/.local/bin:$PATH

# for python virtualenvs
if [[ -x /usr/local/bin/virtualenvwrapper.sh || -x /usr/bin/virtualenvwrapper.sh ]]; then
    WORKON_HOME=~/.python-virtualenvs
    source virtualenvwrapper.sh
fi

# set up SSH keychain
case $HOST in
    lanning)
        eval `keychain -Q id_rsa --eval`
    ;;
    vulcan)
        eval `keychain -Q vulcan_primary 0x2E7E2C25C26D70D3 0x5131CA9463219CCD --eval`
    ;;
    pazuzu)
        eval `keychain -Q saul-pazuzu 23E69FB1 DD9CCD1A --eval`
    ;;
    krang)
        eval `keychain -Q krang_rsa --eval`
    ;;
    *)
        eval `keychain -Q id_rsa --eval`
    ;;
esac
source ~/.keychain/${HOST}-sh

# teamocil - add autocompletion
compctl -g '~/.teamocil/*(:t:r)' teamocil

# some variables for building debian packages
export DEBEMAIL="saulrh@gatech.edu"
export DEBFULLNAME="Saul Reynolds-Haertle"

###############################################################################
################################# Other Junk ##################################
###############################################################################

. ~/src/z/z.sh

###############################################################################
############################### C code tagging ################################
###############################################################################

CODEDIR=~/src
alias mktags='cd $CODEDIR && etags "`find $CODEDIR -name *.[h|c|py|cpp|cc|hh|hpp|java]`" && cd -'
alias mktagscurrent='etags `find . -name "*.[h|c|py|cpp|cc|hh|hpp|java]"`'

###############################################################################
################################## Aliases ####################################
###############################################################################

# make ls more friendly with lesspipe
#  note that we're using the pipe option, so we don't get nice %-progress
#  indicators.
export LESSOPEN="|lesspipe %s"

# editor
alias e='emacsclient'
alias enw='emacsclient -nw'
alias enc='emacsclient -nc'

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

alias rolldice="rolldice -s"

# tmux stuff
alias tmux='tmux -2'
alias teamocil="teamocil --here"

alias open="gnome-open"

###############################################################################
#################################### prompt ###################################
###############################################################################

# turn on command substitution in the prompt!
# Also parameter expansion and artihmetic expansion, but we don't use those.
setopt promptsubst
autoload colors && colors

###############################################################################

# [user@host (chroot?) (git?) dir]$ 
# ^                              ^^
color_brackets="%{$fg_bold[red]%}"

# [user@host (chroot?) (git?) dir]$ 
#  ^^^^
color_username_normal="%{$fg_bold[green]%}"

# [root@host (chroot?) (git?) dir]$ 
#  ^^^^
color_username_root="%{$fg_bold[yellow]%}"

# [user@host (chroot?) (git?) dir]$ 
#      ^
color_at="%{$fg_bold[green]%}"

# [user@host (chroot?) (git?) dir]$ 
#       ^^^^
color_host_normal="%{$fg_bold[green]%}"

# [user@important (chroot?) (git?) dir]$ 
#       ^^^^^^^^^
color_host_special="%{$fg_bold[red]%}"

# [user@host (chroot?) (git?) dir]$ 
#             ^^^^^^
color_host_chroot="%{$fg_bold[yellow]%}"

# [user@host (chroot?) (git?) dir]$ 
#                       ^^^
color_git_clean="%{$fg[green]%}"

# [user@host (chroot?) (git?) dir]$ 
#                       ^^^
color_git_dirty="%{$fg[yellow]%}"

# [user@host (chroot?) (git?) dir]$ 
#                             ^^^
color_dir="%{$fg_bold[blue]%}"

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
        git_dirty_string=$color_git_dirty
    else
        git_dirty_string=$color_git_clean
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
function toggle_git()
{
    if [ -z "$DO_ZSH_GIT" ] ;
    then
        echo "Will not include git status in prompt"
        DO_ZSH_GIT="foo"
    else
        echo "Will include git status in prompt"
        DO_ZSH_GIT=""
    fi
}

# now, update our variables whenever we change directory. 
function chpwd()
{
    if [ -z "$DO_ZSH_GIT" ] ;
    then
        git_branch
        git_dirty
        git_prompt
    else
        unset git_prompt_string
    fi
}

function precmd {
    case $USER in
        root)
            prompt_user_string=$color_username_root$USER
            ;;
        *)
            prompt_user_string=$color_username_normal$USER
            ;;
    esac

    case $HOST in
        krang)
            # jobs=`sudo ps -eo pcpu,user`
            # achd_cpu=`ps -eo pcpu,fname | grep achd | cut -d " " -f 2 | paste -sd+ | bc`
            prompt_host_string=$color_host_special$HOST"[`bmsGetLast.sh`]"
            #prompt_host_string=$color_host_special$HOST
            ;;
        thebrain)
            prompt_host_string=$color_host_special$HOST
            ;;
        *)
            prompt_host_string=$color_host_normal$HOST
            ;;
    esac

    if [ -n "$SCHROOT_CHROOT_NAME" ] ;
    then
        prompt_host_string=$prompt_host_string$color_host_chroot" ($SCHROOT_CHROOT_NAME)"
    fi
}

# start with git status enabled
DO_ZSH_GIT=""

#              command                       # part
PROMPT=$color_brackets"["                    # [
PROMPT=$PROMPT'$prompt_user_string'          # username
PROMPT=$PROMPT'$color_at'"@"         # @
PROMPT=$PROMPT'$prompt_host_string'          # host
PROMPT=$PROMPT'$color_dir'" %1~"       # dir
PROMPT=$PROMPT'$git_prompt_string'           # git status
PROMPT=$PROMPT'$color_brackets'"]"            # ]
PROMPT=$PROMPT"%(#.# .$ )"                    # root gets a #, normal a $.
PROMPT=$PROMPT"%{$reset_color%}"             # reset   

###############################################################################
########################## emacs editing things ###############################
###############################################################################

# Local Variables:
# mode: sh
# End:

###############################################################################
########################## cpan setup #########################################
###############################################################################

case $HOST in
    lanning)
        export PERL_LOCAL_LIB_ROOT="/home/saul/perl5";
        export PERL_MB_OPT="--install_base /home/saul/perl5";
        export PERL_MM_OPT="INSTALL_BASE=/home/saul/perl5";
        export PERL5LIB="/home/saul/perl5/lib/perl5/x86_64-linux-gnu-thread-multi:/home/saul/perl5/lib/perl5";
        export PATH="/home/saul/perl5/bin:$PATH";
        ;;
esac
