#!/bin/zsh -xv

###############################################################################
############################# dumb terminals ##################################
###############################################################################

if [[ "$TERM" == "dumb" ]]; then
    return
fi
if [[ "$TERM" != "screen-256color" && "$TERM" != "screen" && "$TERM" != "eterm-color" ]]; then
    tmux -2
fi

###############################################################################
############################ zsh Configuration ################################
###############################################################################
COMPLETION_WAITING_DOTS="true"

source ~/.zsh/antigen-hs/init.zsh

###############################################################################
############################### completion ####################################
###############################################################################
# This way the completion script does not have to parse Bazel's options
# repeatedly.  The directory in cache-path must be created manuallya.
fpath[1,0]=~/.zsh/completion/
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

# finally, build completion database
autoload -U compinit
compinit

# don't autocorrect. just autcomplete.
unsetopt correct_all
unsetopt correct

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

# configure history - no duplicates, nothing starting with whitespace (for passwords), 10k lines of
# history file
setopt hist_ignore_all_dups

# Can now prevent something from being added to history by prepending a space
setopt hist_ignore_space
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000

# more powerful globbing
setopt extendedglob

# autocmoplete switches even on aliases
setopt completealiases

# don't change into a directory unless I tell you to!
unsetopt autocd

# don't autocorrect. just autcomplete.
unsetopt correct_all
unsetopt correct

# for gitolite and things from pip
# note that we put in our local bin files before anything else, which means
# that we prefer local versions of things
PATH=/home/ivan/bin:/home/ivan/.local/bin:$PATH

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
export DEBEMAIL="ijimenez3@gatech.edu"
export DEBFULLNAME="Ivan Dario Jimenez"

###############################################################################
############################### C code tagging ################################
###############################################################################

CODEDI_R=~/src
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

alias open="xdg-open"

###############################################################################
#################################### prompt ###################################
###############################################################################

# turn on command substitution in the prompt!
# Also parameter expansion and artihmetic expansion, but we don't use those.
# setopt promptsubst
# autoload colors && colors

###############################################################################

# #[user@host (chroot?) (git?) dir]$ 
# #^                              ^^
# color_brackets="%{$fg_bold[red]%}" 

# # [user@host (chroot?) (git?) dir]$ 
# #  ^^^^
# color_username_normal="%{$fg_bold[green]%}"

# # [root@host (chroot?) (git?) dir]$ 
# #  ^^^^
# color_username_root="%{$fg_bold[yellow]%}"

# # [user@host (chroot?) (git?) dir]$ 
# #      ^
# color_at="%{$fg_bold[green]%}"

# # [user@host (chroot?) (git?) dir]$ 
# #       ^^^^
# color_host_normal="%{$fg_bold[green]%}"

# # [user@important (chroot?) (git?) dir]$ 
# #       ^^^^^^^^^
# color_host_special="%{$fg_bold[red]%}"

# # [user@host (chroot?) (git?) dir]$ 
# #             ^^^^^^
# color_host_chroot="%{$fg_bold[yellow]%}"

# # [user@host (chroot?) (git?) dir]$ 
# #                       ^^^
# color_git_clean="%{$fg[green]%}"

# # [user@host (chroot?) (git?) dir]$ 
# #                       ^^^
# color_git_dirty="%{$fg[yellow]%}"

# # [user@host (chroot?) (git?) dir]$ 
# #                             ^^^
# color_dir="%{$fg_bold[blue]%}"

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
        krang-vision)
            prompt_host_string=$color_host_special$HOST
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
# PROMPT=$color_brackets"["                    # [
# PROMPT=$PROMPT'$prompt_user_string'          # username
# PROMPT=$PROMPT'$color_at'"@"         # @
# PROMPT=$PROMPT'$prompt_host_string'          # host
# PROMPT=$PROMPT'$color_dir'" %1~"       # dir
# PROMPT=$PROMPT'$git_prompt_string'           # git status
# PROMPT=$PROMPT'$color_brackets'"]"            # ]
# PROMPT=$PROMPT"%(#.# .$ )"                    # root gets a #, normal a $.
# PROMPT=$PROMPT"%{$reset_color%}"             # reset   

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


###############################################################################
############################ ros setup ########################################
###############################################################################

#source /opt/ros/indigo/setup.zsh
# export PATH="/usr/lib/ccache:$PATH:$HOME/bin"
export PATH="/usr/local/cuda/bin:$PATH"
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/lib/nvidia-384"
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/cuda/lib64"
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/cuda/extras/CUPTI/lib64"
export CUDA_HOME=/usr/local/cuda
#SNOPT INSTALL STUFF
export SNOPT7LIB="/home/ivan/libs/snopt/lib"
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/home/ivan/libs/snopt/lib"
