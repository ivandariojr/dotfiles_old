#!/bin/bash

##########################################
########### Helper Functions #############
##########################################

# $1 is the file to back up. If it's an actual file, it will be moved
# to $HOME/old-dotfiles/; if it's a symlink, the symlink will be
# deleted to leave room for a new one.
function makebackup() {
    if [[ ! -e "$1" ]]; then
	return
    fi
    if [[ ! -h "$1" ]]; then
	mv "$1" "$HOME/old-dotfiles/"
        echo "backed up old $1"
    else
	rm "$1"
    fi
}

# This function will update a git repository, cloning it first if it
# doesn't exist. $1 is the location of the git repository to clone
# from if cloning is necessary. $2 is the directory that the git
# repository should be or is in. $3, if provided, is an
# easily-identifiable name for the git repo for printing.
function handlegitrepo() {
    if [ ! -d "$2" ]; then
        if [ -n "$3" ]; then echo "%{$fg[green]%}Installing%{$reset_color%} $3"; fi
        git clone "$1" "$2"
    else
        if [ -n "$3" ]; then echo "Updating $3"; fi
        (cd $2; git pull)
    fi
}

#################################################
########### Install things to $HOME #############
#################################################
test=( 'xmodmap.conf' 'bashrc' 'emacs.d' 'stumpwmrc' 'screenrc' 'tmux.conf' 'vimrc' 'wl' 'folders' 'zshrc' 'xmonad' 'xmobarrc' 'vimperatorrc' 'xbindkeysrc.scm' )

mkdir -p "$HOME/old-dotfiles"

echo "Symlinking dotfiles"

for (( i = 0 ; i < ${#test[@]} ; i++ ))
do
    makebackup "$HOME/.${test[i]}"
    
    ln -s "$PWD/${test[i]}" "$HOME/.${test[i]}" 
    echo "symlinked $HOME/.${test[i]}"
done

[[ "$(ls -A $HOME/old-dotfiles)" ]] && echo "Copied old dotfiles to $HOME/old-dotfiles" || rm -rf "$HOME/old-dotfiles"

####################################################
########### Configure Vim's directories ############
####################################################

mkdir -p "$HOME/.vim/"
mkdir -p "$HOME/.vim/bundle/"
mkdir -p "$HOME/.vim/autoload/"
mkdir -p "$HOME/.vim/backup/"
mkdir -p "$HOME/.vim/undo/"
mkdir -p "$HOME/.vim/swap/"

curl -LSso ~/.vim/autoload/pathogen.vim https://raw.github.com/tpope/vim-pathogen/master/autoload/pathogen.vim

echo "Updating vim plugins"
$HOME/dotfiles/vim-update.sh repos

################################################
############## Install oh-my-zsh ###############
################################################

handlegitrepo "git://github.com/robbyrussell/oh-my-zsh.git" "$HOME/.oh-my-zsh" "oh-my-zsh"

################################################
########### Install Awesome Configs ############
################################################

handlegitrepo "git://github.com/bioe007/awesome-revelation.git" "$HOME/.config/awesome/revelation" "revelation"
handlegitrepo "git://github.com/cycojesus/awesome-solarized.git" "$HOME/.config/awesome/themes/awesome-solarized" "awesome-solarized"

makebackup "$HOME/.config/awesome/rc.lua"

ln -s "$PWD/awesomerc.lua" "$HOME/.config/awesome/rc.lua"

##########################################################
########### Install random things from github ############
##########################################################

handlegitrepo "git://github.com/rupa/z.git" "$HOME/src/z" "z"
