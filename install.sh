#!/bin/bash


#############################################
########### Install things to $HOME #############
#############################################
test=( 'bashrc' 'emacs' 'emacs.d' 'stumpwmrc' 'screenrc' 'tmux.conf' 'vimrc' 'wl' 'folders' 'zshrc' 'xmonad' 'xmobarrc' 'vimperatorrc' )

mkdir -p "$HOME/old-dotfiles"

for (( i = 0 ; i < ${#test[@]} ; i++ ))
do
	[[ -e "$HOME/.${test[i]}" ]] && [[ ! -h "$HOME/.${test[i]}" ]] && mv "$HOME/.${test[i]}" "$HOME/old-dotfiles/${test[i]}"
	[[ -e "$HOME/.${test[i]}" ]] && [[ -h "$HOME/.${test[i]}" ]] && rm "$HOME/.${test[i]}"

	ln -s "$PWD/${test[i]}" "$HOME/.${test[i]}" 
    echo "installed $HOME/.${test[i]}"
done

[[ "$(ls -A $HOME/old-dotfiles)" ]] && echo "Copied old dotfiles to $HOME/old-dotfiles" || rm -rf "$HOME/old-dotfiles"

####################################################
########### Configure Vim's directories ############
####################################################

mkdir -p "$HOME/.vim/"
mkdir -p "$HOME/.vim/backup/"
mkdir -p "$HOME/.vim/undo/"
mkdir -p "$HOME/.vim/swap/"

################################################
############## Install oh-my-zsh ###############
################################################

if [ ! -d "$HOME/.oh-my-zsh" ]; then
    git clone git://github.com/robbyrussell/oh-my-zsh.git $HOME/.oh-my-zsh
fi

################################################
########### Install Awesome Configs ############
################################################

### awesome
mkdir -p "$HOME/.config/awesome"
if [ ! -d "$HOME/.config/awesome/revelation" ]; then
    git clone git://github.com/bioe007/awesome-revelation.git $HOME/.config/awesome/revelation
fi

mkdir -p "$HOME/.config/awesome/themes"
if [ ! -d "$HOME/.config/awesome/themes/awesome-solarized" ]; then
    git clone git://github.com/cycojesus/awesome-solarized.git $HOME/.config/awesome/themes/awesome-solarized
fi

[[ -e "$HOME/.config/awesome/rc.lua" ]] && [[ ! -h "$HOME/.config/awesome/rc.lua" ]] && mv "$HOME/.config/awesome/rc.lua" "$HOME/old-dotfiles/awesomerc.lua"
[[ -e "$HOME/.config/awesome/rc.lua" ]] && [[ -h "$HOME/.config/awesome/rc.lua" ]] && rm "$HOME/.config/awesome/rc.lua"

ln -s "$PWD/awesomerc.lua" "$HOME/.config/awesome/rc.lua"
