#!/bin/bash


#############################################
########### Install things to ~ #############
#############################################
test=( 'bashrc' 'emacs' 'emacs.d' 'stumpwmrc' 'screenrc' 'tmux.conf' 'vimrc' 'zshrc' 'xmonad' 'xmobarrc' )

[[ ! -e "$HOME/old-dotfiles" ]] && mkdir "$HOME/old-dotfiles"

#echo "${test[@]}"
#echo "${#test[@]}"

for (( i = 0 ; i < ${#test[@]} ; i++ ))
do
	[[ -e "$HOME/.${test[i]}" ]] && [[ ! -h "$HOME/.${test[i]}" ]] && mv "$HOME/.${test[i]}" "$HOME/old-dotfiles/${test[i]}"
	[[ -e "$HOME/.${test[i]}" ]] && [[ -h "$HOME/.${test[i]}" ]] && rm "$HOME/.${test[i]}"

	ln -s "$PWD/${test[i]}" "$HOME/.${test[i]}" 
    echo "installed $HOME/.${test[i]}"
done

[[ "$(ls -A $HOME/old-dotfiles)" ]] && echo "Copied old dotfiles to $HOME/old-dotfiles" || rm -rf "$HOME/old-dotfiles"

#############################################
########### Install other things ############
#############################################

### awesome
if [ ! -d "~/.config/awesome" ]; then
    mkdir "~/.config/awesome"
fi

if [ ! -d "~/.config/awesome/revelation" ]; then
    git clone git://github.com/bioe007/awesome-revelation.git ~/.config/awesome/revelation
fi

if [ ! -d "~/.config/awesome/themes/" ]; then
    mkdir "~/.config/awesome/themes"
fi

if [ ! -d "~/.config/awesome/themes/awesome-solarized" ]; then
    git clone git://github.com/cycojesus/awesome-solarized.git ~/.config/awesome/themes/awesome-solarized
fi

[[ -e "$HOME/.config/awesome/rc.lua" ]] && [[ ! -h "$HOME/.config/awesome/rc.lua" ]] && mv "$HOME/.config/awesome/rc.lua" "$HOME/old-dotfiles/awesomerc.lua"
[[ -e "$HOME/.config/awesome/rc.lua" ]] && [[ -h "$HOME/.config/awesome/rc.lua" ]] && rm "$HOME/.config/awesome/rc.lua"

ln -s "$PWD/awesomerc.lua" "$HOME/.config/awesome/rc.lua"
