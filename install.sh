test=( 'bashrc' 'emacs' 'emacs.d' 'stumpwmrc' 'screenrc' 'tmux.conf' )

[[ ! -e ~/old-dotfiles ]] && mkdir ~/old-dotfiles

for (( i = 0 ; i < ${#test} ; i++ ))
do
	[[ -e "$HOME/.${test[i]}" ]] && [[ ! -h "$HOME/.${test[i]}" ]] && mv "$HOME/.${test[i]}" "$HOME/old-dotfiles/${test[i]}"
	[[ -e "$HOME/.${test[i]}" ]] && [[ -h "$HOME/.${test[i]}" ]] && rm "$HOME/.${test[i]}"

	ln -s "$PWD/${test[i]}" "$HOME/.${test[i]}" 
	echo "$HOME/.${test[i]}"
done
