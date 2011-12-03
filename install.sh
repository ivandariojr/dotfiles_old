test=( 'bashrc' 'emacs' 'emacs.d' 'stumpwmrc' 'screenrc' 'tmux.conf' )

[[ ! -e "$HOME/old-dotfiles" ]] && mkdir "$HOME/old-dotfiles"

for (( i = 0 ; i < ${#test} ; i++ ))
do
	[[ -e "$HOME/.${test[i]}" ]] && [[ ! -h "$HOME/.${test[i]}" ]] && mv "$HOME/.${test[i]}" "$HOME/old-dotfiles/${test[i]}"
	[[ -e "$HOME/.${test[i]}" ]] && [[ -h "$HOME/.${test[i]}" ]] && rm "$HOME/.${test[i]}"

	ln -s "$PWD/${test[i]}" "$HOME/.${test[i]}" 
done

[[ "$(ls -A $HOME/old-dotfiles)" ]] && echo "Copied old dotfiles to $HOME/old-dotfiles" || rm -rf "$HOME/old-dotfiles"
