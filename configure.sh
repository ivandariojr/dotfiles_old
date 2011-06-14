test=( bashrc emacs emacs.d stumpwmrc screenrc )

for (( i = 0 ; i < ${#test} - 1 ; i++ ))
do
    mv ~/.${test[i]} ~/old-dotfiles/${test[i]}
    ln -s $PWD/${test[i]} ~/.${test[i]}
    # echo ${test[i]}
done
