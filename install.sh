BASE=$(pwd)

# git identity
git config --global user.name "George H. Seelinger"

# vim setup
if [ -f $HOME/.vimrc ];
then
    mv $HOME/.vimrc .vimrc.old
fi
if [ -f $HOME/.vim ];
then
    mv $HOME/.vim .vim.old
fi
ln -s $BASE/.vim/.vimrc $HOME/.vimrc
ln -s $BASE/.vim $HOME/.vim
./install-pathogen.sh
