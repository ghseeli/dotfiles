BASE="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# git identity
git config --global user.name "George H. Seelinger"

# bash settings
if [ -f ~/.bashrc ];
then
    mv ~/.bashrc ~/.bashrc.local
fi

ln -s $BASE/.bashrc ~/.bashrc

# vim setup
if [ -f $HOME/.vimrc ];
then
    mv $HOME/.vimrc $HOME/.vimrc.old
fi
if [ -f $HOME/.vim ];
then
    mv $HOME/.vim $HOME/.vim.old
fi
ln -s $BASE/.vim/.vimrc $HOME/.vimrc
ln -s $BASE/.vim $HOME

. $BASE/install/install-pathogen.sh
. $BASE/install/install-vim-plugins.sh

