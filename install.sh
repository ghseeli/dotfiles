BASE="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

function install_file_mv {
    # Note that -h flag is used because the assumption
    # is that if you have a symlink in place for these
    # files, you already know what you are doing.
    # 
    # -h flag also helps ensure idempotency.

    if [ -f $2 ] && ! [ -h $2 ];
    then
        if [ -f "${2}.local" ];
        then
            echo "Moving ${2}.local to ${2}.backup !"
            mv ${2}.local ${2}.backup
        fi
        echo "Moving ${2} to ${2}.local"
        mv ${2} ${2}.local
    fi
    if [ -h $2 ];
    then
        echo "$2 exists but not as a symbolic link. Leaving it as is."
    else
        ln -s $1 $2
    fi
}

# git identity
install_file_mv $BASE/.gitconfig ~/.gitconfig

# bash settings
install_file_mv $BASE/.bashrc ~/.bashrc

# vim setup
install_file_mv $BASE/.vim/.vimrc $HOME/.vimrc
install_file_mv $BASE/.vim $HOME

. $BASE/install/install-pathogen.sh
. $BASE/install/install-vim-plugins.sh

