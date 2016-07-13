DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if ! [ -f $DIR/../.vim/autoload/pathogen.vim ]; then
    echo "Pathogen not found. Installing Pathogen."
    mkdir -p $DIR/../.vim/autoload $DIR/../.vim/bundle &&
    curl -LSso $DIR/../.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
fi

