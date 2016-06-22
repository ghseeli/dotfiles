DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

mkdir -p $DIR/../.vim/autoload $DIR/../.vim/bundle 
if ! [ -f $DIR/../.vim/autoload/pathogen.vim ]; then
    curl -LSso $DIR/../.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
fi

