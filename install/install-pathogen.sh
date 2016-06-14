DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

mkdir -p $DIR/../.vim/autoload $DIR/../.vim/bundle && \
curl -LSso $DIR/../.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
