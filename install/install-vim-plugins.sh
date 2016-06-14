DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Initialize submodule repositories
git submodule init

# Copy attributes files into submodule attributes files
cp $DIR/../.gitattributes $DIR/../.git/modules/.vim/bundle/vim-latex/info/attributes

# Download contents
git submodule update
