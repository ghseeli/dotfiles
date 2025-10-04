#!/bin/bash

docker run -it -v /tmp:/tmp -v $(pwd):/code -v /var/folders:/var/folders -v /Users/ghseeli/.emacs.d:/Users/ghseeli/.emacs.d -v /Users/ghseeli/qt-catalan-code:/Users/ghseeli/qt-catalan-code -v /Users/ghseeli/k_combinat_for_sage:/Users/ghseeli/k_combinat_for_sage -v /Users/ghseeli/combinatorialpolynomials:/Users/ghseeli/combinatorialpolynomials --platform=linux/amd64 sagemath/sagemath sage --simple-prompt
