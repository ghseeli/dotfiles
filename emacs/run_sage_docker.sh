#!/bin/bash

docker run -it -v /tmp:/tmp -v $(pwd):/code -v /var/folders:/var/folders -v /Users/ghseeli/.emacs.d:/Users/ghseeli/.emacs.d sagemath/sagemath:latest sage --simple-prompt
