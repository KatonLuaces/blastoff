#!/bin/bash

# OLD:
# docker run --rm -it -v `pwd`:/home/microc -w=/home/microc columbiasedwards/plt

# NEW:
# Note: the build will take a long time at first, but will take trivial time
#       in subsequent runs so long as the Dockerfile isn't modified
docker build -t plt_image . \
    && docker run --rm -it -v `pwd`:/home/blastoff -w=/home/blastoff plt_image
