#!/bin/bash

. setup.sh
for x in $GAMES; do
    pushd programs/${x}
    if [ ! -e output ]; then
        if [ -f output.xz ]; then
            echo "decompressing programs/${x}/output.xz"
            unxz -c output.xz > output
        else
            echo "programs/${x}/output.xz does not exist"
        fi
    fi
    popd
done
