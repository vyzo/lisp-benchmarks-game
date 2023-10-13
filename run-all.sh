#!/bin/bash

. setup.sh

for x in $GAMES; do
    echo ">>> Running game $x"
    if [ -z "$1" ]; then
       for y in $PLAYERS; do
           echo ">>> Playing with $y"
           ./run.sh $y $x
       done
    else
        ./run.sh $1 $x
    fi
done
