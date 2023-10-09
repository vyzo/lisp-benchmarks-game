#!/bin/bash

. setup.sh
./prepare.sh

for x in $GAMES; do
    echo ">>> Running game $x"
    for y in $PLAYERS; do
        echo ">>> Playing with $y"
        ./run.sh $y $x
    done
done
