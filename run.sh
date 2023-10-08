#!/bin/bash
set -eu

target=$1
program=$2
runs=${3:-3}

run_it() {
    case $target in
        racket)
            compile="raco make ${program}.rkt"
            run="racket ${program}.rkt"
            ;;
        go)
            compile="go build -o ${program}-go ${program}.go"
            run="./${program}-go"
            ;;
        gerbil)
            compile="gxc -exe -o ${program}-gerbil -O ${program}.ss"
            run="./${program}-gerbil"
            ;;
        gerbil-fpo)
            compile="gxc -exe -o ${program}-gerbil-fpo -O -full-program-optimization -prelude '(declare (not safe))' ${program}.ss"
            run="./${program}-gerbil-fpo"

    esac

    echo "${compile}"
    ${compile}

    rm -f $target.time $target.output
    i=1
    while [ ${i} -le ${runs} ]; do
        echo "run $i"
        /usr/bin/time -f "%U,%S,%M" -a -o ${target}.time ${run} "$(cat input)" > ${target}.output
        diff -q output ${target}.output
        if [ $? -ne 0 ]; then
            echo "output does not match expected"
            exit 1
        fi
        i=`expr $i + 1`
    done
    cat ${target}.time
}


pushd "programs/${program}"
run_it
popd
