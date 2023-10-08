#!/bin/bash
set -eu

target=$1
program=$2
runs=${3:-3}

compile_racket() {
    raco make ${program}.rkt
}

compile_go() {
    go build -o ${program}-go ${program}.go
}

compile_gerbil() {
    gxc -exe -o ${program}-gerbil -O ${program}.ss
}

compile_gerbil_fpo() {
    gxc -exe -o ${program}-gerbil-fpo -O -full-program-optimization -prelude '(declare (not safe))' ${program}.ss
}

compile_gcc() {
    cp ${program}.gcc ${program}.c
    gcc -O2 -o ${program}-gcc ${program}.c -lm
}

run_it() {
    case $target in
        racket)
            compile=compile_racket
            run="racket ${program}.rkt"
            ;;
        go)
            compile=compile_go
            run="./${program}-go"
            ;;
        gerbil)
            compile=compile_gerbil
            run="./${program}-gerbil"
            ;;
        gerbil-fpo)
            compile=compile_gerbil_fpo
            run="./${program}-gerbil-fpo"
            ;;
        gcc)
            compile=compile_gcc
            run="./${program}-gcc"
    esac

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
