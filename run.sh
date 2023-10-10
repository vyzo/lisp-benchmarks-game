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

check() {
    local file=${1}
    if [ ! -e ${file} ]; then
        echo "${file} does not exist"
        exit 42
    fi
}

run_it() {
    case $target in
        gcc)
            check ${program}.gcc
            compile=compile_gcc
            run="./${program}-gcc"
            ;;
        go)
            check ${program}.go
            compile=compile_go
            run="./${program}-go"
            ;;
        racket)
            check ${program}.rkt
            compile=compile_racket
            run="racket ${program}.rkt"
            ;;
        gerbil)
            check ${program}.ss
            compile=compile_gerbil
            run="./${program}-gerbil"
            ;;
        gerbil-fpo)
            check ${program}.ss
            compile=compile_gerbil_fpo
            run="./${program}-gerbil-fpo"
            ;;
    esac

    ${compile}

    rm -f $target.time $target.output
    i=1
    while [ ${i} -le ${runs} ]; do
        echo "run $i"
        if [ -e input.stdin ]; then
            /usr/bin/time -f "%U,%S,%M" -a -o ${target}.time ${run} < input.stdin > ${target}.output
        else
            /usr/bin/time -f "%U,%S,%M" -a -o ${target}.time ${run} "$(cat input)" > ${target}.output
        fi
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
