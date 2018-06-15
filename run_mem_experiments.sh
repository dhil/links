#!/bin/bash

### Configuration
NITER=1
LINKS_DIR="$HOME/projects/links/my-links"
RESULT_DIR="$LINKS_DIR/results-mem"
ENV_LOG="$RESULT_DIR/env.log"
DATE_LOG="$RESULT_DIR/date.log"
ULIMIT_LOG="$RESULT_DIR/ulimit.log"
PROG_DIR="$LINKS_DIR/benchmarks/handlers/js"
PROGS=("shallow_pipes.links" "deep_pipes.links" "deep_pipes_enc.links" "shallow_pipes_enc.links")
CPS_CC="$LINKS_DIR/linksjs.cps2"
CEK_CC="$LINKS_DIR/linksjs.cek"
JS="$HOME/projects/ecmascript/engines/bin/d8"
FLAGS="--stack-size 131072 --use-strict"

### Main driver
# Dump env
if [[ ! -d $RESULT_DIR ]]; then
    mkdir -p "$RESULT_DIR"
fi
ulimit -a > $ULIMIT_LOG
env > $ENV_LOG
date > $DATE_LOG

function monitor
{
    local cmd="d8"
    local log=$1
    echo "$cmd $log"
    local i=0
    printf "time,mem0,mem\n" > $log
    while true; do
        printf "%d,%s,%s\n" $i $(ps -C "$cmd" -o %mem=) >> $log
        i=$(($i + 1))
        sleep 1
    done
}

# CPS
for prog in ${PROGS[@]}; do
    echo "CPS $(date): $prog"
    out="a.js"
    # Compile CPS
    bash $CPS_CC "$PROG_DIR/$prog" > $out
    # Run CPS
    log="$RESULT_DIR/$prog-cps.log"
    rm -f $log
    for i in $(seq 1 $NITER); do
        monitor "$log" &
        pid=$!
        sleep 1
        $JS $FLAGS $out > /dev/null
        sleep 1
        kill -9 $pid
    done
done

# CEK
for prog in ${PROGS[@]}; do
    echo "CEK $(date): $prog"
    # Compile CEK
    rm -f $out
    bash $CEK_CC "$PROG_DIR/$prog" > $out
    # Run CPS
    log="$RESULT_DIR/$prog-cek.log"
    rm -f $log
    for i in $(seq 1 $NITER); do
        monitor "$log" &
        pid=$!
        sleep 1
        $JS $FLAGS $out > /dev/null
        sleep 1
        kill -9 $pid
    done
done
date >> $DATE_LOG

