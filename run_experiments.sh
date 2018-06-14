#!/bin/bash

### Configuration
NITER=1
LINKS_DIR="$HOME/projects/links/my-links"
RESULT_DIR="$LINKS_DIR/results"
ENV_LOG="$RESULT_DIR/env.log"
DATE_LOG="$RESULT_DIR/date.log"
ULIMIT_LOG="$RESULT_DIR/ulimit.log"
PROG_DIR="$LINKS_DIR/benchmarks/handlers/js"
PROGS=("shallow_pipes.links" "deep_pipes.links" "deep_pipes_enc.links" "shallow_pipes_enc.links" "queens_deep.links" "queens_shallow.links" "queens_deep_enc.links" "queens_shallow_enc.links" "count_deep_fun.links" "count_shallow_fun.links" "count_deep_fun.links" "count_deep_enc.links" "count_shallow_enc.links")
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
       $JS $FLAGS $out >> $log
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
      $JS $FLAGS $out >> $log
    done
done
date >> $DATE_LOG

