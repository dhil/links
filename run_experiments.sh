#!/bin/bash

### Configuration
NITER=1
LINKS_DIR="$HOME/projects/links/my-links"
RESULT_DIR="$LINKS_DIR/results"
ENV_LOG="$RESULT_DIR/env.log"
DATE_LOG="$RESULT_DIR/date.log"
PROG_DIR="$LINKS_DIR/benchmarks/handlers/js"
PROGS=("shallow_pipes.links" "deep_pipes.links" "count_deep_fun.links" "queens.links" "queens_shallow.links" "count_shallow_fun.links" "count_deep_fun.links" "count_deep_enc.links" "count_shallow_enc.links")
CPS_CC="$LINKS_DIR/linksjs.cps2"
CEK_CC="$LINKS_DIR/linksjs.cek"
JS="$HOME/projects/v8/out.gn/x64.release/d8"

### Main driver
# Dump env
if [[ ! -d $RESULT_DIR ]]; then
    mkdir -p "$RESULT_DIR"
fi
env > $ENV_LOG
date > $DATE_LOG

for prog in ${PROGS[@]}; do
    echo $prog
    out="a.js"
    # Compile CPS
    bash $CPS_CC "$PROG_DIR/$prog" > $out
    # Run CPS
    log="$RESULT_DIR/$prog-cps.log"
    rm -f $log
    for i in $(seq 1 $NITER); do
       $JS $out >> $log
    done

    # Compile CEK
    rm -f $out
    bash $CPS_CKE "$PROG_DIR/$prog" > $out
    # Run CPS
    log="$RESULT_DIR/$prog-cek.log"
    rm -f $log
    for i in $(seq 1 $NITER); do
       $JS $out >> $log
    done
done
date >> $DATE_LOG

