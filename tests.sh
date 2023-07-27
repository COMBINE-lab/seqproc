#!/bin/bash

IN_DIR="./tests/test_data/in"
OUT_DIR="./tests/test_data/out"
FGDL_DIR="./tests/fgdl"
EXPECTED_DIR="./tests/test_data/expected_out"
DEV_NULL="/dev/null"

exec_test () {
    echo "Test $1"

    IN1="$IN_DIR/$1/$1""_l.fastq"
    IN2="$IN_DIR/$1/$1""_r.fastq"
    OUT="$OUT_DIR/$1/$1.fastq"
    FGDL="$FGDL_DIR/$1.fgdl"
    EXPECTED="$EXPECTED_DIR/$1.fastq"

    if [ -z "$2" ]
    then
        cargo run -- -g $FGDL -1 $IN1 -2 $IN2 -o $OUT -t 6
    else
        cargo run -- -g $FGDL -1 $IN1 -2 $IN2 -o $OUT -t 6 -a $2
    fi

    cmp -s $OUT $EXPECTED && echo "## Test Passed ##" || echo "## Test failed ##" && diff $OUT $EXPECTED
}

# ---------- #

exec_test "pad"
exec_test "match"
exec_test "revcomp"
exec_test "map" "$IN_DIR/map/map.tsv"
exec_test "filter" "$IN_DIR/filter/filter.txt"
exec_test "trunc"