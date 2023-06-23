#!/bin/bash

DESCRIPTION="1{b[9-10]f[CAGAGC]u[8]b[10]}2{r:}"

# FILE1="test_outliars.fastq"
FILE1="sciseq3_r1.fastq"
FILE2="sciseq3_r2.fastq"

IN1="./example_data/$FILE1"
IN2="./example_data/$FILE2"

OUT1="./out/$FILE1"
OUT2="./out/$FILE2"


cargo run -- -g $DESCRIPTION -1 $IN1 -2 $IN2 -o $OUT1 -w $OUT2