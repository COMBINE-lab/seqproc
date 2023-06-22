#!/bin/bash

DESCRIPTION="1{b[9-10]f[CAGAGC]u[8]b[10]}"
FILE="sciseq3_r1.fastq"
IN="./example_data/$FILE"
OUT="./out/$FILE"

echo $DESCRIPTION
echo $IN
echo $OUT

cargo run $DESCRIPTION $IN $OUT