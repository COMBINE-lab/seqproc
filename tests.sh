#!/bin/bash

IN_DIR="./tests/test_data/in"
OUT_DIR="./tests/test_data/out"
FGDL_DIR="./tests/fgdl"
DEV_NULL="/dev/null"


echo "Test Pad"
PAD_L="$IN_DIR/pad/pad_l.fastq"
PAD_R="$IN_DIR/pad/pad_r.fastq"
PAD_OUT="$OUT_DIR/pad/pad.fastq"
PAD_FGDL="$FGDL_DIR/pad.fgdl"

cargo run -- -g $PAD_FGDL -1 $PAD_L -2 $PAD_R -o $PAD_OUT -t 6
head $PAD_OUT

echo "Test Match"
PAD_L="$IN_DIR/match/match_l.fastq"
PAD_R="$IN_DIR/match/match_r.fastq"
PAD_OUT="$OUT_DIR/match/match.fastq"
PAD_FGDL="$FGDL_DIR/match.fgdl"

cargo run -- -g $PAD_FGDL -1 $PAD_L -2 $PAD_R -o $PAD_OUT -t 6
head $MATCH_OUT