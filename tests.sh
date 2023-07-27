#!/bin/bash

IN_DIR="./tests/test_data/in"
OUT_DIR="./tests/test_data/out"
FGDL_DIR="./tests/fgdl"
EXPECTED_DIR="./tests/test_data/expected_out"
DEV_NULL="/dev/null"

# ---------- #

echo "Test Pad"
PAD_L="$IN_DIR/pad/pad_l.fastq"
PAD_R="$IN_DIR/pad/pad_r.fastq"
PAD_OUT="$OUT_DIR/pad/pad.fastq"
PAD_FGDL="$FGDL_DIR/pad.fgdl"
PAD_EXPECTED="$EXPECTED_DIR/pad.fastq"

cargo run -- -g $PAD_FGDL -1 $PAD_L -2 $PAD_R -o $PAD_OUT -t 6 -a "more" "another" 
diff $PAD_OUT $PAD_EXPECTED

echo "Test Match"
MATCH_L="$IN_DIR/match/match_l.fastq"
MATCH_R="$IN_DIR/match/match_r.fastq"
MATCH_OUT="$OUT_DIR/match/match.fastq"
MATCH_FGDL="$FGDL_DIR/match.fgdl"
MATCH_EXPECTED="$EXPECTED_DIR/match.fastq"

cargo run -- -g $MATCH_FGDL -1 $MATCH_L -2 $MATCH_R -o $MATCH_OUT -t 6
diff $MATCH_OUT $MATCH_EXPECTED

echo "Test Reverse Compliment"
REVCOMP_L="$IN_DIR/revcomp/revcomp_l.fastq"
REVCOMP_R="$IN_DIR/revcomp/revcomp_r.fastq"
REVCOMP_OUT="$OUT_DIR/revcomp/revcomp.fastq"
REVCOMP_FGDL="$FGDL_DIR/revcomp.fgdl"
REVCOMP_EXPECTED="$EXPECTED_DIR/revcomp.fastq"

cargo run -- -g $REVCOMP_FGDL -1 $REVCOMP_L -2 $REVCOMP_R -o $REVCOMP_OUT -t 6
diff $REVCOMP_OUT $REVCOMP_EXPECTED

echo "Test Map"
MAP_L="$IN_DIR/map/map_l.fastq"
MAP_R="$IN_DIR/map/map_r.fastq"
MAP_OUT="$OUT_DIR/map/map.fastq"
MAP_FGDL="$FGDL_DIR/map.fgdl"
MAP_EXPECTED="$EXPECTED_DIR/map.fastq"
MAP_FILE="$IN_DIR/map/map.tsv"

cargo run -- -g $MAP_FGDL -1 $MAP_L -2 $MAP_R -o $MAP_OUT -t 6 -a $MAP_FILE
diff $MAP_OUT $MAP_EXPECTED

echo "Test Filter"
FILTER_L="$IN_DIR/filter/filter_l.fastq"
FILTER_R="$IN_DIR/filter/filter_r.fastq"
FILTER_OUT="$OUT_DIR/filter/filter.fastq"
FILTER_FGDL="$FGDL_DIR/filter.fgdl"
FILTER_EXPECTED="$EXPECTED_DIR/filter.fastq"
FILTER_FILE="$IN_DIR/filter/filter.txt"

cargo run -- -g $FILTER_FGDL -1 $FILTER_L -2 $FILTER_R -o $FILTER_OUT -t 6 -a $FILTER_FILE
diff $FILTER_OUT $FILTER_EXPECTED