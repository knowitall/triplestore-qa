#!/bin/bash
set -u
set -e
input=$1
kenlm=$PWD/kenlm
$kenlm/bin/lmplz -o 5 < $input > $input.arpa
$kenlm/bin/build_binary $input.arpa $input.binary
