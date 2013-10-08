#!/bin/bash
set -e
set -u

# download the code
wget -O - http://kheafield.com/code/kenlm.tar.gz |tar xz 

cd kenlm

# build it
./bjam -j4

# build and install the python module
python setup.py install
