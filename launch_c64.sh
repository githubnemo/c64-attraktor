#!/bin/sh

script_dir="$(dirname "$(readlink -e "$0")")"
readonly script_dir

# NOTE: RESTORE is ESC + Pg Up
#
# NOTE: VICE monitor (for debugging & disassembling) is launched with Alt+h
# on Linux

# DONE there's also x64sc used by https://github.com/nullman/c64-cc65,
# not sure what the difference is - apparently it is the 'more correct' version
#
# Note that we supply the `-autostartprgmode 1` flag so that we can launch
# .PRG files (actually CBM files) directly from the host system, skipping
# the creating of d64 disk images. 1 means to inject into RAM.
x64sc \
    -kernal "$script_dir"/ROM/vice-3.8/data/C64/kernal-901227-03.bin \
    -basic "$script_dir"/ROM/vice-3.8/data/C64/basic-901226-01.bin \
    -chargen "$script_dir"/ROM/vice-3.8/data/C64/chargen-906143-02.bin \
    -autostartprgmode 1 \
    -autostart-warp \
    +confirmonexit \
    "$@"
