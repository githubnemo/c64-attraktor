# Lorenz Attraktor on C64

https://github.com/user-attachments/assets/e2ddfaac-57ff-45fa-83a3-d15bf8e24276

## Compiling / Development

This project uses the [ACME][acme] assembler for no particular reason
but that it was the least complicated and most easy to find assembler.
I used ACME 0.97.

For testing I found it best to use [VICE][vice], I had a few problems
launching .PRG files with the default configuration so I created my
own launch script which enables auto start PRG mode.

Note: the script assumes that ROM files (KERNAL, BASIC, character set)
are present in the ROM/ directory. Make sure to place them there.
They can be found in the release [download][vice download] of VICE.

[acme]: https://github.com/meonwax/acme
[vice]: https://vice-emu.sourceforge.io/
[vice download]: https://vice-emu.sourceforge.io/index.html#download

## Running

    cd src
    make run

The screen should turn black and green dots should appear.

Note that normally you would need to start the program yourself using
a `SYS 2049` statement (or whatever the initial address is) to jump into
the code. We add a header value that adds a bit of BASIC code that does
this for us.

If you want to run a specific version of VICE, set the `X64SC` environment
variable.

## Running on C64

I used a SD2IEC adapter and copied the `src/myprg.prg` file to the root
of the SD card. Then it is just a matter of loading the program:

    LOAD "myprg.prg",8
    RUN

I had problems where the SD2IEC adapter didn't even load the directory list
(`LOAD "$",8`) and simply flashed the error LED. Do diagnose this I ran this
code:

    10 OPEN 15,8,15: INPUT #15,A$,B$,C$,D$
    20 CLOSE 15
    30 PRINT A$,B$,C$,D$
    RUN

The message I got was "74,Drive not ready,12,0" which was caused by a
intermittent fault of the SD card socket (card detect or write protect was not
triggered correctly). Re-inserting the SD card a few times solved the issue.

## Usage

There are three keys you can press:

- R for clearing the screen and resetting the X/Y/Z values
- N to play a melodic sound based on the current trajectory
- M to play a bass sound based on the current trajectory

## Resources

This project used several resources for which I am really grateful:

- [6502.org Instruction reference](http://www.6502.org/users/obelisk/6502/reference.html)
- [6502.org Addressing reference](http://www.6502.org/users/obelisk/6502/addressing.html)
- [6502.org Opcodes reference](http://www.6502.org/tutorials/6502opcodes.html)
- [c64-wiki BASIC floating point ops](https://www.c64-wiki.com/wiki/Floating_point_arithmetic)
- [c64-wiki Zeropage layout](https://www.c64-wiki.com/wiki/Zeropage)
- [codebase64 floating point ops](https://codebase64.org/doku.php?id=base:kernal_floating_point_mathematics)
- [codebase64 screen mode doc](https://www.codebase64.org/doku.php?id=base:built_in_screen_modes)
- [Christian Bauer's VIC-II screen doc](http://www.zimmers.net/cbmpics/cbm/c64/vic-ii.txt)
  (it seems to be the most correct one I found)
- [codebase64 micro tracker example](https://codebase64.org/doku.php?id=base:microtracker_v1.0)
- [cbm64 memory assignment map](https://sta.c64.org/cbm64mem.html)

There are three python scripts that were helpful in the development of
the final assembler code.

1. [`attraktor.py`](./attraktor.py) - a reference implementation that got
   subsequently optimized to bit oeprations
2. [`encode_xy.py`](./src/encode_xy.py) - a way of testing & reverse engineering
   the bit addressing in the 320x200 (but actually 40x25 byte) screen of the
   C64
3. [`cbm_to_float.py`](./src/cbm_to_float.py) - a script of converting C64
   floating point numbers (5 byte) to python floats for easy debugging with
   the VICE monitor
