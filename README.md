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

At the BASIC screen, type

    SYS2049

The screen should turn black and dots should appear.

If you want to run a specific version of VICE, set the `X64SC` environment
variable.

## Resources

This project used several resources for which I am really grateful:

- [6502.org Instruction reference](http://www.6502.org/users/obelisk/6502/reference.html)
- [c64-wiki BASIC floating point ops](https://www.c64-wiki.com/wiki/Floating_point_arithmetic)
- [c64-wiki Zeropage layout](https://www.c64-wiki.com/wiki/Zeropage)
- [codebase64 floating point ops](https://codebase64.org/doku.php?id=base:kernal_floating_point_mathematics)
- [codebase64 screen mode doc](https://www.codebase64.org/doku.php?id=base:built_in_screen_modes)
- [Christian Bauer's VIC-II screen doc](http://www.zimmers.net/cbmpics/cbm/c64/vic-ii.txt)
  (it seems to be the most correct one I found)

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
