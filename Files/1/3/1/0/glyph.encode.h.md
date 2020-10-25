glyph.encode.h
==============

encode text as bits

Usage
-----

glyph.encode.h(h, ncol=8)

| Argument | Description                                                    |
| -------: | :------------------------------------------------------------- |
|      `h` | a `character string`                                           |
|   `ncol` | the number of columns to pack into each element of the return. |

Value
-----

integer vector whose values represent, bit-wise, the given character string.

Details
-------

Given a character string of "0" and "1" characters (whitespace ignored)
that may represent positive and negative space in an image
(such as are used to produce monochrome characters on a computer screen),
returns an integer vector whose values represent, bit-wise,
the given character string.

Exmaple
-------

    glyph.encode.h("
        01111100
        01100110
        01100110
        01111100
        01111000
        01101100
        01100110
        00000000")
    # [1]  62 102 102  62  30  54 102   0
                
Keywords
--------

keywords: character graphics binary
