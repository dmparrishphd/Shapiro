Daisy
=====

Place preformatted lines of text.

Usage
-----

    Daisy(
        labels=character(),
        spacing=1,
        justification=1,
        xpd=T,
        ...)

|        Argument | Description                                 |
| --------------: | :------------------------------------------ |
|        `labels` | passed to `text`                            |
|       `spacing` | line spacing, multiple of em space          |
| `justification` | justification (should be `1`, `2`, or `1:2` |
|           `xpd` | passed to `text`                            |
|           `...` | passed to `text`                            |

Effect
------

Adds text to the current device.

Value
-----

A `double` vector with the lenghts of the en space and em space.
Can be used to compute parameters such as `cex` and `spacing`.
