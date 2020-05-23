strtrunc <- function(x, unstop=1)
        substr(x, 1, nchar(x) - unstop)

    Doc$strtrunc <- '
        strtrunc returns a truncated copy of the character
        vector (`x`, arg 1). The return is formed as if a copy
        of the argument had the right `unstop` (arg 2)
        characters removed.

        If unstop is greater than or equal to the nchar of an
        element, the corresponding element of the return will be
        the null character, "".

        WORDS <- c("AS", "YOU", "WISH")

        WORDS
        # [1] "AS"   "YOU"  "WISH"

        strtrunc(WORDS)
        # [1] "A"   "YO"  "WIS"

        strtrunc(WORDS, 3)
        # [1] ""  ""  "W"

        strtrunc(WORDS, 1:3)
        # [1] "A" "Y" "W"
        '
