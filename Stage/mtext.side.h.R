mtext.side.h <- function(h="top")
        if (h %|% is.character) match(h, nomatch=3L,
            table="bottom left top right" %|% words
        ) else 3L

    Doc$mtext.side.h <- '
        mtext.side.h returns an integer suitable for the side
        argument of mtext. The expected input is one of
        "bottom", "left", "top", or "right".

        > mtext.side.h("bottom")

        # [1] 1

        > mtext.side.h()

        # [1] 3
        '
