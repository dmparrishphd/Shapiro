isodates.mdy <- function(x, sep="/")
        strsplit(x, sep) %=>% vapply2 %:|% function(y)
                paste(
                    pad.h(y[  3], 4, "0") %,%
                    pad.h(y[1:2], 2, "0"),
                    collapse="-")
