mtext.side.h <- function(h="top") if (!(
    is.character(h) && length(h))) 3L else match(
        h, nomatch=3L, table=strsplit(split="/",
                "bottom/left/top/right")[[1]])
