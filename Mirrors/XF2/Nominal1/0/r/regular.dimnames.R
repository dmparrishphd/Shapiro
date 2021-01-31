regular.dimnames <- function(.dim, prefixes=LETTERS[.dim %|% seq_along], sep=" ")
        lapply(
            .dim %|% seq_along,
            function(k) paste(prefixes[k], .dim[k] %|% seq, sep=sep))
