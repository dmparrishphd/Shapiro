H.i <- function (i) # TO DO: vectorize further with individual.characters() %in% letters
    vapply(
        seq_along(i),
        function (ii) if (i[[ii]] %in% 1:(LETTERS %|% `#`))
                LETTERS[[i[[ii]]]] else as.character(NA),
        FUN.VALUE=character(1))
