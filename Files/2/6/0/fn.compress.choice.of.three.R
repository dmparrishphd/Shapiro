fn.compress.choice.of.three <- function(choices) { #TAGS factor

        if (!is.list(choices))
                stop("Arument must be a list or data.frame.")

        LENGTH <- length(choices)

        if (!LENGTH %in% 2:3)
                stop("The list argument must have two or three elments.")

        if (!all(vapply(choices, is.vector, T)))
                stop("All elements of the list argument must be vector or list or data.frame.")

        if (sum(abs(diff( # sum abs diff : ARE 3 VALUES UNEQUAL?
            vapply(choices, length, 1L)))))
                stop("The list argument must have elements of equal length.")

        names(choices) <- c("TRUE.", "FALSE.", "NA.")[1:LENGTH]

        inner <- function(opts, x)
            if (missing(x)) return (opts) else c(F, NA, T)[
                opts[[2]] %in% x * -1L +
                opts[[1]] %in% x * +1L +
                2L] # +2L MAPS -1:1 TO 1:3
        inner %<=% choices }
