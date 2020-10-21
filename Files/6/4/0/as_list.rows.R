as_list.rows <- function(X)
        lapply(X %|% nrow %|% seq, function(k) X[k,] %|% as.list)

    Doc$as_list.rows <- '
        as_list.rows interprets each row of the matrix or
        data.frame argument as a list of values.  The return
        contains one item per row of the argument; each item
        contains the items of the corresponding row of the
        argument.

        Originally developed for the purpose of creating
        compound attributes (possibly compound or composite keys;
        see also https://en.wikipedia.org/wiki/Compound_key).

        as_list.rows(matrix(1:6, 2))

        returns the equivalent of 

        list(
            list(1, 3, 5),
            list(2, 4, 6)))
        '
