hetero.apply <- function(X, FUN, ...)
        lapply(seq_along(X),
            function(k) do.call(FUN, c(X[[k]], list(...))))

    Doc$hetero.apply <- '
        hetero.apply is similar to `lapply`, except that `X`
        (arg 1) is a compound list.  Furthermore, each element
        of `X` is a list of arguments that will be passed to
        `FUN` (arg 2).  The remaining args, `...` are also
        passed to `FUN`; they are positioned AFTER `X` in the
        call to `FUN`.

        for (item in hetero.apply(
                              
            list( list(7), list(to=41, from=43) ),

            seq) )
        
                cat(item, "\n")

        # 1 2 3 4 5 6 7 
        # 43 42 41 
        '
