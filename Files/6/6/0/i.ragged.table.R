i.ragged.table <- function(lengths) #TAGS index
        do.call(rbind, lapply(
            seq_along(lengths),
            function(k, n=lengths[k])
                    cbind(seq(n), rep(k, n) ) ) )

    Doc$i.ragged.table <- '
        i.ragged.table returns an index into a ragged, two-
        dimensional table.

        The argument specifies, for each outer element, the
        number of inner elements.

        Like an index matrix, the first column represents the
        innermost, "faster" dimension, while the second column
        represents the outer, "slower" dimension.

        A more precise interpretation of the return requires
        knowledge of the object being indexed. See examples.

        SEE ALSO

        https://en.wikipedia.org/wiki/Jagged_array

        EXAMPLES

        RAG <- list(LETTERS[1:9], letters[1:3]); RAG
    
        IDX <- `colnames<-`(

            i.ragged.table(vapply(RAG, length, 1L)),

            c("INNER", "OUTER")); IDX

        data.frame(IDX, ITEM=vapply(

            as.data.frame(t(IDX), optional=T),

            function(i) RAG[[i[2]]][i[1]],

            RAG[[1]][1]) )


        RAG <- list(matrix(0:3, 2), matrix(4:9, ncol=2)); RAG

        IDX <- `colnames<-`(

            i.ragged.table(vapply(RAG, nrow, 1L)),

            c("INNER", "OUTER")); IDX

        do.call(rbind, lapply(

            seq(nrow(IDX)),

            function(k) RAG[[IDX[k, "OUTER"]]][IDX[k, "INNER"],] ) )'

