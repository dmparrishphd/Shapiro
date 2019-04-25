list(
    .DOC='
            An object prototype for building slices.

            MEMBER FUNCTIONS

            init

            creates the slice proper from a vector, matrix, or
            list argument.

            init(vector)

            A vector arg2 specifies indices of a one-dimensional
            array.

            init(matrix)

            A matrix arg2 specifies indices of a
            multi-dimensional array; the values in the matrix
            specify the bounding box (one column per dimension,
            consistent with Extract, `[`).

            Typically (always?), the return of self$bounding.box
            will match arg2.

            init(list)

            A list arg2 specifies array section components (one
            dimension per list element)

            as.list

            returns a representation of the slice as a list, one
            element per dimension, each list element containing
            a vector of indices of array sections for that
            dimension.

            bounding.box

            returns a matrix, one column per dimension,
            containin the range of indices found in the slice.
            The return could be used to draw a rectangle around
            a graphical representation of a slice.',
    doc=function (.) catpars(.$.DOC),
    .init=function (., x) {
        .$.SLICE <- list(x)
        . },
    .init.m=function (., m) {
        .$.SLICE <- lapply(
            m %|% colNos,
            function(i) cols(m, i) %|% i.seq.range)
        . },
    .init.l=function (., X) {
        .$.SLICE <- X
        . },
    .init.default=function (., X) {
        .$.SLICE <- list()
        . },
    init=function (., X)
            (      if (X %|% is.list)   { .$.init.l(., X)
            } else if (X %|% is.matrix) { .$.init.m(., X)
            } else if (X %|% is.vector) { .$.init(., X)
            } else                        .$.init.default(., X) ),
    as.list=function (.) .$.SLICE,
    bounding.box=function(.) .$.SLICE %|% ranges,
    .NULL=NULL)
