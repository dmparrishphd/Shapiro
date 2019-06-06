list(
    init=function (., bb) {
        .$.BOX <- bb
        . },
    as.matrix=function (.) .$.BOX,
    as.points=function (.)
            if (.$.BOX %|% ncol == 2) {
                matrix2r(
                    .$.BOX[1, ] %,%               # LOWER-LEFT
                    .$.BOX[2:3] %,% # LOWER-RIGHT
                    .$.BOX[2, ] %,%               # UPPER-RIGHT
                    .$.BOX[1]  %,% .$.BOX[4])    # UPPER-RIGHT
            } else {
                'as.points not designed for other than 2D
                bounding boxes.' %|% crunch.h %|% warning
                matrix(
                    vector(mode=.$.BOX %|% typeof),
                    nrow=2,
                    ncol=0) },
    .NULL=NULL )
