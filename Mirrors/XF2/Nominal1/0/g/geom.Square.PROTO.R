geom.Square.PROTO <- list(
    init=function (., side.length=1L) {
        .$.SIDE.LENGTH <- side.length
        . },
    as.pg=function(.) {
        as_xy(c(
                        0L,             0L,
            .$.SIDE.LENGTH,             0L,
            .$.SIDE.LENGTH, .$.SIDE.LENGTH,
                        0L, .$.SIDE.LENGTH))},
    .NULL=NULL)                                        
