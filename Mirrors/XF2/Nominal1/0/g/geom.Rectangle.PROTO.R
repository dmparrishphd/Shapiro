geom.Rectangle.PROTO <- list(
    init=function (.=geom.Rectangle.PROTO, dims=1L %|% rep2) {
        .$.WIDTH  <- dims[1]
        .$.HEIGHT <- dims[2]
        . },
    width =function(.) .$.WIDTH  %|% as.double,
    height=function(.) .$.HEIGHT %|% as.double,
    dims=function(.) .$.WIDTH %,% .$.HEIGHT %|% as.double,
    as.pg=function(.) {
        as_xy(c(
                  0L,        0L,
            .$.WIDTH,        0L,
            .$.WIDTH, .$.HEIGHT,
                  0L, .$.HEIGHT))},
    walk.uniform=function(., scale=1L)
            walk.pg(
	            . %|% .$as.pg,
                length.out=round(. %|% .$dims %|% rep2 * scale)),
    .NULL=NULL)                                        
