as_bb <- function(X) #TAGS bounding box
    matrix(
        lapply(
            .bounding.box(X),
            function(Y) if (Y %|% is.empty) -Inf %,% Inf else Y) %|%
        unlist,
        nrow=2)
