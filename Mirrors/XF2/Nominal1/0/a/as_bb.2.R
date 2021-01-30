as_bb <- function(X)
    lapply(
        .bounding.box(X),
        function(Y) if (Y %|% is.empty) -Inf %,% Inf else Y) %|%
        unlist %|% matrix2r
