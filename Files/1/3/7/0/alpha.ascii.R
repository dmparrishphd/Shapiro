ALPHA.ASCII <- (function() {
    SPLIT <- strsplit(strsplit(.ALPHA.ASCII, "\n")[[1]], "")
    `rownames<-`(
        matrix(vapply(
            SPLIT,
            function(x) x[1], "")),
        unlist(lapply(
            lapply(
                SPLIT,
                function(x) x[2:3]),
            paste,
            collapse="") ) ) } )()
