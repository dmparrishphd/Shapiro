ramdisc_find <- function(ramdisc, ...) names(ramdisc)[vapply(
    X=seq_along(ramdisc), FUN.VALUE=T, FUN=
    function(k) any(grepl(x=ramdisc[[k]], ...)))]
