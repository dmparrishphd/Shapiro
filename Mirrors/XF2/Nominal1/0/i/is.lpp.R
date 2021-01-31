is.lpp <- function(x)
        none(vapply2(x, mode) != "numeric") &
        none(vapply2(x, class) %notin% words("integer numeric")) &
        vapply2(x, `#`) %|% diff %|% none
