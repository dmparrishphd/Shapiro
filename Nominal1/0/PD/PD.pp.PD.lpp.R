.pp.lpp <- function(l)
        if (l %|% is.empty) m.empty() else
                matrix(l %|% unlist, l[[1]] %|% `#`)
