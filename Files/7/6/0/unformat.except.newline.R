unformat.except.newline <- function(h)
        vapply2(h %|% strsplitbynewline %|% unlist, unformat)
        
