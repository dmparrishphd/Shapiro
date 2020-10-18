is.NULLs <- function (x)
        x  % %  is.list &&
        lapply(x, is.null)  % %  unlist  % %  all
