print_x <- function(FUN, x) {
   lapply(x, function (X) X  % %  FUN  % %  pad.h  % %  cat)  % %  invisible
    cat.eol() }
