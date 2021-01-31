i.look.inner <- function(Table, ...) Reduce(
    function(i, x) Table[i, x %|% first] == x %|% second,
    list(...) %|% enumerate,
    init=Table %|% nrow %|% logical %|% not)
