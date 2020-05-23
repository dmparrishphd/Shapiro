specialize <- function(name, n=2)
        eval.parent(n=n, parse(text=paste0(
            "`%", name, "%`", "<-", name)))
