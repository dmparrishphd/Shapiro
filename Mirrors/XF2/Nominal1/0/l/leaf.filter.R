leaf.filter <- function(t, FUN=typeof, what="NULL", compar=`!=`)
        leaf.indices(t)[compar(unlist(lapply(leaves(t), FUN)), what)]
