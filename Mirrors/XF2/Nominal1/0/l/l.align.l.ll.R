l.align.l..l <- function (list1, list2) { #TAGS join
    # REMIDER: PRESERVE THE ASPECT OF THIS CODE THAT ALLOWS THE
    # LIST ARGUMENTS TO BE LIST OF PARALLEL ATOMIC VECTORS.
    index1 <- list1 %|% first
    index2 <- list2 %|% first
    index  <- index1 %,% index2 %|% unique
    list(
         index,
         (list1 %|% second) [match(index, index1)],
         (list2 %|% second) [match(index, index2)] ) }
