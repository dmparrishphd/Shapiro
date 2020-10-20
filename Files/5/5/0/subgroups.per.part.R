subgroups.per.part <- function (dims, subgroup.size=10) {
    if (length(dims) < 1) return (0)
    nsg = ceiling(dims[1] / subgroup.size) # NUMBER OF SUBGROUPS
    if (length(dims) < 2) return (nsg)
    # COMPUTE NUMBER OF SUBGROUPS IN EACH AGGREGATION
    sed = seq(2, length(dims))
    nlin = c(nsg, sed) # PREALLOCATE
    for (j in sed) { nlin[j] = nlin[j-1] * dims[j] }
    nlin }
