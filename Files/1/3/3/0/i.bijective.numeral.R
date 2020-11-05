i.bijective.numeral <- function(bn, base=26L)
        isum(bn * base ^ (bn %|% seq_along %|% pred))
