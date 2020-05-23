index.factors.dim <- function (dims)
        if (dims %|% `#` < 2) {
            1L
        } else {
            make.sequence(
                function(i, seed, ref) seed[i %|% pred] * ref[i %|% pred],
                seed=1L,
                length.out=length(dims),
                ref=dims) }
