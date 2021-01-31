n.look.d <- function (d, target)
        if (target < d %|% first) {
            1L
        } else {
            (d <= target) %|% which %|% last }
