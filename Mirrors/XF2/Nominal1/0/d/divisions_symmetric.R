divisions_symmetric <- function (n)
        (n %|% divisions %|% `-` %|% rev %,% 0 %,% divisions(n))[-1]
