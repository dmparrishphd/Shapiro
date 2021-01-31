lcopy.nom <- function (x, y=list()) {
    for (nom in names(y))
            if (nom %in% names(x))
                    x[[nom]] <- y[[nom]]
    x }
