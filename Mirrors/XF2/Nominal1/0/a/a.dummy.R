a.dummy <- function (dim)
        array(
            rapply.(
                as_storage.mode(
                    "character",
                    arrayInd(
                        dim %|% prod %|% seq,
                        dim) %|% cflip),
                `%//%`),
              dim)
