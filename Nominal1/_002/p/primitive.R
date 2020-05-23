prefix <- function (h, p="-") paste(p, h, sep="")

unformat <- function (h, whitespace="( |\t|\r|\n)+")
        gsub(whitespace, " ", h) %|% trimws


