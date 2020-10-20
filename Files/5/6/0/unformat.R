unformat <- function (h, whitespace="( |\t|\r|\n)+") gsub(whitespace, " ", h) %|% trimws
