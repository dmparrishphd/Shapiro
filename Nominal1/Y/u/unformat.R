unformat <- function (.character, whitespace="( |\t|\r|\n)+")
        gsub(whitespace, " ", .character) %|% trimws
