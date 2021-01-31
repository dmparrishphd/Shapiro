nl.extract.names <- function(X, names.=NA_character_)
        nl.extract(X, match(names., X %|% names) %|% rmna)
