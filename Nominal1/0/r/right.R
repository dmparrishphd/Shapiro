right <- function(character_, start=1)
        substr(
            character_,
            character_ %|% nchar - start + 1L,
            character_ %|% nchar)
