unleft <- function (character_, stop=1)
        substr(
            character_,
            start=1L + stop,
            stop=character_ %|% nchar)
