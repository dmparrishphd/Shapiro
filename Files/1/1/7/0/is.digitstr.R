is.digitstr <- function(h)
        vapply(characters(h), is.digit, T) %|% all
