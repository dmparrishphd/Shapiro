chop <- function(character_string, lengths=1) { #TAGS substr
    if (
        lengths %|% length %|% `!` ||
        any(lengths < 0)           ||
        lengths %|% `!` %|% all    ||
        !nchar(character_string)) return (character_string)
    lengths <- recycle( # EXTEND LENGTHS SO THAT ALL CHARACTERS ARE PROCESSED.
        lengths,
        function(x) character_string %|% nchar   <   x %|% sum)
    1L %,% lengths %|% accum -> limits
    result <- vapply(
        seq_along(limits[-1]),
        function(i) substr(character_string, limits[i], limits[1+i] - 1),
        "")
    result[0:(result %|% nchar %|% `!!` %|% which1rev)] }
