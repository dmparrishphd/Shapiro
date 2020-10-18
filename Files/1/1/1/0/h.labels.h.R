h.labels.h <- function (h, prefix="", suffix=":", unlabel="")
{   if (!length(h)) return (character())
    unlabel -> h[!nchar(h)]
    prefix(prefix(suffix, h), prefix) }
