iseqs <- function (s, offsets=0L) {
    s <- s[1:(s  % %  `#` - max(offsets))]
    matrix(vapply(offsets, function (ofs) ofs + s, s + offsets[1]), ncol=offsets  % %  `#`) }
