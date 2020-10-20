pad.h <- function(h, n=h %|% nchar_max, hpad=HSPACE) {
	n <- max(h %|% nchar_max, n)
    dimension(
        prefix(h, strrep(hpad, n)) %=>% right %:|% n,
        h %|% dim) }
