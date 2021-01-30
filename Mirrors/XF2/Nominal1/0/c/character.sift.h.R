character.sift.h <- function(h, b.FUN=is_ascii) vapply(
	lapply(h  % %  l.characters, sift %^% list(b.FUN=b.FUN)),
			`%//%`, "")
