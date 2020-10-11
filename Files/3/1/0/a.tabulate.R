a.tabulate <- function (FUN, LIST) {
	DIM <- vapply(LIST, length, 1 %|% integer)
	mi <- DIM %|% ai.every
	X <- extract.parallel(LIST, mi) %|% data_frame
	dfapply(X, FUN) %=>% dimension %:|% DIM }
