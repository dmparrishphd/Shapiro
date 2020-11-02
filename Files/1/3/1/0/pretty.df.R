pretty.df <- function(.data.frame, na="NA") {
	ROWNAMES <- rownames(.data.frame)
	NCOL <- ncol(.data.frame)
	if (!NCOL) return (matrix(ROWNAMES))
	NAMES.USE <- names(.data.frame)
	if (is.null(NAMES.USE)) NAMES.USE <- rep("", NCOL)
	COLUMNS <- c(
		lapply(
			seq_along(.data.frame),
			function(i) c(NAMES.USE[[i]], as.character(.data.frame[[i]]))),
		list(c("rowname", ROWNAMES)))
	FMT <- lapply(
		COLUMNS,
		function(x) {
			x[is.na(x)] <- na
			formatC(x, width=max(nchar(x)))})
	matrix(unlist(FMT), ncol=1 + NCOL)}
