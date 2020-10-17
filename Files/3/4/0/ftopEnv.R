function(path) {
	`%//%` <- paste0
	ftopEnv <- new.env(parent=as.environment("package:stats"))
	source. <- function(filename) source(filename, local=ftopEnv)
	source.(path %//% "fhead.n.R")
	source.(path %//% "funtil.R")
	source.(path %//% "fhead.pattern.R")
	attach(ftopEnv) }
