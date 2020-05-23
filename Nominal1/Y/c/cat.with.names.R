cat.with.names <- function(x, file="", sep=" ", end="\n", append=F)
		cat(
            rbind(x %|% names, sep, x %|% unlist, end),
            file=file, sep="", append=append)
