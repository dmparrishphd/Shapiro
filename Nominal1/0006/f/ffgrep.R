fgrep <- function(filename, ...) #TAGS search text file
		fapply.f(filename, grepl %^% list(...)) %|% unlist %|% which

    Doc$fgrep <- '
        fgrep is similar to grep, except that the character
        vectors analyzed are interpreded from the text file
        named by arg 1. The `...` are passed to grepl. The return
        is an integer vector of line numbers within the file
        specified.'

.ffgrep <- function(filenames, ...)
		lapply(filenames, fgrep, ...)

ffgrep <- function(filenames, ..., quiet=T) { #TAGS search text file
	i <- .ffgrep(filenames, ...)
	Result <- filenames %|% `#` %|% nulls
	for (k in filenames %|% seq_along) {
		if (!quiet) flush.cat(
            "ffgrep: searching: ", filenames[[k]], HNL,
            sep="", file=stderr())
		File <- file(filenames[[k]], open="rt")
		Result[[k]] <- lines_extract.cn(File, i[[k]])
		close(File) }
	Filter(`#`, `names<-`(Result, filenames)) }

    Doc$ffgrep <- '
        ffgrep searches multiple files using grepl. The files to
        be searched are specified the character vector of
        filenames, arg 1. The `...` are passed to grepl. The
        return is a list, one element per file **** in which a
        match was found **** (i.e., there will be no return-
        element corresponding to a file in whihc no matches were
        found). Each element of the return is a two-column
        character matrix, where column 1 is the line number
        (converted to character) on which a match was found and
        column 2 contains the text found on that line, ****
        WITHOUT **** the end-of-line marker. The name of each
        element is a copy of the corresponding filename.'

.ffgrep.brief <- ffgrep %O% (vapply2 %|% argswap %<=% `#`)

ffgrep.brief <- function(filenames, ...) #TAGS search text file
		Filter(id, `names<-`(
			.ffgrep.brief(filenames, ...),
			filenames))

    Doc$ffgrep.brief <- '
        ffgrep.brief is similar to ffgrep, except that, for each
        file, only the number of matches (when there are one or
        more matches) is returned, rather than the matches
        themselves.'
