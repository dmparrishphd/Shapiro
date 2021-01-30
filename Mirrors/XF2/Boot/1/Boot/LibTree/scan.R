function(file) {
    	x <- scan(
        file,
	what="",
	comment.char="#",
	blank.lines.skip=F)
    list(
        PATH=x[1],
        PKGS=Filter(function(y) !!nchar(y), x[-1]) )
}
