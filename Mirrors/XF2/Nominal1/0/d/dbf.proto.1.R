DBF.PROTO <- (function(){
    X <- source(local=T, file="//ad.sfwmd.gov/dfsroot/data/worm/Files/COMP/SOFTWARE/ShapiroXF2/Nominal1/0/d/dbf.proto.R")[[1]]
    X$.TRANSLATION$D=function(r) {
			WORD <- intToUtf8(as.integer(r))
			start <- c(1, 5, 7)
			stop  <- c(4, 6, 8)
			Text <- vapply(1:3, function(i) substr(WORD, start[i], stop[i]), "")
			Arg <- paste0(Text[1], "-", Text[2], "-", Text[3])
			tryCatch(as.Date(Arg), error=function(e) as.Date(NA))
		}
    X })()
