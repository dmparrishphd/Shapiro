df.initialized <- function(nrows, colClasses, stringsAsFactors=F)
		data.frame(
			lapply(colClasses, function(cc) vector(mode=cc, length=nrows)),
			fix.empty.names=F,
			stringsAsFactors=stringsAsFactors)  % %
        rename.all
