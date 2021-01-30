iRaggedToRecycled

ARGUMENTS

        lengths

                The lengths of the columns of a hypothetical
                ragged matrix with columns of different lengths.

DESCRIPTION

        Conceptually, the argument is transformed in to parallel
        `seq`-uences, with recycling. These sequences are
        `cbind`-ed.

        Originally intened as a step on the way to making petty
        tables (lines of nicely formatted text representing the
        rows of a table).

EXAMPLES

        X <- list(
	        COLUMNS=lapply(head(iris)[-5], formatf),
        	SEPS=list("|"))
        InnerIndex <- iRaggedToRecycled(vapply2(X, length))
        InnerIndex

        #      [,1] [,2]
        # [1,]    1    1
        # [2,]    2    1
        # [3,]    3    1
        # [4,]    4    1
        
        mi <- cbind(seq(ncol(InnerIndex)), as.vector(t(InnerIndex)))
        Intermediate <- do.call(cbind, lapply(
            seq(nrow(mi)), 
            function(k) X[[mi[k, 1]]][[mi[k, 2]]])[-nrow(mi)])
        vapply(
            as.data.frame(t(Intermediate), stringsAsFactors=F),
            FUN=function(x) paste0(x, collapse=""),
            FUN.VALUE="",
            USE.NAMES=F)


