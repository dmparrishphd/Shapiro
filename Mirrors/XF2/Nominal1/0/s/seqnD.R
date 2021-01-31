seqnD <- function(dims, SEQ=integer()) {
	if (SEQ  % %  length) {
		if (dims  % %  length) seqnD(   # recurse with lower dimensionality
			dims[-1],
			cbind(
				rep_m(SEQ, dims[1]),
				rep(dims[1] % % seq,   SEQ  % %  nrow)  % %  sort)   # next iteration of SEQ
		) else SEQ
	} else   # SEQ has not yet begun to be built up.
		if (dims  % %  length) {
			seqnD(dims[-1],   dims[1]  % %  seq  % %  matrix)
		} else integer() }
