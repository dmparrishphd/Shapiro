line.pp <- function(pp) c(
		+pp %|% identity %|% det2,
		-pp %|% secondr  %|% diff,
		+pp %|% firstr   %|% diff)
    
