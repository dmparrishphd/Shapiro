extract.parallel <- function(.list, mi)
		lapply(
			mi %|% colNos,
			function(k) k %th% .list %=>% `[` %:|% mi[,k])
