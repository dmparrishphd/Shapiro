.mb.preplaid <- function(nrow, ncol) {
	XX <- F %,% T
	Mats <- array(F, c(nrow, ncol, 2L))
	Mats[,,1][  rep(XX, length.out=nrow), ] <- T
	Mats[,,2][, rep(XX, length.out=ncol)  ] <- T
	Mats }
