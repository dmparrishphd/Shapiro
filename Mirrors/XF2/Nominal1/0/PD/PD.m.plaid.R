.m.plaid <- function(preplaid) {
	Return <- array(F, preplaid[,,1] %|% dim)
	Return[    preplaid[,,1] & preplaid[,,2] ] <-  T
	Return[xor(preplaid[,,1],  preplaid[,,2])] <- NA
	Return }
