#USAGE: COPY AND MODIFY IN GUI

RAMDISC <- source(paste0(
"//ad.sfwmd.gov/dfsroot/data/worm/Files/COMP/",
"SOFTWARE/ShapiroXF2/Indices/Content/all.the.R"))[[1]]

find. <- function(x) Filter(length, lapply(RAMDISC,
	function(y) grep(x, y, value=T) ) )

find.("^splitr")

