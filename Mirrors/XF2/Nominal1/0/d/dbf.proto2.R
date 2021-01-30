DBF.PROTO <- (function() {
    X <- source(local=T, file=paste0(
        "//ad.sfwmd.gov/dfsroot/data/worm/Files/COMP/",
        "SOFTWARE/ShapiroXF2/Nominal1/0/d/dbf.proto.1.R"))[[1]]
    X$fullInit <- function(., description) {
	    . <- .$init(., description)
    	. <- .$readHead(.)
	    . <- .$parseHead(.)
    	. <- .$parsedHeadInterpret(.)
	    . <- .$computeNumberOfFields(.)
    	. <- .$readRawFieldDescriptors(.)
	    . <- .$parseRawFieldDescriptors(.)
    	. <- .$translateRawParsedFieldDescriptors(.)
	    . <- .$compileFieldLengths(.)
    	. <- .$compileFieldTypes(.)
	    . }
    X$extractRecords <- function(., fromto) {
	    . <- .$readRawSequentialRecords(., seq(fromto[1], fromto[2]))
	    . <- .$parseRawRecords(.)
    	. <- .$translateRecords(.)
    	. }
    X })()
