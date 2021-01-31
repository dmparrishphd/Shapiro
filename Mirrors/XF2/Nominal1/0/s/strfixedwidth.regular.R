strfixedwidth.regular <- function(string, nfields, width) #TAGS strsplit text columns delimited
        vapply(
            nfields %|% seq,
            FUN=function(i) substr(string, i %|% pred * width + L, i * width),
            FUN.VALUE="",
            USE.NAMES=F)
