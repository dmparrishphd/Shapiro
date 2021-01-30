# PROTO-PACKAGE LOADER
# PART OF SHAPIRO
# COPYRIGHT (C) 2020 D. Michael Parrish
# SEE ACCOMPANYING "COPYRIGHT" FILE FOR DETAILS.
#
# **** WORKING FILE **** NOT YET INTEGRATED ****
#
# RETURNS A STRUCTURE INDICATING R FILES FOUND IN A DIRECTORY
#
# EXAMPLE USAGE: SEE ../boot.R (references sources.R)
#
# FILES ARE RECOGNIZED BY PREFIXES OF EITHER:
#  -    FOUR DIGIT (0-9) CHARACTERS OR
#  -    AN UNDERSCORE FOLLOWED BY THREE DIGIT CHARACTERS (THE
#       UNDERSCORE STANDS IN FOR A NEGATIVE SIGN.)
function(repo, level=Inf) {

    # { ## SAVE
    SAVE.WD <- getwd()

        setwd(repo)

        M <- rbind(
            (function(FILENAME=list.files(pattern="^_[0-9]{3,}-.*R$") )
                    cbind(FILENAME, paste0("-", substr(FILENAME, 2, 4) ) ) )(),
            (function(FILENAME=list.files(pattern="^[0-9]{4,}-.*R$") )
                    cbind(FILENAME, substr(FILENAME, 1, 4)) )() )
        iFILE <- 1L;   iLEVEL <- 2L

    setwd(SAVE.WD) 
    # } ## RESTORE

    M <- M[M[, iLEVEL] <= level, , drop=F]
    as.character(M[order(as.integer(M[, iLEVEL])), iFILE]) }
