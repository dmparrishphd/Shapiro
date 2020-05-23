# Shapiro: A Handsome Helper for R
# Copyright (C) 2019 D. Michael Parrish
# COPYRIGHT NOTICE CONTINUES AT ./COPYRIGHT2.txt
# 
# BRIEF TABLE OF CONTENTS
#
# ./COPYRIGHT1.R                     Copyright Notice (PART 1/2)
# ./COPYRIGHT2.R                     Copyright Notice (PART 2/2)
# ./LICESE.txt                                 License (Primary)
# ./LICENSE-Stack_Overflow.htm                 License for curry
# *.R                                   (Body / Primary Content)



.PACKBITS3.TABLE <- "T T F F T F F T" %|% b.h %|% matrix2r

.PACKBITS3.TABLE.COLNAMES <- "NA. FALSE. TRUE. PAD." %|% words

.packBits3 <- function(b) {
    #REMINDER: as.integer(c(F, T)) == c(0L, 1L)
    i <- b %|% as.integer
    i[i %|% is.na] <- -L
    cbind(
        .PACKBITS3.TABLE[, 2L + i, drop=F],
        rep(
            .PACKBITS3.TABLE[,4],
            b %|% `#` %=>% modinv %:|% 4L) %|%
    matrix2r ) }

packBits3 <- function(b)
    b %|% .packBits3 %|% as.vector %|% packBits

unpackBits3 <- function(.raw) {
    i <- L + capply(.raw %|% b.r %|% matrix2r, i.b)
    #REMINDER: i == 3 MAPS TO THE PADDING NA-VALUE, WHICH, IF
    #PRESENT, SHOULD OCCUR ONLY AT THE END OF THE VECTOR.
    c(F, T, NA, NA)[i][i != 3] }

    Doc$packBits3 <- '
        packBits3 and unpackBits3 present a scheme for packing
        and unpacking logical vectors into and out of raw
        vectors, which, unlike base::packBits, handles all three
        logical values, including NA (hence the "3" suffix).
        
        DETAILS

        Values are mapped to two-bit binary values as follows
        (little endian):

        LOGICAL VALUE   MAPPED VALUE       BITS AS INTEGER*
        --------------- --------------- ------- ----------
        (PADDING)       FALSE   TRUE         01         -2
        NA              TRUE    TRUE         11         -1
        FALSE           FALSE   FALSE        00          0
        TRUE            TRUE    FALSE        10          1

        Under this scheme, the high-order bit is viewed as an
        "NA-flag," where a "1" bit indicates that the 2-bit
        value is an NA value, while a "0" bit indicates that the
        two-bit value is a true binary logical value.  There are
        two true binary logical values, corresponding to "0" and
        "1." similarly, NA is distinguised from padding by the
        low-order bit.

        EXAMPLES

        packBits3(c(F, F))
        packBits3(c(T, F))
        #packBits3(c(PADDING, F)) #PADDING DOES NOT EXIST AMONG logical VALUES.
        packBits3(c(NA, F))
        packBits3(c(F, T))
        packBits3(c(T, T))
        #
        packBits3(c(NA, T))
        #
        #
        #
        #
        packBits3(c(F, NA))
        packBits3(c(T, NA))
        #
        packBits3(c(NA, NA))
        '

    Doc$unpackBits3 <- Doc$packBits3
