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


data_frame <- function(
    ...,
    row.names=NULL,
    check.rows=FALSE,
    check.names=FALSE,
    fix.empty.names=FALSE,
    stringsAsFactors=FALSE)
        data.frame(
            list(...) %|% with_regular.names,
            row.names=row.names,
            check.rows=check.rows,
            check.names=check.names,
            fix.empty.names=fix.empty.names,
            stringsAsFactors=stringsAsFactors) %|% rename.all

    Doc$data_frame <- '
        Same as data.frame EXCEPT that check.names,
        fix.empty.names, and stringsAsFactors default to FALSE.
        Furthermore, there are no (column)names in the return.
        
        HISTORY:
        
        2019-11-08: internally, names are reassigned so as to
        avoid the creation of intermediate long names by data.frame.
        '


