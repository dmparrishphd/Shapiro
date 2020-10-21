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

crlf2lf <- gsub %<=% "\r\n" %<=% "\n"

cr2lf <- gsub %<=% "\r" %<=% "\n"

    Doc$cr2lf <- '
        cr2lf returns a modified copy of the character vector 
        argument. The return has line-feed where the argument
        has carriage-return.'

    Doc$crlf2lf <- '
        crlf2lf returns a modified copy of the character vector 
        argument. The return has line-feed where the argument
        has carriage-return followed by line-feed.'

txt2unix <- crlf2lf %O% cr2lf

    Doc$txt2unix <- '
        txt2unix returns a modified copy of the character vector
        argument: in an intermediate step, all sequences of
        line-feed--carriage-return are replaced with line-feed,
        then all carriage-returns in the intermediate results
        are replaced with line-feed.'

