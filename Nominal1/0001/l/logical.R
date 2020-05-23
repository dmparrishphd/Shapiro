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



i.b <- function(b)
		sum(iPOW2 %[along% b * b)

	Doc$i.b <- '
        i.b returns an integer value, converted from a logical
        vector representing an unsigned binary value, provided
        that the argument has **** FEWER THAN **** 32 elements.

		> i.b(c(T, T, T, T))

		# [1] 15

		> i.b(c(F, F, F, F, T, T, T, T))

		# [1] 240
		'


b.tfz <- characters %O% as.logical

    Doc$b.tfz <- '
        b.tfz interprets the individual characters of the
        character string argument as logical values and returns
        the resultant vector. The length of the return should
        equal the nchar of the argument.'

