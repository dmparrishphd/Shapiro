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



colors.d <- function(FUN, n, black=T, white=T)
        vapply.unif(n, FUN, FUN.VALUE="", zero.include=black, one.include=white)

    Doc$colors.d <- '
        colors.d

        Originally inteded to be applied via partial funcitons
        wherein the primary argument is baked in.

        The primary argument (FUN) is a function that returns a
        color given a double value in [0, 1]. It is anticipated
        that FUN will return low-luminance colors for lower
        values and higher-luminance colors for higher values.

        The optional arguments black and white tell whether to
        include the least an most intense colors. This can be
        useful when displaying greys on a black or white
        background, for example.

        Returns a vector of n (arg 2) colors which map to evenly
        spaced values in [0, 1] and are ordered.

        For exaples, see greys and greens.'

greys <- colors.d %<=% grey

    Doc$greys <- '
        greys returns a sequence of n (arg 1) grey shades which
        gradually increase in luminous intensity. Black and/or
        white may be excluded by specifying black=F and/or
        white=F.'

greens <- colors.d %<=% (rgb %<=% 0 % % argswap %<=% 0)

    Doc$greens <- '
        Similar to greys, except that the return is a vector of
        shades of green.'

smoke <- colors.d %<=% grey %^% list(black=F, white=F)

SMOKE <- 254 %|% smoke

