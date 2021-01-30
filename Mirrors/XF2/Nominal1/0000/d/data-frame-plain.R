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

.AS.PLAIN.DATA.FRAME.OPTIONS.COMMON <- list(optional=T, stringsAsFactors=F)

as.plain.data.frame.h <-  as.data.frame %^% c(
    .AS.PLAIN.DATA.FRAME.OPTIONS.COMMON, list(
    col.names=NULL, fix.empty.names=F) )

as.plain.data.frame.l <- as.data.frame %^% c(
    .AS.PLAIN.DATA.FRAME.OPTIONS.COMMON, list(
    col.names=NULL, fix.empty.names=F) )

as.plain.data.frame.m <- as.data.frame %^% c(
    .AS.PLAIN.DATA.FRAME.OPTIONS.COMMON, list(
    make.names=F) )

as.plain.data.frame <- function(x) (
    if (x %|% is.matrix) {
        as.plain.data.frame.m
    } else if (x %|% is.data.frame) {
        anon
    } else if (x %|% is.list) {
        as.plain.data.frame.l
    } else if (x %|% is.character) {
        as.plain.data.frame.h
    } else if (x %|% is.vector) {
        matrix %O% as.plain.data.frame
    } else nop )(x)


'""
#TESTS
as.plain.data.frame("A")
as.plain.data.frame(data.frame(1))
as.plain.data.frame(matrix(1))
as.plain.data.frame(list(A=1:9, B=21:29))
as.plain.data.frame(1)
""'

