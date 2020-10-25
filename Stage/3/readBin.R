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



# HISTORY 2018-02-28: moved from readBin-ext.R



readBinAtX <- function(con, where, ...) {
    seek(con, where)
    readBin(con, ...) }

readBinArray <- function ( #TAGS array extract
    what, size, dim, con, where, ...)
        dimension(
            readBinAtX(
                con,
                where %|% pred %,% size %,% dim %|% prod,
                what=what,
                n=dim %|% prod,
                size=size, ...),
            dim)

    Doc$readBinArray <- '
        readBinArray treats a binary file like a stack of
        arrays. The what, size, and con arguments are consistent
        with readBin. The dim argument is the dimensions of one
        of the arrays of the stack. Unlike the where argument of
        readBin, where argument of readBinArray tells which
        array to read (indexed from 1).
        
        The return is an array consistent with what, size, and
        dim, provided that all of the arguments are consistent
        (including the avaiability of data at the corresponding
        file position of the connection con).

        The ... is passed to readBin (may contain signed and /
        or endian arguments; inclusion of the n argument of
        readBin will likely cause error.)
        
        The order of arguments is designed for currying.'


l.readBin.formals.fmt <- function () { l <- lformals(readBin);   l$con <- NULL;   l }

readBin2 <- function (con, fmt) # the "2" suffix indicates a function of 2 parameters
        readBin(con, fmt$what, n=fmt$n, size=fmt$size, signed=fmt$signed, endian=fmt$endian)

readBinAt <- function(con, fmt, where=0) { seek(con, where);   readBin2(con, fmt) }

readBin.where <- function (readBin.args, seek.where) {
        print(seek.where)
        caterr("readBin.where: seek.where:", seek.where, heol)
        vapply(
            seek.where,
            function(i) {
                seek(readBin.args[[1L]], i)
                (readBin %^% readBin.args)() },
            FUN.VALUE=vector(
                mode=typeof(readBin.args[[2L]]),
                length=readBin.args[["n"]]))
}

        Doc$readBin.where <- '
                Reads binary data as specified by the parameter
                list (arg 1) at each of the file positions given
                (arg 2). Example:
                readBin.where(list(file("filename",
                                        open="rb"), integer(), ))'

'
function l.readBin.formals.fmt

        Returns the format-related formals of readBin. Can be
        used in an application of ReadBin2.

function readBin2

        A 2-parameter version of readBin. The second argument is
        a list containing all of the format parameters, i.e.:
        what, n, size, signed, and endian.

function readBinAt

        Reads the binary file specified by con (arg 1), in the
        format specified by fmt (arg 2, compatible with the
        return of l.readBin.formals.fmt), at location where (arg 3).
'

NULL

