# Shapiro: A Handsome Helper for R
# Copyright (C) 2018 D. Michael Parrish
# 
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public
# License along with this program.  If not, see
# <https://www.gnu.org/licenses/>.
#
# END OF COPYRIGHT NOTICE
#
#
# Binary Files (Read)
# HISTORY 2018-02-28: moved from readBin-ext.R

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

