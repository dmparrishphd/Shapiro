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

scan.line.length <- function (...)
        scan(nlines=1, ...) %|% `#`

    Doc$scan.line.length <- '
        scan.line.length is a wrapper for scan. It scans one
        line of the file specified, and returns its length
        (number of items scan-ned.)'

scan.entire.as.matrix. <- function (file="", ...)
        matrix(
            data=scan(file=file, ...),
            nrow=scan.line.length(file=file, ...))

    Doc$scan.entire.as.matrix. <- '
        scan.entire.as.matrix. is basically the same as matrix
        (where ncol, byrow, and dimnames are default values),
        except that the matrix data argument is populated by
        scan-ning from file according to ...

        scan.entire.as.matrix. scan-s the speficied file;
        internally, file and ... are passed to scan. The items
        returned from scan are passed to matrix as data. The
        nrow argument is interpreted from the
        first line of file (the number of items scan-ned) and
        the other dimension is computed internally by matrix.

        The return may be transposed (instead of specifying
        byrow=T). Similarly, dimnames may be added the return.
        '

scan.entire.as.matrix <- #DEPRECATED USE scan.entire.as.matrix.  with t, with_dimnames, etc.
        function (file="", nrow=1, ncol=1, byrow=F, dimnames=NULL, ...) {
    DATA <- scan(file=file, ...)
    N <- scan.line.length(file=file, ...)
    if (byrow) { NCOL <- N;   NROW <- DATA %|% `#` %\% NCOL
    } else     { NROW <- N;   NCOL <- DATA %|% `#` %\% NROW }
    matrix(DATA, nrow=NROW, ncol=NCOL, byrow=byrow, dimnames=dimnames) }

    Doc$scan.entire.as.matrix <- '
        scan.entire.as.matrix is basically the same as matrix,
        except that the matrix data argument is populated by
        scan-ning from file according to ...

        scan.entire.as.matrix scan-s the speficied file;
        internally, file and ... are passed to scan. The items
        returned from scan are passed to matrix as data. One
        dimension (either nrow or ncol) is interpreted from the
        first line of file (the number of items scan-ned) and
        the other dimension is computed by dividing that
        dimension into the number of data items, using ****
        CEILING DIVISION **** (i.e., rounding nonzero fractional
        parts up to the nearest integer).

        nrow and ncol are dummy arguments. FUTURE: allow
        specification of nrow and ncol in a manner consistent
        with matrix.'

scan.entire.as.integer <- function (file) scan(file, what=integer())

scan.entire.as.mask <- function (file) {
    dat <- scan(file, what=integer()) % % as.logical
    dat[!dat] <- NA
    dat }

rewind <- function (con) seek(con, where=0)

cn.rewind <- function (con) { rewind(con);   con }
rewindc <- cn.rewind #DEPRECATED use cn.rewind

        Doc$rewindc <- crunch.h("
            Rewinds the connection specified by the primary
            argument. Returns the connection.")

cn.seek <- function (con, ...) {
    seek(con, ...);   con }

nonadvancing <- function (con, FUN, ...) {
    pos <- seek(con)
    ret <- FUN(con, ...)
    seek(con, pos)
    ret }

readline.cn <- readLines %^% list(
    n=1, ok=T, warn=F, encoding="UTF-8", skipNul=T)

line.count.cn <- function(con) {
    n <- 0
    repeat {
        line <- con %|% readline.cn
        if (`#` %|% un %-|% line) break
        n <- n %|% succ }
    n }

    Doc$line.count.n <- '
        line.count.n returns the number of lines from the
        current file position of the connection argument through
        to the end.'

scan.lines <- curry(scan, what=character(), sep="\n",
        na.strings=NULL, blank.lines.skip=F)

    Doc$scan.lines <- '
        scan.lines is a curried version of scan with minimal
        processing: it merely separates the character vectors by
        newline.'

scan.lines.head = function (file="", n=1, ...)
        scan.lines(file=file, n=n, ...)

scan.words = function()
{   'STATUS: IN PROGRESS, UNTESTED'
    ulinessplit = unlist(strsplit(scan.lines(fi), ' '))
    nchars = unlist(lapply(ulinesplit, nchar))
    words = ulinessplit[nchars > 0] }

scan.con <- function(file, ...) {
    print(match.call())
    print(as.list(match.call()))
    pos <- seek(file, NA)
    items <- scan(file, ...)
    list(con=file, position.initial=pos, position.final=seek(file, NA), items=items)
}





'
lc.scan = latent.call(scan)

        REMOVED. May move to other file at a later time.

function scan.lines : file read text character

        By default, returns the entire contents of the file,
        specified by file (arg 1), as a character vector, where
        each line of the file is converted to an element of the
        character vector.  end-of-line markers are discarded.

        HISTORY: OLD DEFINITION (2018-03-05):
        scan.lines = function (file="", what=character(), sep="\n", ...)
                scan(file=file, what=what, sep=sep, na.strings=NULL, blank.lines.skip=F, ...)

function scan.lines.head

        Similar to scan.lines, but returns no more than the
        first n (arg 2) lines of data; by default, only the
        first line.
'

NULL

