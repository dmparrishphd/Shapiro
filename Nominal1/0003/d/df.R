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

# reminder: subset data frame by row name using
# df1[c("ROW.NAME1", "ROW.NAME2"),]

'
cat.df #DEPRECATED use write.table
'
cat.df <- function(df1, file=HNULL, sep=SPACE, append=F, row_sep=HEOL, ...)
        cat.m(df1  % %  m.df  % %  t, file=file, sep=sep, append=append, ...)

df.mh <- function(mh, FUNs=list(identity))
        data.frame(lapply1to1(mh  % %  cols.m, FUNs), stringsAsFactors=F)  % %  rename.all

df.h <- function(h, FUNs=list(identity))
        df.mh(h  % %  lines.h1  % %  m.lines, FUNs)

m.df <- function (df) matrix(df  % %  l.flatten.l  % %  unlist, ncol=n.col.m(df))

`df.rows.df,v` <- function (df.in, v) #DEPRECATED use df.rows.df
{   'Returns, as a data frame, rows of the data frame df.in.
    Rows are specified by the logival or integer vector v.'
    df.in[v,]}

rows <- function (X, i=T)
        (`[` %^% list(X, i, X %|% colNos, drop=F))() #FUTURE: allow X to have any number of dimensions

    Doc$rows <- '
        rows returns a matrix or data frame containing only the
        rows specified.'

rows.df <- rows #DEPRECATED use rows

rows.m <- rows #DEPRECATED use rows

nnn.roco.df <- function (df.in, nnn=c(1,2))
{   dim(df.in)[nnn]}

n.row.df <- function (df.in)
{   nnn.roco.df(df.in, 1)}
n.col.df <- function (df.in)
{   nnn.roco.df(df.in, 2)}

.uncross.df <- function (other, df1, rcol)
        cbind(  KEY=rep(names(df1)[other], df1[,1]  % %  `#`),
                reproduce(df1, , 1:rcol),
                VALUE=df1[,other],
                stringsAsFactors=F)

uncross.df <- function (df1, rcol=1L)
    (rbind %^% lapply((rcol  % %  succ):(df1  % %  n.col.m),
            .uncross.df, df1, rcol)) ()


summary_df <- function (df1, FUN, FUN.VALUE, name.split,
                        name.FUN.arg, ...)
        vapply(lapply(split(df1, df1[[name.split]]), `[[`,
                      name.FUN.arg), FUN, FUN.VALUE, ...)

'
function summary_df
'
Doc$summary_df <- "
        Applies vapply (with FUN, FUN.VALUE, and ...) to the
        columns (specified by the character string name.FUN.arg)
        of a split data frame. The split is according to the
        column specified by the character string name.split)."

'Example Split and Row Selection

s <- split(df1, df1$CATEGORY) # s is a list of data frames

(rbind %^% lapply(s, function (df1) df1[df1$COLUMN.NAME == max(df1$COLUMN.NAME),]))()

'



'

function .uncross.df

        Helper function for df.uncross.df

        Notes on the code:
        
                application of the reproduce function ensures
                that the corresponding cbind argument is a data
                frame

                stringsAsFactors=F avoids conflating KEY values
                should the same integer be mapped to multiple
                character strings.

function uncross.df

        Given a cross tabulation (arg 1, a data frame) and the
        right-most index column number (arg 2), returns data
        frame where the index columns are reproduced for each
        data column, and all data point on the interior of the
        cross tabulation are in the same column. Additionally,
        column headers of the cross tabulation are reproduced in
        the left-most column of the return.

        Notes on the Code:

                Don-t need stringsAsFactors=F, since underlying
                helper funciton sets this for the component data
                frames.
        
        Examples:

        > cross <- data.frame(ID=1:2, Name=c("Gabriel", "Michael"),
        + Nickname=c("Gabe", "Mike"))

        > cross
          ID    Name Nickname
        1  1 Gabriel     Gabe
        2  2 Michael     Mike

        > uncross.df(cross)
               KEY ID   VALUE
        1     Name  1 Gabriel
        2     Name  2 Michael
        3 Nickname  1    Gabe
        4 Nickname  2    Mike

function df.mh : data frame character matrix

        Returns a data frame of the character matrix argument.
        **** NOTE **** Unlike data.frame, the stringsAsFactors
        argument is set to F.

function df.h : data frame character vector text

        Similar to read.table, but with a character string
        argument, analogous to a well-laid out text file where
        each line corresponds to one row of a table, values are
        separated by whitespace, and there are a consistent
        number of values per line.

        Columns of text are converted to data vectors using the
        FUNs. Example:
                > df.h("A 1.1 1\nB 2.2 2", c(identity, as.double, as.integer))
                1 A 1.1 1
                2 B 2.2 2
'

