# Shapiro: A Handsome Helper for R
# Copyright (C) 2019 D. Michael Parrish
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


cwidth <- function(h) vapply(h, strwidth, 1) % % max #TAGS text column widths

        Doc$cwidth <- crunch.h('
            cwidth returns the width of the column of text
            represented by the character vector argument. The
            return depends on the current device, font, etc.')

cwidths <- function(hh) vapply(hh, cwidth, 1) #TAGS text column widths

        Doc$cwidths <- crunch.h('
            cwidths returns the widths of each column of text
            represented by the argument, a list of character
            vectors. The return depends on the current device,
            font, etc.')

cwidths.as.knapsack.items <- function (widths)
        l.zip.l(
             list(
                 widths  % %  seq_along  % %  as.list,
                 widths  % %  knapsack.form.simple.attributes))

        Doc$cwidths.as.knapsack.items <- crunch.h('
            cwidths.as.knapsack.items receives a vector of
            column widths an returns a corresponding list of
            items compatible with the knapsack family of
            functions. In the return, the items proper are
            indices of the argument and the single item
            attribute is the corresponding element value.')

cpaginate <- function (widths, page.width)
        lapply(
            knapsack.partition(
                list(
                     fnencumbrance=knapsack.fnenc.direct,
                     capacity=page.width,
                     items=list()),
                cwidths.as.knapsack.items(widths))  % %  unrest  % %  unrest,
            unlist)

        Doc$cpaginate <- crunch.h('
            Given a vector of text column widths a singleton
            vector of page.width, returns a list containing an
            element for each page on which the columns may fit
            together.  Pagination stops when a column wider than
            the page is encountered, or when all columns have
            been paginated. An application might check the
            length of the unlist of the return to verify that
            all columns are represented therein.')


