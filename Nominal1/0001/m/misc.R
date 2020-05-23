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


dim_complete <- function (length_, dims.lower) #TAGS array
    length_ %\% prod(dims.lower) %,% dims.lower %|% rotate

    Doc$dim_complete <- '
        dim_complete returns the dimensions of an array of
        specified length (number of elements) and specified
        lower dimensions. The high dimension of the return is
        such that the product of dimensions is large enough to
        store the number of elements specified.'

full.range <- unlist %O% rmna %O% range

    Doc$full.range <-
            "full.range returns the range of all the values in
            all of the elements of a list. The list elements are
            atomic vectors. Mixed-type elements will be promoted
            per usual."


