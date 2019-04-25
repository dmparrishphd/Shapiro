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


    Doc$demote <- '
        demote returns a logical vector corresponding to the
        logical, numeric, or complex input vector. Zero-elements
        of the argument correspond to FALSE elements of the
        return.  Non-zero elements of the arguemnt correspond to
        TRUE elements of the return.'

na2T.b <- function (b)
        b %|% is.na | b

    Doc$na2T.b <- '
        na2T.b returns a modified copy of the logical vector
        argument. NA values of the argument map to TRUE values
        in the return.'

na2F.b <- function (b)
        b %|% is.na %|% `!` & b

    Doc$na2F.b <- '
        na2F.b returns a modified copy of the logical vector
        argument. NA values of the argument map to FALSE values
        in the return.'


