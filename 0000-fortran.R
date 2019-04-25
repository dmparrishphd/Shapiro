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



which.quotes <- (`==` %<=% HQUOTES) %O% which
which.quote  <- (`==` %<=% HQUOTE ) %O% which
which1.quote.or.quotes <- function (character_)
        (   character_ == HQUOTE |
            character_ == HQUOTES  ) %|% which1


bar <- function (which_) {
    which_ %|% `#` -> `#which`
    1L -> i
    while () {

        if (diff(which_[i + ol]) == 1) {
            i <- i + 2L # SKIP THE DOUBLE
        } else {
            # IT'S NOT A DOUBLE, SO IT MUST BE A MATCH
            return (i) }

        if (i == `#which`) # AT THE LAST ONE: MUST BE A MATCH
                return (i)

        if (`#which` < i) # NO MORE CANDIDATES
                return (iNA)
    }
}

foo <- function (character_, target) {
    i <- which1(character_ == target)
    j <- 1L
    while (j < i %|% `#`) {
        if ()
    
    }


    for (j in i[])
    if ()
}

which1.closing.quotes <- function (character_) {
    (character_ == HQUOTES) %|% which -> qq
    if (qq %|% `#` <= 1) qq %|% return
    for (i in seq_along(qq)[-1]) {
        if (1 < qq[pred(i):i] %|% diff) qq[i %|% pred] %|% return
    }
    vapply_(
        qq %|% seq_along,
        function(i) )
    



     }

foo <- function (h) {
    #REFERENCE Fortran draft standard J3/97-007R2
    h %|% characters -> ch
    if (
        HQUOTE  %in% h ||
        HQUOTES %in% h    ) {
            warning("does not yet support quotes") }
    if ("/" %in% ch) ch <- ch[0:((ch == "/") %|% which1 %|% pred)]
    ch
}


