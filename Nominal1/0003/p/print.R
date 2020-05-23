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
print_b <- print_x %^% h.TF.b
print_h <- print_x %^% identity
print_i <- print_h
print_d <- print_x %^% (formatC %^% list(format="E", digits=6)) # provisional

.print_l <- function (l)
{
    #l.interlace.l(list(h.labels.h(names(l)), prefix(lapply(l, typeof) % %  unlist, " ")))  % % unlist
    "(" %,%
    l.interlace.l(list(h.labels.h(l  % %  nicenames, pad=""), lapply(l, typeof))) %,% ")"  % %  unlist

    #m=t(matrix(c(h.labels.h(names(l$I)), lapply(l$I, typeof)), ncol=2)); dim(m) <- 6; unlist(m)
}
print_l <- function(l) l  % %  .print_l  % % print_h
