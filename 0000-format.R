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
# R Script
#
# Specializatoins of format and formatC


h.i.format.d <- function(d) formatC(d, digits=0, format='f') # h.i.format.d(2.**53-1.) == "9007199254740991"
h.i.format.i <- function(i) formatC(i, format='d')

h.pad.i <- function (i, width=2) formatC(i, width=width, flag="0")


