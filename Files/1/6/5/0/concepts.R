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


.RELATE <- lapply(list(
	replace="subst",
	which="which.max",
    order="l.unstack.a",
	.NULL=""), words)

relate <- function(x) .RELATE[[x]]

    Doc$relate <- '
        relate retuns a printable object, the exact form of
        which is **** TO BE DETERMINED, **** which contains
        words that are related to the concept specified by the
        character string argument.'


