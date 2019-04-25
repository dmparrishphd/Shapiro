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
uppeR <- function(h){
	upper <- function(h) {
		if (h  ==  "") return (h) else {
		ch <- characters(h)
		overlay(LETTERS[match(ch, letters)], ch)  % %  `%//%` } }
	vapply(h, upper, "") }

uppeR.l <- function () { } #FUTURE

'
function uppeR
'
        Doc$uppeR <- crunch.h("
        Returns a modified copy of the character vector
        argument, where each individual character in letters is
        converted to the corresponding individual character in
        LETTERS This is different from case::str_to_upper, which
        is locale-specific in a different way.")
