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
regex.starts.with.character.among.h <- function (h)
        "^[" %//% `%//%`(h) %//% "]"

regex.starts.with.lower <- function () #DEPRECATED use value REGEX.STARTS.WITH.LOWER
        regex.starts.with.character.among.h(letters)

regex.starts.with.upper <- function () #DEPRECATED use value REGEX.STARTS.WITH.UPPER
        regex.starts.with.character.among.h(LETTERS)

REGEX.STARTS.WITH.LOWER <- regex.starts.with.character.among.h(letters)

REGEX.STARTS.WITH.UPPER <- regex.starts.with.character.among.h(LETTERS)

REGEX.ANY.NONDIGIT <- "[^0123456789]"
