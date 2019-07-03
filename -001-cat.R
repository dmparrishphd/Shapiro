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

verbatim <- function(..., file="", sep="", fill=FALSE, labels=NULL, append=FALSE)
        cat(..., file=file, sep="", append=append)

    Doc$verbatim <- '
        verbatim is intended for processing character arguments.
    
        The ..., file, and append arguments are passsed to cat.

        The sep, fill, and labels arguments are **** IGNORED; ****
        **** sep="" is passed to cat. ****'
