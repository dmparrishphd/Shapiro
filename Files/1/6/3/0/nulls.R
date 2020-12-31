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



NULLif <- function (condition, alternative)
        if (condition) NULL else alternative

NULLifnot <- function (condition, alternative)
        NULLif(! condition, alternative)

    Doc$NULLif <- '
        NULLif returns NULL if the condition (arg 1) is TRUE;
        otherwise returns the alternative (arg 2).'

    Doc$NULLifnot <- '
        NULLifnot returns NULL if the condition (arg 1) is FALSE;
        otherwise returns the alternative (arg 2).'

