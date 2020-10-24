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

# calc: functions related to calculus

trapezoidal <- function(xy) #TAGS numeric integration integral integrate rule
        sum(diff(xy[,1]) * sumr(xy[,2])) / 2

    Doc$trapezoidal <- '
        trapezoidal computes the numerical integral of the
        functions represented by the xy matrix or data frame
        argument. The "x" values are **** ASSUMED **** to be
        found in column 1 and the "y" values are **** ASSUMED
        **** to be found in column 2. The x-values are ****
        ASSUMED **** to be sorted. Subsequent, identical
        x-values are allowed; step functions may be represented
        in this manner.'

mean_trapezoidal <- function(xy) #TAGS function mean
        if (xy %|% is.empty) {
            na(mode=xy %|% typeof)
        } else if (xy %|% nrow == 1) {
            xy[,2]
        } else {
            deltax <- xy[,1] %|% last - xy[,1] %|% first
            if (deltax) {
                xy %|% trapezoidal / deltax
            } else {
                xy[,2] %|% mean }
        }

    Doc$mean_trapezoidal <- '
        mean_trapezoidal computes the mean of the piecewise
        linear function represented by the matrix or data frame
        argument via the trapezoidal function.'

