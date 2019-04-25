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
fft.amplitudes <- function(fft.result) {
    ff1 <- fft.result[-1]
    2 * Mod(ff1)[ff1  % %  `#`  %\%  2L  % %  seq] / (fft.result  % %  `#`) }

        Doc$fft.amplitudes <- crunch.h("
            fft.amplitudes returns the amplitudes associated
            with the fft result of a real-valued sequence. The
            n-th element of the return corresponds to the n-th
            frequency, beginning with one-delta-f. Example: y <-
            1.5*cos(2*pi/16*0:63); fft.amplitudes(fft(y))
            returns a 32-element vector with 1.5 in the fourth
            element (the pattern in y repeats 4 times, and the
            amplitude of the sequence is 1.5)")
