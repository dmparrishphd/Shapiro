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

# PDF

pdf.letter.landscape <- pdf %^% list(width=11, height=8.5)
pdf.letter.portrait <- pdf %^% list(width=8.5, height=11)
pdf.letter.landscape.print <- pdf %^% list(width=9, height=6.5)

    Doc$pdf.letter.landscape <- '
        pdf.letter.landscape is the same as pdf, 
        but with width and height specified to 11 x
        8.5.'
'
function pdf.letter
function pdf.letter.print

        Same as pdf, but with width and height specified to 11 x
        8.5 for pdf.letter and 9 x 6.5 for pdf.letter.print
        (designed for printing centered on a letter-page, for
        1-inch margins.) 
'
