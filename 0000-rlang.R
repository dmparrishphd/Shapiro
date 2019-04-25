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


# tf.words words distinct.name
tf.Rwords = function(ch) { "tells whether help is available for each of the
functions named in the character vector ch."; nchar(lapply(ch, FUN=help)) > 27 }
rwords = function(ch=c(LETTERS, ".", letters)) ch[tf.words(ch)]
distinct.name = function (LS, n=8) {
    "Intended to be called with actual argument ls(all.names=T).

    It would be possible to put this, tf.words, and words together in one
    file as an anonymous function, to append to the file the characters needed
    for a function call, and to then source that file. Then we can get a
    distinct name into R without introducing other arbitrary names.
    "

    while (T) { nom <- paste(letters[1+as.integer(runif(n=n, max=26))], collapse="")
        if (!(nom %in% LS) && !(nom %in% words(nom))) break }
    nom
}

'
function tf.Rwords : R language names : #OLDNAME tf.words
function rwords : R language names : #OLDNAME words



'
