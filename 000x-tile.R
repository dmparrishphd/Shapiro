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


tile <- function (x, origin= na(x %|% ndim, "integer"))
		list(x %|% as.array, extend_len(
			origin, length.out=x %|% ndim))

is.tile <- function(tile)
		tile[[1]] %|% is.array &
		tile[[2]] %|% is.integer &
		tile[[2]] %|% `#` == tile[[1]] %|% ndim

bb.tile <- function(tile)
		rbind(
			tile[[2]],
			tile[[2]] + tile[[1]] %|% dim - 1L)

bb.tiles <- function(tiles) lapply(tiles, bb.tile) %|% bb.bb

template.merge.tiles <- function(tiles) {
	tile(array(na(mode=do.call(max_typeof, lapply(
					lapply(tiles, first),
					first))),
		tiles %|% bb.tiles %|% diff_inclusive),
		tiles %|% bb.tiles %|% firstr) }

merge.tiles <- function(tiles) {
	y <- template.merge.tiles %-|% tiles
	for (tile in tiles) {
		k <- tile[[1]] %|% `#`
		arrayInd. <- arrayInd %|% argswap %<=% dim(tile[[1]] %O% (`+` %<=% (tile[[2]] - y[[2]]))
		while (k) {
			y[[1]][arrayInd.(k)] <- tile[[1]][k]
			k[1] <- k[1] - 1L }	}
	y }

    Doc$tile <- '
        A tile is a list whose first element is an array and
        whose second element is an integer-valued vector having
        a length equal to the number of dimensions of the
        array.
        
        The functions bb.tile, bb.tiles, template.merge.tiles,
        and merge.tiles **** ASSUME **** that the second element
        contains the origin of a raster whose cells have values
        as specified in the array.'

    Doc$is.tile <- '
        is.tile returns a single logical value indicating
        whether the argument is a tile. see also Doc$tile'

    Doc$bb.tile <- '
        bb.tile returs the bounding box of the tile. Row 1 of
        the return is equal to the origin of the tile, and the
        diff_inclusive of the return should be equal to the
        dimensions of the tile array.'

    Doc$bb.tiles <- '
        bb.tiles returns the bounding box of the combination of
        all the tiles in the argument (a list of tiles).'

    Doc$template.merge.tiles <- '
        template.merge.tiles returns a template for merging
        multiple tiles into one. The returns is a tile. The type
        of the array of the tile is the "highest" type among the
        types of the tiles found in the list of tiles argument.'

    Doc$merge.tiles <- '
        merge.tiles returns a tile that contains the values
        (possibly promoted) of the tiles found in the list of
        tiles argument. The values found in tiles that occur
        further down the list **** OVERWRITE **** those found
        earlier in the list, if they have the same positions.'
