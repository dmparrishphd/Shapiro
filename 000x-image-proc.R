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


write.multichannel.as.indexed <- function( #TAGS image tiff png raster indexed color
        a, outdir, filenames=list(
            DIM="dim.dat",
            LEVELS="levels.dat",
            FACTORS="factors.bin") ) {

	b <- round(
		a[,,1] * 255 +
		a[,,2] * 255 * 256 +
		a[,,3] * 255 * 65536)

	cat(dim(b), file=paste0(outdir, filenames$DIM), sep="\n")

	dim(b) <- NULL
	uniq <- sort(unique(b))
	cat(uniq, file=paste0(outdir, filenames$LEVELS), sep="\n")

	writeBin(
		object=as.raw(match(b, uniq)),
		con=paste0(outdir, filenames$FACTORS),
		size=1L)

    '#TODO FACTOR OUT BITWISE WRITE
	for (k in seq_along(uniq))
		writeBin(
			object=packBits(
				c(b == uniq[k], rep(F, 8 - length(b) %% 8)),
				"raw"),
			con=paste0(outdir, k, ".dat"),
			size=1L)
    '
    NULL }

	Doc$frame.export <- '
		multichannel.as.indexed writes a binary file and buddy
        files representing the image data of the primary
        argument.

        The primary argument is a 3D image array with one
        channel per page, and channel info stored as doubles in
        increments of 1/255. (Internally, these are multiplied
        by 255 to yield integer-valued doubles).

        The secondary argument specifies a directory (include
        trailing path separator) where the files will be
        written.

        **** ASSUMPTION **** the image has only a few "colors"
        (unique premutations of channel-values). Color indices
        beyond 255 will be represented in the output as
        zero-bytes.
        
		Three files are written: dimensions, levels, and factors.
		There is one factor per pixel: these map to the levels.
		levels are a linear combination of the "color" componenets.

		There must be exactly **** THREE **** color components.

		FUTURE: handle up to 6(?) color components.'
