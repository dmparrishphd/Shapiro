    Doc$rectangular.grid <- '
        A rectangular grid is a regular grid where all cells are
        mutually aligned, congruent orthotopes (rectangles,
        right rectangular prisms, etc.).
        https://en.wikipedia.org/wiki/Hyperrectangle'

georeference.simple <- function (ijxy)
        lapply(
            ijxy %|% colNos,
            function(i) fn.rescale.i2d(from=ijxy[,i,1], to=ijxy[,i,2]))

    Doc$georeference.simple <- '
        georeference.simple returns a family of functions (one
        for each direction / dimension) of one variable that
        transform the cell-coordinates of an n-dimensional
        rectangular grid into the cooridnates of another system,
        provided that the rectangular grid is aligned with that
        other system.

        The argument is a 2 x n x 2 array of doubles. Page 1
        holds the cell-coordinates, and Page 2 holds the
        corresponding alternate coordinate system coordinates.
        Colums 1--n hold values for dimensions 1--n. Row 1 holds
        coordinates for the first point along a given dimension,
        and row 2 holds the coordinates for the second point
        along that dimension.

        This function could be used to generate
        conversions for a nautical chart (rasterized Mercator
        projection) of a small area of the Earth-s surface that
        is aligned with lines of latitude and longitude from
        raster cell coordinates to latitude and longitude.

        Example. The following argument might apply to a
        2-degree by 2-degree raster with one arc-minute
        resolution covering New Orleans (with west-to-east,
        north-to-south ordering).

        array(c(

        1, 120,   1, 120,

        -91, -89,  31,  29), rep(2, 3))'

