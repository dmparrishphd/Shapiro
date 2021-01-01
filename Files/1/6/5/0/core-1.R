# INTENDED TO REPLACE ./core.R
core <- function(.array, isample, direction) # extract along direction
        .array[mi.core(.array %|% dim, isample, direction)]

    Doc$core <- '
        core returns the vector of elements along a particular
        direction, through a specified sample cell in an array.
        
        > core(matrix(1:6, 2), c(2,1), 2)

        [1] 2 4 6
        '
