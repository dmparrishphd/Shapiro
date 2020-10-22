i.disjoint.windows
==================

Value
-----

a matrix of `width` rows whose columns are
integer indices into the first few disjoint, contiguous intervals
of a hypothetical vector of the specified length,
rounded down to the nearest multiple of `width`.

Designed for use in sampling a raster at regular
intervals (the columns of the return are interpreted as
column or row numbers of the raster to be sampled.)

Example
-------

    i.disjoint.windows(11, 3)
    #      [,1] [,2] [,3]
    # [1,]    1    4    7
    # [2,]    2    5    8
    # [3,]    3    6    9
 
 Keywords
 --------
 
 keywords: windowing, window function, range
