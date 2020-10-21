mi.core
=======

Elements along an array dimension.

Value
-----

an index matrix that specifies a "core" in a given `direction`,
through the element specified by `isample`
of an `array` or `data.frame` of the given dimensions (`.dim`).

Example
-------

    mi.core(dim(iris), c(5, 1), 2)
    #      [,1] [,2]
    # [1,]    5    1
    # [2,]    5    2
    # [3,]    5    3
    # [4,]    5    4
    # [5,]    5    5
    
