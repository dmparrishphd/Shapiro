linegraph
=========

Plot multiple linegraphs

**The intended purpose** of linegraph is
to produce a new graph on the current device.
Other uses are possible.

PARAMETERS
----------

`X` is a list of vectors.
Each vector contains the x-values which may be plotted.
    
`Y` is similar to `X`, but for y-values.
    
`G` is a nx2 array-index of lines to plot.
Column 1 indexes `X` and column 2 indexes `Y`.

`par.mar` is the value for setting the mar parameter using the
    par function. See also documentation for `PAR.MAR.C`

`par.new` is used to set the `new` parameter using `par` for the
    initial plot only. All successive plots have `new=T`---which
    is the essential feature of `linegraph`.

`uncol` is the _default_ color for every graph element
    _except_ the data to be plotted.
    
`col` is a vector of colors for each of the lines to be
    plotted. These colors are recycled if their number is fewer
    than the length of `G`.
    
All remaining parameters, including `...`, are passed to `plot`.

The `col` and `type` parameters vary with each line plotted,
    while the other parameters are fixed for the entire graph.

`xaxs` and `yaxs`.   The default values for `xaxs` and `yaxs` are
    different from those of the plot function.

## Details 

`...` is passed to the plot function, which will
    be executed with the same set of `...` parameters for each
    line graphed.

**Axis labels are overprinted** for each line graph beyond the
    first. This is understood as a feature as opposed to a bug:
    if there are conflicting axis labels in the final plot, it
    is a sign that something has gone wrong.
    
**Margins** are set according to `par.mai` or `par.mar`. If both are
    specified, `par.mar` takes precedence. If neither are
    specified, `mar` is set to `PAR.MAR.C`

EXAMPLES
    
**Default plot**. Call this function as linegraph() to see an
    example plot.
    
**Triangle Waves**. This call plots a pair of triangle waves,
    where the y-values are reused:

    linegraph(
        X=list(      seq(8),   # first set of x-values
                .5 + seq(8)),  # second set of x-values
        Y=list(rep(c(1,2),4)), # y-values for waves
        G=list( c(1,1),        # use X[[1]] and Y[[1]]
                c(2,1)),       # use X[[2]] and Y[[1]]
        col=c("red", "blue"))  # optional color specs.



**Kindly acknowledge** the author and SFWMD if you use this for
    published work.

**Author**. Developed by D. Michael Parrish at the South Florida Water
    Management District.
