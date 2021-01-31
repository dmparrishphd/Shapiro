pch.plot <- function(npch=25, bg=grey(.5)) {
    qpch <- ceiling(sqrt(npch)) # next nearest perfect square
    shift <- ceiling(qpch / 2)
    inner.limits <- c(-1, 1) * shift # nominal range of plotted data
    outer.limits <- 1.5 * inner.limits # range of plotting area
    middle.limits <- vapply( # mean of inner- and outer.limits
            1:2, function(i, m) mean(m[,i]), double(1), matrix(
                    c(inner.limits, outer.limits), ncol=2, byrow=T))
    dev.new(width=4, height=4)
    par(mar=rep(1, 4))
    image(matrix(), xlim=outer.limits, ylim=outer.limits,
            xaxt="n", yaxt="n", asp=1, bty="n") # a blank plot
    x <- rep(1:qpch, qpch) - shift - ((1 + qpch) %% 2)/2
            # x-coords for plotting symbols
    y <- rev(sort(x)) # y-coords for plotting symbolic
    offset <- 1/6 # relatively less space with increasing qpch
    CEX <- 2*3/qpch
    points(x - offset, y, pch=1:npch, cex=CEX, bg=bg)
            # draw every valid pch
    text  (x + offset, y, as.character(1:npch), cex=CEX)
            # label each pch with its number
    rect(   middle.limits[1], middle.limits[1],
            middle.limits[2], middle.limits[2]) # an aesthetic box
    text(0, mean(c(middle.limits[1], outer.limits[1])),
            labels="R Plotting Characters (pch)") }
