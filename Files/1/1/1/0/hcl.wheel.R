hcl.wheel <- function (n=16, cex=4, plot.args=NULL, par.args=NULL) {
    dev.new()
    par(mar=0 %|% rep4)
    image_bare(LL * 9/8, LL * 9/8, M1, col=BLK, asp=1, add=F)
    xy <- n %|% m.seq.kk
    points(xy, col=hc1(h=0:n %|% except.last / n), cex=cex,
           pch="CIRCLE SOLID" %|% pch.h) }
