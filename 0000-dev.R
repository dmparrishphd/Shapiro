page_setup <- function(
        x=0:1, y=0:1, xlab="", ylab="", xaxt="n", yaxt="n",
        bty="n", ...) {
    image(
        x=x, y=y, z=matrix(),
        zlim=c(0, 1), # AVOIDS WARNING
        xlab=xlab, ylab=ylab,
        xaxt=xaxt, yaxt=yaxt, bty=bty,
        ...) }
