image_setup <- function( #TAGS page setup
        mar=NULL,
        mai=NULL,
        x=par.usr.x(),
        y=par.usr.y(),
        bg=par("bg"),
        xaxs="i",
        yaxs="i",
        xaxt="n",
        yaxt="n",
        bty="n",
        main="",
        xlab="",
        ylab="",
        asp=NULL,
        ...) {
    if (!is.null(mar)) par(mar=mar)
    if (!is.null(mai)) par(mai=mai)
    if (!is.null(bg )) par( bg= bg)
    image(
        x=x,
        y=y,
        z=M1,
        col=TRANSPARENT,
        xaxs=xaxs,
        yaxs=yaxs,
        xaxt=xaxt,
        yaxt=yaxt,
        bty=bty,
        main=main,
        xlab=xlab,
        ylab=ylab,
        useRaster=T,
        asp=asp,
        ...) }
