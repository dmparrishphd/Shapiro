linegraph <- function (X=NULL, Y=X,
        G=matrix(Y %|% Nos. %|% rep2, ncol=2), # lapply(seq_along(Y), function (x) c(x,x)),
        par.mai=NULL,
        par.mar=NULL,
        par.new=F,
        uncol=BLU,
        col=Colors64,
        type=rep("l", times=min(1, length(Y))),
        xlab="",  ylab="",
        xaxs="i", yaxs="i",
        col.axis=uncol, col.lab=uncol, # CHANGED FROM NULL
        col.main=uncol, col.sub=uncol, fg=uncol, # CHANGED FROM NULL
        xlim=c( X %|% unlist %|% rmna %|% min,
                X %|% unlist %|% rmna %|% max  ),
        ylim=c( Y %|% unlist %|% rmna %|% min,
                Y %|% unlist %|% rmna %|% max  ),
        FUN.plot=plot,
         ...) {
    if (dev.list() %|% is.empty) { #ADDED 2019-08-06
        warning("linegraph: No device available. No plot will be produced.")
        return (NULL %|% invisible) }
    if (is.null(X)) { linegraph(
            list((seq(17)-1)/16*2),
            c(lapply(seq(16), function (n) cos((seq(17)-n)/16*2*pi))),
            c(lapply(seq(16), function (x) c(1, x))),
            type="l", bg="black", col.axis="grey") }
    args.consistent <-
            list(...) %,% list(
                xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs,
                col.axis=col.axis, col.lab=col.lab,
                col.main=col.main, col.sub=col.sub, fg=fg)
    par(new=par.new)
    if (par.mai %|% is.null && par.mar %|% is.null) par.mar[1:4] <- PAR.MAR.C
    if (par.mai %|% is.null %|% `!`) par(mai=par.mai)
    if (par.mar %|% is.null %|% `!`) par(mar=par.mar)
    for (i in G %|% rowNos) {
        args.i.pos <- list(X[[G[i, 1]]], Y[[G[i, 2]]])
        args.i.nom <- list(
            type=type %[mod% i, col=col %[mod% i,
            xlab=if (i < 2) xlab else HNULL,
            ylab=if (i < 2) ylab else HNULL)
        #if (i < 2) par(new=par.new) else
        do.call(FUN.plot, c(args.i.pos, args.i.nom, args.consistent))
        par(new=T) }
    invisible() }
    
