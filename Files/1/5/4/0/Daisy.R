Daisy <- function(labels=character(), spacing=1, justification=1, xpd=T, ...) {
    em <- diff(strheight(c("\n\n", "\n\n\n")))
    en <- diff(strwidth(c("NN", "NNN")))
    if (length(labels)) {
        i <- match(mean(justification), 0:2 / 2 + 1)
        do.call(text, c(list(...), list(
            x=mean(par("usr")[justification]) + en * c(-1, 0, 1)[i],
            y=(.5 - seq(length(labels))) * em * spacing + par("usr")[4],
            labels=labels, pos=list(4, NULL, 2)[[i]], xpd=xpd ) ) ) }
    c(en, em) }
