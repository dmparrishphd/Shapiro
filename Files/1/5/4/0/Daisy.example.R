# EXAMPLE OF SPECIFYING 1/6-INCH LINE SPACING BY SCALING THE "em" RETURN OF `Daisy`.
# THE CALLS TO `Daisy` ARE AT THE END OF THE INDENTED BLOCK.
#
# **** WARNING: **** EXECUTING THIS EXAMAPLE OVERWRITE A FILE
# CALLED Daisy.example.pdf IN THE CURRENT WORKING DIRECTORY.

LABELS <- rep(
    paste0("   456", paste0(rep(1:6, 12), collapse="")),
    54)

NUMBER <- formatC(1:54, width=2)

PAGE.HEIGHT <- 11
PAGE.WIDTH <- 8.5
MARGINS <- c(BOTTOM=1, LEFT=1, TOP=1, RIGHT=1)
MARGINS.SIDE <- sum(MARGINS[c("LEFT", "RIGHT")])
MARGINS.VERTICAL <- sum(MARGINS[c("TOP", "BOTTOM")])
pdf(file="Daisy.example.pdf", height=PAGE.HEIGHT, width=PAGE.WIDTH)
    par(mai=MARGINS)
    image( # USER UNITS AND PAGE UNITS MATCH.
        c(0, PAGE.WIDTH - MARGINS.SIDE),
        c(0, PAGE.HEIGHT - MARGINS.VERTICAL),
        matrix(1),
        col=NA, xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
    box(col="red", ljoin="mitre")
    Daisy(LABELS, family="mono", spacing=1/Daisy()[2]/6, cex=5/6)
    Daisy(NUMBER, family="mono", spacing=1/Daisy()[2]/6, cex=5/6)
dev.off()

