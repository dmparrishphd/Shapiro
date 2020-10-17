#USAGE: source("Daisy.README.pdf.make.R")
cat("Overwirte Daisy.README.pdf in the current directory,\n\"",
        getwd(), "\"\n? (Y/N).\n", sep="")
choice <- scan(what="", n=1)
if (choice == "Y") {
H <- 7.5
pdf(file="Daisy.README.pdf", width=H, height=H)
    par(mai=rep(1/2, 4))
    OL <- c(0, 1)
    image(OL*(H-1), OL*(H-1), matrix(1), col=NA,
        xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
    NPB <- "#A4DDED"
    box(ljoin="mitre", col=NPB)
    x <- seq(from=par("usr")[1], to=par("usr")[2], by=1/10)[-1]
    y <- seq(from=par("usr")[4], to=par("usr")[3], by=-1/5)[-1]
    segments(x, par("usr")[3], x, par("usr")[4], col=NPB)
    segments(0, y, par("usr")[2], y, col=NPB)
    Daisy(family="mono", col="lightgrey", c("",
        "12345678901234567890123456789012345678901234567890123456789012345"))
    Daisy(family="mono", font=2, "Daisy")
    Daisy(family="mono", c(
        "",
        "",
        "Description: Add pre-formatted text to an existing plot.",
        "",
        "Usage: Daisy(labels=character(), spacing=2, justification=1,",
        "    xpd=T, ...)",
        "",
        "Arguments:",
        "",
        "       labels: passed to `text`.",
        "      spacing: a factor applied to `strwidth(\"N\")` (one en space)",
        "               to produce spacing between baselines.",
        "justification: 1, 1:2, or 2 for left, center, or right",
        "               justification.",
        "          xpd: passed to `text`.",
        "          ...: passed to `text`.",
        "",
        "Value:",
        "",
        "The en space (width) and em space (height), which could be used to",
        "compute formatted text for `Daisy`.",
        "",
        "Details:",
        "",
        "The defaults are designed for monospace type with spacing",
        "proportional to 1/10 inch (pica) character spacing and 5 lines",
        "per inch. Specify `spacing=5/6` for (1/10):(1/6) proportions.",
        "",
        "Examples: See the accompanying file, Daisy.README.pdf.make.R",
        ""))
dev.off() }
