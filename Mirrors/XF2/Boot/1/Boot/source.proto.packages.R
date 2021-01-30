#FUTURE: ???? HANDLE QUOTED STRINGS AS FILENAMES, TO ACCOMODATE SPACES IN FILE NAMES. ????
#FUTURE: HANDLE A FEW SIMPLE COMMANDS: SKIP, NO_MORE (USEFUL FOR DEBUGGING)
function(dir, proto.pkg.config.file, EnvName="ShapirEnv") {

        cat. <- function(...) cat(..., file=stderr())

        detach. <- function() detach(EnvName, character.only=T)
        while(EnvName %in% search()) detach.()
        Attached <- F

        cat.("\n\nsource.proto.packages: getting names of source packages... ")
        flush.console()
        SOURCES <- scan(
            file=proto.pkg.config.file,
            what="character",
            sep="\n",
            comment.char="#")

        cat(" Done.\n")

        assign(x=EnvName, value=new.env(parent=globalenv()), envir=globalenv())

        cat("\n\nsource.proto.packages will attempt to `source`:---\n")
        for (Source in SOURCES) cat(Source, "\n")
        cat("---\n")
        flush.console()

        NSOURCES <- length(SOURCES)
        cat("\n\nsource.proto.packages: `source`-ing...\n")
        REPORT <- t(matrix(unlist( {
                Report <- rep(list(character(2)), length(SOURCES))
                for (k in seq(NSOURCES)) {
                    cat("\nCurrent:", SOURCES[k], "\n")
                    cat(
                        "Next: ", 
                        if (k < NSOURCES) SOURCES[1L + k] else "(no more)",
                        "\n")
                    flush.console()
                    source(SOURCES[k], local=ShapirEnv)
                    Report[[k]][1:2] <- c( SOURCES[k], length(ShapirEnv) ) }
                Report } ), nrow=2) )
        if (Attached) detach.()
        attach(ShapirEnv)
        rm(list=EnvName, envir=globalenv())

        REPORT }
