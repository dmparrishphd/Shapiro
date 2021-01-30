#TODO:---
#       (MAYBE) HANDLE CASES WHERE `DEPENDS` ARE NOT FOUND
#       IN THE LIBRARIES SPECIFIED BY THE .dat FILES.
#           **** UNTIL THEN **** `DEPENDS` ARE EXPECTED TO
#       BE FOUND IN THE SAME LIBRARY IN WHICH THE DEPENDENT
#       IS FOUND.
#
#ASSUMPTION:
#
#       THE DEEPEST 8 PARTS OF THE SEARCH PATH CORRESPOND
#       WITH THE SEARCH PATH OF A CLEAN BOOT OF R:
#
#               package:stats               package:graphics
#               package:grDevices           package:utils
#               package:datasets            package:methods
#               Autoloads                   package:base
# 
function(dir, libtree.dir) {

        cat. <- function(...) cat(..., file=stderr())

        while (9 < length(search())) detach()
        scan. <- source(paste0(dir, "Boot/LibTree/scan.R"))[[1]]

        cat.("load.packages: loading lists of packages:---\n")
        PKGSS <- do.call(c, lapply(
            # list-files.R SHOULD PUT ITEMS IN LOAD ORDER
            source(paste0(dir, "Boot/LibTree/list-files.R"))[[1]](libtree.dir),
            function(filename) {
                    cat("file:", filename, "\n")
                    `names<-`(list(scan.(filename)), filename) }
            ) )
         cat("---\n")
         flush.console()

        #SAVE
        SAVED.libPaths <- .libPaths()

            cat("\nload.packages: attempting to load and attach groups of packages:---\n\n")
            for (Pkgs in PKGSS) {
                Path <- Pkgs$PATH
                cat("\nFrom[0:", nchar(Path), "]: ", Path, "\n", sep="")
                cat("No. of Packages: ", length(Pkgs$PKGS), "\n\n", sep="")
                .libPaths(Pkgs$PATH)
                for (Pkg in Pkgs$PKGS) {
                    cat("Package: ", Pkg, "\n", sep="")
                    library(Pkg, lib.loc=Pkgs$PATH, character.only=T, verbose=T)
                }
            }

        cat("\n---\nload.packages: Done loading and attach groups of packages.\n")

        #RESTORE
        .libPaths(SAVED.libPaths)

        if (!identical(.libPaths(), SAVED.libPaths))
                warning("After loading libraries, .libPaths and SAVED.libPaths do not match.")
    }
