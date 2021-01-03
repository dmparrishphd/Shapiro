.RamDisc <- function(files, path="")
        lapply(files, function(filename)
                scanRest(paste0(path, filename)))

RamDisc <- function(files, path="") data.frame(
    stringsAsFactors=F, DESCRIPTION=files, LINES=matrix(
        .RamDisc(files, path)))
