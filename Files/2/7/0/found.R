found <- function () { #TAGS search names environment objects list
    NAMES <- search() %|% rev
    rename.all(lapply(NAMES, list(all=T) %v% ls), NAMES)
}
