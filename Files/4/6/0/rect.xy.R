rect.xy <- function(xy, ...) {
    S <- xy %|% nrow %/% 2L %|% seq * 2L
    rect(
        xy[S - 1L, 1L],
        xy[S - 1L, 2L],
        xy[S     , 1L],
        xy[S     , 2L], ...)
}
