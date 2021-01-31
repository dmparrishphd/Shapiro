.lines_grid.y <- function (spacing=NULL, start="bottom", ...)
        linesh(
            (if (start == "bottom") {
                usr.y1()
            } else if (start == "integer") {
                usr.y1() %|% ceiling
            } else start) +
           usr.height() %\% spacing %|% seq %|% pred * spacing, ...)
