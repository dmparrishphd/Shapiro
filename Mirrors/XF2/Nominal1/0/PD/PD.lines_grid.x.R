.lines_grid.x <- function (spacing=NULL, start="left", ...)
        linesv(
            (if (start == "left") {
                usr.x1()
            } else if (start == "integer") {
                usr.x1() %|% ceiling
            } else start) +
           usr.width() %\% spacing %|% seq %|% pred * spacing, ...)
