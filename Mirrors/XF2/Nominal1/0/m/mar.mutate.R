mar.mutate <- function (
        bottom=NULL, left=NULL, top=NULL, right=NULL) {
    if (bottom %|% is.null) bottom <- mar.bottom()
    if (left %|% is.null) left <- mar.left()
    if (top %|% is.null) top <- mar.top()
    if (right %|% is.null) right <- mar.right()
    par(mar=c(bottom, left, top, right)) }
