mai.mutate <- function (
        bottom=NULL, left=NULL, top=NULL, right=NULL) {
    if (bottom %|% is.null) bottom <- mai.bottom()
    if (left %|% is.null) left <- mai.left()
    if (top %|% is.null) top <- mai.top()
    if (right %|% is.null) right <- mai.right()
    par(mai=c(bottom, left, top, right)) }
