wash <- function (opaque.colors, .alpha=.5) {
    HCL     <- hcl(a=.alpha)
    ALPHA   <- alpha(HCL)
    NOALPHA <- noalpha(opaque.colors)
    NOALPHA %//% ALPHA }
