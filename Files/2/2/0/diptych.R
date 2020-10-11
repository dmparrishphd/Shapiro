diptych <- function( #TAGS image matrix side-by-side
        z1,
        z2,
        x=0:(z1  % %  nrow + z2  % %  nrow),
        y=-ncol(z1):0, ...)
    image(x, y, rbind(z1, z2), ...)
