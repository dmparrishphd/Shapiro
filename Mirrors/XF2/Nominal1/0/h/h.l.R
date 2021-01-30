h.l <- function(l, hopenlist="(", hcloselist=")")
{    c(	hopenlist,
        interlace.l(list(
            l  % %  nicenames  % %  h.labels.h,
            l)),
        hcloselist) }
