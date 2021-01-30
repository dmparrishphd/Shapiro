df.align <- function (x, y) #TAGS table join
        rename.all.columns.m(
            do.call(
                data.frame,
                l.align.l..l(
                    list(x, x %|% Nos.),
                    list(y, y %|% Nos.))),
            "INDEX FIRST SECOND" %|% words)
