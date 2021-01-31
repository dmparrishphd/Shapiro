walk.pg <- function(pg, length.out=1L) #REF Wildberger (sp?) N
        do.call(rbind, lapply(
            pg %|% nrow %|% seq,
            function(i) rename.all.rows.m(
                m.add.m..v(
                    (rows %|% argswap %<=% -1)( #TODO why doesn't rest.m work?
                        walk(
                            rows %|% argswap %<=% i %-|% (
                                rbind(pg, rows %<=% pg %-|% 1) %|% diff),
                            length.out=length.out %|% succ %[mod% i)),
                    rows %<=% pg %-|% i %|% as.vector),
            (rep %<=% i)(length.out %|% succ %[mod% i - 1))))
