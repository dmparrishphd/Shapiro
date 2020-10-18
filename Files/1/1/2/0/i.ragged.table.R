            seq_along(lengths),
            function(k, n=lengths[k])
                    cbind(seq(n), rep(k, n) ) ) )
