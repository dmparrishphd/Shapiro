vapplyf <-  function (X, FUN, FUN.VALUE=NULL)
        vapply(
            FUN %|% seq_along,
            function(i) FUN %[[% i %-|% X,
            if (FUN.VALUE %|% is.null)
                    FUN %|% first %:|% X else
                    FUN.VALUE)
