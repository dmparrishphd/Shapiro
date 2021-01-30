applyf <-  function (X, FUN)
        lapply(
            FUN %|% seq_along,
            function(i) FUN %[[% i %:|% X)
