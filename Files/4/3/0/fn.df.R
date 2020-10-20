fn.df <- function(df, n=1, compar=`==`) function(x)
    compar %<=% df[[n]] %O% which %:|% x %=>% lapply %:|% (rows %<=% df)
