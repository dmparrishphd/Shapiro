rename <- function(X, i, .names)
      `names<-`(X, `[<-`(names(X), i, .names))
