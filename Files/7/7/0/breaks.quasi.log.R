breaks.quasi.log <- function(decades=1)
        (matrix(c(1, 2, 5), nrow=decades, ncol=3, byrow=T) *
                crep(10 ^ seq0(decades - 1), 3)) %|% t
                
