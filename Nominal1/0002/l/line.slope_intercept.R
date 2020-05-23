line.slope_intercept <- function(slope_intercept)
        `colnames<-`(
            slope_intercept %|% first2c %|% cswap %cbind% -1L,
            "C A B" %|% words)

    Doc$line.slope_intercept <- '
        line.slope_intercept returns the parameters of the lines
        whose slopes and intercepts are found in the first and
        second columns of the matrix-like, numeric argument.

        The rows of the return each correspond with the rows of
        the argument and the columns of each row of the return
        correspond with the constant (C); first-order "x" (A),
        and first-order "y" (B) terms, respectively of the line
        whose equation is C + A * x + B * y = 0. Note the sign
        on the "B" term.

        > line.slope_intercept((matrix(1:4, ncol=2)))

             C A  B

        [1,] 3 1 -1

        [2,] 4 2 -1

        > df1 <- as.data.frame(matrix(1:9, ncol=3))

        > df1

          V1 V2 V3

        1  1  4  7

        2  2  5  8

        3  3  6  9

        > line.slope_intercept(df1)

             C A  B

        [1,] 4 1 -1

        [2,] 5 2 -1

        [3,] 6 3 -1
'
