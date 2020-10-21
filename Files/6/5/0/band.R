band <- function(x, lo=0, hi=0, lin=T, hin=T) {
    tf = x[x >= lo];   tf = tf & x[x <= hi]
    if (!lin) tf = tf & x[x > lo] # conditionally exclude lo values
    if (!hin) tf = tf & x[x < hi] # conditionally exclude hi values
    tf }
