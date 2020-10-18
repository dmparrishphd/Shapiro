iterate.simple <- function(post, FUN, fnbreak)
{   while (!fnbreak(post)) {
        pre  <- post;   post <- FUN(pre) }
    post }
