close.ALL.THE.windows <- function()
        while(length(dev.list())) dev.off()
