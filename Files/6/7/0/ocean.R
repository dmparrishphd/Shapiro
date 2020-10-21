ocean <- function(n=16, alpha=1, fixup=F)
        hcl(alpha=alpha, fixup=fixup,
            h=seq(from=150, to=285, length.out=n),
            c=seq(from=60,  to=20,  length.out=n),
            l=seq(from=80,  to=20,  length.out=n) )
