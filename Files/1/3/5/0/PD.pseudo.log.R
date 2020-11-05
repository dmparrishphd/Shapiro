.pseudo.log <- function(base, n) {
        truncate <- n.round.down.n %<=2% base
        shift <- n.right.shift.n %<=2% base
        m <- O
        while (0 < n) {
            m <- m %|% succ
            n <- n %|% truncate %|% shift }
        m }
