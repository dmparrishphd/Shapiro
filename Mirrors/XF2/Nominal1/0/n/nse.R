nse <- function(sim, obs) 1 -
        ssq.diff2(obs, sim) / ssq.diff2(obs, obs %|% mean)
