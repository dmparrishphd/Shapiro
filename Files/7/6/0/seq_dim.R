seq_dim <- function(.array) lapply(
    .array %|% dimNos,
    .array %|% dim %=>% `[` %O% seq)
    
