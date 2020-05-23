seq_dim <- function(.array)
        lapply(
            .array %|% dimNos,
            .array %|% dim %=>% `[` %O% seq)

    Doc$seq_dim <- '
        seq_dim returns a list, one item per dimension of the
        array argument. Each element contains a sequence of
        indices along the corresponding direction.'
