`%=:%` <- function(.list, newname_for_last_item) {
    names(.list)[length(.list)] <- newname_for_last_item
    .list }
