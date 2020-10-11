ancestry <- function(l) {
    ll <- l  % %  length  % %  NULLs
    nom <- nicenames(l)
    names(ll) <- nom
    for (i in seq_along(l))
            if (!is.list(l[[i]])) ll[[i]] <- nom[i] else {
            ll[[i]] <- ancestry(l[[i]]) }
    ll }
