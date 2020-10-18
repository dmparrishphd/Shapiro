is_ascii <- function (h) vapply(
    h,
    function(h)
            if (h  % %  nchar) h  % %  utf8ToInt < 128L else F,
    T)
