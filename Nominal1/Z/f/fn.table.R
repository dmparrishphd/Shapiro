fn.table <- function (mapFrom, mapTo)
        function(x) mapTo[[match(x, mapFrom)]]
