named.list.lines <- function(.lines) { #TAGS text
    part <- function(from) .lines[seq(from=from, to=length(.lines), by=2)]
    `names<-`(as.list(part(2)), part(1) ) }

    Doc$named.list.lines <- '
        named.list.lines returns a named list interpreted from
        the character vector argument.

        The items with odd indices are interpreted as names,
        while the items with even indices are interpreted as
        list items.

        EXAMPLE

        named.list.lines(c(
                           
        "THING1", "raindrops on roses",

        "THING2", "whiskers on kittens"))

        # $THING1

        # [1] "raindrops on roses"

        # $THING2

        # [1] "whiskers on kittens"
        '
