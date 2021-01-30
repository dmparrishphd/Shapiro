splitr.df

DESCRIPTION

        Reflexive split

USAGE

        splitr.df(x, col.names, ...)

ARGUMENTS

        x

                a `data.frame` or other object.

        col.names

                column names of the `data.frame` or `matrix`
                argument. The corresponding list of columns will
                be passed to `split` as the `f` ("factor")
                argument.

        ...

                passed to `split`

DETAILS

SEE ALSO

EXAMPLES
inventory <- data.frame(MEMBER=c("Frodo", "Frodo", "Sam"), ITEM=c("ring", "Sting", "lembas bread"))
splitr.df(inventory, "MEMBER")

