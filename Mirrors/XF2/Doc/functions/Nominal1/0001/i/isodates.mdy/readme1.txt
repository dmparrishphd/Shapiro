isodates.mdy

Reformat an M/D/Y date string as an ISO date (ISO 8601)

USAGE

        isodates.mdy(x, sep="/")

ARGUMENTS

        `x`

                A character vector, each element of which is of
                an M/D/Y date format.

        `sep`

                A character vector that specifies a separator
                between the M, D, and Y components of the
                elements of `x`.

VALUE

        A character vector whose `length` matches that of `x`.
        Each elment of the return is the ISO 8601 equivalent of
        the corresponding element of `x`.

WARNINGS

        There are no checks for valid input.

REFERENCES

        https://en.wikipedia.org/wiki/ISO_8601

SEE ALSO

        `as.Date` (`base`)

EXAMPLES

        isodates(c("11/5/1955"))
        # [1] "1955-11-05"
