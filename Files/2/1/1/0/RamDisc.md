2/1/1/0/RamDisc
=======

load file contents

Usage
-----

    RamDisc <- function(files, path="")
    
| Argument | Description                                                                 |
| -------: | :-------------------------------------------------------------------------- |
|  `files` | `character` vector of file names                                            |
|   `path` | path to the files described by `files`, _including_ a final path separator. |
 
Value
-----

A `data.frame` of two columns.
The first column contains a copy of `files`.
The second column contains the `character` vectors read from the files specified by `files` and `path`.
Each `character` vector in the second column is wrapped in a `list`.

Example
-------

    # THIS EXAMPE WILL **** OVERWRITE **** A FILE CALLED trash IN THE WORKING DIRECTORY.
    f <- file("trash")
    write(c("AS", "YOU", "WISH"), file=f)
    close(f)
    RamDisc("trash")
    #   DESCRIPTION         LINES
    # 1       trash AS, YOU, WISH
    RamDisc("trash")[1, "LINES"][[1]]
    # [1] "AS"   "YOU"  "WISH"
