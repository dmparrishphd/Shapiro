lines_extract.cn
================

Value
-----

An nx1 `matrix`,
where each elment is the line read from the text connection (arg 1)
that corresponds to the integer index i (arg 2).

Details
-------

END-OF-LINE markers are **stripped**.

The rownames of the return are the `character` equivalent of the line numbers.

Numbering begins at 1, _regardless of the file position_ of the text connection on entry.

The index (arg 2) is **assumed** to be sorted.
Expect strange behavior if that is not the case.

Tips
----

If lines are needed out of order, call multiple times or reorder the return.
