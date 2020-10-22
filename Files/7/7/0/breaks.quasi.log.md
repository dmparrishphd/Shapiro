breaks.quasi.log
================

Value
-----

a sequence of values on a quasi-logarithmic (base 10) scale.
The argument specifies the number of decades.

Tips
----

1. If the _number of bins_ desired is not a multiple of 3,
use, e.g., `[` to remove unneeded elements from a return
that has more than enough bins.

2. The return may be scaled to produce the desired range of breaks.

Examples
--------

    breaks.quasi.log()
    # [1] 1 2 5
    breaks.quasi.log(3)
    # [1]   1   2   5  10  20  50 100 200 500
    100 * breaks.quasi.log(3)
    #[1]   100   200   500  1000  2000  5000 10000 20000 50000
    
