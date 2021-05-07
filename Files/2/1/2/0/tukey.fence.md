tukey.fence
===========

Tell which numeric values are outside of a Tukey Fence.

Usage
-----

    tukey.fence(x, fence=1.5, na.rm=T, type=7, ...)

| Argument | Description          |
| -------: | :------------------- |
|      `x` | a `numeric` vector   |
|  `fence` | a numeric value      |
|  `na.rm` | passed to `quantile` |
|   `type` | passed to `quantile` |
|    `...` | passed to `quantile` |

Value
-----

A logical vector telling whether each element of `x` is outside the fence.

See Also
--------

[InterQuartile Range](http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/BS704_SummarizingData/BS704_SummarizingData7.html)

[Tukey's Fences](https://en.wikipedia.org/wiki/Outlier#Tukey's_fences)

[Why \[did\] John Tukey set 1.5 IQR to detect outliers instead of 1 or 2?](https://math.stackexchange.com/questions/966331/why-john-tukey-set-1-5-iqr-to-detect-outliers-instead-of-1-or-2)

Code
----

["153"](../../../../1/5/3/0/tukey.fence.R)

Keywords
--------
 
IQR, interquartile range, summary statistics, outlier
