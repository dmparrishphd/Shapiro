foo <- source.object %-|% "R:/0000-array-block--proto.R"

bar <- foo$init(foo, 2 * m.(), c(8, 7))
bar$as.array(bar)
baz <- foo$init(foo, m.(), c(0, 0))
baz$as.array(baz)

bar$merge(list(bar, baz))


matrix(nrow=20, ncol=20) %|% ndim %|% ones

foo$init(foo, matrix(nrow=20, ncol=20))

qux <- foo$merge(list(foo$init(foo, matrix(nrow=20, ncol=20)), baz, bar))

#qux$ibb(qux)

qux

