ia <- source.object %-|% "R:/000x-iarray--proto.R"

ia <- ia$init(ia, m.(), list(function(x) x %|% as.integer))
ia$`[`(ia, 1)
ia$`[`(ia, 2)
ia$`[`(ia, 6)

ia$`[`(ia, c(1,1))
ia$`[`(ia, c(2,1))
ia$`[`(ia, c(1,2))


# INDEXING FROM ZERO
ia <- ia$init(ia, m.(), list(function(x) 1L + x))
ia$`[`(ia, c(0,0))
ia$`[`(ia, c(1,0))
ia$`[`(ia, c(1,1))


(swmatch %<=% 11:12)(11:12)


# INDEXING FROM 11:12, 21:22
ia <- ia$init(ia, m.(), list(swmatch %<=% 11:12, swmatch %<=% 21:22))
ia$`[`(ia, c(11,21))
ia$`[`(ia, c(12,21))
ia$`[`(ia, c(12,22))



# INDEXING FROM 11:12, 21:22; method 2
ia <- ia$init(ia, m.(), array.spec.contiguous(list(11:12, 21:22)))
ia$`[`(ia, c(11,21))
ia$`[`(ia, c(12,21))
ia$`[`(ia, c(12,22))
ia$`[`(ia, list(11:12, 21:22))
ia$`[`(ia, list(NULL, 21:22))


# INDEXING BY OTHER VALUES
ia <- ia$init(
	ia,
	m.(),
	array.spec.contiguous(list("A B" %|% words, 0:2)))
ia$`[`(ia, list("A", 0))
ia$`[`(ia, list("B", 0))
ia$`[`(ia, list("B", 2))



