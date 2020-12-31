# Shapiro: A Handsome Helper for R
# Copyright (C) 2019 D. Michael Parrish
# COPYRIGHT NOTICE CONTINUES AT ./COPYRIGHT2.txt
# 
# BRIEF TABLE OF CONTENTS
#
# ./COPYRIGHT1.R                     Copyright Notice (PART 1/2)
# ./COPYRIGHT2.R                     Copyright Notice (PART 2/2)
# ./LICESE.txt                                 License (Primary)
# ./LICENSE-Stack_Overflow.htm                 License for curry
# *.R                                   (Body / Primary Content)



environments <- function() {
	l <- parent.frame() %|% list
	while (!identical( emptyenv(), l %|% last ))
			l <- append(l, l %|% last %|% parent.env)
	rename.all(
        l,
        vapply2(lapply(l, attr %|% argswap %<=% "name"), unnull %|% argswap %<=% "") ) }

    Doc$environments <- '
        environments returns a list of environments beginning
        from the environment in which environments was called,
        and following the chain of enclosing environments to
        emptyenv().
        
        The names of the return match the "name" attribute of
        the corresponding environment, or the null string for
        cases where there is no such attribute.'



llse <- function(...) #TAGS ls environment
		lapply(
			environments()[-1],
		 	function(X, ...) list(
				ENVIRONMENT=X, LS=ls(X, ...)),
			...)

    Doc$llse <- '
        llse applies ls in the environment from which lls is
        called as well as all enclosing environments. The `...`
        argument is passed to ls. The return is a compound list
        with one item per environment searched. Each list item
        is a list containing the environment searched followed
        by the result of the corresponding call to ls.'

gllse <- function (h, value=T, ...)
        grep(h, lapply(llse(), second) %|% unlist %|% anon, value=value)


