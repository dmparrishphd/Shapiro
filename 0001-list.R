# Shapiro: A Handsome Helper for R
# Copyright (C) 2018 D. Michael Parrish
# 
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public
# License along with this program.  If not, see
# <https://www.gnu.org/licenses/>.
#
# END OF COPYRIGHT NOTICE
#
#

l.align.l..l <- function (list1, list2) { #TAGS join
    # REMIDER: PRESERVE THE ASPECT OF THIS CODE THAT ALLOWS THE
    # LIST ARGUMENTS TO BE LIST OF PARALLEL ATOMIC VECTORS.
    index1 <- list1 %|% first
    index2 <- list2 %|% first
    index  <- index1 %,% index2 %|% unique
    list(
         index,
         (list1 %|% second) [match(index, index1)],
         (list2 %|% second) [match(index, index2)] ) }

df.align <- function (x, y) #TAGS table join
        rename.all.columns.m(
            do.call(
                data.frame,
                l.align.l..l(
                    list(x, x %|% Nos.),
                    list(y, y %|% Nos.))),
            "INDEX FIRST SECOND" %|% words)

    Doc$df.align <- '
        Given two vectors, each representing a set values
        (possibly row numbers), returns a data frame that
        contains three columns: 1) INDEX, the superset of the
        two input sets; FIRST, the indices of the INDEX-values
        found in the primary argument; and SECOND, the indices
        of the INDEX-values found in the secondary argument.'

l.extract <- function(X, indices=list()) lapply(indices, `[[` %<=% X)

nl.extract <- function(X, indices=list()) {
	r <- l.extract(X, indices)
    if (X %|% names %|% is.null %|% `!`) names(r) <- names(X)[n.index(X, indices)]
	r }

nl.extract.names <- function(X, names.=NA_character_)
        nl.extract(X, match(names., X %|% names) %|% rmna)

    Doc$nl.extract.names <- '
        nl.extract.names returns a named list containing the
        items from arg1 that are named by arg2, in the order
        listed in arg2. Names in arg2 that are not found among
        the names of arg1 are ignored. **** NOT INTENDED ****
        for cases where duplicate name occur in arg1.'

b.nonnull.l <- function(l) vapply(l, un(is.null), T)

which.nonnull.l <- b.nonnull.l %O% which

first.nonnull.l <- function(l) l[[match(T, l  % %  b.nonnull.l)]]

branches <- function (l) {
    ll <- list()
    nom <- names(l)
    i <- 0
    for (item in l) {
            i  % %  succ -> i
            if (item  % %  is.list) {
                    ll <- c(list(item), ll)
                    if (!!(nom[i]  % %  `#`))
                            names(ll)[1] <- nom[i] } }
    ll }

morph <- function(X, FUN, test=function(y)T, ...)
		lapply(X, function(x, ...) if (test(x)) FUN(x, ...) else x, ...)

swap.l <- function (l)
{   if (l  % %  `#`  <  2) return (l)
    l.join.l(l[c(2, 1)], l[-1][-1]) }

select.l <- function (X, FUN, ...)
        lapply(X, FUN, ...) %|% unlist %|% which

sift.l <- function (X, FUN, ...)
        X[select.l(X, FUN, ...)]

sift.enumerated.l <- function (X, FUN, ...)
        l.enumerate(X)[select.l(X, FUN, ...)]

.l.promoted.l.2 <- function (l) list(   l[[1]] %,% l[[2]][[1]], l[[2]][-1]   )
.l.promoted.l.more <- function (l) l  % %  .l.promoted.l.2 %,% l[-2:-1]
l.promoted.l <- function (l)
{   if (l       % %  length < 2) return (l)
    if (l[[2]]  % %  length < 1) return (l)
    .l.promoted.l.more(l) }

l.unparallel <- function (n, l) lapply(l, function(ll) ll[[n]])

l.zip.l <- function (l)
        if (l  % %  length)
        lapply(Nos.(l[[1]]), l.unparallel, l) else list()

group.l <- l.unlace %O% l.zip.l

l.enumerate <- function (x) l.zip.l(list(Nos.(x), as.list(x)))

l.compound.n <- function (n) lapply(NULLs(n), function(x) list())

`%=%` = function(x, y) { l = list(y); names(l) = x; l }

leaves.gather <- function(l) {
	s <- l  % %  b.leaves # selection
	list(l[s], l[!s]) }

l.flatten.l <- function(l)
{ # l is a compound list
	list() -> ll
	for (item in l) ll %,% item -> ll
	ll }

l.join.l <- function (...) l.flatten.l(list(...))

.l.extend.l <- function (l, n)
{   NA -> l[[succ(n)]];   NULL -> l[[succ(n)]];   l }

l.extend.l <- function (l, n=0L, what=NULLs(1))
{   n - `#`(l) -> xlen # length of the extended part
    if (xlen <= 0L) return (l)
    l.join.l(l, rep_len(what, xlen)) }

t.branch.t <- function (t, i=integer(), allowMetamporphosis=F, value=NULL) {
	if (i  % %  `#` == 0L) return (t)
    i  % %  first -> j
	t <- l.extend.l(t, j)
    if (i  % %  `#` == 1L)
            if (value  % %  is.null  % %  `!`) t[[j]] <- value
	if (i  % %  `#` >  1L) {
        t[[j]] -> tj
		if (
				(tj  % %  is.leaf) &&
				(tj  % %  is.null  % %  `!`) &&
				!allowMetamporphosis) {
			warning("Contradiction: Metamporphosis both requested and disallowed.")
			return (list()) }
        #
        # don't want to destroy structure that's already there
		if (tj  % %  is.leaf) t[[j]] <- list() 
		t[[j]] <- t.branch.t(tj, i[-1], allowMetamporphosis, value) }
	t }

leaves <- function(l) {
	ll <- list()
	while (l  % %  `#`) {
		l  <- leaves.gather(l)
		ll <- c(ll, l[[1]])
		l  <- l.flatten.l(l[[2]]) }
	ll }

leaf.indices <- function(l) i.leaves.t(l)[[2]]  % %  leaves

leaf.filter <- function(t, FUN=typeof, what="NULL", compar=`!=`)
        leaf.indices(t)[compar(unlist(lapply(leaves(t), FUN)), what)]

treeapply <- function(X, FUN, ...) {
	m <- list()
	for (i in leaf.filter(X))
		    t.branch.t(m, i, value=FUN(element.l(X, i), i, ...)) -> m
	m }

l.interlace.l <- function (l) l  % %  l.zip.l  % %  l.flatten.l

interlace.l <- function (l) l  % %  l.interlace.l  % %  unlist

h.l <- function(l, hopenlist="(", hcloselist=")")
{    c(	hopenlist,
        interlace.l(list(
            l  % %  nicenames  % %  h.labels.h,
            l)),
        hcloselist) }

rake <- function (t)
{   tt <- t
    for (i in seq_along(t)) {
        if (!is.list(t[[i]])) tt[[i]] <- t[[i]] else # is.list was is.List
        if (t[[i]]  % %  is.list.simple) tt[[i]] <- h.l(t[[i]]) else
        tt[[i]] <- rake(t[[i]]) }
    tt }

l.unflat.l <- function (l, n)
{   list() -> ll;   1 -> i
    while (length(l)) { l[1:n] -> ll[[i]];   NULL -> l[1:n];   succ(i) -> i }
    ll }

lassign = function(x, y) {
    for (i in seq_along(x)) x[[i]] = y[[i]];   x }

ltaggednull = function (h="tag") {
    l = as.list(alist(x=));   names(l) = h;   l }

laddtag = function (l, h="tag") c(l, ltaggednull(h))

l.taggednull.l = function (h="tag")
        { l = as.list(alist(x=));   names(l) = h;   l }

l.append.l = function (l, h="tag")
{   m = ltaggednull(h)
    m[[1]] = list()
    c(l, m) }

l.append.l.nonempty = function (l, h="tag", x="value")
{   l=l.append.l(l, h)
    l[[h]]=x
    l }

rename <- function (l, n, newname) {
    # HISTORY 2018-05-31 changed to return a new list
    names(l)[[n]] <- newname;   l }

renameend <- function (l, newname) rename(l, l  % %  `#`, newname)

rename.some..m <- function(X, mnames) {
	names(X)[match(mnames[,1], names(X))] <- mnames[,2]
	X }

rename.some..df <- rename.some..m

rename.some..l <- function (X, lnames) {
    names(X)[n.index(X, lnames[[1]])] <- lnames[[2]]
    X }


#lappend = function (l, x=NA) l <<- c(l, x) #DELETED

inext.l <- function (l) l  % %  `#`  % %  succ

        Doc$inext.l <- crunch.h('
            inext.l returns the next available numeric index of
            the list argument, which is equal to one plus the
            current list length. Originally intended for
            list-building, e.g., L[[inext.l(L)]] <- 42')

lappendnamed = function (l, newnamed)
{
}


laddtag = function (l, h="tag") { l[[1+length(l)]] <<- NA;   names }

lassign = function (l, n, x)
{   if (length(n) == 1) { l[[n]] <<- x;   return (NULL) }
    l[[n[1]]] <<- lassign(l, n[2:length(n)], x)
}

named = function (x) { if (is.null(names(x))) names(x) = rep("", times=length(x));   x }

is.named.element = function (x) { x=named(x);    names(x) != "" }
is.unnamed.element = function (x) { !is.named.element(x) }

unnamed.elements = function (x) x[is.unnamed.element(x)]
named.elements   = function (x) x[  is.named.element(x)]
unnamed.element.indices = function (x) { which(is.unnamed.element(x)) }
named.element.indices     = function (x) { which(is.named.element(x)) }

lcopy.pos = function (x, y=list())
{   # STATUS: IN PROGRESS
    u = unnamed.element.indices(y) # candidates from y to specify in x
    j = which(is.unnamed.element(x) | !(names(x) %in% names(y))) # indices of x where the element is not named by y
    m = min(length(u), length(j))
    loopdomain=seqN(min(length(u), length(j)))
    for (i in loopdomain) x[[j[i]]] = y[[u[i]]]
    x
}
lcopy.nom = function (x, y=list())
{   for (nom in names(y)) if (nom %in% names(x)) x[[nom]] = y[[nom]];   x }

lcopy = function (lst, mods=list()) lcopy.pos(lcopy.nom(lst, mods), mods)

lassign.next.symbol = function (x, y) {}

# Apply a list of functions to a list or vector

applyf <-  function (X, FUN)
        lapply(
            FUN %|% seq_along,
            function(i) FUN %[[% i %-|% X)

    Doc$applyf <- '
        applyf is somewhat the inverse of lapply. The return is
        a list of values. Each value k is the result of applying
        the k-th function in FUN to X.'

vapplyf <-  function (X, FUN, FUN.VALUE=NULL)
        vapply(
            FUN %|% seq_along,
            function(i) FUN %[[% i %-|% X,
            if (FUN.VALUE %|% is.null) {
                FUN %|% first %-|% X
            } else {
                FUN.VALUE } )

    Doc$vapplyf <- '
        vapplyf is somewhat the inverse of vapply. The return is
        an array of values. The k-th slice is the result of
        applying the k-th function in FUN to X.
        
        > vapplyf(1:42, list(range, first2)) 
            [,1] [,2]
        [1,]    1    1
        [2,]   42    2'

lapply1to1 = function (X, FUNs, ...)
        lapply(
            X %|% seq_along,
            function(n) `%[[mod%`(FUNs, n)(X %[[% n, ...))

vapply1to1 <- function (X, FUNs, FUN.VALUE, ...)
        vapply(
            X %|% seq_along,
            function(n) `%[[mod%`(FUNs, n)(X %[[% n, ...),
            FUN.VALUE)

    Doc$vapply1to1 <- '
        vapply1to1 is similar to vapply, but the values X are
        matched to functions FUNs according to their position.
        FUNs are recycled if necessary to achieve the number of
        elements in X.'

lapply1tomany <- function (x, FUNs) lapply1to1(rep_along(list(x), FUNs), FUNs)

lapply1to1.as = function (x, mode.abbr="i", ...)
{   CH = strsplit("lidchr", "")[[1]]
    AS = c(as.logical, as.integer, as.double, as.complex, as.character, as.raw)

    Ch = strsplit(mode.abbr, "")[[1]]
    As = list()
    for (i in Nos.(Ch)) { As[[i]] = AS[[which(Ch[i] == CH)]] }

    lapply1to1(x, As)
}

spread.l <- function(l, sep=NULL, spread=1L, trailing=T)
{	if (spread != 1L) warning("spread != 1 not yet implemented!")
    # TO DO: REFACTOR SO THAT jump.b is unnecesary. can use
    # something like c(NULLs(1), l)
    l  % %  `#` -> lgt
    if (is.null(sep)) m <- NULLs(lgt) else
            m <- rep(sep  % %  list, lgt)
    l.interlace.l(
            jump.b(trailing, list(rev, identity), list(l, m))) }

'
function b.nonnull.l : logical index

        Returns a logical index of the non-NULL values in the
        list argument.

function which.nonnull.l : index non NULL

        Returns an integer index of the non-NULL values in the
        list argument.

function i.first.nonnull.l : index NULL

        Returns the index of the first non-NULL value in the
        list argument, if such a value is present; NA otherwise.

function first.nonnull.l : index NULL

        Returns the first non-NULL value in the list argument,
        or NULL if there is no such value.

function morph : apply

        Simmilar to lapply, except that items of X failing test
        will be simply reproduced in the return, rather than 

function spread.l : interlace list zip

        Returns a modified copy of the list argument (arg 1),
        where NULL-s have been placed between the original list
        elements as well as at the end (default case) or at the
        beginning (if the optional agrument trailing is F).
        FUTURE: implement the spread argument, which will allow
        the placement of more than one NULL between each
        original element. Example:

                > spread.l(list(1,2))
                [[1]]
                [1] 1

                [[2]]
                NULL

                [[3]]
                [1] 2

                [[4]]
                NULL
        

function ltaggednull 

        Returns a singleton list with its symbol element tagged
        according to h.  Originally inteded to produce a
        placeholder for an existing list.

function l.taggednull.l

        Returns a singleton list with its symbol element tagged
        according to h.  Originally inteded to produce a
        placeholder for an existing list.

function newlist

        REMOOVED. Use NULLs

function NULLs

        Returns a list of length n (arg 1), all elements NULL

function .l.promoted.l.2 

        Helper function for l.promoted.l

function .l.promoted.l.more 

        Helper function for l.promoted.l

function l.promoted.l 

        Returns a modified version of the compound list
        argument. The returned value is a list whose first
        element is identical to the first element of the input,
        except that it is longer by one elment, and that element
        is equivalent to the first element of the second element
        of the input. The second element of the return is
        equivalent to the second element of the input, except
        that its first element has been removed.

        Example: by analogy to schools, the following call
        produces a list representing the conditoin after the
        ninth grade class has been moved to the high school:

                l.promoted.l(list(list(12,11,10), list(9,8,7)))

        Using Lisp notation:

                Input:
                        ((12 11 10) (9 8 7))

                Return:

                        ((12 11 10 9) (8 7))

function l.unparallel

        Input: list of parallel lists l and an index n
        Output: list containing items n of each member of l.

function l.zip.l : parallel lists table

        C.f. Python zip. l.zip.l is its own inverse.

function l.append.l

        Returns a modified copy of list l, where a list element
        named h has been appended. To use the terminology of
        TAOCP (2.3), can be used to insert a node of degree zero
        into an existing tree...


function lcopy.pos

        Assigns values of elements of list y to elements of list
        x, based on position, in the fashion described in
        "Argument Matching" (in the R Language Definition
        document). Originally intended to produce modified
        versions of lists returned by the fomrals function.
        

        ASSUMPIONS

        All elements of x have names, all of which are unique.
        All elements of y have names.


        DETAILS

        Elements of y having names other than "" are ignored.
        Zero or more elements of y may be named "".

function lcopy.nom

        Assigns values of elements of list y to elements of list
        x, provided the names of the elements match.""

function listfrompairlist

        REMOVED. Use as.list instead.

function applyf

        Builds a list of values, where each element is the
        result of applying the corresponding element of FUN to
        X. C.f.  lapply

        TO DO: allow passing of additional parameters to X

        Arguments:

                X      a list or vector
                FUN    a list of functions

        Example:

                applyf(seq(3), list(abs, log, sqrt))

                returns

                [[1]]
                [1] 1 2 3

                [[2]]
                [1] 0.0000000 0.6931472 1.0986123

                [[3]]
                [1] 1.000000 1.414214 1.732051

function lapply1to1

        Returns a list containing the result of applying each
        element of list FUNs to the corresponding element of
        list X.  Elements of FUNs are recycled if necessary to
        obtain enough functions. HISTORY 2018-10-12: recycling
        feature added.

        UNTESTED: the "..." feature.

        Example:
    
                > lapply1to1(
                +         list(c("F", "T"), c("0", "1"), c("2.7", "3.1")),
                +         list(as.logical, as.integer, as.double))
                [[1]]
                [1] FALSE  TRUE

                [[2]]
                [1] 0 1

                [[3]]
                [1] 2.7 3.1

function lapply1to1.as

        A specialization of lapply1to1. Produces a version of
        list x, where each item of the returned value is a
        conversion of the corresponding item of x. Valid
        conversions are as.logical, as.integer, as.double,
        as.complex, as.character, and as.raw (see also
        as.vector), each of which is specified by the substrings
        "l", "i", "d", "c", "h", and "r", respectively.

        Example (see also Example in lapply1to1 documentation):

                > lapply1to1.as(list(c("F", "T"), c("0", "1"), c("2.7", "3.1")), "lid")
                [[1]]
                [1] FALSE  TRUE

                [[2]]
                [1] 0 1

                [[3]]
                [1] 2.7 3.1

function l.unflat.l :

        Somewhat the opposite of unlist. Example:

                l.unflat.l(as.list(seq(9)), 3)

        returns the equivalent of

                list(list(1,2,3), list(4,5,6), list(7,8,9))
        
function l.part.l : subset indexing list

        DELETED. Use `[` instead.

function l.flatten.l :

        Intended for compound lists. If the list argument
        contains an item that is not a list, results may be
        surprising.

        Example:

                l.flatten.l(list(list(1, 2), list(3, 4)))

        returns the equivalent of

                list(1, 2, 3, 4)

        NOTE: l.flatten.l old name l.flat.l

function leaves.gather : list reorganize

        Returns a reorganization of the data stored in the list
        argument. The return is a compound list containing two
        items: 1) a list of leaves of the argument, 2) the
        remaining items of the argument. Example:

                > leaves.gather(list(1,list("A")))
                [[1]]
                [[1]][[1]]
                [1] 1


                [[2]]
                [[2]][[1]]
                [[2]][[1]][[1]]
                [1] "A"
                
function leaves : list tree terminal nodes

        Returns a simple list of all leaves of the tree
        argument. The order of the items in the returns from two
        calls on two trees of the same structure should be the
        same. Example:

        > leaves(list(1, list(2, 3), seq(12)))
        [[1]]
        [1] 1

        [[2]]
        [1]  1  2  3  4  5  6  7  8  9 10 11 12

        [[3]]
        [1] 2

        [[4]]
        [1] 3

function leaf.indices: index list tree leaves

        Returns a simple list where each element is a vector of
        natural numbers (integer vector). Each vector corre-
        sponds to the (possiblt compound) index of a leaf of the
        list (or tree) argument.

        > leaf.indices(list(list(1), list(2), list(list(list(3)))))
        [[1]]
        [1] 1 1

        [[2]]
        [1] 2 1

        [[3]]
        [1] 3 1 1 1

function leaf.filter

        Returns the leaf indices (see leaf.indices function) for
        which compar(leaf, what) returns T. By default, returns
        the leaf indices of non-NULL leaves.

        C.f. sift

        FUTURE: refactor with consideration for sift

function l.join.l : concatenate

        Returns the concatenation of the list arguments. C.f.
        l.flatten.l. Exmaple:

                > l.join.l(list(seq(2), seq(3)), list(seq(3), seq(4)))
                [[1]]
                [1] 1 2

                [[2]]
                [1] 1 2 3

                [[3]]
                [1] 1 2 3

                [[4]]
                [1] 1 2 3 4

function l.extend.l

        Returns a modified copy of the list (arg 1) where n
        (optional arg) elements have been appended. These new
        elements are copies of the elements of what (a list;
        optional arg), which are recycled as needed.

        C.f. Python list.extend

function t.branch.t : extend tree list grow

        Returns a modified copy of the list (arg 1; possibly a tree),
        where the structure has been extended according to the
        vector index i (optional arg; integer vector): the
        return will have a structure such that there is an
        element at the index indicated.
            Exception: Returns an empty list if there is
        contradictoin among the arguments (see discussion on the
        allowMetamporphosis argument).
            The optional argument allowMetamporphosis either
        allows or disallows non-NULL leaves of the list (arg 1)
        to be absent in the return, but rather for there to be a
        branch at the corresponding location instead.
            Example:

function treeapply : apply list leaf leaves

        Similar to lapply. However, FUN is a function of at
        least TWO arguments. Leaves of X (arg 1) AS WELL AS the
        vector indices of these leaves will be passed to FUN as
        arguments 1 and 2 of FUN, respectively. The vector
        indices may be used within FUN to determine the details
        of application, e.g., what ancillary data to use. Of
        course, a particular FUN may ignore its second argument
        (or any argument, for that matter.)

        Additional arguments (...) of treeapply will also be
        passed, in a manner similar to that of lapply.

        Returns a tree with leaves in positions corresponding to
        the leaves of X (arg 1).

        Example:

                > treeapply(list(list(1, 2), list(3, 4)),
                            function(x, y) c(as.character(x),
                                             as.character(y)))

        returns the equivalent of
        
                list(list(c("1", "1", "1"),
                          c("1", "2", "2")),
                     list(c("2", "1", "3"),
                          c("2", "2", "4")))

function rake : tree leaf leaves list

        Primarily a helper function for a few (one?) other
        Shapiro functions.

        Returns a simplified representation of the list
        argument, in the direction of a simple list. Can be
        applied recursively. Intended to be used as a step in
        developing a representation of an entire tree as text.

function branches : tree

        Returns a copy of the tree argument without any of the
        leaves of the original.

function locate : search lookup find

        Returns the positions where each item of arg 1 is found
        in arg 2. For each item, only one position-value will be
        found in the return. If an item is not found, NA will be
        placed in the corresponding slot.

function lapply1tomany : 1:many

        The inverse of lapply: single data argument, multiple
        FUNctions Example:

                > lapply1tomany(1, list(identity, cos, sin, sqrt)) % % unlist
                [1] 1.0000000 0.5403023 0.8414710 1.0000000

function l.extract : list

        Returns a list composed of the items of the primary
        argument (a vector or list) specified by the indices
        (arg 2).

        See also: nl.extract
        
        EXAMPLES

                > l.extract(1, 1)
                [[1]]
                [1] 1

                > l.extract(list(A=65, B=66), "B") 
                [[1]]
                [1] 66

                > l.extract(list(A=65, B=66), list("B", 1)) 
                [[1]]
                [1] 66

                [[2]]
                [1] 65

function nl.extract

        Similar to l.extract . Also preserves the names
        attribute, if present.

function rename.some..m : names named vector named list
function rename.some..df : names named vector named list
function rename.some..l : names named vector named list

        Returns a modified copy of the vector or list (arg 1),
        where the names have been modified according to the
        matrix, data frame, or compound list (arg 2) for the
        "..m", "..df", and "..l" versions, respectively.
        
        Arg 2 has the old names in column 1 and the new names in
        column 2 for the "..m" and "..df" versions and in
        element 1 and element 2 for the "..l" version.

        Arg 2 sublists may be in any order, but the two lists
        are assumed to be parallel.

        A request to rename the same element multiple times in
        the same call is untested.

        "..l" version

                Sublists of arg 2 may contain numeric or
                character indices, or both. be lists of either
                numeric or character indices. New names will be
                converted to character names in the return.

                TIP: It may be convenient to apply unlace.l in
                building arg 2 for "..l" (see examples).

        EXAMPLE(S)

                rename.some..l(
                    list(A=1, B=2, C=3),
                    unlace.l(list("C", "SEA", 2, "BEE")))

                returns the equivalent of
                
                list(A=1, BEE=2, SEA=3)


function group.l : dictionary key-value pairs

        Given a list (arg 1) and an optional group size (arg 2,
        default n=2), returns a compound list where the m-th
        sublist contains the m-th n elements of arg 1. Example:
        group.l(as.list(1:6)) returns the equivalent of
        list(list(1, 2), list(3, 4), list(5, 6))

        group.l can be used with a single list argument to form
        a list of key-value pairs from a simple list:

                > group.l(as.list(words("KEY1 VALUE1 KEY2 VALUE2")))
                [[1]]
                [[1]][[1]]
                [1] "KEY1"
                
                [[1]][[2]]
                [1] "VALUE1"
                
                
                [[2]]
                [[2]][[1]]
                [1] "KEY2"
                
                [[2]][[2]]
                [1] "VALUE2"


function l.align.l..l

        Form aligned lists from two indexed (or KEYED) lists.

        Arguments: two pairs of parallel lists.
        
        Within each pair, the first item is viewed as a list of
        KEYS and the second item is viewed as a list of VALUES.

        Between pairs, the KEYS are viewed as equivalent if
        they *match* (as in base:match).

        Returns a list of THREE parallel lists, where the first
        contains the unique KEYS among the KEYS of *both*
        arguments, the second contains the VALUES from the first
        argument corresponding the KEYS of the return, and the
        third contains the VALUES crom the second argument
        corresponding to the KEYS of the return.

        **** ASSUMPTIONS ****
        
                Each of the arguments is a pair of parallel
                vectors or lists, perhaps the members of a data
                frame.

                The lengths of the lists BETWEEN the arguments
                may differ, but the listst WITHIN an argument
                are the same.

        ORIGINAL INTENT

                Within the first list of each parallel list, the
                items are unique.

        EXAMPLE

                the call
        
                        align(
                            list(
                                 list("ZERO", "ONE", "TWO"),
                                 list(0, 1, 2)),
                            list(
                                 list("ONE", "TWO", "THREE"),
                                 list("I", "II", "III"))
                        )

                returns the equivalent of

                    list(
                         list("ZERO", "ONE", "TWO", "THREE"),
                         list(0, 1, 2, NULL),
                         list(NULL, "I", "II", "III"))
'
