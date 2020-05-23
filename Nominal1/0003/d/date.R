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



is_midnight.Date <- function (date) {
    dd <- as.double %-|% date
    dd %|% floor == dd }

    Doc$is_midnight.Date <- '
        is_midnight.Date returns a logical vector indicating
        whether the corresponding elements of the Date argument
        correspond to midnight.'

MONTHS <-   "JANUARY FEBRUARY MARCH APRIL MAY JUNE JULY AUGUST
            SEPTEMBER OCTOBER NOVEMBER DECEMBER" %|% words

MONTHS3 <- left(MONTHS, 3)

MONTHS1 <- left(MONTHS, 1)

months_ <- function (style=NULL) {
    if (style %|% is.null) return(MONTHS)
    if (style == 1) return(MONTHS1)
    if (style == 3) return(MONTHS3)
    if (style %|% typeof != "" %|% typeof) return (MONTHS)
    if (style %|% nchar == 1) return(MONTHS1)
    if (style %|% nchar == 3) return(MONTHS3)
    MONTHS }

h.digits.h <- (gsub %<=% REGEX.ANY.NONDIGIT %<=% HSPACE) %O% words

        Doc$h.digits.h <- '
            h.digits.h returns a character vector of consecutive
            digits interpreted from the character string
            argument.'

mh.digits.h <- function(h) vapply(h, h.digits.h, h.digits.h(h[1]))

lh.digits.h <- lapply % % argswap %<=% h.digits.h

i.digits.h <- h.digits.h %O% as.integer

        Doc$i.digits.h <- crunch.h('
            i.digits.h returns an integer vector of values
            interpreted from the character string argument.')

li.digits.h <- lapply % % argswap %<=% i.digits.h

        Doc$li.digits.h <- crunch.h('
            li.digits.h returns a list of integer vectors
            interpreted from the character vector argument.')

#TODO USE A vector INTERMEDIATE RATHER THAN A LIST.
#TODO WHY DOESN'T { vapply % % argswap %<=% i.digits.h % % argswap %<=% integer(3) } WORK?
m.datetext.h <- li.digits.h %O% unlist %O% (matrix %^% list(ncol=3, byrow=T))

        Doc$m.datetext.h <- crunch.h('
            m.datetext.h returns a three-column integer matrix
            of values interpreted from the character vector
            argument. An element of the argument might be
            "1985-11-05" or "1999/1/1"')

m.ymdgreg.h <- function (date, offsets=0L) {
    lapply( as.character(date  % %  as.Date + offsets), strsplit, "-")  % %
            l.flatten.l  % %  unlist  % %  as.integer  % %
            ("nrow" %=% 3L %=>% matrix)  % % t }

isodate.year <- function(year)
        year % % as.character %//% "-01-01"

isodates.mymd <- function (mymd) paste(
    pad.h(mymd[,1], n=4, hpad="0"), "-",
    pad.h(mymd[,2], n=2, hpad="0"), "-",
    pad.h(mymd[,3], n=2, hpad="0"), sep=HNULL)

as_Date.origin.default <- function () as.Date("1970-01-01")

as_DateR.d <- function (x, ...) # is.numeric(x) == T
        as.Date(x, as_Date.origin.default(), ...)

as_DateR.DateMSO.d <- function (d, ...)
    vapply(
        d,
        function(x) as_DateR.d(
            if (x < 1) NA else
            if (x < 60)
                    as.Date("1899-12-31", ...)  % %  as.double + x else
            if (x < 61) NA else
                    as.Date("1899-12-30", ...)  % %  as.double + x),
        as_DateR.d(1)) % % as_DateR.d


JD.as.Date.origin <- function () 2440588

JD.greg.start <- function () 2299161 

.Date.JD.warn <- function (jd)
        if (jd < JD.greg.start()) warning ("Date occurs" % //%
        " before the beginning of the Gregorian Calendar.")

JD.Date <- function (date) {
    jd <- as.double(date) + JD.as.Date.origin(T)
    .Date.JD.warn(jd)
    jd }

Date.JD <- function (jd)
{   warning("needs testing after implemented with as_DateR.d")
    date <- as_DateR.d(jd - JD.as.Date.origin())
    .Date.JD.warn(jd)
    jd }

mh.ymd.Dates <- mh.digits.h

hyr.Dates <- mh.digits.h %O% (row.m  % %  argswap %<=% 1)
hmo.Dates <- mh.digits.h %O% (row.m  % %  argswap %<=% 2)
hda.Dates <- mh.digits.h %O% (row.m  % %  argswap %<=% 3)

iyr.Date <- hyr.Dates %O% as.integer
imo.Date <- hmo.Dates %O% as.integer
ida.Date <- hda.Dates %O% as.integer

iyr.Dates <- function (Dates1)
        vapply(Dates1, iyr.Date, integer(1))  % % rename.all

'
.dst.ygreg.data #OLDNAME was function returning character vector
'
.DST.YGREG.DATA <- '
        1999-04-04 1999-10-31
        2000-04-02 2000-10-29

        2001-04-01 2001-10-28
        2002-04-07 2002-10-27
        2003-04-06 2003-10-26
        2004-04-04 2004-10-31
        2005-04-03 2005-10-30

        2006-04-02 2006-10-29
        2007-03-11 2007-11-04
        2008-03-09 2008-11-02
        2009-03-08 2009-11-01
        2010-03-14 2010-11-07

        2011-03-13 2011-11-06
        2012-03-11 2012-11-04
        2013-03-10 2013-11-03
        2014-03-09 2014-11-02'  % %
        hh  % %  (matrix %^% list(ncol=2, byrow=T))

DST.DATES <- data.frame(
	START=col.m(.DST.YGREG.DATA, 1)  % %  as.Date,
	END  =col.m(.DST.YGREG.DATA, 2)  % %  as.Date,
	row.names=do.call(
		seq,
		rename.all(
			lapply(
				.DST.YGREG.DATA  % %  bookends,
				(left % % argswap %<=% 4) %O% as.integer),
			"from to"  % %  hh)))

'#REMOVED : use DST.DATES table instead

.dst.ygreg.selection <- function (v) #REMOVED
    as_DateR.d(lapply(.dst.ygreg.data(), as.Date)[v]  % %  unlist)

.dst.ygreg.starts <- function () #REMOVED
        .dst.ygreg.selection(T %,% F)

.dst.ygreg.ends <- function () #REMOVED
        .dst.ygreg.selection(F %,% T)

.dst.ygreg.years <- function () #REMOVED
{   w <- .dst.ygreg.data()
    substr(w[1], 1, 4)  % %  as.double -> orig
    seq(from=orig, length.out=(.dst.ygreg.starts() % % `#`)) }

dst.ygreg <- function (years) #REMOVED
{   i <- vapply(years, function(x) which(.dst.ygreg.years()==x), FUN.VALUE=integer(1))
    data.frame(DST_START=.dst.ygreg.starts()[i],
               DST_END=.dst.ygreg.ends()[i]) }

iyr.dst.ygreg.data.range <- function () #REMOVED
    matrix(.dst.ygreg.data(), ncol=2, byrow=T)[,1]  % %  iyr.Dates

dst.Dates <- function () { #REMOVED
        iyr <- iyr.dst.ygreg.data.range()
        df1 <- dst.ygreg(iyr)
        row.names(df1) <- iyr
        df1 }
'

is_dst.Date <- function(Dates, dst.dates=DST.DATES) {
	rows <- DST.DATES[Dates  % %  hyr.Dates,]
	rows % % row.names % % as.integer % % as.logical &
	rows$START <= Dates &
	Dates < rows$END }

iymd.Date <- function (Date1)
        iyr.Date(Date1) %,% imo.Date(Date1) %,% ida.Date(Date1)

dfymd.Dates <- function (Dates)
{   df1 <- data.frame(t(vapply(Dates, iymd.Date, integer(3))))
    names(df1)  <- "YR MO DA"  % %  words.h
    df1 }

part.Date <- function(start, stop, Dates)
        Dates %|% as.character %|% (
            (substr %|% argswap %<=% start) %|%
            argswap %<=% stop) %|% as.integer

year.Date  <- part.Date %<=% 1 %<=%  4 # function
month.Date <- part.Date %<=% 6 %<=%  7 # function
day.Date   <- part.Date %<=% 9 %<=% 10 # function

    Doc$year.Date <- '
        year.Date returns the year (as integer) of a given Date.'

    Doc$month.Date <- '
        month.Date returns the month (as integer) of a given Date.'

    Doc$day.Date <- '
        day.Date returns the day (as integer) of a given Date.'


df.hDates <- function (h.Date.start="1970-01-01", h.Date.end="1999-12-31") {
    Year.start <- h.Date.start  % %  as.Date  % %  year.Date
    df1 <- data.frame(IDATE=as.Date(h.Date.start):as.Date(h.Date.end))
    df1 <- cbind(df1, RDATE=as_DateR.d(df1$IDATE))
    df1 <- cbind(df1, HDATE=as.character(df1$RDATE))
    df1 <- cbind(df1, IYR=year.Date(df1$RDATE))
    df1 <- cbind(df1, IMO=month.Date(df1$RDATE))
    df1 <- cbind(df1, IDA=day.Date(df1$RDATE))
    df1 <- cbind(df1, JYR=df1$IYR - Year.start + 1L)
    df1 <- cbind(df1, JMO=(12L*(df1$IYR - Year.start) + df1$IMO))
    df1 <- cbind(df1, JDA=row.names(df1)  % %  as.integer)
    df1[,"IDATE RDATE IYR IMO IDA JYR JMO JDA"  % %  words.h] }



'
function .dst.ygreg.data
function .dst.ygreg.selection
function .dst.ygreg.starts
function .dst.ygreg.ends
function .dst.ygreg.years
function dst.ygreg : daylight savings time

        These functions are tightly coupled.

function .dst.ygreg.data

        Helper data for .dst.ygreg function

        Returns a character string representing alternating
        start- and end-dates of US Daylight Savings Time. The
        format is ISO date (Gregorian Calendar).

function dst.ygreg : daylight savings time

        Given a year (arg 1, single-value double or integer
        vector), returns a data frame with the corresponding
        start and end dates of DST for that year.
        

function m.ymdgreg.h : dates gregorian calendar year month day

        Given a representation of a date (arg 1; i.e.,
        "1776-07-04"), convertible to a Date, and an optional
        vector of offsets (effectively converted to integer
        vector), returns a matrix whose columns contain the
        year, month, and day (Gregorian Calendar) of the
        corresponding Dates. Examples:

        > m.ymdgreg.h("1776-07-04")
             [,1] [,2] [,3]
        [1,] 1776    7    4

        > m.ymdgreg.h("1776-07-04", seq(7))
             [,1] [,2] [,3]
        [1,] 1776    7    5
        [2,] 1776    7    6
        [3,] 1776    7    7
        [4,] 1776    7    8
        [5,] 1776    7    9
        [6,] 1776    7   10
        [7,] 1776    7   11

function as_DateR.d

        same as as.Date(x, origin, ...) where origin is
        as_Date.origin.default(). Reminder: x is numeric.

function isodates.mymd

        Returns the ISO dates (character strings of the form
        "YYYY-MM-DD") for each of the dates represented in the
        integer, double, or character matrix argument. Each row
        of the matrix corresponds to one date. The first three
        columns of the matrix correspond to the integtral-valued
        year, month, and day respectively.

function iyr.Dates

        Returns a vector of years given a vector of Date-s.

function iyr.dst.ygreg.data.range

        Returns the years for which the DST table is populated.

function is_dst.Date

        Given a vector of Date-s, returns a logical vector
        indicating whether Daylight Savings Time is in effect
        for the corresponding Date-s.

function JD.as.Date.origin

        Returns the Julian Day number (integer-valued double)
        associated with the beginning of the default Epoch used
        by R (1970-01-01).

function JD.greg.start

        Returns the Julian Day number (integer-valued double)
        corresponding with the beginning of the Gregorian
        Calendar.

function isodate.year

        Returns an ISO Date (YYYY-MM-DD) given the year as
        integer, double, or character

'
