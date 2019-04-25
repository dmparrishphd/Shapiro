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

n.days.ygreg <-
        "-01-01 -12-31" %|% words %=>% prefix %O% as.Date %O%
                diff %O% succ %O% as.integer

nn.days.ygreg <- function (ygreg) # days per year
        vapply(ygreg,   n.days.ygreg,   1970 %|% n.days.ygreg)

years.Dates  <- function (Dates)
    vapply(Dates,   year.Date,   1970 %|% year.Date)

Dates.time.interval.Dates <- range %O% as.Date

years.time.interval.Dates <- Dates.time.interval %O% year.Date

Dates.time.interval.ygreg <- function (ygreg) {
    yy <- ygreg %|% range %|% years.Dates;   c(
        yy %|% first           %//% "-01-01" %|% as.Date,
        yy %|% second %|% succ %//% "-01-01" %|% as.Date) }

daycount.time.interval.ygreg <- Dates.time.interval.ygreg %O% diff

start_Date.ygreg <- function (ygreg) ygreg %//% "-01-01" %|% as.Date

ygreg.periods.Date <- function (Date) {
    yr <- Date %|% year.Date
    if (Date == yr %|% start_Date.ygreg) yr + .lo else yr %|% rep2 }

ygreg.period.early.Date <- ygreg.periods.Date %O% first
ygreg.period.late.Date  <- year.Date

ygreg.inclusive.range.Dates <- function (Dates) {
    rng <- Dates %|% range
    (        rng %|% first  %|% ygreg.period.late.Date ) %,%
            (rng %|% second %|% ygreg.period.early.Date) }

ygreg.range.Dates <- Dates.time.interval.Dates %O% year.Date

ygregs.inclusive.range.Dates <- ygreg.range.Dates %O% i.seq.range

nn.days.inclusive.ygreg <- range %O% i.seq.range %O% nn.days.ygreg

n.days.inclusive.ygreg <- nn.days.inclusive.ygreg %O% sum

newyearsday.ygreg <- function(ygreg)
        prefix("-01-01", ygreg) %|% as.Date

newyearsdays.Date <- function(dates) {
    r <- range(dates)
    nyd <- r %|% ygregs.inclusive.range.Dates %|% newyearsday.ygreg
    nyd[r %|% first <= nyd & nyd <= r %|% second]
}

midyears.ygreg <- function(ygreg) #TAGS Date
	    ygreg %|% newyearsday.ygreg + 366/2

midmonths.ygreg <- function(ygreg) #TAGS seq Date
        seq.Date(
            from=ygreg %|% min %|% newyearsday.ygreg + 15,
            to  =ygreg %|% max %|% newyearsday.ygreg - 15,
            by="month")

firstofmonths.Dates <- function (dates) {
    d <- (dates %|% range %|% year.Date + ol) %|% newyearsday.ygreg
    clip_sift(
        seq.Date(from=d[[1]], to=d[[2]], by="month"),
        min_=dates %|% min,
        max_=dates %|% max) }

month.labels.ygreg <- function (ygreg, style=NULL)
		rep(
            months_(style),
            ygreg %|% range %|% diff %|% succ)

    Doc$month.labels.ygreg <- '
            month.labels.ygreg returns a character vector whose
            members repeat a sequence of 12 month labels for
            each the range of years specified. The style
            argument is interpreted according to months_.'

plot_year.labels <- function (
        y=y.center.line.number.below.x.axis(2),
        null=NULL,
        ...) {
    x <- par.usr.x() %|% as_DateR.d %|%
            ygregs.inclusive.range.Dates %|% midyears.ygreg %|%
            clip_sift.par.usr.x
    text.axis.x(x=x, y=y, labels=x %|% year.Date, ...)
}

    Doc$plot_year.labels <- '
        plot.year.labels: Assuming that the values on the x-axis
        are aligned with Dates defaults (i.e., the date origin),
        plots the corresponding year labels below the x-axis.'

    '
    windows(); par(bg=BLK, fg=DGR); par.col.dot(DGR)
    image(x="2001-01-01 2011-01-01" %|% hh %|% as.Date, y=ol,
          z=M1, col=TRANSPARENT, xaxt="n")
    plot_year.labels(col=BRN)
    plot_month.labels(col=GRN, cex=1/2)
    ticks.years.x()
    '

plot_month.labels <- function (
        y=y.center.line.number.below.x.axis(),
        months_style="J",
        null=NULL,
        ...) {
    x <- par.usr.x() %|% as_DateR.d %|% year.Date %|%
            midmonths.ygreg %|% clip_sift.par.usr.x
    text.axis.x(
        x=x,
        y=y,
        labels=rep_along(months_style %|% months_, x),
        ...) }

ticks.years.x <- function (...)
        ticks.x(clip_sift(
            par.usr.x() %|% as_DateR.d %|%
                    ygregs.inclusive.range.Dates %|%
                    newyearsday.ygreg,
            min_=par.usr.x1(),
            max_=par.usr.x2()), ...)

ticks.months.x <- function (...)
        ticks.x(
            par.usr.x() %|% as_DateR.d %|% firstofmonths.Dates,
            ...)

    Doc$n.days.ygreg <-
            "n.days.year returns the number of days in a given
            year as a Time difference."

    Doc$nn.days.ygreg <-
            "nn.days.ygreg is a vectorized version of
            n.days.year.  The argument is a vector of any number
            of years."

    Doc$year.Dates <-
            "year.Dates is a vectorized version of year.Date."
          
    Doc$Dates.time.interval.Dates <-
            "Dates.time.interval.Dates gives the first and last
            date among those of the vector argument of Dates."

    Doc$years.time.interval.Dates <-
            "years.time.interval.Dates returns the first and
            last year associated with the given Dates."

    Doc$Dates.time.interval.ygreg <-
        "Dates.time.interval.ygreg returns the beginning and end
        dates (viewed as instances in time, YYYY-01-01T00:00) of
        the time interval represented by the year or years
        given."

    Doc$days.time_span.ygreg <-
            "days.time_span.ygreg gives the number of days from
            the BEGINNING of the earliest year given by the
            vector argument to the END of the last year given by
            the vector argument."

    Doc$daycount.time.interval.ygreg <-
            "daycount.time.interval.ygreg returns the number of
            days in the given year or during the timespan of the
            earliest and latest years given."

    Doc$ygreg.periods.Date <-
            "ygreg.periods.date returns a 2-vector of Gregorian
            year(s) in which the date occurs. The date is viewed
            as an instant in time, so a date falling at the
            beginning of the year also falls at the end of the
            previous year."
    
    Doc$ygreg.period.early.Date <-
            "ygreg.period.early.date returns the first Gregorian
            year (an integer) during which the Date argument
            occurs. The date argument is viewed as an instant in
            time, so, e.g., an argument of '2000-01-01' returns
            1999L."
            
    Doc$ygreg.period.late.Date <-
            "ygreg.period.late.date returns the last Gregorian
            year (an integer) during which the Date argument
            occurs. Should have the same effect as year.Date."

    Doc$ygreg.inclusive.range.Dates <- 
            'ygreg.range.dates returns the range of Gregorian
            years (viewed as entire calendar years---12 months
            in duration) during which the given dates occur.
            Dates are considered to be instants in time, and
            years are considered to include the last instant of
            time so, e.g., ygreg.range.dates(as.Date(c("2000-01-01",
            "2001-01-01"))) returns c(2000L, 2000L).'

    Doc$ygregs.inclusive.range.Dates <-
            'ygregs.range.dates returns the sequence of Gregorian
            years (viewed as entire calendar years---12 months
            in duration) during which the given dates occur.
            Dates are considered to be instants in time, and
            years are considered to include the last instant of
            time so, e.g., ygreg.range.dates(as.Date(c("2000-01-01",
            "2001-01-01"))) returns 2000L.'

