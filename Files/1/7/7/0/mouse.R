# Shapiro: A Handsome Helper for R
# Copyright (C) 2019 D. Michael Parrish
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

#234567890123456789012345678901234567890123456789012345678901234-----0-2-------*

GRAPHICSEVENTENV.PROTO <- list(
	.Events=list(),
	push=function(., which = dev.cur()) {
		.$.Events <- c(
			list(getGraphicsEventEnv(which)),
			.$.Events)
		. },
	pop=function(.) {
		.$.Events <- .$.Events[-1]
		. },
	top=function(.) .$.Events[1],
	.NULL=NULL )

'
foo <- GRAPHICSEVENTENV.PROTO
foo <- foo$push(foo)
foo
'


.onMouse <- function(buttons, x, y)
		list(BUTTONS=buttons, XY=c(x, y))

.onMouseDown <- c %<=% list(ACTION="DOWN") %-O% .onMouse


#setGraphicsEventHandlers.


.onMouseButton <- list(
	onMouseDown=.onMouse,
	onMouseUp=.onMouse)

xy.mouse <- function() {
	mouseup <- function(buttons, x, y) c(x, y)
	keybd <- function(key) NA_real_ %|% rep2
	getGraphicsEvent(
		prompt="",
		onMouseUp=mouseup,
		onKeybd=keybd) }

xyin.mouse <- function()
	xy.mouse() * par("din")

usr.mouse <- function()
	(xyin.mouse() - par.mai()[2:1]) * par.usr.per.in() + par.usr()[c(1, 3)]

iusr.mouse <- usr.mouse %O% ceiling %O% as.integer

lpt.mouse <- function() {
	lp <- list()
	repeat {
		p <- usr.mouse()
		if (p[1] %|% is.na) return (lp)
		lp[[lp %|% `#` + 1]] <- p } }

pp.mouse <- lpt.mouse %O% cbind_l

ipp.usr.mouse <- pp.mouse %O% ceiling %O% as_integer.storage.mode

line.mouse <- function(
        line.only=T, .round=F, roundFUN=ceiling %O% as_integer.storage.mode) {
    PP <- pp.mouse() %|% (if (.round) roundFUN else identity)
    LN <- PP %|% line.pp
    if (line.only) LN else list(LN, PP) }

