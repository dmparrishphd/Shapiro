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
# R Script
# Steps
#
# Functions to facilitate the plotting of stairstep functions.

.stepi <- function (n) n %,% succ(n) ###
.stepj <- function (n) n %,% n ###
.steps <- function (FUN, n) unlist(lapply(seq(n), FUN)) ###

stepsi <- succ %O% seq %O% rep2 %O% sort %O% inner
stepsj <- function (n) .steps(.stepj, n) #.#

stepsx <- function (x) x[x  % %  length  % %  pred  % % stepsi]
stepsy <- function (v) v[v  % %  length  % %  stepsj] #.#

ipairs.n <- function (n)
        n %|% stepsi %mod1% n %|% matrix2r

pairs.n <- ipairs.n #DEPRECATED USE ipairs.n

    Doc$ipairs.n <- '
        ipairs.n returns a 2-row matrix of sequential pairs of
        indices into a structure of n items.

        Originally intended to be used as indices into a
        polygon. Pairs of points may represent the sides of a
        polygon.'
'

function *step*

        A family of functions intended for use in plotting a
        sequence of ordered pairs as a piecewise constant
        (a.k.a. stairstep) function, where there are no vertical
        lines at the boundary of the plot.

function .stepi
function .stepj
function .steps

        Helper functions for stepsi and stepsj

function stepsi 
function stepsj

        Each returns an integer vector of indices into the x- or
        y- components of a sequence of ordered pairs.

        > stepsi(3)
        [1] 1 2 2 3 3 4
        > stepsj(3)
        [1] 1 1 2 2 3 3
        > # you may wish to begin your plot at x = zero:
        > plot(pred(stepsi(3)), stepsj(3), type="l")

function stepsx : step plot steps-y
function stepsy : step plot steps-y

        Each returns a vector of values intended for plotting
        step plots. In such an application, the length of the
        "x" vector should be one more than the length of the
        "y" vector. Example:

        > stepsx(seq(4))
        [1] 1 2 2 3 3 4
        > stepsy(c(1,3,2))
        [1] 1 1 3 3 2 2

        > plot(stepsx(seq(4)), stepsy(c(1,3,2)), type="l")
'
