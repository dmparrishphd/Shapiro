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


list(
    axis.x=function(.,
                    tick.length.month=par.usr.line.height()/2,
                    tick.length.year=par.usr.line.height()*3/2) {
        plot_month.labels()
        plot_year.labels()
        ticks.months.x(tick.length=tick.length.month)
        ticks.years.x(tick.length=tick.length.year)
    },
    .NULL=NULL
)
