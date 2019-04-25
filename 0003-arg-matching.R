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

# ARGUMENT MATCHING

.fn.match.arg.quiet. <- function(
        argument=NULL,
        argument_name="",
        source_of_choices_FUN=function(){},
        choices=eval( #REMINDER: typeof(lformals( ---- )) == "language"
            lformals(source_of_choices_FUN)                  [[
                if (argument_name %|% nchar) { argument_name
                } else {                                1L } ]]
        ) ) {
    if (argument %|%   is.null) return (1L)
    if (argument %|% `#` != 1L) return (1L)
    if (argument %|%     is.na) return (1L)
    match(argument, choices)
}

fn.match.arg.quiet. <- function(FUN) #, argument_name="")
      EMPTY.LIST %,% FUN %=:% "source_of_choices_FUN" %v% .match.arg.quiet.

choices <- function (FUN, argument_name)
    lformals(FUN)[[argument_name]] %|% as.list %|% rest %|% unlist

match.arg.quiet <- function (FUN=nop, argument=NULL, FUN.formal.name="") {
    iwhere <- fn.match.arg.quiet.(FUN)(argument, argument_name=FUN.formal.name)
    if (iwhere %|% is.na) return (na(mode=argument %|% typeof))
    if (FUN.formal.name == HNULL) return(eval(lformals(FUN)[[1]])[iwhere])
    eval(lformals(FUN)[[FUN.formal.name]])[iwhere]
}

'
function match.arg.quiet

        Similar to match.arg, HOWEVER:

                There are THREE arguments:

                        1.  FUN, The function whose arguments
                            are being matched against.

                        2.  argument, The value of the argument
                            to be matched.

                        3.  FUN.formal.name (Optional) The name
                            of the argument (as specified by the
                            function whose arguments are being
                            tested against).
                                If FUN.formal.name is not
                            specified FUN-s first argument is
                            assumed.

        There is **** NO CHECKING **** to see that the arguments
        to match.arg.quiet are internally consistent, nor that
        they are consistent with the function within which they
        are called, if that is the case (match.arg.quiet could
        be called from outside a function).

function .fn.match.arg.quiet.
function  fn.match.arg.quiet.

        Similar to the single-argument version of match.arg,
        with some important exceptions:

                DESIGNED exceptions:

                        match.arg.quiet. returns:
        
                        The INDEX of a true match;

                        1L if the corresponding actual
                        argument  is NULL, missing, a 
                        zero-length vector, or is.na;

                        NA if the corresponding actual argument
                        does not match any of the choices.*

                ACCIDENTAL exeption:

                        match.arg.quiet. must be called in a
                        manner different from match.arg

                        A FUTURE match.arg.quiet might be called
                        in the same manner as match.arg.

        Usage Example:

                foo <- function(a=c("one", "two"), b="bee")
                        match.arg.quiet.(foo)(a)
                        # NOTE THE DOUBLE PARENS:+ (foo)(a)

                foo()               # returns 1
                foo(NULL)           # returns 1
                foo(character(0))   # returns 1
                foo(NA_character_)  # returns 1
                foo("one")          # returns 1
                foo("two")          # returns 2
                foo("three")        # returns NA
        ________

        *   Choices are explained in help(match.arg)

        +   match.arg.quiet. returns a function that returns the
            index of the match. match.arg.quiet takes a single
            argument: the function within which to compute the
            index. The function that match.arg.quiet takes a
            single argument: the actual argument for which to
            compute the index.
'
