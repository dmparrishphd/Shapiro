**** THIS README FILE WILL BE UPDATED WITH FACTUAL INFORMA- ****
**** TION ABOUT ITS SUBJECT.                                ****

Record of Inclusion of NeattO as one-file-per-assignment

Note

        NeatO is located at ../../../R/PkgSrc/DMP/

2020-04-03, -04

        The following R files are found in the NeatO directory tree

                "curry:"        neatoCurry/neatoCurry.1/R/neato.curry.R
                "SBOP/curry":   neatoCurrySBOP/neatoCurrySBOP.1/R/neato.curry.sbop.R
                "pipe:"         neatoPipeSBOP/neatoPipeSBOP.1/R/neato.pipe.sbop.R
                "arith:"        neatoArithA/neatoArithA.1/R/neato.arithA.R
                "fp:"           neatoFunProgA/neatoFunProgA.1/R/neatoFFFF.fun.prog.A.R
                "SBOP/compose:" neatoComposeSBOP/neatoComposeSBOP.1/R/neato.compose.SBOP.R

        "curry"

                The R file contains only one assignment. Rather
                than split-copy the file, it is incorporated by
                reference:

                ../../Indirect/R/Z/c/curry.dat

        1.  "SBOP/curry" (Defines `%<=%`, `%=>%`, `%v%`, and `%^%`)
        2.  "pipe" (Defines `%|%`, `%||%`, `%:|%`, and `%:||%`)
        3.  "arith" (Defines pred, succ, `%mod1%`)
        4.  "fp" (Defines argswap, un)
        5.  "SBOP/compose" (Defines `%O%` and `%:O%`)

                Split-copied to

                    1.  Nominal1/Z/PC/PC.LT.EQ.PC.R
                        Nominal1/Z/PC/PC.EQ.GT.PC.R
                        Nominal1/Z/PC/PC.v.PC.R
                        Nominal1/Z/PC/PC.CX.PC.R

                    2.  Nominal1/Z/PC/PC.VL.PC.R
                        Nominal1/Z/PC/PC.VL.VL.PC.R
                        Nominal1/Z/PC/PC.CO.VL.PC.R
                        Nominal1/Z/PC/PC.CO.VL.VL.PC.R

                    3.  Nominal1/W/PC/PC.mod1.PC.R
                        Nominal1/Y/s/succ.R
                        Nominal1/Y/p/pred.R

                    4.  Nominal1/Z/a/argswap.R
                        Nominal1/Z/u/un.R

                    5.  Nominal1/Y/PC/PC.O.PC.R
                        Nominal1/Y/PC/PC.CO.O.PC.R
