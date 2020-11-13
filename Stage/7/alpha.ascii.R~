.ALPHA.ASCII <- ' SP
!EX
"QO
#NO
$DL
%PC
&AM
\'AP
(LP
)RP
*AS
+AD
,CM
-HY
.PD
/SF
:CL
;SC
<LT
=EQ
>GT
?QE
@AT
[LB
\\SR
]RB
^CX
_LN
`GR
{LC
|LV
}RC
~TL'

ALPHA.ASCII <- (function() 
    SPLIT <- strsplit(strsplit(.ALPHA.ASCII, "\n")[[1]], "")
    `rownames<-`(
        matrix(vapply(
            SPLIT,
            function(x) x[1], "")),
        unlist(lapply(
            lapply(
                SPLIT,
                function(x) x[2:3]),
            paste,
            collapse="") ) ) } )()
