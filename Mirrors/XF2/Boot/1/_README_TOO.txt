ADDENDUM 1: Library Trees


Library trees are specified by the libtree.dir value in the
configuration file.

In the context of ShapiroXF, a library tree is a system directory
(or folder) containing files with special names. The text
specifying the library tree is a valid argument for the
`lib.loc` parameter of the `library` function (and that is how
ShapirXF applies it):

    1.  only files with names ending in ".dat" (DAT files) will
        be processed.

        TIP: you can use non-DAT files to provide metadata,
        notes, etc.

    2.  DAT files must be named using digit characters, e.g.:
        1.dat, 2.dat, etc.

    3.  DAT files are processed in order, where the order is
        determined by transforming the leading characters into
        integer values (using R's as.integer function, so don't
        number above 2^31 - 1, approx. 2 billion)

        TIP: you may skip numbers. This allows you to go back
        later and insert another file between two existing ones.
        If things get too messy, you can renumber or make a new
        library tree.

        TIP: you can load from the same library directory
        multiple times. This technique can be useful when there
        are dependencies between library directories.

    4.  Each DAT file has the following contents:

        a.  The first line specifies an R library (a directory
            of installed packages), WITHOUT a terminating path
            separator (/).

        b.  The remaining lines are either

            i.  Comment lines (where the first character is the
                pound sign, #) or

                TIP: comment lines can be used to keep libraries
                from being loaded (e.g., for debugging).
                
            ii. Names of installed packages found in the library
                specified on the first line.

    5.  (See File Listing 1 for an example.)



ADDENDUM 2: Protopackage Specification


        The term "protopackages" is applied because the
        corresponding code might eventually be transformed into
        a formal package.


Protopackage specification is provided by the
proto.pkg.config.file value in the configuration file.

proto.pkg.config.file value should be a valid file name.

The protopackage configuration file contains one line for each
protopackage to be sourced; that line specifies the
corresponding file. See File Listing 2 for an example.

To load no protopackages, the protopackage configuration file
may contain a single line with the system null file (nul on
Windows /dev/null [I think] on *nix) specified.

FUTURE: allow for multiple protopackage specification files,
        comments in protopackage specification files, etc.



FILE LISTING 1: EXAMPLE DAT FILE


        The following example works only if the
        libraries anon, s3d7a, etc. are found at
        R:/library


**** DAT FILE BEGINS ON NEXT LINE ****
R:/library
#tRivia
anon
s3d7a
rawrw
rmALLTHE
zeroone
#
neatoCurry
neatoCurrySBOP
neatoPipeSBOP
neatoFunProgA
neatoComposeSBOP
# comment
neatoArithmetic0
**** DAT FILE ENDS AT END OF PREVIOUS LINE ****



FILE LISTING 2: EXAMPLE PROTOPACKAGE SPECIFICATION


        The following example works only if certain
        libraries have already been loaded.


**** PROTOPACKAGE SPECIFICATION FILE BEGINS ON NEXT LINE ****
//ad.sfwmd.gov/dfsroot/data/worm/Files/COMP/SOFTWARE/ShapiroXF2/Nominal1/_003/d/doc.R
//ad.sfwmd.gov/dfsroot/data/worm/Files/COMP/SOFTWARE/ShapiroXF2/Nominal1/_002/p/primitive.R
//ad.sfwmd.gov/dfsroot/data/worm/Files/COMP/SOFTWARE/ShapiroXF2/Nominal1/_001/m/misc.R
**** PROTOPACKAGE SPECIFICATION FILE ENDS AT END OF PREVIOUS LINE ****
