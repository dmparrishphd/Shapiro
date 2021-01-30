Metadata for directory

        ********************************************************
        **** IT IS INTEDED THAT THE .tsv FILES HERE WILL BE ****
        **** APPENDED TO.                                   ****
        ********************************************************

DESCRIPTION

        An index of ShapiroXF2 source files.

*.tsv FILES

        NAMES

                The names of the TSV files map to the
                subdirectories of ../../../Nominal1

        FORMAT

            tab-separated-values files [1, 2].

            Column 1: ISO Date Stamp, Modified date

            Column 2: file name

        DETAILS

                The file name contains only the least
                significant portion of the full path. To produce
                the full path, concatenate

                //ad.sfwmd.gov/dfsroot/data/worm/Files/COMP/
                SOFTWARE/ShapiroXF2/Nominal1/

                <TSV FILENAME>

                <file name>

                For example, the file 0000.tsv contains the
                record

                2020-03-04 f/fcat.cn.R

                Therefore, the full path would be

                //ad.sfwmd.gov/dfsroot/data/worm/Files/COMP/
                SOFTWARE/ShapiroXF2/Nominal1/0000/f/fcat.cn.R

                The .tsv files were initially create with via
                the source code of Appendix 1.

REFERENCES

        [1] http://www.iana.org/assignments/media-types/text/
            tab-separated-values

        [2] https://en.wikipedia.org/wiki/Tab-separated_values#
            cite_note-IANA-2

APPENDIX 1

        # R SCRIPT (EXECUTED IN VERSION 3.5.3)
        # RECORD OF INITIAL CREATION OF .tsv FILES
        #
        # DESCRIPTION
        # CREATE INITIAL CHRONOLOGICAL INDICES OF PRESENT R SOURCE
        #
        # USAGE
        # EXECUTED SEMIMANUALLY
        #
        # ARGUMENTS
        # .dir  THE PROTOPACKAGE DIRECTORY NAME (WITHOUT TERMINAL PATH SEPARATOR)
        
        .dir <- "//ad.sfwmd.gov/dfsroot/data/worm/Files/COMP/SOFTWARE/ShapiroXF2/Nominal1"
        
        PATHS <- list.dirs(.dir, recursive=F)
        
        DIRS <- sub(paste0(.dir, "/"), "", PATHS)
        
        FILENAMES <- lapply(PATHS, list.files, recursive=T, pattern="*[.]R$")
        
        mdate <- function(s)
        
        INFO <- `names<-`(
	        lapply(
		        seq_along(FILENAMES),
		        function(k) {
			        vapply(
				        FILENAMES[[k]],
				        function(Name) c(mdate(paste0(PATHS[k], "/", Name)), Name),
				        character(2) ) } ),
	        DIRS)
        
        for (k in seq_along(INFO)) {
	        TABLE <- INFO[[k]]
	        Outfilename <- paste0(sub("/Nominal1", "", .dir, fixed=T), "/Indices/Chrono/1/", DIRS[k], ".tsv")
	        write(TABLE[,order(TABLE[1,])], file=Outfilename, ncolumns=2, sep="\t")
        }
        
