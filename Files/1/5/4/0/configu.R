(function(mirrors="~/Mirrors/") {
    `%//%` <- paste0

    words <- function(x) strsplit(x, " ")[[1]]

    MIRRORS <- mirrors

    DMP <- "GitHub/dmparrishphd/"

    NEATOVERSEF <- MIRRORS %//% DMP %//% "neatOveRse/Files/"
    NEATOVERSEP <- MIRRORS %//% DMP %//% "neatOveRse/Pkgs/"

    CLICK <- MIRRORS %//% DMP %//% "click/Files/"

    TRIVIA <- MIRRORS %//% DMP %//% "tRivia/Files/"

    SHAPIRO <- MIRRORS %//% DMP %//% "Shapiro/master/Files/"
    
    source(NEATOVERSEF %//% "4/0/" %//% "build.env.build.R"
        )[[1]](MIRRORS)

    attach(name="clickEnv",
        what=build.env(path=CLICK, subdirs="3/0/"))

    attach(name="clicksEnv",
        what=build.env(path=CLICK, subdirs="4/0/",
        parent.name="clickEnv"))

    attach(name="zeroOneEnv", what=build.env(
        path=TRIVIA, subdirs="1/0"))

    attach(name="zeroOneTwoEnv", what=build.env(
        path=TRIVIA, subdirs="8/0"))

    attach(name="heaviside", what=build.env(
        path=TRIVIA, subdirs="1/2/0"))

    attach(name="funProgEnv", what=build.env(
        path=NEATOVERSEP,
        subdirs=words("1/0 2/0 3/0 4/0 5/0 6/0 1/1/0")))

    attach(name="ordinalEnv", what=build.env.from.files(
        parent.name="funProgEnv", path=SHAPIRO, files=c(
            "1/4/1/0/first.R",
            "5/1/0/rest.R",
            "5/2/0/second.R",
            "1/2/3/0/last.R",
            "2/3/0/except.last.R")))

    attach(name="nestEnv", what=build.env.from.files(
        parent.name="package:stats", path=SHAPIRO, files=c(
            "3/3/0/depth.nest.R",
            "3/4/0/is.nest.R",
            "3/4/0/unnest.R")))

    invisible()
})()
