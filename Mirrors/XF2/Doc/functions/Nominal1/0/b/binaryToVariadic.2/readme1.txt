binaryToVariadic.2.R

Design/Intent

        Defines binaryToVariadic function.

        to supersede binaryToVariadic.R.

Example

    `&&&` <- binaryToVariadic(`&`)
    i <- with(iris, `&&&`(
	    as.character(Species) == "setosa",
    	Sepal.Length < 5,
	    Sepal.Width < 3,
    	Petal.Length > 1))
    iris[i,]

    #    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    # 9           4.4         2.9          1.4         0.2  setosa
    # 42          4.5         2.3          1.3         0.3  setosa
