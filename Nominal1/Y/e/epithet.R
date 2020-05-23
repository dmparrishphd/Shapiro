epithet <- function()
	    match.call(
		    definition=sys.function(-1),
    		call=sys.call(-1),
	    	expand.dots=F) %|% first %|% as.character %|% first

