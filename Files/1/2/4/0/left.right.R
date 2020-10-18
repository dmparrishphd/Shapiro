left.right <- function(character_, stop.left=1) {
    unleft %<=% character_ %-|% stop.left -> rights
      left %<=% character_ %-|% stop.left -> lefts
    rbind_ %<=% lefts %-|% rights }
