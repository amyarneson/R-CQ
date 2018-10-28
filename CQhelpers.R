### Some helper functions for writing CQ input files
## author: Amy Arneson

# this function makes a scoring statement for RECODING data to be consective
#   integers starting at 0 for each item
makeCQkey <- function(scores0, writeOut = FALSE, filePre = NULL) {
  CQkey <- apply(scores0, 2, function(X) { as.character(sort(unique(X))[1:6]) })
  CQkey[is.na(CQkey)] = "X"
  CQkey = data.frame(col1 = rep("key ", nrow(CQkey)), CQkey)
  CQkey$col2 <- paste0(" ! ", 0:5, ";")

  if (writeOut) {
    if (is.null(filePre)) {
      stop('provide filePre')
    } else {
      write.table(CQkey, paste0(filePre, "KEYSTATEMENT.txt"), sep = "",
                  quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
  }

  CQkey
}

# this function creates a "blank" scoring statement for each item with a
#   specified number of dimensions. I've been filling in the appropriate
#   dimensions by hand
makeCQscoreState <- function(scores0, ndim, writeOut = FALSE, filePre = NULL) {
  scoreState <- c()

  for (i in 1:ncol(scores0)) {
    # use table() instead of unique() to avoid NA problem
    scoreState[i] <- paste0("score (", paste(0:(length(table(scores0[,i]))-1),
                                             collapse = ","),
                            ") ", paste0(rep("()", ndim), collapse = " "),
                            " ! items(", i, ");")
  }

  if (writeOut) {
    if (is.null(filePre)) {
      stop('provide filePre')
    } else {
      write.table(print(paste0(scoreState, collapse = "\n")),
                  paste0(filePre, "SCORESTATEMENT", ndim, ".txt"),
                  quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
  }

  print(scoreState)
}

# I haven't written a function for creating the data matrix, but here is some
#   code that might help you 1) get all student IDs the same length with a
#   space before the scores and 2) write scores without delimiters
row.names(scores) = stringr::str_pad(row.names(scores), 6, "right", "X")
row.names(scores) = stringr::str_pad(row.names(scores), 7, "right", " ")
write.table(scores, "CQscores.txt", quote = FALSE, na = ".",
            col.names = FALSE, sep = "")
