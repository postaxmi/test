#' Excerpt from the latest version of the tidytext source (18/06/2018), including the CTM implementation of the tidy function
#'
#' @name lda_tidiers
#'
#' @export
tidy.CTM <- function(x, matrix = c("beta", "gamma"), log = FALSE, ...) {
  tidy_topicmodels(x = x, matrix = matrix, log = log, ...)
}

tidy_topicmodels <- function(x, matrix = c("beta", "gamma"), log = FALSE, ...) {
  matrix <- match.arg(matrix)
  if (matrix == "gamma") {
    mat <- x@gamma
  } else {
    mat <- x@beta
  }

  ret <- reshape2::melt(mat) %>%
    tbl_df()

  if (matrix == "beta") {
    ret <- transmute(ret, topic = Var1, term = x@terms[Var2], beta = value)
  } else {
    ret <- transmute(ret, document = Var1, topic = Var2, gamma = value)
    if (!is.null(x@documents)) {
      ret$document <- x@documents[ret$document]
    }
  }

  if (matrix == "beta" && !log) {
    ret[[matrix]] <- exp(ret[[matrix]])
  } else if (matrix == "gamma" && log) {
    ret[[matrix]] <- log(ret[[matrix]])
  }
  ret
}
