#' Display Arguments of a Function in \code{rmd}
#' 
#' Function to return arguments of a \code{Rhr*}-Object as \code{data.frame} or \code{rmd} table.
#' @param x Object of class \code{Rhr*}.
#' @param as.rmd Logical value, indicating if result should be coerced to \code{rmd}. 
#' @param includeName Logical value indicating if the estimator name should be included or not.
#' @export
rhrPrettyArgs <- function(x, as.rmd = TRUE, includeName = FALSE, ...) {
  UseMethod ("rhrPrettyArgs", x)
}

#' @export
rhrPrettyArgs.default <- function (x, as.rmd = TRUE, ...) {
  paste0 ("rhrPrettyArgs is not defined for object of class ", class(x))
}
