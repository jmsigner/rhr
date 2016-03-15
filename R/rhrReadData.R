#' Wrapper around \code{readLines} to read data with different field separators and run some checks, mainly used for the GUI
#'
#' @title read separator; delimited text files
#' @param file A character; the file name
#' @param sep character; the separator
#' @param skip numeric; the number of lines to skip
#' @param hasHeader logical; indicates whether or not a header is presence
#' @param sepDec character; indicating the decimal separator
#' @param stringsAsFactors logical; indicating whether or not strings should be read as factors.
#' @param filename character: if file and filename differ (e.g., when called through shiny)
#' @param ... passed to scan
#' @details ...
#' @return object of class \code{data.frame}
#' @export
#' @examples 
#' \dontrun{
#' data(datSH)
#' write.csv(datSH, file.path(tempdir(), "file1.csv"), sep = ",", dec = ".")
#' dat <- rhrReadData(file.path(tempdir(), "file1.csv"), sep = ",", sepDec = ",")
#' head(dat)
#' rhr2md(dat)
#' }
rhrReadData <- function(file, sep=",", skip=0, hasHeader=TRUE, sepDec=".", stringsAsFactors=FALSE, filename = NULL, ...) {
  
  if (!sep %in% c("|", "\\|", ";", ",", "\t")) {
    stop("rhrReadData: unknown field separator")
  }

  ## Account for pipe as sep
  if (sep == "|") {
    sep <- "\\|"
  }

  if (!sepDec %in% c(".", ",")) {
    stop("rhrReadData: unknown decimal separator")
  }

  if (file.exists(file)) {
    fileRaw <- read.table(file, header=hasHeader, dec=sepDec, sep=sep,
                          stringsAsFactors=FALSE, ...)
  } else {
    stop("rhrReadData: file does not exist")
  }


  if (skip >= nrow(fileRaw)) {
    stop("rhrReadData: you want to skip more lines that the data has")
  }

  if (!is.logical(hasHeader)) {
    stop("rhrReadData: hasHeader is expected to be logical")
  }

  if (!is.numeric(skip)) {
    stop("rhrReadData: skip is expected to be numeric")
  }
    
  if (skip < 0) {
    stop("rhrReadData: skip is expected to be positive")
  }

  ## discard lines to be skipped
  if (skip != 0) {
    fileRaw <- tail(fileRaw, -skip)
  }
  
  attr(fileRaw, "rhrReadData") <- list(file = file, sep = sep, sepDec = sepDec, skip = skip, hasHeader = hasHeader, 
                                       filename = filename)
  class(fileRaw) <- c("data.frame", "RhrDat")
  fileRaw
}

#' @export
rhr2md.RhrDat <- function(x) {
  
  x <- attributes(x)$rhrReadData
  
  
  md <- c(
    paste0("## Settings:", "\n"), 
    if (is.null(x$filename)) paste0("- File path: `", x$file, "`\n") else
      paste0("- File name: `", x$filename, "`\n"),
    paste0("- Field separator: `", x$sep, "`\n"), 
    paste0("- Decimal separator: `", x$sepDec, "`\n"), 
    paste0("- Skip: `", x$skip, "`\n"), 
    paste0("- Header: `", x$hasHeader, "`\n\n"))
  md
}

