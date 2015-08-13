#' Markdown summary
#' 
#' Creates a markdown summary of an rhr object.
#'
#' @param x Object of class \code{RhrTrack*}
#' @export
#' @example inst/examples/ex-rhr2md.R

rhr2md <- function(x) {
  UseMethod("rhr2md", x)
}

##' @export
rhr2md.RhrTrackS <- function(x) {
  x <- trackS
  id <- 1
  xx <- summary(rhrSegments(x)$dist)
  xxv <- as.vector(xx)
  xxn <- names(xx)
  
  tab <- knitr::kable( data.frame(measure = xxn, distance = xxv), row.names = FALSE, digits = 3)
  capture.output(xx)
  
  md <- c(
    paste("# Relocation summary for:", id, "\n\n"), 
    paste("- **Number of relocations:**", rhrN(x), "\n\n"), 
    paste("## Track statistics:\n\n"),
    paste("### Step lengths:\n\n"),
    paste0("```\n",  
           paste0(capture.output(summary(rhrSegments(x)$dist)), collapse = "\n"), "\n```\n\n"))
  
  
  f <- markdownToHTML(text = md, output = "/tmp/lala.html")
  
}

    
