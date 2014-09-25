##' @export

rhrGUI <- function(datFromR=NULL, outDir=getwd()) {
    if (!is.null(datFromR)) {
      if (is.data.frame(datFromR)) {
        assign(".datFromR", datFromR, envir=globalenv())
      } else {
        stop("datFromR must be a data.frame")
      }
    } else {
      assign(".datFromR", NULL, envir=globalenv())
    }

    if (!is.null(outDir)) {
      if (file.exists(outDir)) {
        assign(".outDir", outDir, envir=globalenv())
      } else {
        stop("outDir does not exists")
      }
    } else {
        stop("outDir is required")
    }

    shiny::runApp(system.file('gui', package='rhr'))
}
  