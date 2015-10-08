##' Starts GUI
##'
##' Start a web-based Graphical User Interface (GUI) to perform analyses.
##' @title rhrGUI
##' @export

rhrGUI <- function() {
    shiny::runApp(system.file('gui', package='rhr'))
}
  
