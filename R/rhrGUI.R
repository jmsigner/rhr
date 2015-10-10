##' Starts GUI
##'
##' Start a web-based Graphical User Interface (GUI) to perform some analyses that the rhr package provides.
##' @title rhrGUI
##' @export

rhrGUI <- function() {
    shiny::runApp(system.file('gui', package='rhr'))
}
  
