##' Generates a call with all arguments as literals
##'
##' This is a slightly adapted form from: http://stackoverflow.com/questions/3478923/displaying-the-actual-parameter-list-of-the-function-during-execution
##' @title expandCall
##' @param call a call
##' @param n integer, the number of generations to go back in the parent frame
##' @return call
##' @author Johannes Signer
expandCall <- function(call, n=2) {
  do.call("call", 
          c(list(as.character(call[[1]])),
            lapply(as.list(call)[-1], eval)), envir=parent.frame(n=n))
}
