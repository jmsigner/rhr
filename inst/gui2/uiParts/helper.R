## Do we have time
hasTime <- reactive({
  if (is.null(data2()$dat)) {
    FALSE
  } else {
    if (!all(is.na(data2()$dat$timestamp))) {
      TRUE
    } else {
      FALSE
    }
  }
})


succesfullyFinishedS2 <- reactive({
  if (!is.null(data2())) {
    TRUE
  } else {
    FALSE
  }
})


getConfigValue <- function(val, input, config) {
  if (is.null(input[[val]])) {
    config[[val]]
  } else {
    input[[val]]
  }
}
