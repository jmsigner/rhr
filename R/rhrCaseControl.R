

rhrCaseControl <- function(x, n_controll = 10, resources, replace_zero = 0) {
  
  x <- rhr::rhrSegments(x, spatial = FALSE)
  
  if (any(obs_lengths == 0)) {
    stop("0 length steps are not possible, consider adding a random error")
  }
  
  # params for step lengths
  m <- fitdistrplus::fitdist(obs_lengths, "gamma")
  shape <- unname(coef(m)["shape"])
  scale <- unname(1/coef(m)["rate"])
  
  # params for circular
  vm <- circular::mle.vonmises(x$direction)
  
  
  ## Generate random points
  ns <- nrow(x)  # number of steps
  case_for_controll <- rep(1:ne, each = n_controll)
  
  sl <- rgamma(ne * n_controll, shape = shape, scale = scale)  # step lengths for new steps
  ta <- x[case_for_controll, "direction"] + 
    circular::rvonmises(ne * n_controll, mu = 0, kappa = vm$kappa)  # turning angles for new stps
  
  ## Controll points
  xy_cc <- x[case_for_controll, ]
  xy_cc[, "x0"] <- xy_cc[, "x0"] + sl * cos(ta)
  xy_cc[, "y0"] <- xy_cc[, "y0"] + sl * sin(ta)
  
  cc_df <- do.call(rbind, list(
    data.frame(
      step_id = 1:ne, 
      case = TRUE,
      xstart = x[, "x0"], 
      ystart = x[, "y0"], 
      xend = x[, "x1"], 
      yend = x[, "y1"], 
      distance = x$distance
    ), 
    data.frame(
      step_id = rep(1:ns, each = n_controll), 
      case = FALSE,
      xstart = x[case_for_controll, "x0"], 
      ystart = x[case_for_controll, "y0"], 
      xend = xy_cc[, 1], 
      yend = xy_cc[, 2], 
      distance = sl
    )
  ))
  cc_df
}
  
