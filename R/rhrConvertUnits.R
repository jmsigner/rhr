#' rhrConvertUnit
#' 
#' Converts areal units
#' @param x a vector
#' @param inUnit input unit 
#' @param outUnit output unit
#' @note \code{inUnit} can be one of:
#' \itemize{
#' \item{"ido"}{stands for I dont know and will return the input}
#' \item{"m"}{meter}
#' \item{"km"}{kilometer}
#' \item{"ft"}{feet}
#' \item{"yd"}{yard}
#' \item{"mi"}{mile}
#' }
#' \code{outUnit} can be one of:
#' \itemize{
#' \item{"sqm"}{square meter}
#' \item{"ha"}{hectar}
#' \item{"sqkm"}{square kilometer}
#' \item{"sqft"}{square feet}
#' \item{"acres"}{acres}
#' \item{"sqyd"}{square yard}
#' \item{"sqmi"}{square mile}
#' \item{"ius"}{input unit squared}
#' }
#' @export

rhrConvertUnit <- function(x, inUnit="m", outUnit="ha") {

  if (!inUnit %in% c("ido", "m", "km", "ft", "yd", "mi")) {
    stop(paste0("inUnit is not valid. It should be one of: ",
                paste0(c("ido", "m", "km", "ft", "yd", "mi"), collapse=",")))
  }

  if (!outUnit %in% c("sqm", "ha", "sqkm", "sqft", "acres", "sqyd", "sqmi", "ius")) {
    stop(paste0("outUnit is not valid. It should be one of: ",
                paste0(c("sqm", "ha", "sqkm", "sqft", "acres", "sqyd", "sqmi", "ius"), collapse=",")))
  }

  if (inUnit == "ido" || outUnit == "ius") {
    return(x)
  } else if (inUnit == "m") {
    if (outUnit == "sqm") {
      return(x)
    } else if (outUnit == "ha") {
      return(x / 1e4)
    } else if (outUnit == "sqkm") {
      return(x / 1e6)
    } else if (outUnit == "acres") {
      return(x / 4046.85642)
    } else if (outUnit == "sqft") {
      return(x / 10.76391041671)
    } else if (outUnit == "sqyd") {
      return(x / 1.1959900463011)
    } else if (outUnit == "sqmi") {
      return(x / 0.00000038610215854781)
    }
  } else if (inUnit == "km") {
    if (outUnit == "sqm") {
      return(x / 0.000001)
    } else if (outUnit == "ha") {
      return(x /0.01)
    } else if (outUnit == "sqkm") {
      return(x)
    } else if (outUnit == "acres") {
      return(x / 0.0040468564224)
    } else if (outUnit == "sqft") {
      return(x /0.00000009290304)
    } else if (outUnit == "sqyd") {
      return(x /0.00000083612736)
    } else if (outUnit == "sqmi") {
      return(x /2.5899881103)
    }
  } else if (inUnit == "ft") {
    if (outUnit == "sqm") {
      return(x / 10.76391041671)
    } else if (outUnit == "ha") {
      return(x /107639.1041671 )
    } else if (outUnit == "sqkm") {
      return(x / 10763910.41671 )
    } else if (outUnit == "acres") {
      return(x /43560 )
    } else if (outUnit == "sqft") {
      return(x /1)
    } else if (outUnit == "sqyd") {
      return(x /9)
    } else if (outUnit == "sqmi") {
      return(x / 27878399.999612)
    }
  } else if (inUnit == "yd") {
    if (outUnit == "sqm") {
      return(x /1.1959900463011 )
    } else if (outUnit == "ha") {
      return(x / 11959.900463011)
    } else if (outUnit == "sqkm") {
      return(x /1195990.0463011  )
    } else if (outUnit == "acres") {
      return(x /4840 )
    } else if (outUnit == "sqft") {
      return(x /0.11111111111111 )
    } else if (outUnit == "sqyd") {
      return(x / 1)
    } else if (outUnit == "sqmi") {
      return(x / 3097599.9999569)
    }
  } else if (inUnit == "mi") {
    if (outUnit == "sqm") {
      return(x /0.00000038610215854781)
    } else if (outUnit == "ha") {
      return(x /0.0038610215854781)
    } else if (outUnit == "sqkm") {
      return(x / 0.38610215854781)
    } else if (outUnit == "acres") {
      return(x / 0.0015625000000217)
    } else if (outUnit == "sqft") {
      return(x /0.000000035870064279654)
    } else if (outUnit == "sqyd") {
      return(x / 0.00000032283057851688)
    } else if (outUnit == "sqmi") {
      return(x /1)
    }
  }
}
