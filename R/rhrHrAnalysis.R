## ============================================================================== ##  
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
## ============================================================================== ##

##' Function to run perform analyses and save results to temporary file
##'
##' This functions wraps many rhr* methods. The purpose of this function to run a series of
##' of estimates given data, a vector of desired results and a list with arguments.
##' @param dat RhrMappedData
##' @param what character vector; indicating which steps should be performed, order does not matter
##' @param args list; with arguments for the different steps
##' @param verbose logical; whether or not progress should be given on the command line
##' @param duplicated character; determine what to do with either 'nothing', 'delete' or 'random', to add random noise. 
##' @return List
##' @export
##' @author Johannes Signer
rhrHrAnalysis <- function(dat, what=c("rhrSiteFidelity", "rhrTTSI", "rhrMCP", "rhrKDE", "rhrAsymptote", "rhrCoreArea"),
                          args=NULL, verbose=TRUE, duplicated="nothing",
                          outDir=file.path(tempdir(), paste0("rhr", format(Sys.time(), "%Y%m%d%H%M%S"))),
                          inGUI=FALSE,
                          epsg=NULL,
                          inUnit="ido",
                          outUnit="ius", 
                          writeResultsSpatial=TRUE
                          ## vectorFormats=c("shp", "kml"),
                          ## rasterFormats=c("tif")
                          ## fullReport=TRUE,
                          ## browseReport=TRUE, 
                          ## reportType=c("html", "pdf", "pdf-simpel"),
                          ## publish="dropbox",
                          ## publishWithResults=FALSE,
                          ## publishWithData=TRUE
                          ) {

  ## Debugging
  if (FALSE) {
    library(rhr)
    data(datSH)
    dat <- datSH[1:1500,]
    fields <- list(lon="x_epsg31467",
                   lat="y_epsg31467",
                   id="collar",
                   date="day",
                   time="time")
    dateFormat <- "ymd"
    timeFormat <- "hms"
    inGUI <- FALSE

    outDir <- normalizePath(file.path(tempdir(), paste0("rhr", format(Sys.time(), "%Y%m%d%H%M%S"))), mustWork=FALSE, winslash="/")
    outDir <- normalizePath(file.path(tempdir(), paste0("rhr", format(Sys.time(), "rumba"))), mustWork=FALSE, winslash="/")
    dat <- rhrMapFields(dat, fields, dateFormat=dateFormat, timeFormat=timeFormat)$dat
    dat1 <- rhrMapFields(dat, fields, dateFormat=dateFormat, timeFormat=timeFormat)
    names(dat)[1:3] <- c("id", "lon", "lat")
    epsg <- "31467"

    ## rhrHrAnalysis
    what <- c("rhrSiteFidelity", "rhrTTSI", "rhrMCP", "rhrKDE", "rhrLoCoH", "rhrAsymptote", "rhrCoreArea", "rhrBBMM", "rhrUniNorm", "rhrBiNorm")
    args=list(rhrKDE=list(
                h=as.list(c("href", "hpi", "hlscv")),
                trast=rhrRasterFromExt(rhrExtFromPoints(dat[, c("lon", "lat")], buffer=5000), res=100))) 
    verbose=TRUE
    writeResultsSpatial=TRUE
    inUnit <- "m"
    outUnit <- "ha"
  }

  ## ------------------------------------------------------------------------------ ##  
  ## Check if the minimum is provided

  if (missing(dat)) {
    stop("rhrHrAnalysis: dat is missing")
  }

  if (is(dat, "rhrMappedData")) {
    stop("rhrHrAnalysis: dat should be of class rhrMappedData")
  }

  ## ------------------------------------------------------------------------------ ##  
  ## Default args

  defaultArgs <- list()

  ## Site fidelity
  defaultArgs$rhrSiteFidelity <- list()
  defaultArgs$rhrSiteFidelity$n <- 100
  defaultArgs$rhrSiteFidelity$alpha <- 0.05

  ## Site TTSI
  defaultArgs$rhrTTSI <- list()
  defaultArgs$rhrTTSI$init <- 6 * 60 * 60
  defaultArgs$rhrTTSI$consec <- TRUE
  defaultArgs$rhrTTSI$ntimes <- 3
  defaultArgs$rhrTTSI$alpha <- NULL

  ## MCP
  defaultArgs$rhrMCP <- list()
  defaultArgs$rhrMCP$levels <- 95

  ## KDE
  defaultArgs$rhrKDE <- list()
  defaultArgs$rhrKDE$levels <- 95
  defaultArgs$rhrKDE$userh <- NULL

  ## trast overrides everything else
  defaultArgs$rhrKDE$trast <- NULL
  defaultArgs$rhrKDE$h <- as.list("href", "hlscv", "hpi", c(100, 100))

  ## BBMM
  defaultArgs$rhrBBMM <- list()
  defaultArgs$rhrBBMM$levels <- 95
  defaultArgs$rhrBBMM$rangesigma1 <- c(1, 10000)
  defaultArgs$rhrBBMM$sigma2 <- 10

  ## either buffer or extendrange (extendrange takes precedence over buffer)
  defaultArgs$rhrBBMM$buffer <- NULL
  defaultArgs$rhrBBMM$extendRange <- 0.2

  ## either res or ncol; res take precedence
  defaultArgs$rhrBBMM$res <- 50
  defaultArgs$rhrBBMM$nrow <- NULL
  defaultArgs$rhrBBMM$ncol <- NULL


  ## UniNorm
  defaultArgs$rhrUniNorm <- list()
  defaultArgs$rhrUniNorm$levels <- 95

  ## either buffer or extendrange (extendrange takes precedence over buffer)
  defaultArgs$rhrUniNorm$buffer <- NULL
  defaultArgs$rhrUniNorm$extendRange <- 0.2

  ## either res or ncol; res take precedence
  defaultArgs$rhrUniNorm$res <- 50
  defaultArgs$rhrUniNorm$nrow <- NULL
  defaultArgs$rhrUniNorm$ncol <- NULL
  ## trast overrides everything else
  defaultArgs$rhrUniNorm$trast <- NULL
  
  ## BiNorm
  defaultArgs$rhrBiNorm <- list()
  defaultArgs$rhrBiNorm$levels <- 95

  ## either buffer or extendrange (extendrange takes precedence over buffer)
  defaultArgs$rhrBiNorm$buffer <- NULL
  defaultArgs$rhrBiNorm$extendRange <- 0.2

  ## either res or ncol; res take precedence
  defaultArgs$rhrBiNorm$res <- 50
  defaultArgs$rhrBiNorm$nrow <- NULL
  defaultArgs$rhrBiNorm$ncol <- NULL
  ## trast overrides everything else
  defaultArgs$rhrBiNorm$trast <- NULL


  ## LoCoH
  defaultArgs$rhrLoCoH <- list()
  defaultArgs$rhrLoCoH$levels <- 95
  defaultArgs$rhrLoCoH$type <- c("k")
  defaultArgs$rhrLoCoH$n <- c(10)
  defaultArgs$rhrLoCoH$autoN <- c(TRUE)

  ## Asymptote
  defaultArgs$rhrAsymptote <- list()
  defaultArgs$rhrAsymptote$minNP <- 100
  defaultArgs$rhrAsymptote$estimators <- c("rhrMCP")
  defaultArgs$rhrAsymptote$nrep <- 10
  defaultArgs$rhrAsymptote$tolTotArea <- 0.05
  defaultArgs$rhrAsymptote$nTimes <- 2
  defaultArgs$rhrAsymptote$si <- 100
  defaultArgs$rhrAsymptote$sampling <- "random"
  
  ## ------------------------------------------------------------------------------ ##  
  ## Prep results

  ## That's the temp dir where all results are saved

  if (!file.exists(outDir)) {
    dir.create(outDir, recursive=TRUE)
  }

  outDirData  <- normalizePath(file.path(outDir, "results", "data"),mustWork=FALSE, winslash="/")
  outDirPlots <- normalizePath(file.path(outDir, "results", "plots"),mustWork=FALSE, winslash="/")
  outDirVect  <- normalizePath(file.path(outDir, "results", "vector"),mustWork=FALSE, winslash="/")
  outDirRast  <- normalizePath(file.path(outDir, "results", "raster"),mustWork=FALSE, winslash="/")

  if (!file.exists(outDirData)) {
    dir.create(outDirData, recursive=TRUE)
  }

  if (!file.exists(outDirPlots)) {
    dir.create(outDirPlots, recursive=TRUE)
  }

  if (!file.exists(outDirVect)) {
    dir.create(outDirVect, recursive=TRUE)
  }

  if (!file.exists(outDirRast)) {
    dir.create(outDirRast, recursive=TRUE)
  }

  ## ------------------------------------------------------------------------------ ##  
  ## proj4string


  projString <- if (is.null(epsg)) NA_character_ else paste0("+init=epsg:", epsg)

  ## A list where all results are written, that are not saved.
  ## All entries in the list are always a list with one entry for every animal
  resList <- list()  # per animal results
  resGlobal <- list()  # global results
  resLog <- c()

  resLog <- c(resLog, paste0("[", Sys.time(), "]: Starting with analysis"))

  ## ============================================================================== ##  
  ## Summary of data
  
  ## ------------------------------------------------------------------------------ ##  
  ## Prep data

  resList$summary <- list()

  resLog <- c(resLog, paste0("[", Sys.time(), "]: Preparing Data"))

  ## Do we work with or without time?
  resGlobal$hasTimestamp <- ifelse(any(is.na(dat$timestamp)), FALSE, TRUE)
  if (!resGlobal$hasTimestamp) {
    warning("rhrHrAnalysis: timestamp contains NA, time is not considered")
  }
                    
  ## split data by animal
  resLog <- c(resLog, paste0("[", Sys.time(), "]: Splitting data by animal"))
  dat <- split(dat, dat$id)

  resList$summary$nobs <- lapply(dat, nrow)

  ## check if there are missing
  resLog <- c(resLog, paste0("[", Sys.time(), "]: Check missing data"))
  resList$summary$nIncompleteCases <- lapply(dat, function(x) sum(!complete.cases(x[, c("id", "lon", "lat")])))

  ## remove missing
  dat <- lapply(dat, function(x) x[complete.cases(x[, c("id", "lon", "lat")]), ])

  ## check for duplicated with/without time
  if (resGlobal$hasTimestamp) {
    resList$summary$nDuplicated <- lapply(dat, function(x) sum(duplicated(x[, c("id", "lon", "lat")])))
    dat <- lapply(dat, function(x) x[!duplicated(x[, c("id", "lon", "lat")]), ])
  } else {
    resList$summary$nDuplicated <- lapply(dat, function(x) sum(duplicated(x[, c("id", "lon", "lat", "timestamp")])))
    dat <- lapply(dat, function(x) x[!duplicated(x[, c("id", "lon", "lat", "timestamp")]), ])
  }

  ## final number of obs
  resList$summary$nobs <- sapply(dat, nrow)

  ## ============================================================================== ##  
  ## Estimators

  resList$est <- list()

  ## ------------------------------------------------------------------------------ ##  
  ## rhrSiteFidelity

  if ("rhrSiteFidelity" %in% what) {
    thisEst <- "rhrSiteFidelity"
    if (inGUI) shiny::setProgress(message="Starting with site fidelity", detail=".....")
    resLog <- c(resLog, paste0("[", Sys.time(), "]: Start with", thisEst))

    ## sanity check on args
    if (thisEst %in% names(args)) {
      if (!is.null(args[[thisEst]])) {
        ## check on each arg
        if ((!"n" %in% names(args[[thisEst]])) | is.null(args[[thisEst]]$n)) {
          args[[thisEst]]$n <- defaultArgs[[thisEst]]$n
        }
        if ((!"alpha" %in% names(args[[thisEst]])) | is.null(args[[thisEst]]$alpha)) {
          args[[thisEst]]$alpha <- defaultArgs[[thisEst]]$alpha
        }
      }
    } else {
      args[[thisEst]] <- defaultArgs[[thisEst]]
    }

    ## Define possible different scenarios here
    resList$est[[thisEst]] <- list()

    scenarios <- expand.grid(n=args[[thisEst]]$n)
    scenarios$basename <- paste0("rhrSiteFidelity_n_", scenarios$n)
    resList$est$rhrSiteFidelity$scenarios <- scenarios
    scenarios <- split(scenarios, 1:nrow(scenarios))

    resList$est[[thisEst]]$res <- lapply(dat, function(animal) {

      resLog <- c(resLog, paste0("[", Sys.time(), "]: rhrSiteFidelity", animal[1, "id"]))
      if (inGUI) shiny::setProgress(message="Site fidelity",
                             detail=paste0("Starting with animal: ", animal[1, "id"], " (", which(animal[1, "id"] == names(dat)), " of ",
                               length(dat), ")"))
      lapply(scenarios, function(scn) {
        sf <- tryCatch(rhrSiteFidelity(animal[, c("lon", "lat")], n=scn$n, alpha=args[[thisEst]]$alpha),
                       error=function(e) e)

        if (inherits(sf, "error")) {
          fnRDS <- normalizePath(file.path(outDirData, paste0("animal_", animal[1, "id"], "_", scn$basename, ".Rds")),mustWork=FALSE, winslash="/")
          saveRDS(sf, file=fnRDS)
          list(rds=fnRDS)
        } else {
          ## Write the results
          fnRDS <- normalizePath(file.path(outDirData, paste0("animal_", animal[1, "id"], "_", scn$basename, ".Rds")),mustWork=FALSE, winslash="/")
          saveRDS(sf, file=fnRDS)

          ## Plot results
          fnPlotPNG <- normalizePath(file.path(outDirPlots, paste0("animal_", animal[1, "id"], "_", scn$basename, ".png")),mustWork=FALSE, winslash="/")
          fnPlotPDF <- normalizePath(file.path(outDirPlots, paste0("animal_", animal[1, "id"], "_", scn$basename, ".pdf")),mustWork=FALSE, winslash="/")
          suppressMessages(p1 <- plot(sf, plotit=FALSE))
          suppressMessages(ggsave(filename=fnPlotPNG, p1))
          suppressMessages(ggsave(filename=fnPlotPDF, p1))

          plts <- list(
            list(name="Distribution of bootstrap results",
                 plotPNG=fnPlotPNG,
                 plotPDF=fnPlotPDF,
                 legend="The bold red lines indicates results from empirical data and the dashed lines show the confidence interval from bootstrapping. If the bold line is below the bootstrapped CI, we can assume site fidelity")
            )

          ## tables

          return(list(rds=fnRDS, plots=plts, name=scn$name))
        }
      })
    })

    
    ## Parameters
    fnRDS <- normalizePath(file.path(outDirData, paste0(thisEst, "Params.Rds")),mustWork=FALSE, winslash="/")
    sfp <- data.frame(Parameter=names(args[[thisEst]]),
                      Value=as.character(sapply(args[[thisEst]], paste0, collapse=", ")))

    saveRDS(sfp, file=fnRDS)
    resList$est[[thisEst]]$parameters <- fnRDS

  } 


  ## ------------------------------------------------------------------------------ ##  
  ## rhrTTSI

  if ("rhrTTSI" %in% what) {
    thisEst <- "rhrTTSI"
    if (inGUI) shiny::setProgress(message=thisEst, detail="initializing")
    resLog <- c(resLog, paste0("[", Sys.time(), "]: Start with", thisEst))

    ## sanity check on args
    if (thisEst %in% names(args)) {
      if (!is.null(args[[thisEst]])) {
        ## check on each arg
        if ((!"init" %in% names(args[[thisEst]]) | is.null(args[[thisEst]]$init))) {
          args[[thisEst]]$init $alpha
        }
      }
    } else {
      args[[thisEst]] <- defaultArgs[[thisEst]]
    }


    ## Define possible different scenarios here
    resList$est[[thisEst]] <- list()
    scenarios <- expand.grid(scn=1)
    scenarios$basename <- paste0(thisEst, scenarios$scn)
    resList$est[[thisEst]]$scenarios <- scenarios 
    scenarios <- split(scenarios, 1:nrow(scenarios))


    resList$est[[thisEst]]$res <- lapply(dat, function(animal) {
      resLog <- c(resLog, paste0("[", Sys.time(), "]: rhrTTSI", animal[1, "id"]))
      if (inGUI) shiny::setProgress(message="Time to statistical independence",
                             detail=paste0("Starting with animal: ", animal[1, "id"], " (", which(animal[1, "id"] == names(dat)), " of ",
                               length(dat), ")"))
      lapply(scenarios, function(scn) {
        ttsi <- tryCatch(rhrTTSI(animal[, c("lon", "lat", "timestamp")], interval=args[[thisEst]]$init, ntimes=args[[thisEst]]$ntimes,
                                 consec=args[[thisEst]]$consec),
                       error=function(e) e)

        if (inherits(ttsi, "error")) {
          fnRDS <- normalizePath(file.path(outDirData, paste0("animal_", animal[1, "id"], "_", scn$basename, ".Rds")),mustWork=FALSE, winslash="/")
          saveRDS(ttsi, file=fnRDS)
          list(rds=fnRDS)
        } else {

          ## Write the results
          fnRDS <- normalizePath(file.path(outDirData, paste0("animal_", animal[1, "id"], "_", scn$basename, ".Rds")),mustWork=FALSE, winslash="/")
          saveRDS(ttsi, file=fnRDS)

          ## Plot results
          fnPlotPNG <- normalizePath(file.path(outDirPlots, paste0("animal_", animal[1, "id"], "_", scn$basename, ".png")),mustWork=FALSE, winslash="/")
          fnPlotPDF <- normalizePath(file.path(outDirPlots, paste0("animal_", animal[1, "id"], "_", scn$basename, ".pdf")),mustWork=FALSE, winslash="/")

          png(fnPlotPNG)
          print(plot(ttsi))
          dev.off()

          pdf(fnPlotPDF)
          print(plot(ttsi))
          dev.off()

          plts <- list(
            list(name="TTSI-Plots",
                 plotPNG=fnPlotPNG,
                 plotPDF=fnPlotPDF)
            )

          ## Write the message
          messageRDS <- normalizePath(file.path(outDirData, paste0("animal_", animal[1, "id"], "_", scn$basename, "_message.Rds")),mustWork=FALSE, winslash="/")
          msg1 <- paste0("Time to statistical independence was ", if (ttsi$cvReached) paste0("reached after ", ttsi$cvReachedAt, " seconds.") else "not reached.")

          saveRDS(msg1, file=messageRDS)

          msg <- list(
            list(name="What does it mean?",
                 message=messageRDS))

          ## tables

          return(list(rds=fnRDS, plots=plts, name=scn$name, messages=msg))

          
        }
      })
    })

    
    ## Parameters
    fnRDS <- normalizePath(file.path(outDirData, paste0(thisEst, "Params.Rds")),mustWork=FALSE, winslash="/")
    sfp <- data.frame(Parameter=names(args[[thisEst]]),
                      Value=as.character(sapply(args[[thisEst]], paste0, collapse=", ")))

    saveRDS(sfp, file=fnRDS)
    resList$est[[thisEst]]$parameters <- fnRDS



  } 

  ## ------------------------------------------------------------------------------ ##  
  ## rhrMCP

  if ("rhrMCP" %in% what) {
    thisEst <- "rhrMCP"
    resLog <- c(resLog, paste0("[", Sys.time(), "]: Start with ", thisEst))
    if (inGUI) shiny::setProgress(message=paste0("Starting with ", thisEst), detail=".....")

    ## sanity check on args
    if (thisEst %in% names(args)) {
      if (!is.null(args[[thisEst]])) {
        ## check on each arg
        if (is.null(args[[thisEst]]$levels)) {
          args[[thisEst]]$levels <- defaultArgs[[thisEst]]$levels
        }
      }
    } else {
      args[[thisEst]]$levels <- defaultArgs[[thisEst]]$levels
    }

    ## Define possible different scenarios here
    resList$est[[thisEst]] <- list()
    scenarios <- expand.grid(scn=1)
    scenarios$basename <- paste0("rhrMCP", scenarios$scn)
    scenarios$name <- "rhrMCP"
    resList$est[[thisEst]]$scenarios <- scenarios 
    scenarios <- split(scenarios, 1:nrow(scenarios))

    resList$est[[thisEst]]$res <- lapply(dat, function(animal) {
      resLog <- c(resLog, paste0("[", Sys.time(), "]: rhrMCP ", animal[1, "id"]))
      if (inGUI) shiny::setProgress(message=thisEst,
                             detail=paste0("Starting with animal: ", animal[1, "id"], " (", which(animal[1, "id"] == names(dat)), " of ",
                               length(dat), ")"))
      lapply(scenarios, function(scn) {
        mcp <- tryCatch(rhrMCP(animal[, c("lon", "lat")], levels=args[[thisEst]]$levels,
                               proj4string=projString),
                       error=function(e) e)

        if (inherits(mcp, "error")) {
          return(mcp)
        } else {
          ## Write the results

          ## Write the results
          fnRDS <- normalizePath(file.path(outDirData, paste0("animal_", animal[1, "id"], "_", scn$basename, ".Rds")),mustWork=FALSE, winslash="/")
          saveRDS(mcp, file=fnRDS)

          ## Plot results
          fnPlotPNG <- normalizePath(file.path(outDirPlots, paste0("animal_", animal[1, "id"], "_", scn$basename, ".png")),mustWork=FALSE, winslash="/")
          fnPlotPDF <- normalizePath(file.path(outDirPlots, paste0("animal_", animal[1, "id"], "_", scn$basename, ".pdf")),mustWork=FALSE, winslash="/")
          suppressMessages(p1 <- plot(mcp))
          suppressMessages(ggsave(filename=fnPlotPNG, p1))
          suppressMessages(ggsave(filename=fnPlotPDF, p1))

          plts <- list(
            list(name="Isopleths",
                 plotPNG=fnPlotPNG,
                 plotPDF=fnPlotPDF)
            )

          ## tables
          fnTbl1 <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_", scn$basename, "_tbl2.Rds")),mustWork=FALSE, winslash="/")
          t1 <- data.frame(rhrArea(mcp, args[[thisEst]]$levels))
          names(t1) <- c("Level", "Area")
          t1$Area <- rhrConvertUnit(t1$Area, inUnit, outUnit)
          saveRDS(t1, file=fnTbl1)


          tbls <- list(
            list(name="Home range areas",
                 path=fnTbl1))

          ## Spatial Data
          fnVect <- normalizePath(file.path(outDirVect,
                              paste0("animal_", animal[1, "id"], "_", scn$basename, "_isopleths.shp")),mustWork=FALSE, winslash="/")
          writeOGR(rhrIsopleths(mcp), dsn=fnVect, layer=basename(tools::file_path_sans_ext(fnVect)), driver="ESRI Shapefile",
                   overwrite_layer=TRUE)

          vcts <- list(
            fnVect)

          return(list(rds=fnRDS, plots=plts, name=scn$name, tables=tbls, vectordata=vcts))
        }
      })
    })
      

    
    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- list()

    for (scn in seq_along(scenarios)) {
      scns <- sapply(resList$est[[thisEst]]$res, function(x) x[[scn]]$vectordata[[1]])
      vcts <- lapply(scns, shapefile)
      animas <- names(resList$est[[thisEst]]$res)


      for (s in seq_along(vcts)) {
	vcts[[s]] <- spChFIDs(vcts[[s]], paste(scenarios[[scn]]$basename,
                                               animas[s], vcts[[1]]$level, sep="_"))
      }

      vcts <- do.call(rbind, vcts)

      ## Spatial Data
      fnVect <- normalizePath(file.path(outDirVect,
                          paste0("allAnimals_", scenarios[[scn]]$basename, "_isopleths.shp")),mustWork=FALSE, winslash="/")
      writeOGR(vcts, dsn=fnVect, layer=basename(tools::file_path_sans_ext(fnVect)), driver="ESRI Shapefile",
               overwrite_layer=TRUE)

      vcts <- list(
        fnVect)

      resList$est[[thisEst]]$allAnimals[[scn]] <- vcts
    }



    ## Parameters
    fnRDS <- normalizePath(file.path(outDirData, paste0(thisEst, "Params.Rds")),mustWork=FALSE, winslash="/")
    sfp <- data.frame(Parameter=names(args[[thisEst]]),
                      Value=as.character(sapply(args[[thisEst]], paste0, collapse=", ")))

    saveRDS(sfp, file=fnRDS)
    resList$est[[thisEst]]$parameters <- fnRDS
  }

  ## ------------------------------------------------------------------------------ ##  
  ## rhrKDE

  if ("rhrKDE" %in% what) {
    thisEst <- "rhrKDE"
    if (inGUI) shiny::setProgress(message=paste0("Starting with ", thisEst), detail=".....")
    resLog <- c(resLog, paste0("[", Sys.time(), "]: Start with", thisEst))
    ## sanity check on args
    if (!is.null(args[[thisEst]])) {
      ## check on each arg
      if (is.null(args[[thisEst]]$levels)) {
        args[[thisEst]]$levels <- defaultArgs[[thisEst]]$levels
      }
      if (is.null(args[[thisEst]]$trast)) {
        args[[thisEst]]$trast <- defaultArgs[[thisEst]]$trast
      }
      if (is.null(args[[thisEst]]$h)) {
        args[[thisEst]]$h <- defaultArgs[[thisEst]]$h
      }
    } else {
      ## No args provided, take default
      args[[thisEst]] <- defaultArgs[[thisEst]]
    }

    ## Define possible different scenarios here
    resList$est[[thisEst]] <- list()
    scenarios <- expand.grid(h=unlist(args[[thisEst]]$h))
    scenarios$basename <- paste0(thisEst, scenarios$h)
    scenarios$name <- paste0("KDE (bandwidth: ", scenarios$h, ")")
    resList$est[[thisEst]]$scenarios <- scenarios 
    scenarios <- split(scenarios, 1:nrow(scenarios))

    resList$est[[thisEst]]$res <- lapply(dat, function(animal) {
      resLog <- c(resLog, paste0("[", Sys.time(), "]: ", thisEst, " ", animal[1, "id"]))
      if (inGUI) shiny::setProgress(message=thisEst,
                             detail=paste0("Starting with animal: ", animal[1, "id"], " (", which(animal[1, "id"] == names(dat)), " of ",
                               length(dat), ")"))
      lapply(scenarios, function(scn) {

        ## h (its not possible yet to control h, if so you should calc a priori
        if (scn$h == "user") {
          hres <- list(h=rep(args[[thisEst]]$userh, 2), name="user")
        } else if (scn$h == "href") {
          hres <- rhrHref(animal[, c("lon", "lat")])
        } else if (scn$h == "hlscv") {
          hres <- rhrHlscv(animal[, c("lon", "lat")])
        } else if (scn$h == "hpi") {
          hres <- rhrHpi(animal[, c("lon", "lat")])
        } else {
          hres$msg <- "sorry, h input not understood, this calculation will be skipped"
        }

        kde <- tryCatch(rhrKDE(animal[, c("lon", "lat")], h=hres$h,
                               trast=args[[thisEst]]$trast, 
                               proj4string=projString),
                        error=function(e) e)

        if (inherits(kde, "error")) {
          fnRDS <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_", scn$basename, ".Rds")),mustWork=FALSE, winslash="/")
          saveRDS(kde, file=fnRDS)
          return(kde)
        } else {
          ## Write the results
          fnRDS <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_", scn$basename, ".Rds")),mustWork=FALSE, winslash="/")
          saveRDS(kde, file=fnRDS)
          ## Plot results
          fnPlotPNG <- normalizePath(file.path(outDirPlots,
                                 paste0("animal_", animal[1, "id"], "_", scn$basename, ".png")),mustWork=FALSE, winslash="/")
          fnPlotPDF <- normalizePath(file.path(outDirPlots,
                                 paste0("animal_", animal[1, "id"], "_", scn$basename, ".pdf")),mustWork=FALSE, winslash="/")
          png(fnPlotPNG)
          plot(kde)
          dev.off()

          pdf(fnPlotPDF)
          plot(kde)
          dev.off()

          plts <- list(
            list(name="Kernel density estimate with isopleths",
                 plotPNG=fnPlotPNG,
                 plotPDF=fnPlotPDF)
            )

          ## tables
          fnTbl1 <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_", scn$basename, "_tbl1.Rds")),mustWork=FALSE, winslash="/")
          t1 <- data.frame(Parameter="Value for bandwidth", Value=paste0(round(hres$h, 2), collapse=","))
          saveRDS(t1, file=fnTbl1)

          fnTbl2 <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_", scn$basename, "_tbl2.Rds")),mustWork=FALSE, winslash="/")
          t2 <- data.frame(rhrArea(kde, args[[thisEst]]$levels))
          names(t2) <- c("Area", "Level")
          t2$Area <- rhrConvertUnit(t2$Area, inUnit, outUnit)
          saveRDS(t2, file=fnTbl2)

          tbls <- list(
            list(name="Tuning Parameters",
                 path=fnTbl1),
            list(name="Home range areas",
                 path=fnTbl2))

          ## message

          ## Spatial Data
          fnVect <- normalizePath(file.path(outDirVect,
                              paste0("animal_", animal[1, "id"], "_", scn$basename, "_isopleths.shp")),mustWork=FALSE, winslash="/")
          writeOGR(rhrIsopleths(kde), dsn=fnVect, layer=basename(tools::file_path_sans_ext(fnVect)), driver="ESRI Shapefile",
                   overwrite_layer=TRUE)

          vcts <- list(
            fnVect)

          ## raster
          fnRast <- normalizePath(file.path(outDirRast,
                              paste0("animal_", animal[1, "id"], "_", scn$basename, "_UD.tif")),mustWork=FALSE, winslash="/")
          writeRaster(rhrUD(kde), dsn=fnVect, filename=fnRast, overwrite=TRUE)

          rsts <- list(
            fnRast)

          ## Write the message
          if (!is.null(hres$converged)) {
            if (!hres$converged) {
              messageRDS <- normalizePath(file.path(outDirData, paste0("animal_", animal[1, "id"], "_", scn$basename, "_message.Rds")),mustWork=FALSE, winslash="/")
              msg1 <- paste0("Caution, bandwidth with least square cross validation did not converge, defaulted back to the smallest value in search range")

              saveRDS(msg1, file=messageRDS)

              msg <- list(
                list(name="LSVC did not converge",
                     message=messageRDS))
            }
          } else {
            msg <- NULL
          }


          return(list(rds=fnRDS, plots=plts, name=scn$name, tables=tbls, vectordata=vcts,
                      rasterdata=rsts, messages=msg))
        }
      })
    })

    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- list()

    for (scn in seq_along(scenarios)) {
      scns <- sapply(resList$est[[thisEst]]$res, function(x) x[[scn]]$vectordata[[1]])
      vcts <- lapply(scns, shapefile)
      animas <- names(resList$est[[thisEst]]$res)

      for (s in seq_along(vcts)) {
	vcts[[s]] <- spChFIDs(vcts[[s]], paste(scenarios[[scn]]$basename,
                                               animas[s], vcts[[1]]$level, sep="_"))
      }

      vcts <- do.call(rbind, vcts)

      ## Spatial Data
      fnVect <- normalizePath(file.path(outDirVect,
                          paste0("allAnimals_", scenarios[[scn]]$basename, "_isopleths.shp")),mustWork=FALSE, winslash="/")
      writeOGR(vcts, dsn=fnVect, layer=basename(tools::file_path_sans_ext(fnVect)), driver="ESRI Shapefile",
               overwrite_layer=TRUE)

      vcts <- list(
        fnVect)

      resList$est[[thisEst]]$allAnimals[[scn]] <- vcts
    }

    ## Parameters
    fnRDS <- normalizePath(file.path(outDirData, paste0(thisEst, "Params.Rds")),mustWork=FALSE, winslash="/")
    sfp <- data.frame(Parameter=names(args[[thisEst]]),
                   Value=as.character(sapply(args[[thisEst]], paste0, collapse=", ")))
    saveRDS(sfp, file=fnRDS)
    resList$est[[thisEst]]$parameters <- fnRDS
  }

  ## ------------------------------------------------------------------------------ ##  
  ## rhrBBMM

  if ("rhrBBMM" %in% what) {
    thisEst <- "rhrBBMM"
    if (inGUI) shiny::setProgress(message=paste0("Starting with ", thisEst), detail=".....")
    resLog <- c(resLog, paste0("[", Sys.time(), "]: Start with", thisEst))
    ## sanity check on args
    if (!is.null(args[[thisEst]])) {
      ## check on each arg
      if (is.null(args[[thisEst]]$levels)) {
        args[[thisEst]]$levels <- defaultArgs[[thisEst]]$levels
      }
      if (is.null(args[[thisEst]]$trast)) {
        args[[thisEst]]$trast <- defaultArgs[[thisEst]]$trast
      }
      if (is.null(args[[thisEst]]$rangesigma1)) {
        args[[thisEst]]$rangesigma1 <- defaultArgs[[thisEst]]$rangesigma1
      }
      if (is.null(args[[thisEst]]$sigma2)) {
        args[[thisEst]]$sigma2 <- defaultArgs[[thisEst]]$sigma2
      }
    } else {
      ## No args provided, take default
      args[[thisEst]] <- defaultArgs[[thisEst]]
    }

    ## Define possible different scenarios here
    resList$est[[thisEst]] <- list()
    scenarios <- expand.grid(scn=1)
    scenarios$basename <- paste0(thisEst, scenarios$scn)
    scenarios$name <- paste0("BBMM")
    resList$est[[thisEst]]$scenarios <- scenarios 
    scenarios <- split(scenarios, 1:nrow(scenarios))

    resList$est[[thisEst]]$res <- lapply(dat, function(animal) {
      resLog <- c(resLog, paste0("[", Sys.time(), "]: ", thisEst, " ", animal[1, "id"]))
      if (inGUI) shiny::setProgress(message=thisEst,
                             detail=paste0("Starting with animal: ", animal[1, "id"], " (", which(animal[1, "id"] == names(dat)), " of ",
                               length(dat), ")"))
      lapply(scenarios, function(scn) {

        bbmm <- tryCatch(rhrBBMM(animal[, c("lon", "lat", "timestamp")],
                                 rangesigma1=args[[thisEst]]$rangesigma1, 
                                 sigma2=args[[thisEst]]$sigma2,
                                 trast=args[[thisEst]]$trast,
                                 proj4string=projString),
                        error=function(e) e)

        if (inherits(bbmm, "error")) {
          fnRDS <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_", scn$basename, ".Rds")),mustWork=FALSE, winslash="/")
          saveRDS(bbmm, file=fnRDS)
          return(bbmm)
        } else {
          ## Write the results
          fnRDS <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_", scn$basename, ".Rds")),mustWork=FALSE, winslash="/")
          saveRDS(bbmm, file=fnRDS)
          ## Plot results
          fnPlotPNG <- normalizePath(file.path(outDirPlots,
                                 paste0("animal_", animal[1, "id"], "_", scn$basename, ".png")),mustWork=FALSE, winslash="/")
          fnPlotPDF <- normalizePath(file.path(outDirPlots,
                                 paste0("animal_", animal[1, "id"], "_", scn$basename, ".pdf")),mustWork=FALSE, winslash="/")
          png(fnPlotPNG)
          plot(rhrUD(bbmm))
          dev.off()

          pdf(fnPlotPDF)
          plot(rhrUD(bbmm))
          dev.off()

          plts <- list(
            list(name="Brownian Bridge UD",
                 plotPNG=fnPlotPNG,
                 plotPDF=fnPlotPDF)
            )

          ## tables
          fnTbl1 <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_", scn$basename, "_tbl1.Rds")),mustWork=FALSE, winslash="/")
          t1 <- data.frame(Parameter="Value for Sigma 1", Value=paste0(round(bbmm$sigma1, 2), collapse=","))
          saveRDS(t1, file=fnTbl1)

          fnTbl2 <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_", scn$basename, "_tbl2.Rds")),mustWork=FALSE, winslash="/")
          t2 <- data.frame(rhrArea(bbmm, args[[thisEst]]$levels))
          names(t2) <- c("Area", "Level")
          t2$Area <- rhrConvertUnit(t2$Area, inUnit, outUnit)
          saveRDS(t2, file=fnTbl2)

          tbls <- list(
            list(name="Tuning Parameters",
                 path=fnTbl1),
            list(name="Home range areas",
                 path=fnTbl2))

          ## Spatial Data
          fnVect <- normalizePath(file.path(outDirVect,
                              paste0("animal_", animal[1, "id"], "_", scn$basename, "_isopleths.shp")),mustWork=FALSE, winslash="/")
          writeOGR(rhrIsopleths(bbmm, levels=args[[thisEst]]$levels), dsn=fnVect, layer=basename(tools::file_path_sans_ext(fnVect)), driver="ESRI Shapefile",
                   overwrite_layer=TRUE)

          vcts <- list(
            fnVect)

          ## raster
          fnRast <- normalizePath(file.path(outDirRast,
                              paste0("animal_", animal[1, "id"], "_", scn$basename, "_UD.tif")),mustWork=FALSE, winslash="/")
          writeRaster(rhrUD(bbmm), dsn=fnVect, filename=fnRast, overwrite=TRUE)

          rsts <- list(
            fnRast)

          return(list(rds=fnRDS, plots=plts, name=scn$name, tables=tbls, vectordata=vcts,
                      rasterdata=rsts))
        }
      })
    })

    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- list()

    for (scn in seq_along(scenarios)) {
      scns <- sapply(resList$est[[thisEst]]$res, function(x) x[[scn]]$vectordata[[1]])
      vcts <- lapply(scns, shapefile)
      animas <- names(resList$est[[thisEst]]$res)

      for (s in seq_along(vcts)) {
	vcts[[s]] <- spChFIDs(vcts[[s]], paste(scenarios[[scn]]$basename,
                                               animas[s], vcts[[1]]$level, sep="_"))
      }

      vcts <- do.call(rbind, vcts)

      ## Spatial Data
      fnVect <- normalizePath(file.path(outDirVect,
                          paste0("allAnimals_", scenarios[[scn]]$basename, "_isopleths.shp")),mustWork=FALSE, winslash="/")
      writeOGR(vcts, dsn=fnVect, layer=basename(tools::file_path_sans_ext(fnVect)), driver="ESRI Shapefile",
               overwrite_layer=TRUE)

      vcts <- list(
        fnVect)

      resList$est[[thisEst]]$allAnimals[[scn]] <- vcts
    }

    ## Parameters
    fnRDS <- normalizePath(file.path(outDirData, paste0(thisEst, "Params.Rds")),mustWork=FALSE, winslash="/")
    sfp <- data.frame(Parameter=names(args[[thisEst]]),
                      Value=as.character(sapply(args[[thisEst]], paste0, collapse=", ")))

    saveRDS(sfp, file=fnRDS)
    resList$est[[thisEst]]$parameters <- fnRDS
  }

  ## ------------------------------------------------------------------------------ ##  
  ## rhrUniNorm

  if ("rhrUniNorm" %in% what) {
    thisEst <- "rhrUniNorm"
    if (inGUI) shiny::setProgress(message=paste0("Starting with ", thisEst), detail=".....")
    resLog <- c(resLog, paste0("[", Sys.time(), "]: Start with", thisEst))
    ## sanity check on args
    if (!is.null(args[[thisEst]])) {
      ## check on each arg
      if (is.null(args[[thisEst]]$levels)) {
        args[[thisEst]]$levels <- defaultArgs[[thisEst]]$levels
      }
      if (is.null(args[[thisEst]]$trast)) {
        args[[thisEst]]$trast <- defaultArgs[[thisEst]]$trast
      }
    } else {
      ## No args provided, take default
      args[[thisEst]] <- defaultArgs[[thisEst]]
    }

    ## Define possible different scenarios here
    resList$est[[thisEst]] <- list()
    scenarios <- expand.grid(scn=1)
    scenarios$basename <- paste0(thisEst, scenarios$scn)
    scenarios$name <- paste0("UniNorm")
    resList$est[[thisEst]]$scenarios <- scenarios 
    scenarios <- split(scenarios, 1:nrow(scenarios))

    resList$est[[thisEst]]$res <- lapply(dat, function(animal) {
      resLog <- c(resLog, paste0("[", Sys.time(), "]: ", thisEst, " ", animal[1, "id"]))
      if (inGUI) shiny::setProgress(message=thisEst,
                             detail=paste0("Starting with animal: ", animal[1, "id"], " (", which(animal[1, "id"] == names(dat)), " of ",
                               length(dat), ")"))
      lapply(scenarios, function(scn) {

        est <- tryCatch(rhrUniNorm(animal[, c("lon", "lat")],
                                 trast=args[[thisEst]]$trast,
                                 proj4string=projString),
                        error=function(e) e)

        if (inherits(est, "error")) {
          fnRDS <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_", scn$basename, ".Rds")),mustWork=FALSE, winslash="/")
          saveRDS(est, file=fnRDS)
          return(est)
        } else {
          ## Write the results
          fnRDS <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_", scn$basename, ".Rds")),mustWork=FALSE, winslash="/")
          saveRDS(est, file=fnRDS)
          ## Plot results
          fnPlotPNG <- normalizePath(file.path(outDirPlots,
                                 paste0("animal_", animal[1, "id"], "_", scn$basename, ".png")),mustWork=FALSE, winslash="/")
          fnPlotPDF <- normalizePath(file.path(outDirPlots,
                                 paste0("animal_", animal[1, "id"], "_", scn$basename, ".pdf")),mustWork=FALSE, winslash="/")
          png(fnPlotPNG)
          print(plot(est, levels=args[[thisEst]]$levels))
          dev.off()

          pdf(fnPlotPDF)
          print(plot(est, levels=args[[thisEst]]$levels))
          dev.off()

          plts <- list(
            list(name="UD",
                 plotPNG=fnPlotPNG,
                 plotPDF=fnPlotPDF)
            )

          ## tables
          fnTbl1 <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_", scn$basename, "_tbl1.Rds")),mustWork=FALSE, winslash="/")
          t1 <- data.frame(Parameter=c("Mu", "Sigma", "AIC", "AICc"),
                           Value=c(paste0(round(est$parameters$mean, 2), collapse=", "),
                             paste0(round(est$parameters$sigma, 2), collapse=", "),
                             round(est$AIC, 2),
                             round(est$AICc, 2)
                           ))
          saveRDS(t1, file=fnTbl1)

          fnTbl2 <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_", scn$basename, "_tbl2.Rds")),mustWork=FALSE, winslash="/")
          t2 <- data.frame(rhrArea(est, args[[thisEst]]$levels))
          names(t2) <- c("Area", "Level")
          t2$Area <- rhrConvertUnit(t2$Area, inUnit, outUnit)
          saveRDS(t2, file=fnTbl2)

          tbls <- list(
            list(name="Tuning Parameters",
                 path=fnTbl1),
            list(name="Home range areas",
                 path=fnTbl2))

          ## Spatial Data
          fnVect <- normalizePath(file.path(outDirVect,
                              paste0("animal_", animal[1, "id"], "_", scn$basename, "_isopleths.shp")),mustWork=FALSE, winslash="/")
          writeOGR(rhrIsopleths(est, levels=args[[thisEst]]$levels), dsn=fnVect, layer=basename(tools::file_path_sans_ext(fnVect)), driver="ESRI Shapefile",
                   overwrite_layer=TRUE)

          vcts <- list(
            fnVect)

          ## raster
          fnRast <- normalizePath(file.path(outDirRast,
                              paste0("animal_", animal[1, "id"], "_", scn$basename, "_UD.tif")),mustWork=FALSE, winslash="/")
          writeRaster(rhrUD(est), dsn=fnVect, filename=fnRast, overwrite=TRUE)

          rsts <- list(
            fnRast)

          return(list(rds=fnRDS, plots=plts, name=scn$name, tables=tbls, vectordata=vcts,
                      rasterdata=rsts))
        }
      })
    })

    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- list()

    for (scn in seq_along(scenarios)) {
      scns <- sapply(resList$est[[thisEst]]$res, function(x) x[[scn]]$vectordata[[1]])
      vcts <- lapply(scns, shapefile)
      animas <- names(resList$est[[thisEst]]$res)

      for (s in seq_along(vcts)) {
	vcts[[s]] <- spChFIDs(vcts[[s]], paste(scenarios[[scn]]$basename,
                                               animas[s], vcts[[1]]$level, sep="_"))
      }

      vcts <- do.call(rbind, vcts)

      ## Spatial Data
      fnVect <- normalizePath(file.path(outDirVect,
                          paste0("allAnimals_", scenarios[[scn]]$basename, "_isopleths.shp")),mustWork=FALSE, winslash="/")
      writeOGR(vcts, dsn=fnVect, layer=basename(tools::file_path_sans_ext(fnVect)), driver="ESRI Shapefile",
               overwrite_layer=TRUE)

      vcts <- list(
        fnVect)

      resList$est[[thisEst]]$allAnimals[[scn]] <- vcts
    }

    ## Parameters
    fnRDS <- normalizePath(file.path(outDirData, paste0(thisEst, "Params.Rds")),mustWork=FALSE, winslash="/")
    sfp <- data.frame(Parameter=names(args[[thisEst]]),
                      Value=as.character(sapply(args[[thisEst]], paste0, collapse=", ")))

    saveRDS(sfp, file=fnRDS)
    resList$est[[thisEst]]$parameters <- fnRDS
  }

  ## ------------------------------------------------------------------------------ ##  
  ## rhrBiNorm

  if ("rhrBiNorm" %in% what) {
    thisEst <- "rhrBiNorm"
    if (inGUI) shiny::setProgress(message=paste0("Starting with ", thisEst), detail=".....")
    resLog <- c(resLog, paste0("[", Sys.time(), "]: Start with", thisEst))
    ## sanity check on args
    if (!is.null(args[[thisEst]])) {
      ## check on each arg
      if (is.null(args[[thisEst]]$levels)) {
        args[[thisEst]]$levels <- defaultArgs[[thisEst]]$levels
      }
      if (is.null(args[[thisEst]]$trast)) {
        args[[thisEst]]$trast <- defaultArgs[[thisEst]]$trast
      }
    } else {
      ## No args provided, take default
      args[[thisEst]] <- defaultArgs[[thisEst]]
    }

    ## Define possible different scenarios here
    resList$est[[thisEst]] <- list()
    scenarios <- expand.grid(scn=1)
    scenarios$basename <- paste0(thisEst, scenarios$scn)
    scenarios$name <- paste0("BiNorm")
    resList$est[[thisEst]]$scenarios <- scenarios 
    scenarios <- split(scenarios, 1:nrow(scenarios))

    resList$est[[thisEst]]$res <- lapply(dat, function(animal) {
      resLog <- c(resLog, paste0("[", Sys.time(), "]: ", thisEst, " ", animal[1, "id"]))
      if (inGUI) shiny::setProgress(message=thisEst,
                             detail=paste0("Starting with animal: ", animal[1, "id"], " (", which(animal[1, "id"] == names(dat)), " of ",
                               length(dat), ")"))
      lapply(scenarios, function(scn) {

        est <- tryCatch(rhrBiNorm(animal[, c("lon", "lat")],
                                  trast=args[[thisEst]]$trast,
                                  proj4string=projString),
                        error=function(e) e)

        if (inherits(est, "error")) {
          fnRDS <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_", scn$basename, ".Rds")),mustWork=FALSE, winslash="/")
          saveRDS(est, file=fnRDS)
          return(est)
        } else {
          ## Write the results
          fnRDS <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_", scn$basename, ".Rds")),mustWork=FALSE, winslash="/")
          saveRDS(est, file=fnRDS)
          ## Plot results
          fnPlotPNG <- normalizePath(file.path(outDirPlots,
                                 paste0("animal_", animal[1, "id"], "_", scn$basename, ".png")),mustWork=FALSE, winslash="/")
          fnPlotPDF <- normalizePath(file.path(outDirPlots,
                                 paste0("animal_", animal[1, "id"], "_", scn$basename, ".pdf")),mustWork=FALSE, winslash="/")
          png(fnPlotPNG)
          print(plot(est, levels=args[[thisEst]]$levels))
          dev.off()

          pdf(fnPlotPDF)
          print(plot(est, levels=args[[thisEst]]$levels))
          dev.off()

          plts <- list(
            list(name="UD",
                 plotPNG=fnPlotPNG,
                 plotPDF=fnPlotPDF)
            )

          ## tables
          fnTbl1 <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_", scn$basename, "_tbl1.Rds")),mustWork=FALSE, winslash="/")
          t1 <- data.frame(Parameter=c("Mu1", "Sigma1", "Mu2", "Sigma2", "AIC", "AICc"),
                           Value=c(
                             paste0(round(est$parameters$mean1, 2), collapse=", "),
                             paste0(round(est$parameters$sigma1, 2), collapse=", "),
                             paste0(round(est$parameters$mean2, 2), collapse=", "),
                             paste0(round(est$parameters$sigma2, 2), collapse=", "),
                             round(est$AIC, 2),
                             round(est$AICc, 2)
                           ))
          saveRDS(t1, file=fnTbl1)

          fnTbl2 <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_", scn$basename, "_tbl2.Rds")),mustWork=FALSE, winslash="/")
          t2 <- data.frame(rhrArea(est, args[[thisEst]]$levels))
          names(t2) <- c("Area", "Level")
          t2$Area <- rhrConvertUnit(t2$Area, inUnit, outUnit)
          saveRDS(t2, file=fnTbl2)

          tbls <- list(
            list(name="Tuning Parameters",
                 path=fnTbl1),
            list(name="Home range areas",
                 path=fnTbl2))

          ## Spatial Data
          fnVect <- normalizePath(file.path(outDirVect,
                              paste0("animal_", animal[1, "id"], "_", scn$basename, "_isopleths.shp")),mustWork=FALSE, winslash="/")
          writeOGR(rhrIsopleths(est, levels=args[[thisEst]]$levels), dsn=fnVect, layer=basename(tools::file_path_sans_ext(fnVect)), driver="ESRI Shapefile",
                   overwrite_layer=TRUE)

          vcts <- list(
            fnVect)

          ## raster
          fnRast <- normalizePath(file.path(outDirRast,
                              paste0("animal_", animal[1, "id"], "_", scn$basename, "_UD.tif")),mustWork=FALSE, winslash="/")
          writeRaster(rhrUD(est), dsn=fnVect, filename=fnRast, overwrite=TRUE)

          rsts <- list(
            fnRast)

          return(list(rds=fnRDS, plots=plts, name=scn$name, tables=tbls, vectordata=vcts,
                      rasterdata=rsts))
        }
      })
    })

    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- list()

    for (scn in seq_along(scenarios)) {
      scns <- sapply(resList$est[[thisEst]]$res, function(x) x[[scn]]$vectordata[[1]])
      vcts <- lapply(scns, shapefile)
      animas <- names(resList$est[[thisEst]]$res)

      for (s in seq_along(vcts)) {
	vcts[[s]] <- spChFIDs(vcts[[s]], paste(scenarios[[scn]]$basename,
                                               animas[s], vcts[[1]]$level, sep="_"))
      }

      vcts <- do.call(rbind, vcts)

      ## Spatial Data
      fnVect <- normalizePath(file.path(outDirVect,
                          paste0("allAnimals_", scenarios[[scn]]$basename, "_isopleths.shp")),mustWork=FALSE, winslash="/")
      writeOGR(vcts, dsn=fnVect, layer=basename(tools::file_path_sans_ext(fnVect)), driver="ESRI Shapefile",
               overwrite_layer=TRUE)

      vcts <- list(
        fnVect)

      resList$est[[thisEst]]$allAnimals[[scn]] <- vcts
    }

    ## Parameters
    fnRDS <- normalizePath(file.path(outDirData, paste0(thisEst, "Params.Rds")),mustWork=FALSE, winslash="/")
    sfp <- data.frame(Parameter=names(args[[thisEst]]),
                      Value=as.character(sapply(args[[thisEst]], paste0, collapse=", ")))

    saveRDS(sfp, file=fnRDS)
    resList$est[[thisEst]]$parameters <- fnRDS
  }

  ## ------------------------------------------------------------------------------ ##  
  ## rhrLoCoH

  if ("rhrLoCoH" %in% what) {
    thisEst <- "rhrLoCoH"
    if (inGUI) shiny::setProgress(message="Local Convex Hull", detail="initializing")
    resLog <- c(resLog, paste0("[", Sys.time(), "]: Start with rhrLoCoH"))

    ## sanity check on args
    if (!is.null(args[[thisEst]])) {
      ## check on each arg
      if (is.null(args[[thisEst]]$levels)) {
        args[[thisEst]]$levels <- defaultArgs[[thisEst]]$levels
      }
      if (is.null(args[[thisEst]]$type)) {
        args[[thisEst]]$type <- defaultArgs[[thisEst]]$type
      }
      if (is.null(args[[thisEst]]$ns)) {
        args[[thisEst]]$n <- defaultArgs[[thisEst]]$n
      }
      if (is.null(args[[thisEst]]$autoN)) {
        args[[thisEst]]$autoN <- defaultArgs[[thisEst]]$autoN
      }
    } else {
      ## No args provided, take default
      args[[thisEst]] <- defaultArgs[[thisEst]]
    }

    ## Define possible different scenarios here
    resList$est[[thisEst]] <- list()
    scenarios <- data.frame(type=args[[thisEst]]$type, n=args[[thisEst]]$n, autoN=args[[thisEst]]$autoN)
    scenarios$name <- paste0("LoCoH (type: ", scenarios$type, ")")
    scenarios$basename <- paste0(thisEst, scenarios$type)
    resList$est[[thisEst]]$scenarios <- scenarios 
    scenarios <- split(scenarios, 1:nrow(scenarios))

    resList$est[[thisEst]]$res <- lapply(dat, function(animal) {
      resLog <- c(resLog, paste0("[", Sys.time(), "]: rhrLoCoH ", animal[1, "id"]))
      if (inGUI) shiny::setProgress(message="Local Convex Hull",
                             detail=paste0("Starting with animal: ", animal[1, "id"], " (", which(animal[1, "id"] == names(dat)), " of ",
                               length(dat), ")"))
      lapply(scenarios, function(scn) {

        locoh <- tryCatch(rhrLoCoH(animal[, c("lon", "lat")], type=scn$type, autoN=scn$autoN, n=scn$n, levels=args[[thisEst]]$levels,
                               proj4string=projString),
                          error=function(e) e)

        if (inherits(locoh, "error")) {
          fnRDS <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_rhrLoCoH_type_", paste0(scn, collapse="_"), ".Rds")),mustWork=FALSE, winslash="/")
          saveRDS(locoh, file=fnRDS)
          return(locoh)
        } else {
          ## Write the results
          fnRDS <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_", scn$basename, ".Rds")),mustWork=FALSE, winslash="/")
          saveRDS(locoh, file=fnRDS)

          ## Plot results
          fnPlotPNG <- normalizePath(file.path(outDirPlots,
                                 paste0("animal_", animal[1, "id"], "_", scn$basename, ".png")),mustWork=FALSE, winslash="/")
          fnPlotPDF <- normalizePath(file.path(outDirPlots,
                                 paste0("animal_", animal[1, "id"], "_", scn$basename, ".pdf")),mustWork=FALSE, winslash="/")

          suppressMessages(p1 <- plot(locoh))
          suppressMessages(ggsave(filename=fnPlotPNG, p1))
          suppressMessages(ggsave(filename=fnPlotPDF, p1))

          plts <- list(
            list(name="Isopleths",
                 plotPNG=fnPlotPNG,
                 plotPDF=fnPlotPDF)
            )

          ## tables
          fnTbl1 <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_", scn$basename, "_tbl1.Rds")),mustWork=FALSE, winslash="/")
          t1 <- data.frame(Parameter="n", Value=locoh$res$n)
          saveRDS(t1, file=fnTbl1)

          fnTbl2 <- normalizePath(file.path(outDirData,
                             paste0("animal_", animal[1, "id"], "_", scn$basename, "_tbl2.Rds")),mustWork=FALSE, winslash="/")
          t2 <- data.frame(rhrArea(locoh)[, 1:2])
          names(t2) <- c("Level", "Area")
          t2$Area <- rhrConvertUnit(t2$Area, inUnit, outUnit)
          saveRDS(t2, file=fnTbl2)


          tbls <- list(
            list(name="Tuning Parameters",
                 path=fnTbl1),
            list(name="Home range areas",
                 path=fnTbl2))

          ## Spatial Data
          fnVect <- normalizePath(file.path(outDirVect,
                              paste0("animal_", animal[1, "id"], "_", scn$basename, "_isopleths.shp")),mustWork=FALSE, winslash="/")
          writeOGR(rhrIsopleths(locoh), dsn=fnVect, layer=basename(tools::file_path_sans_ext(fnVect)), driver="ESRI Shapefile",
                   overwrite_layer=TRUE)

          vcts <- list(
            fnVect)

          return(list(rds=fnRDS, plots=plts, name=scn$name, tables=tbls, vectordata=vcts))
          
        }
      })
    })

    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- list()

    for (scn in seq_along(scenarios)) {
      scns <- sapply(resList$est[[thisEst]]$res, function(x) x[[scn]]$vectordata[[1]])
      vcts <- lapply(scns, shapefile)
      animas <- names(resList$est[[thisEst]]$res)

      for (s in seq_along(vcts)) {
	vcts[[s]] <- spChFIDs(vcts[[s]], paste(scenarios[[scn]]$basename,
                                               animas[s], vcts[[1]]$level, sep="_"))
      }

      vcts <- do.call(rbind, vcts)

      ## Spatial Data
      fnVect <- normalizePath(file.path(outDirVect,
                          paste0("allAnimals_", scenarios[[scn]]$basename, "_isopleths.shp")),mustWork=FALSE, winslash="/")
      writeOGR(vcts, dsn=fnVect, layer=basename(tools::file_path_sans_ext(fnVect)), driver="ESRI Shapefile",
               overwrite_layer=TRUE)

      vcts <- list(
        fnVect)

      resList$est[[thisEst]]$allAnimals[[scn]] <- vcts
    }

    ## Parameters
    fnRDS <- normalizePath(file.path(outDirData, paste0(thisEst, "Params.Rds")),mustWork=FALSE, winslash="/")
    sfp <- data.frame(Parameter=names(args[[thisEst]]),
                      Value=as.character(sapply(args[[thisEst]], paste0, collapse=", ")))

    saveRDS(sfp, file=fnRDS)
    resList$est[[thisEst]]$parameters <- fnRDS
  }

  ## ------------------------------------------------------------------------------ ##  
  ## rhrCoreArea

  if ("rhrCoreArea" %in% what) {
    thisEst <- "rhrCoreArea"
    if (inGUI) shiny::setProgress(message=thisEst, detail="initializing")
    resLog <- c(resLog, paste0("[", Sys.time(), "]: Start with rhrCoreArea"))

    if ("rhrKDE" %in% what) {
      ## everything makes sense and we can go on from here
      resList$est[[thisEst]] <- list()
      scenarios <- resList$est[["rhrKDE"]]$scenarios
      scenarios$name <- paste0("Core Area for ", scenarios$name)
      scenarios$basename <- paste0("ca_", scenarios$basename)
      scenarios <- split(scenarios, 1:nrow(scenarios))

      resList$est[[thisEst]]$res <- lapply(names(dat), function(animalId) {
        resLog <- c(resLog, paste0("[", Sys.time(), "]: rhrCoreArea ", animalId))
        if (inGUI) shiny::setProgress(message="Core Area ",
                               detail=paste0("Starting with animal: ", animalId, " (", which(animalId == names(dat)), " of ",
                                 length(dat), ")"))

          lapply(seq_along(scenarios), function(scn) {

            kdeDensity <- readRDS(resList$est[["rhrKDE"]]$res[[animalId]][[scn]]$rds)
            ca <- tryCatch(rhrCoreArea(kdeDensity), error=function(e) e)

            if (inherits(ca, "error")) {
              fnRDS <- normalizePath(file.path(outDirData,
                                 paste0("animal_", animalId, "_", scenarios[[scn]]$basename, ".Rds")),mustWork=FALSE, winslash="/")
              saveRDS(ca, file=fnRDS)
              return(ca)
            } else {
              ## Write the results
              fnRDS <- normalizePath(file.path(outDirData,
                                 paste0("animal_", animalId, "_", scenarios[[scn]]$basename, ".Rds")),mustWork=FALSE, winslash="/")
              saveRDS(ca, file=fnRDS)

              ## Plot results
              fnPlotPNG <- normalizePath(file.path(outDirPlots,
                                     paste0("animal_", animalId, "_", scenarios[[scn]]$basename, ".png")),mustWork=FALSE, winslash="/")
              fnPlotPDF <- normalizePath(file.path(outDirPlots,
                                     paste0("animal_", animalId, "_", scenarios[[scn]]$basename, ".pdf")),mustWork=FALSE, winslash="/")

              suppressMessages(p1 <- plot(ca))
              suppressMessages(ggsave(filename=fnPlotPNG, p1))
              suppressMessages(ggsave(filename=fnPlotPDF, p1))
              
              fnPlotPNG1 <- normalizePath(file.path(outDirPlots,
                                     paste0("animal_", animalId, "_", scenarios[[scn]]$basename, "_extend.png")),mustWork=FALSE, winslash="/")
              fnPlotPDF1 <- normalizePath(file.path(outDirPlots,
                                     paste0("animal_", animalId, "_", scenarios[[scn]]$basename, "_extend.pdf")),mustWork=FALSE, winslash="/")

              png(fnPlotPNG1)
              print(plot(ca$rast))
              dev.off()

              pdf(fnPlotPDF1)
              print(plot(ca$rast))
              dev.off()
              

              suppressMessages(p1 <- plot(ca))
              suppressMessages(ggsave(filename=fnPlotPNG, p1))
              suppressMessages(ggsave(filename=fnPlotPDF, p1))

              plts <- list(
                list(name="Plot to determine core area",
                     plotPNG=fnPlotPNG,
                     plotPDF=fnPlotPDF),
                list(name="Areal extent of core area",
                     plotPNG=fnPlotPNG1,
                     plotPDF=fnPlotPDF1)
                )

              fnTbl1 <- normalizePath(file.path(outDirData,
                                  paste0("animal_", animalId, "_", scenarios[[scn]]$basename, "_tbl1.Rds")),mustWork=FALSE, winslash="/")
              t1 <- data.frame(Parameter="Size", Value=round(rhrArea(ca), 2))
              t1$Value <- rhrConvertUnit(t1$Value, inUnit, outUnit)
              saveRDS(t1, file=fnTbl1)

              tbls <- list(
                list(name="Size of core area",
                     path=t1)
                )

              ## Spatial Data
              fnVect <- normalizePath(file.path(outDirVect,
                                  paste0("animal_", animalId, "_", scenarios[[scn]]$basename, "_core_area.shp")),mustWork=FALSE, winslash="/")
              writeOGR(rhrIsopleths(ca), dsn=fnVect, layer=basename(tools::file_path_sans_ext(fnVect)), driver="ESRI Shapefile",
                       overwrite_layer=TRUE)

              vcts <- list(
                fnVect)

              return(list(rds=fnRDS, plots=plts, name=scenarios[[scn]]$name, vectordata=vcts))
            }
          })
      })
      names(resList$est[[thisEst]]$res) <- names(dat)
    } else {

      
      resList$est[[thisEst]]$res <- lapply(names(dat), function(animalId) {
        resLog <- c(resLog, paste0("[", Sys.time(), "]: rhrCoreArea ", animalId))
        if (inGUI) shiny::setProgress(message="Core Area ",
                               detail=paste0("Starting with animal: ", animalId, " (", which(animalId == names(dat)), " of ",
                                 length(dat), ")"))

        lapply(1, function(scn) {
          ll <- list(message="Error, in order calculate core areas, you need also to calculate KDE")
          class(ll) <- "error"
          fnRDS <- normalizePath(file.path(outDirData,
                             paste0("animal_", animalId, "_rhrCoreArea_type_", paste0(scn, collapse="_"), ".Rds")),mustWork=FALSE, winslash="/")
          saveRDS(ll, file=fnRDS)
          return(list(rds=fnRDS))

        })
      })
      names(resList$est[[thisEst]]$res) <- names(dat)
        
    }
                                           

    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- list()

    for (scn in seq_along(scenarios)) {
      scns <- sapply(resList$est[[thisEst]]$res, function(x) x[[scn]]$vectordata[[1]])
      vcts <- lapply(scns, shapefile)
      animas <- names(resList$est[[thisEst]]$res)

      for (s in seq_along(vcts)) {
	vcts[[s]] <- spChFIDs(vcts[[s]], paste(scenarios[[scn]]$basename,
                                               animas[s], vcts[[1]]$level, sep="_"))
      }

      vcts <- do.call(rbind, vcts)

      ## Spatial Data
      fnVect <- normalizePath(file.path(outDirVect,
                          paste0("allAnimals_", scenarios[[scn]]$basename, "_isopleths.shp")),mustWork=FALSE, winslash="/")
      writeOGR(vcts, dsn=fnVect, layer=basename(tools::file_path_sans_ext(fnVect)), driver="ESRI Shapefile",
               overwrite_layer=TRUE)

      vcts <- list(
        fnVect)

      resList$est[[thisEst]]$allAnimals[[scn]] <- vcts
    }
  }

  ## ------------------------------------------------------------------------------ ##  
  ## rhrAsymptote

  if ("rhrAsymptote" %in% what) {
    thisEst <- "rhrAsymptote"
    if (inGUI) shiny::setProgress(message=paste0("Starting with ", thisEst), detail=".....")
    resLog <- c(resLog, paste0("[", Sys.time(), "]: Start with", thisEst))

    ## sanity check on args
    if (!is.null(args[[thisEst]])) {
      ## check on each arg
      if (is.null(args[[thisEst]]$minNP)) {
        args[[thisEst]]$minNP <- defaultArgs[[thisEst]]$minNP
      }
      if (is.null(args[[thisEst]]$estimators)) {
        args[[thisEst]]$estimators <- defaultArgs[[thisEst]]$estimators
      }
      if (is.null(args[[thisEst]]$nrep)) {
        args[[thisEst]]$nrep <- defaultArgs[[thisEst]]$nrep
      }
      if (is.null(args[[thisEst]]$tolTotArea)) {
        args[[thisEst]]$tolTotArea <- defaultArgs[[thisEst]]$tolTotArea
      }
      if (is.null(args[[thisEst]]$nTimes)) {
        args[[thisEst]]$nTimes <- defaultArgs[[thisEst]]$nTimes
      }
      if (is.null(args[[thisEst]]$sampling)) {
        args[[thisEst]]$sampling <- defaultArgs[[thisEst]]$sampling
      }
      if (is.null(args[[thisEst]]$si)) {
        args[[thisEst]]$si <- defaultArgs[[thisEst]]$si
      }
    } else {
      ## No args provided, take default
      args[[thisEst]] <- defaultArgs[[thisEst]]
    }

    ## Define possible different scenarios here
    resList$est[[thisEst]] <- list()

    ## scenarios
    foo <- data.frame(name="ifrst", basename=":====:")

    if (length(intersect(args[[thisEst]]$estimators, what)) > 0) {
      ## everything makes sense and we can go on from here

      resList$est[[thisEst]]$res <- lapply(names(dat), function(animalId) {
        resLog <- c(resLog, paste0("[", Sys.time(), "]: rhrAsymptote ", animalId))
        if (inGUI) shiny::setProgress(message=thisEst,
                               detail=paste0("Starting with animal: ", animalId, " (", which(animalId == names(dat)), " of ",
                                 length(dat), ")"))

        asymKDE <- asymMCP <- NULL
        if ("rhrKDE" %in% what) {

          scenariosKDE <- resList$est[["rhrKDE"]]$scenarios
          scenariosKDE$name <- paste0("Asymptote ", scenariosKDE$name)
          scenariosKDE$basename <- paste0("asymptote_", scenariosKDE$basename)
          foo <- rbind(foo, scenariosKDE[, c("name", "basename")])

          asymKDE <- lapply(seq_along(resList$est[["rhrKDE"]]$res[[animalId]]), function(scn) {

            kde <- readRDS(resList$est[["rhrKDE"]]$res[[animalId]][[scn]]$rds)
            asym <- tryCatch(rhrAsymptote(kde, ns=seq(from=args[[thisEst]]$minNP, by=args[[thisEst]]$si, to=nrow(rhrData(kde))),
                                          nrep=args[[thisEst]]$nrep, tolTotArea=args[[thisEst]]$tolTotArea,
                                          nTimes=args[[thisEst]]$nTimes,
                                          sampling=args[[thisEst]]$sampling), error=function(e) e)
            
            if (inherits(asym, "error")) {
              fnRDS <- normalizePath(file.path(outDirData,
                                 paste0("animal_", animalId, "_", scenariosKDE[scn, "basename"], ".Rds")),mustWork=FALSE, winslash="/")
              saveRDS(asym, file=fnRDS)
              return(list(rds=fnRDS))
            } else {
              ## Write the results
              fnRDS <- normalizePath(file.path(outDirData,
                                 paste0("animal_", animalId, "_", scenariosKDE[scn, "basename"], ".Rds")),mustWork=FALSE, winslash="/")
              saveRDS(asym, file=fnRDS)

              ## Plot results
              fnPlotPNG <- normalizePath(file.path(outDirPlots,
                                     paste0("animal_", animalId, "_", scenariosKDE[scn, "basename"], ".png")),mustWork=FALSE, winslash="/")
              fnPlotPDF <- normalizePath(file.path(outDirPlots,
                                     paste0("animal_", animalId, "_", scenariosKDE[scn, "basename"], ".pdf")),mustWork=FALSE, winslash="/")

              suppressMessages(p1 <- plot(asym))
              suppressMessages(ggsave(filename=fnPlotPNG, p1))
              suppressMessages(ggsave(filename=fnPlotPDF, p1))

              plts <- list(
                list(name="Home range asymptote",
                     plotPNG=fnPlotPNG,
                     plotPDF=fnPlotPDF)
                )

              return(list(rds=fnRDS, plots=plts, name=scenariosKDE[scn, "name"]))
            }
          })
        }
        if ("rhrMCP" %in% what) {
          ## Asymp MCP
          scenariosMCP <- resList$est[["rhrMCP"]]$scenarios
          scenariosMCP$name <- paste0("Asymptote ", scenariosMCP$name)
          scenariosMCP$basename <- paste0("asymptote_", scenariosMCP$basename)
          foo <- rbind(foo, scenariosMCP[, c("name", "basename")])

          asymMCP <- lapply(seq_along(resList$est[["rhrMCP"]]$res[[animalId]]), function(scn) {

            mcp <- readRDS(resList$est[["rhrMCP"]]$res[[animalId]][[scn]]$rds)
            asym <- tryCatch(rhrAsymptote(mcp, ns=seq(from=args[[thisEst]]$minNP, by=args[[thisEst]]$si, to=nrow(rhrData(mcp))),
                                          nrep=args[[thisEst]]$nrep, tolTotArea=args[[thisEst]]$tolTotArea,
                                          nTimes=args[[thisEst]]$nTimes,
                                          sampling=args[[thisEst]]$sampling), error=function(e) e)
            
            if (inherits(asym, "error")) {
              fnRDS <- normalizePath(file.path(outDirData,
                                 paste0("animal_", animalId, "_", scenariosMCP[scn, "basename"], ".Rds")),mustWork=FALSE, winslash="/")
              saveRDS(asym, file=fnRDS)
              return(list(rds=fnRDS))
            } else {
              ## Write the results
              fnRDS <- normalizePath(file.path(outDirData,
                                 paste0("animal_", animalId, "_", scenariosMCP[scn, "basename"], ".Rds")),mustWork=FALSE, winslash="/")
              saveRDS(asym, file=fnRDS)

              ## Plot results
              fnPlotPNG <- normalizePath(file.path(outDirPlots,
                                     paste0("animal_", animalId, "_", scenariosMCP[scn, "basename"], ".png")),mustWork=FALSE, winslash="/")
              fnPlotPDF <- normalizePath(file.path(outDirPlots,
                                     paste0("animal_", animalId, "_", scenariosMCP[scn, "basename"], ".pdf")),mustWork=FALSE, winslash="/")

              suppressMessages(p1 <- plot(asym))
              suppressMessages(ggsave(filename=fnPlotPNG, p1))
              suppressMessages(ggsave(filename=fnPlotPDF, p1))

              plts <- list(
                list(name="Home range asymptote",
                     plotPNG=fnPlotPNG,
                     plotPDF=fnPlotPDF)
                )

              return(list(rds=fnRDS, plots=plts, name=scenariosMCP[scn, "name"]))

            }
          })
        }
        c(asymMCP, asymKDE)
      })

      names(resList$est[[thisEst]]$res) <- names(dat)
    } else {
      resList$est[[thisEst]]$res <- lapply(names(dat), function(animalId) {
        resLog <- c(resLog, paste0("[", Sys.time(), "]: rhrAsymptote ", animalId))
        if (inGUI) shiny::setProgress(message=thisEst,
                               detail=paste0("Starting with animal: ", animalId, " (", which(animalId == names(dat)), " of ",
                                 length(dat), ")"))

          lapply(1, function(scn) {

            ll <- list(message="Error, in order calculate home range asymptote, you must also select MCP or KDE")
            class(ll) <- "error"
            fnRDS <- normalizePath(file.path(outDirData,
                               paste0("animal_", animalId, "_", scenariosMCP[scn, "basename"], ".Rds")),mustWork=FALSE, winslash="/")
            saveRDS(ll, file=fnRDS)
            return(list(rds=fnRDS))

          })
      })
      names(resList$est[[thisEst]]$res) <- names(dat)
    }
    ## Parameters
    fnRDS <- normalizePath(file.path(outDirData, paste0(thisEst, "Params.Rds")),mustWork=FALSE, winslash="/")
    sfp <- data.frame(Parameter=names(args[[thisEst]]),
                      Value=as.character(sapply(args[[thisEst]], paste0, collapse=", ")))

    saveRDS(sfp, file=fnRDS)
    resList$est[[thisEst]]$parameters <- fnRDS
  }


  ## Result
  return(resList)
  
}
 
