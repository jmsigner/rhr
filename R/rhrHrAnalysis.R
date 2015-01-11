##' Function to run perform analyses and save results to temporary file
##'
##' This functions wraps many rhr* methods. The purpose of this function to run a series of
##' of estimates given data, a vector of desired results and a list with arguments.
##' @param datIn RhrMappedData
##' @param what character vector; indicating which steps should be performed, order does not matter
##' @param args list; with arguments for the different steps
##' @param verbose logical; whether or not progress should be given on the command line
##' @param inGUI logical; indicating whether or not the call was made from a GUI
##' @param inUnit character; units of the input
##' @param outUnit character; units of the output
##' @param outDir character; path to the directory where the results are saved.
##' @return List
##' @export
##' @author Johannes Signer
rhrHrAnalysis <- function(datIn, what=c("rhrSiteFidelity", "rhrTTSI", "rhrMCP", "rhrKDE", "rhrAsymptote", "rhrCoreArea"),
                          args=NULL, verbose=TRUE, 
                          outDir=file.path(tempdir(), paste0("rhr", format(Sys.time(), "%Y%m%d%H%M%S"))),
                          inGUI=FALSE,
                          inUnit="ido",
                          outUnit="ius" 
                          ## dataMan = NULL
                          ## vectorFormats=c("shp", "kml"),
                          ## rasterFormats=c("tif")
                          ## fullReport=TRUE,
                          ## browseReport=TRUE, 
                          ## reportType=c("html", "pdf", "pdf-simpel"),
                          ## publish="dropbox",
                          ## publishWithResults=FALSE,
                          ## publishWithData=TRUE
                          ) {


  ## ------------------------------------------------------------------------------ ##  
  ## Check if the minimum is provided

  if (missing(datIn)) {
    stop("rhrHrAnalysis: dat is missing")
  }

  if (!is(datIn, "RhrMappedData")) {
    stop("rhrHrAnalysis: dat should be of class rhrMappedData")
  }

  ## ------------------------------------------------------------------------------ ##  
  ## Functions

  ## check args and fill with default one
  checkArgs <- function(args, defaultArgs, toCheck) {
    if (!is.null(args)) {
      ## check on each arg
      for (arg in toCheck) {
        if (is.null(args[[arg]])) {
          args[[arg]] <- defaultArgs[[arg]]
        }
      }
    } else {
      ## No args provided, take default
      args <- defaultArgs
    }
    return(args)
  }

  ## log progress
  logProg <- function(logf, thisEst, animal, dat, inGUI) {
    if (inGUI) {
      shiny::setProgress(message=thisEst,
                         detail=paste0("Starting with animal: ",
                           animal$id[1], " (", which(animal$id[1] == names(dat)), " of ",
                           length(dat), ")"))
    }
    c(logf, paste0("[", Sys.time(), "]: ", thisEst, " ", animal$id[1]))
  }

  logStartEst <- function(logf, thisEst, inGUI, detail="...") {
    if (inGUI) shiny::setProgress(message=paste0("Starting with ", thisEst), detail=".....")
    c(logf, paste0("[", Sys.time(), "]: Start with ", thisEst))
  }


  ## Saving RDS
  saveRds <- function(est, animal, scn, outDirData, filename=NULL) {
    fnRDS <- normalizePath(file.path(outDirData,
                                     paste0("animal_", animal$id[1], "_", scn$basename, ".Rds")),mustWork=FALSE, winslash="/")

    if (!is.null(filename)) {
      fnRDS <- normalizePath(file.path(outDirData, paste0(filename, ".Rds")), mustWork=FALSE, winslash="/")
    }

    saveRDS(est, file=fnRDS)
    return(fnRDS)
  }

  savePlots <- function(est, animal, scn, outDirPlots, legend="Home Range Estimates", filename=NULL, ...) {
    ## Plot results
    fnPlotPNG <- normalizePath(
      file.path(outDirPlots, paste0("animal_", animal$id[1], "_", scn$basename, ".png")),
      mustWork=FALSE, winslash="/")
    fnPlotPDF <- normalizePath(
      file.path(outDirPlots, paste0("animal_", animal$id[1], "_", scn$basename, ".pdf")),
      mustWork=FALSE, winslash="/")

    if (!is.null(filename)) {
      fnPlotPNG <- normalizePath(
        file.path(outDirPlots, paste0(filename, ".png")), mustWork=FALSE, winslash="/")
      fnPlotPDF <- normalizePath(
        file.path(outDirPlots, paste0(filename, ".pdf")), mustWork=FALSE, winslash="/")
    }

    png(filename=fnPlotPNG)
    print(plot(est, ...))
    dev.off()

    pdf(file=fnPlotPDF)
    print(plot(est, ...))
    dev.off()

    list(
      list(name="Home range estimate",
           plotPNG=fnPlotPNG,
           plotPDF=fnPlotPDF)
    )
  }

  saveMsg <- function(animal, scn, outDirData, name="ABC", msg="abc") {
    messageRDS <- normalizePath(file.path(outDirData, paste0("animal_", animal$id[1], "_", scn$basename, "_message.Rds")), mustWork=FALSE, winslash="/")
    msg1 <- msg
    saveRDS(msg1, file=messageRDS)
    msg <- list(
      list(name=msg, message=messageRDS))
  }
  
  saveVect <- function(est, animal, scn, outDirVect, filename=NULL, ...) {
    ## Spatial Data
    fnVect <- normalizePath(file.path(outDirVect,
                                      paste0("animal_", animal$id[1], "_", scn$basename, "_isopleths.shp")),mustWork=FALSE, winslash="/")
    writeOGR(rhrIsopleths(est, ...), dsn=fnVect, layer=basename(tools::file_path_sans_ext(fnVect)), driver="ESRI Shapefile",
             overwrite_layer=TRUE)

    list(fnVect)
  }


  saveRast <- function(est, animal, scn, outDirRast) {
    ## raster
    fnRast <- normalizePath(
      file.path(outDirRast, paste0("animal_", animal$id[1], "_", scn$basename,
                                   "_UD.tif")),mustWork=FALSE, winslash="/")
    raster::writeRaster(rhrUD(est), dsn=fnRast, filename=fnRast, overwrite=TRUE)

    rsts <- list(
      fnRast)
  }

  mergeIsos <- function(scenarios, resList, thisEst) {
    resMISO <- list()
    for (scn in seq_along(scenarios)) {
      scns <- sapply(resList$est[[thisEst]]$res, function(x) x[[scn]]$est$vcts[[1]])
      vcts <- lapply(scns, raster::shapefile)
      animals <- names(resList$est[[thisEst]]$res)

      for (s in seq_along(vcts)) {
        vcts[[s]] <- sp::spChFIDs(vcts[[s]], paste(scenarios[[scn]]$basename,
                                               animals[s], vcts[[1]]$level, sep="_"))
      }

      vcts <- do.call(rbind, vcts)

      ## Spatial Data
      fnVect <- normalizePath(file.path(outDirVect,
                                        paste0("allAnimals_", scenarios[[scn]]$basename, "_isopleths.shp")),
                              mustWork=FALSE, winslash="/")
      rgdal::writeOGR(vcts, dsn=fnVect,
                      layer=basename(tools::file_path_sans_ext(fnVect)),
                      driver="ESRI Shapefile",
                      overwrite_layer=TRUE)

      resMISO[[scn]] <- list(fnVect)

    }
    return(resMISO)
  } 

  saveParameters <- function(thisEst, outDirData, args) {
    ## Parameters
    fnRDS <- normalizePath(file.path(outDirData, paste0(thisEst, "Params.Rds")), mustWork=FALSE, winslash="/")
    sfp <- data.frame(Parameter=names(args[[thisEst]]),
                      Value=as.character(sapply(args[[thisEst]], paste0, collapse=", ")))
    saveRDS(sfp, file=fnRDS)
    return(fnRDS)
  }

  ## ------------------------------------------------------------------------------ ##  
  ## Properties of a HR-Estimate

  rhrRes <- function(est, animal, scn, outDirs, msg = NULL) {

    if (inherits(est, "error")) {
      fnRDS <- saveRds(est, animal, scn, outDirs$data)
      return(list(

        class = class(est)[1], 
        rds = fnRDS,
        plts = NULL,
        vcts = NULL,
        rsts = NULL,
        msgs = est$message,
        error = TRUE
      ))
    } else {
      ## Write the results
      fnRDS <- saveRds(est, animal, scn, outDirs$data)

      ## Plot results
      plts <- savePlots(est, animal, scn, outDirs$plots, legend="Home Range Estimates") 

      ## Spatial Data
      vcts <- if (inherits(est, "RhrEst")) saveVect(est, animal, scn, outDirs$vect) else NULL
      rsts <- if (inherits(est, "RhrProbEst")) saveRast(est, animal, scn, outDirs$rast) else NULL
      return(list(
        class = class(est)[1], 
        rds = fnRDS,
        plts = plts,
        vcts = vcts,
        rsts = rsts,
        msgs = msg,
        error = FALSE
      ))
    }
  }

  rhrPArea <- function(est, thisEst, animal, scn, args, outDirs=outDirs, inUnit=inUnit, outUnit=outUnit) {
    ## Assuming only called if it makes sense
    a <- rhrArea(est, args[[thisEst]]$levels)

    ## save tables
    fnTbl1 <- normalizePath(
      file.path(outDirs$data, paste0("animal_", animal$id[1], "_", scn$basename, "_tbl1.Rds")),
      mustWork=FALSE, winslash="/")
    t1 <- data.frame(a)
    names(t1) <- c("Level", "Area")
    t1$Area <- rhrConvertUnit(t1$Area, inUnit, outUnit)
    saveRDS(t1, file=fnTbl1)

    fn <- paste0("animal_", animal$id[1], "_", scn$basename, "_pArea")

    tbls <- list(list(name="Home range areas", path=fnTbl1))
    list(
      name = "area",
      rds = saveRds(a, animal, scn, outDirs$data, filename=fn), 
      msg = NULL, 
      plots = NULL, 
      tables = tbls)

  }

  rhrPAsymptote <- function(est, thisEst, animal, scn, args, outDirs, what) {
    ## check asymptote is requested
    if ("rhrAsymptote" %in% what) {
      if (!thisEst %in% args[[thisEst]]$estimatorsExclude) {
        ## check this est is not excluded
        thisEst <- "rhrAsymptote"

        ## sanity check on args
        args[[thisEst]] <- checkArgs(args[[thisEst]], defaultArgs[[thisEst]], c("minNP", "estimators", "nrep", "tolTotArea", "nTimes", "sampling", "si", "estimatorsExclude"))

        animal <- rhrData(est, spatial=TRUE)
        asym <- tryCatch(rhrAsymptote(est,
                                      ns=seq(from=args[[thisEst]]$minNP,
                                        by=args[[thisEst]]$si, to=nrow(rhrData(est))),
                                      nrep=args[[thisEst]]$nrep, tolTotArea=args[[thisEst]]$tolTotArea,
                                      nTimes=args[[thisEst]]$nTimes,
                                      sampling=args[[thisEst]]$sampling), error=function(e) e)

        fn <- paste0("animal_", animal$id[1], "_", scn$basename, "_pAsym")
        
        if (is(asym, "error")) {
          return(list(
            name = "asymptote",
            rds = saveRds(asym, animal, scn, outDirs$data, filename=fn), 
            msg = list(list(name = "Error", msg = asym$message)), 
            plots = NULL, 
            tables = NULL))
        }

        ## Write the results
        fnRDS <- saveRds(asym, animal, scn, outDirData, filename=fn)

        ## Plot results
        plts <- savePlots(asym, animal, scn, outDirPlots, legend="Home Range Estimates", filename=fn) 

        return(list(
          name = "asymptote",
          rds = saveRds(asym, animal, scn, outDirs$data, filename=fn), 
          msg = NULL, 
          plots = plts, 
          tables = NULL))
      }
      return(NULL)
    }
    return(NULL)
  }

  rhrPCA <- function(est, thisEst, animal, scn, args, outDirs, what) {
    ## check asymptote is requested
    if ("rhrCoreArea" %in% what) {

      ca <- rhrCoreArea(est)

      ## Write the results
      fn <- paste0("animal_", animal$id[1], "_", scn$basename, "_pCA")
      fnRDS <- saveRds(ca, animal, scn, outDirData, filename=fn)

      ## Plot results
      plts <- savePlots(est, animal, scn, outDirPlots, legend="Core Area", filename=fn, levels=ca$iso) 

      ## Spatial Data
      vcts <- saveVect(est, animal, scn, outDirs$vect, filename=fn, levels=ca$iso)

      fn <- normalizePath(file.path(outDirs$data, paste0(fn, ".shp")), mustWork=FALSE, winslash="/")
      writeOGR(rhrIsopleths(est, levels=ca$iso), dsn=fn, layer=basename(tools::file_path_sans_ext(fn)), driver="ESRI Shapefile",
             overwrite_layer=TRUE)

      
    ## save tables
    a <- rhrArea(est, ca$iso)
    fnTbl1 <- normalizePath(
      file.path(outDirs$data, paste0("animal_", animal$id[1], "_", scn$basename, "_tbl_ca.Rds")),
      mustWork=FALSE, winslash="/")
    t1 <- data.frame(a)

      names(t1) <- c("Level", "Area")
    t1$Area <- rhrConvertUnit(t1$Area, inUnit, outUnit)
    saveRDS(t1, file=fnTbl1)

    tbls <- list(list(name="Area of home range core area", path=fnTbl1))

        return(list(
          name = "corearea",
          rds =fnRDS, 
          msg = NULL, 
          plots = plts, 
          vects = list(fn),
          tables = tbls))
    }
    return(NULL)
  }
    


  ## ------------------------------------------------------------------------------ ##  
  ## Default args

### maybe move this to options and access it all with getOption
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
  defaultArgs$rhrKDE$trast <- rhrRasterFromExt(rhrExtFromPoints(datIn, extendRange=0.2), nrow=100, res=NULL)
  defaultArgs$rhrKDE$h <- as.list("href", "hlscv", "hpi", c(100, 100))

  ## BBMM
  defaultArgs$rhrBBMM <- list()
  defaultArgs$rhrBBMM$levels <- 95
  defaultArgs$rhrBBMM$rangesigma1 <- c(1, 10000)
  defaultArgs$rhrBBMM$sigma2 <- 10
  defaultArgs$rhrBBMM$trast <- rhrRasterFromExt(rhrExtFromPoints(datIn, extendRange=0.2), nrow=100, res=NULL)

  ## UniNorm
  defaultArgs$rhrUniNorm <- list()
  defaultArgs$rhrUniNorm$levels <- 95
  defaultArgs$rhrUniNorm$trast <- rhrRasterFromExt(rhrExtFromPoints(datIn, extendRange=0.2), nrow=100, res=NULL)

  ## BiNorm
  defaultArgs$rhrBiNorm <- list()
  defaultArgs$rhrBiNorm$levels <- 95
  defaultArgs$rhrBiNorm$trast <- rhrRasterFromExt(rhrExtFromPoints(datIn, extendRange=0.2), nrow=100, res=NULL)

  ## LoCoH
  defaultArgs$rhrLoCoH <- list()
  defaultArgs$rhrLoCoH$levels <- 95
  defaultArgs$rhrLoCoH$type <- c("k")
  defaultArgs$rhrLoCoH$n <- c(10)
  defaultArgs$rhrLoCoH$autoN <- c(TRUE)

  ## Asymptote
  defaultArgs$rhrAsymptote <- list()
  defaultArgs$rhrAsymptote$minNP <- 100
  defaultArgs$rhrAsymptote$estimatorsExclude <- c("rhrLoCoH", "rhrBBMM")
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

  outDirData  <- normalizePath(file.path(outDir, "results", "data"), mustWork=FALSE, winslash="/")
  outDirPlots <- normalizePath(file.path(outDir, "results", "plots"), mustWork=FALSE, winslash="/")
  outDirVect  <- normalizePath(file.path(outDir, "results", "vector"), mustWork=FALSE, winslash="/")
  outDirRast  <- normalizePath(file.path(outDir, "results", "raster"), mustWork=FALSE, winslash="/")

  outDirs <- list(
    data = outDirData,
    plots = outDirPlots,
    vect = outDirVect,
    rast = outDirRast)

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

  ## if (!is.null(dataMan)) {
    ## Spatial

    ## Temproal

    ## Id

    ## Reproject
  ## }

  resList$summary <- list()

  resLog <- c(resLog, paste0("[", Sys.time(), "]: Preparing Data"))

  ## split data by animal
  resLog <- c(resLog, paste0("[", Sys.time(), "]: Splitting data by animal"))

  ## sp::split necessary
  dat <- sp::split(datIn$dat, datIn$dat$id)

  ## ============================================================================== ##  
  ## Estimators

  resList$est <- list()

  ## ------------------------------------------------------------------------------ ##  
  ## rhrSiteFidelity

  if ("rhrSiteFidelity" %in% what) {

    thisEst <- "rhrSiteFidelity"
    resLog <- logStartEst(resLog, thisEst, inGUI, detail="...") 

    ## sanity check on args
    args[[thisEst]] <- checkArgs(args[[thisEst]], defaultArgs[[thisEst]], c("n", "alpha"))

    ## Define possible different scenarios here
    resList$est[[thisEst]] <- list()

    scenarios <- expand.grid(n=args[[thisEst]]$n)
    scenarios$basename <- paste0("rhrSiteFidelity_n_", scenarios$n)
    resList$est$rhrSiteFidelity$scenarios <- scenarios
    scenarios <- split(scenarios, 1:nrow(scenarios))

    resList$est[[thisEst]]$res <- lapply(dat, function(animal) {
      resLog <- logProg(resLog, thisEst, animal, dat, inGUI)
      lapply(scenarios, function(scn) {
        sf <- tryCatch(rhrSiteFidelity(animal, n=scn$n, alpha=args[[thisEst]]$alpha), error=function(e) e)
        if (inherits(sf, "error")) {
          return(list(est = rhrRes(sf, animal, scn, outDirs, msg = list(list(name="Error", message=sf$message)))))
        } else {
          res <-  rhrRes(sf, animal, scn, outDirs)
          return(list(est = res))
        }
      })
    })

    ## Parameters
    resList$est[[thisEst]]$parameters <- saveParameters(thisEst, outDirData, args)
  } 


  ## ------------------------------------------------------------------------------ ##  
  ## rhrTTSI

  if ("rhrTTSI" %in% what) {
    thisEst <- "rhrTTSI"
    resLog <- logStartEst(resLog, thisEst, inGUI, detail="...") 

    ## sanity check on args
    args[[thisEst]] <- checkArgs(args[[thisEst]], defaultArgs[[thisEst]], c("init", "consec", "ntimes", "alpha"))

    ## Define possible different scenarios here
    resList$est[[thisEst]] <- list()
    scenarios <- expand.grid(scn=1)
    scenarios$basename <- paste0(thisEst, scenarios$scn)
    resList$est[[thisEst]]$scenarios <- scenarios 
    scenarios <- split(scenarios, 1:nrow(scenarios))

    resList$est[[thisEst]]$res <- lapply(dat, function(animal) {
      resLog <- logProg(resLog, thisEst, animal, dat, inGUI)
      lapply(scenarios, function(scn) {
        ttsi <- tryCatch(rhrTTSI(animal, animal$timestamp, interval=args[[thisEst]]$init,
                                 ntimes=args[[thisEst]]$ntimes, consec=args[[thisEst]]$consec), error=function(e) e)
        if (inherits(ttsi, "error")) {
          return(list(est = rhrRes(ttsi, animal, scn, outDirs, msg = list(list(name="Error", message=ttsi$message)))))
        } else {
          msg <- list(list(name="What does it mean", message=paste0("Time to statistical independence was ", if (ttsi$cvReached) paste0("reached after ", ttsi$cvReachedAt, " seconds.") else "not reached.")))
          res <-  rhrRes(ttsi, animal, scn, outDirs, msg=msg)
          return(list(est = res))
        }
      })
    })

    ## Parameters
    resList$est[[thisEst]]$parameters <- saveParameters(thisEst, outDirData, args)
  } 

  ## ------------------------------------------------------------------------------ ##  
  ## rhrMCP

  if ("rhrMCP" %in% what) {
    thisEst <- "rhrMCP"
    resLog <- logStartEst(resLog, thisEst, inGUI, detail="...") 

    ## sanity check on args
    args[[thisEst]] <- checkArgs(args[[thisEst]], defaultArgs[[thisEst]], c("levels"))

    ## Define possible different scenarios here
    resList$est[[thisEst]] <- list()
    scenarios <- expand.grid(scn=1)
    scenarios$basename <- paste0("rhrMCP", scenarios$scn)
    scenarios$name <- "rhrMCP"
    resList$est[[thisEst]]$scenarios <- scenarios 
    scenarios <- split(scenarios, 1:nrow(scenarios))

    resList$est[[thisEst]]$res <- lapply(dat, function(animal) {
      resLog <- logProg(resLog, thisEst, animal, dat, inGUI)
      lapply(scenarios, function(scn) {
        mcp <- tryCatch(rhrMCP(animal, levels=args[[thisEst]]$levels), error=function(e) e)

        if (inherits(mcp, "error")) {
          return(list(est = rhrRes(mcp, animal, scn, outDirs, msg = list(list(name="Error", message=mcp$message)))))
        } else {
          res <-  rhrRes(mcp, animal, scn, outDirs)

          ## hr area
          pArea <- rhrPArea(mcp, thisEst, animal, scn, args, outDirs, inUnit, outUnit)
          pAsymptote <- rhrPAsymptote(mcp, thisEst, animal, scn, args, outDirs, what)

          return(list(est = res,
                      properties = list(area = pArea,
                                        asymptote = pAsymptote)))
        }
      })
    })
    
    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- mergeIsos(scenarios, resList, thisEst)
    ## Parameters
    resList$est[[thisEst]]$parameters <- saveParameters(thisEst, outDirData, args)
  }

  ## ------------------------------------------------------------------------------ ##  
  ## rhrKDE

  if ("rhrKDE" %in% what) {
    thisEst <- "rhrKDE"
    resLog <- logStartEst(resLog, thisEst, inGUI, detail="...") 

    args[[thisEst]] <- checkArgs(args[[thisEst]], defaultArgs[[thisEst]], c("levels", "trast", "h"))

    ## Define possible different scenarios here
    resList$est[[thisEst]] <- list()
    scenarios <- expand.grid(h=unlist(args[[thisEst]]$h))
    scenarios$basename <- paste0(thisEst, scenarios$h)
    scenarios$name <- paste0("KDE (bandwidth: ", scenarios$h, ")")
    resList$est[[thisEst]]$scenarios <- scenarios 
    scenarios <- split(scenarios, 1:nrow(scenarios))

    resList$est[[thisEst]]$res <- lapply(dat, function(animal) {

      ## Log progress
      resLog <- logProg(resLog, thisEst, animal, dat, inGUI)

      lapply(scenarios, function(scn) {

        ## h (its not possible yet to control h, if so you should calc a priori
        if (scn$h == "user") {
          hres <- list(h=rep(args[[thisEst]]$userh, 2), name="user")
        } else if (scn$h == "href") {
          hres <- rhrHref(animal)
        } else if (scn$h == "hlscv") {
          hres <- rhrHlscv(animal, trast=args[[thisEst]]$trast)
        } else if (scn$h == "hpi") {
          hres <- rhrHpi(animal)
        } else {
          hres$msg <- "sorry, h input not understood, this calculation will be skipped"
        }

        kde <- tryCatch(rhrKDE(animal, h=hres$h, trast=args[[thisEst]]$trast,
                               levels = args[[thisEst]]$levels), error=function(e) e)

        if (inherits(kde, "error")) {
          return(list(est = rhrRes(kde, animal, scn, outDirs, msg = list(list(name="Error", message=kde$message)))))
        } else {

          ## msg
          msgs <- list(
            list(name = "Tuning parameter (bandwidth)",
                 msg = paste0("The used value for bandwidth (h) is: ", paste0(round(hres$h, 2), collapse=", "))), 
              if (!is.null(hres$converged)) {
                if (!hres$converged) {
                    list(name = "LSCV: convergence", 
                         msg = "Caution, bandwidth with least square cross validation did not converge, defaulted back to the smallest value in search range")
                } else {
                  NULL
                }
              } else {
                NULL
              }
          )

          res <-  rhrRes(kde, animal, scn, outDirs, msgs)
          pArea <- rhrPArea(kde, thisEst, animal, scn, args, outDirs, inUnit, outUnit)
          pAsymptote <- rhrPAsymptote(kde, thisEst, animal, scn, args, outDirs, what)
          pCA <- rhrPCA(kde, thisEst, animal, scn, args, outDirs, what)

          return(list(est = res, properties = list(area = pArea,
                                   asymptote = pAsymptote,
                                   corearea = pCA)))
        }
      })
    })

    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- mergeIsos(scenarios, resList, thisEst)

    ## Parameters
    resList$est[[thisEst]]$parameters <- saveParameters(thisEst, outDirData, args)
  }

  ## ------------------------------------------------------------------------------ ##  
  ## rhrBBMM

  if ("rhrBBMM" %in% what) {
    thisEst <- "rhrBBMM"
    resLog <- logStartEst(resLog, thisEst, inGUI, detail="...") 

    ## sanity check on args
    args[[thisEst]] <- checkArgs(args[[thisEst]], defaultArgs[[thisEst]], c("levels", "trast", "rangesigma1", "sigma2"))

    ## Define possible different scenarios here
    resList$est[[thisEst]] <- list()
    scenarios <- expand.grid(scn=1)
    scenarios$basename <- paste0(thisEst, scenarios$scn)
    scenarios$name <- paste0("BBMM")
    resList$est[[thisEst]]$scenarios <- scenarios 
    scenarios <- split(scenarios, 1:nrow(scenarios))

    resList$est[[thisEst]]$res <- lapply(dat, function(animal) {
      resLog <- logProg(resLog, thisEst, animal, dat, inGUI)
      lapply(scenarios, function(scn) {

        bbmm <- tryCatch(rhrBBMM(animal, animal$timestamp, rangesigma1=args[[thisEst]]$rangesigma1, 
                                 sigma2=args[[thisEst]]$sigma2,
                                 trast=args[[thisEst]]$trast), error=function(e) e)

        if (inherits(bbmm, "error")) {
          return(list(est = rhrRes(bbmm, animal, scn, outDirs, msg = list(list(name="Error", message=bbmm$message)))))
        } else {

          ## tables
          msgs <- list(
            list(list(name = "Tuning parameter (Sigma 1)",
                      message = paste0("The used value for Sigma 1 is: ", paste0(round(bbmm$sigma1, 2), collapse=",")))))

          res <-  rhrRes(bbmm, animal, scn, outDirs, msgs)
          pArea <- rhrPArea(bbmm, thisEst, animal, scn, args, outDirs, inUnit, outUnit)
          pCA <- rhrPCA(bbmm, thisEst, animal, scn, args, outDirs, what)

          return(list(est = res, properties = list(area = pArea,
                                   corearea = pCA)))
        }
      })
    })

    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- mergeIsos(scenarios, resList, thisEst)

    ## Parameters
    resList$est[[thisEst]]$parameters <- saveParameters(thisEst, outDirData, args)
  }

  ## ------------------------------------------------------------------------------ ##  
  ## rhrUniNorm

  if ("rhrUniNorm" %in% what) {
    thisEst <- "rhrUniNorm"
    resLog <- logStartEst(resLog, thisEst, inGUI, detail="...") 

    ## sanity check on args
    args[[thisEst]] <- checkArgs(args[[thisEst]], defaultArgs[[thisEst]], c("levels", "trast"))

    ## Define possible different scenarios here
    resList$est[[thisEst]] <- list()
    scenarios <- expand.grid(scn=1)
    scenarios$basename <- paste0(thisEst, scenarios$scn)
    scenarios$name <- paste0("UniNorm")
    resList$est[[thisEst]]$scenarios <- scenarios 
    scenarios <- split(scenarios, 1:nrow(scenarios))

    resList$est[[thisEst]]$res <- lapply(dat, function(animal) {
      resLog <- logProg(resLog, thisEst, animal, dat, inGUI)
      lapply(scenarios, function(scn) {

        est <- tryCatch(rhrUniNorm(animal, trast=args[[thisEst]]$trast), error=function(e) e)

        if (inherits(est, "error")) {
          return(list(est = rhrRes(est, animal, scn, outDirs, msg = list(list(name="Error", message=est$message)))))
        } else {

          ## tables
          msgs <- list(
            list(
              list(name = "Tuning parameter (Sigma 1)",
                   msg = paste0("Mu (", paste0(round(est$parameters$mean, 2), collapse=", "), "); Sigma (",
                     paste0(round(est$parameters$sigma, 2), collapse=", "), "); AIC (", round(est$AIC, 2), "); AICc ", round(est$AICc, 2)))))
          res <-  rhrRes(est, animal, scn, outDirs, msgs)
          pArea <- rhrPArea(est, thisEst, animal, scn, args, outDirs, inUnit, outUnit)
          pCA <- rhrPCA(est, thisEst, animal, scn, args, outDirs, what)

          return(list(est = res, properties = list(area = pArea,
                                   corearea = pCA)))
        }
      })
    })

    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- mergeIsos(scenarios, resList, thisEst)

    ## Parameters
    resList$est[[thisEst]]$parameters <- saveParameters(thisEst, outDirData, args)
  }

  ## ------------------------------------------------------------------------------ ##  
  ## rhrBiNorm

  if ("rhrBiNorm" %in% what) {
    thisEst <- "rhrBiNorm"
    resLog <- logStartEst(resLog, thisEst, inGUI, detail="...") 

    ## sanity check on args
    args[[thisEst]] <- checkArgs(args[[thisEst]], defaultArgs[[thisEst]], c("levels", "trast"))

    ## Define possible different scenarios here
    resList$est[[thisEst]] <- list()
    scenarios <- expand.grid(scn=1)
    scenarios$basename <- paste0(thisEst, scenarios$scn)
    scenarios$name <- paste0("BiNorm")
    resList$est[[thisEst]]$scenarios <- scenarios 
    scenarios <- split(scenarios, 1:nrow(scenarios))

    resList$est[[thisEst]]$res <- lapply(dat, function(animal) {
      resLog <- logProg(resLog, thisEst, animal, dat, inGUI)
      lapply(scenarios, function(scn) {

        est <- tryCatch(rhrBiNorm(animal, trast=args[[thisEst]]$trast), error=function(e) e)

        if (inherits(est, "error")) {
          return(list(est = rhrRes(est, animal, scn, outDirs, msg = list(list(name="Error", message=est$message)))))
        } else {

          ## tables
          msgs <- list(
            list( 
              list(name = "Tuning parameter (Sigma 1)",
                   message = paste0("Mu1 (", paste0(round(est$parameters$mean1, 2), collapse=", "), "); Sigma1 (",
                     paste0(round(est$parameters$sigma1, 2), collapse="), "),
                     ") Mu2 (", paste0(round(est$parameters$mean2, 2), collapse=", "), "); Sigma2 (",
                     paste0(round(est$parameters$sigma2, 2), collapse=", "),
                     "); AIC (", round(est$AIC, 2), "); AICc ", round(est$AICc, 2)))))
          res <-  rhrRes(est, animal, scn, outDirs, msgs)
          pArea <- rhrPArea(est, thisEst, animal, scn, args, outDirs, inUnit, outUnit)
          pCA <- rhrPCA(est, thisEst, animal, scn, args, outDirs, what)

          return(list(est = res, properties = list(area = pArea,
                                   corearea = pCA)))
        }
      })
    })

    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- mergeIsos(scenarios, resList, thisEst)

    ## Parameters
    resList$est[[thisEst]]$parameters <- saveParameters(thisEst, outDirData, args)
  }

  ## ------------------------------------------------------------------------------ ##  
  ## rhrLoCoH

  if ("rhrLoCoH" %in% what) {
    thisEst <- "rhrLoCoH"
    resLog <- logStartEst(resLog, thisEst, inGUI, detail="...") 

    ## sanity check on args
    args[[thisEst]] <- checkArgs(args[[thisEst]], defaultArgs[[thisEst]], c("levels", "type", "ns", "autoN"))

    ## Define possible different scenarios here
    resList$est[[thisEst]] <- list()
    scenarios <- data.frame(type=args[[thisEst]]$type, n=args[[thisEst]]$n, autoN=args[[thisEst]]$autoN)
    scenarios$name <- paste0("LoCoH (type: ", scenarios$type, ")")
    scenarios$basename <- paste0(thisEst, scenarios$type)
    resList$est[[thisEst]]$scenarios <- scenarios 
    scenarios <- split(scenarios, 1:nrow(scenarios))

    resList$est[[thisEst]]$res <- lapply(dat, function(animal) {
      resLog <- logProg(resLog, thisEst, animal, dat, inGUI)
      lapply(scenarios, function(scn) {

        locoh <- tryCatch(rhrLoCoH(animal, type=scn$type, autoN=scn$autoN, n=scn$n, levels=args[[thisEst]]$levels), error=function(e) e)

        if (inherits(locoh, "error")) {
          return(list(est = rhrRes(locoh, animal, scn, outDirs, msg = list(list(name="Error", message=locoh$message)))))
        } else {
          res <-  rhrRes(locoh, animal, scn, outDirs)

          ## hr area
          pArea <- rhrPArea(locoh, thisEst, animal, scn, args, outDirs, inUnit, outUnit)

          ## hr asymptote
          ## pAsymptote <- rhrPAsymptote(locoh, thisEst, animal, scn, args, outDirs, what)

          return(list(est = res,
                      properties = list(area = pArea)))
        }
      })
    })
    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- mergeIsos(scenarios, resList, thisEst)

    ## Parameters
    resList$est[[thisEst]]$parameters <- saveParameters(thisEst, outDirData, args)
  }
  return(resList)
}
