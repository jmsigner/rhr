##' Function to run perform analyses and save results to temporary file
##'
##' This functions wraps many rhr* methods. The purpose of this function to run a series of
##' of estimates given data, a vector of desired results and a list with arguments.
##' @param dat RhrMappedData
##' @param what character vector; indicating which steps should be performed, order does not matter
##' @param args list; with arguments for the different steps
##' @param verbose logical; whether or not progress should be given on the command line
##' @param inGUI logical; indicating whether or not the call was made from a GUI
##' @param inUnit character; units of the input
##' @param outUnit character; units of the output
##' @param outDir character; path to the directory where the results are saved.
##' @param report logical; indicating if a report should be created. 
##' @param zip logical; indicating if a zip archive should be created, note this most likely only works under linux. 
##' @param repArgs A list with extra arguments for the report.
##' @return List
##' @export
##' @author Johannes Signer
rhrHrAnalysis <- function(dat, what=c("rhrSiteFidelity", "rhrTTSI", "rhrMCP", "rhrKDE", "rhrAsymptote", "rhrCoreArea"),
                          args=NULL, verbose=TRUE, 
                          outDir=file.path(tempdir(), paste0("rhr", format(Sys.time(), "%Y%m%d%H%M%S"))),
                          inGUI=FALSE,
                          inUnit="ido",
                          outUnit="ius", 
                          report = TRUE, 
                          zip = FALSE, 
                          repArgs = NULL) {


  ## ------------------------------------------------------------------------------ ##  
  ## Check if the minimum is provided

  if (missing(dat)) {
    stop("rhrHrAnalysis: dat is missing")
  }

  if (!is(dat, "RhrTracks")) {
    stop("rhrHrAnalysis: dat should be of class RhrTracks")
  }

  ## ------------------------------------------------------------------------------ ##  
  ## Functions
  
  startTime <- Sys.time()

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
                           animal, " (", which(animal == names(dat)), " of ",
                           length(dat), ")"))
    }
    c(logf, paste0("[", Sys.time(), "]: ", thisEst, " ", animal))
  }

  logStartEst <- function(logf, thisEst, inGUI, detail="...") {
    if (inGUI) shiny::setProgress(message=paste0("Starting with ", thisEst), detail=".....")
    c(logf, paste0("[", Sys.time(), "]: Start with ", thisEst))
  }


  ## Saving RDS
  saveRds <- function(est, animal, estname, outDirData, filename=NULL) {
    fnRDS <- normalizePath(file.path(outDirData,
                                     paste0("animal_", animal, "_", estname, ".Rds")),mustWork=FALSE, winslash="/")

    if (!is.null(filename)) {
      fnRDS <- normalizePath(file.path(outDirData, paste0(filename, ".Rds")), mustWork=FALSE, winslash="/")
    }

    saveRDS(est, file=fnRDS)
    return(fnRDS)
  }

  savePlots <- function(est, animal, est_name, outDirPlots, legend="Home Range Estimates", filename=NULL, ...) {
    ## Plot results
    fnPlotPNG <- normalizePath(
      file.path(outDirPlots, paste0("animal_", animal, "_", est_name, ".png")),
      mustWork=FALSE, winslash="/")
    fnPlotPDF <- normalizePath(
      file.path(outDirPlots, paste0("animal_", animal, "_", est_name, ".pdf")),
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
      list(name=legend,
           plotPNG=fnPlotPNG,
           plotPDF=fnPlotPDF)
    )
  }

  saveMsg <- function(animal, est, outDirData, name="ABC", msg="abc") {
    messageRDS <- normalizePath(file.path(outDirData, paste0("animal_", animal, "_", est, "_message.Rds")), mustWork=FALSE, winslash="/")
    msg1 <- msg
    saveRDS(msg1, file=messageRDS)
    msg <- list(
      list(name=msg, message=messageRDS))
  }
  
  saveVect <- function(est, animal, est_name, outDirVect, filename=NULL, ...) {
    ## Spatial Data
    fnVect <- normalizePath(file.path(outDirVect,
                                      paste0("animal_", animal, "_", est_name, "_isopleths.shp")),mustWork=FALSE, winslash="/")
    rgdal::writeOGR(rhrIsopleths(est, ...), dsn=fnVect, layer=basename(tools::file_path_sans_ext(fnVect)), driver="ESRI Shapefile",
             overwrite_layer=TRUE)

    list(fnVect)
  }


  saveRast <- function(est, animal, est_name, outDirRast) {
    ## raster
    fnRast <- normalizePath(
      file.path(outDirRast, paste0("animal_", animal, "_", est_name,
                                   "_UD.tif")),mustWork=FALSE, winslash="/")
    raster::writeRaster(rhrUD(est), dsn=fnRast, filename=fnRast, overwrite=TRUE)

    rsts <- list(
      fnRast)
  }

  mergeIsos <- function(resList, thisEst, animals) {
    scns <- sapply(resList$est[[thisEst]]$res, function(x) x$est$vcts[[1]])
    whichnn <- which(!sapply(scns, is.null))
    scns <- scns[whichnn]
    animals <- animals[whichnn]
    if (length(scns) > 0) {
      vcts <- lapply(scns, raster::shapefile)
      
      for (s in seq_along(vcts)) {
        vcts[[s]] <- sp::spChFIDs(vcts[[s]], paste(thisEst, animals[s], vcts[[s]]$level, sep="_"))
      }
      vcts <- do.call(rbind, vcts)
      
      ## Spatial Data
      fnVect <- normalizePath(file.path(outDirVect,
                                        paste0("allAnimals_", thisEst, "_isopleths.shp")),
                              mustWork=FALSE, winslash="/")
      rgdal::writeOGR(vcts, dsn=fnVect,
                      layer=basename(tools::file_path_sans_ext(fnVect)),
                      driver="ESRI Shapefile",
                      overwrite_layer=TRUE)
      
      fnVect
      } else {
        NULL
      }
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
  ## Lookup
  
  methodLookup <- data.frame(
    short=c("rhrSiteFidelity", "rhrTTSI", "rhrMCP", "rhrKDE", "rhrLoCoH", "rhrBBMM", "rhrUniNorm", "rhrBiNorm", 
            "rhrUniCirc", "rhrBiCirc"),
    long=c("Site Fidelity", "Time to Statistical Independence", "Minimum Convex Polygon Home Range", 
           "Kernel Density Estimation Home Range",
           "Local Convex Hull Home Range", "Brownian Bridges Movement Model", "Unimodal Normal", "Bimodal Normal", 
           "Unimodal Circular Home Range", "Bimodal Circular Home Range"),
    stringsAsFactors=FALSE)
  
  propertyLookup <- data.frame(
    short=c("area", "asymptote", "corearea"),
    long=c("Home Range Area", "Home Range Asymptote", "Home Range Core Area"), 
    stringsAsFactors=FALSE)
  
  ## ------------------------------------------------------------------------------ ##  
  ## Properties of a HR-Estimate

  rhrRes <- function(est, animal, est_name, outDirs, msg = NULL, figLegend = "Home-range estimates") {

    if (inherits(est, "error")) {
      fnRDS <- saveRds(est, animal, est_name, outDirs$data)
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
      fnRDS <- saveRds(est, animal, est_name, outDirs$data)

      ## Plot results
      plts <- savePlots(est, animal, est_name, outDirs$plots, legend=figLegend) 

      ## Spatial Data
      vcts <- if (inherits(est, "RhrEst")) saveVect(est, animal, est_name, outDirs$vect) else NULL
      rsts <- if (inherits(est, "RhrProbEst")) saveRast(est, animal, est_name, outDirs$rast) else NULL
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

  rhrPArea <- function(est, thisEst, animal, args, outDirs=outDirs, inUnit=inUnit, outUnit=outUnit) {
    ## Assuming that this function is only called for object it makes sense
    a <- rhrArea(est, levels = args[[thisEst]]$levels)

    ## save tables
    fnTbl1 <- normalizePath(
      file.path(outDirs$data, paste0("animal_", animal, "_", thisEst, "_tbl1.Rds")),
      mustWork=FALSE, winslash="/")
    t1 <- data.frame(a)
    names(t1) <- c("Level", "Area")
    t1$Area <- rhrConvertUnit(t1$Area, inUnit, outUnit)
    saveRDS(t1, file=fnTbl1)

    fn <- paste0("animal_", animal, "_", thisEst, "_pArea")

    tbls <- list(list(name="Home range areas", path=fnTbl1))
    list(
      name = "area",
      rds = saveRds(a, animal, thisEst, outDirs$data, filename=fn), 
      msg = NULL, 
      plots = NULL, 
      tables = tbls)

  }

  rhrPAsymptote <- function(est, estName, animal, args, outDirs, what) {
    ## check asymptote is requested
    if (estName %in% args[["rhrAsymptote"]]$include) {
      ## check this est is not excluded
      thisEst <- "rhrAsymptote"
      
      ## sanity check on args
      args[[thisEst]] <- checkArgs(args[[thisEst]], defaultArgs[[thisEst]], c("minNP", "estimators", "nrep", "tolTotArea", "nTimes", "sampling", "si", "estimatorsExclude"))
      
      asym <- tryCatch(rhrAsymptote(est,
                                    ns=seq(from=args[[thisEst]]$minNP,
                                           by=args[[thisEst]]$si, to=nrow(rhrData(est))),
                                    nrep=args[[thisEst]]$nrep, tolTotArea=args[[thisEst]]$tolTotArea,
                                    nTimes=args[[thisEst]]$nTimes,
                                    sampling=args[[thisEst]]$sampling), error=function(e) e)
      
      fn <- paste0("animal_", animal, "_", estName, "_pAsym")
      
      args[[thisEst]] <- args[[thisEst]][names(args[[thisEst]]) != "include"]
      
      
      if (is(asym, "error")) {
        return(list(
          name = "asymptote",
          rds = saveRds(asym, animal, thisEst, outDirs$data, filename=fn), 
          msg = list(list(name = "Error", msg = asym$message)), 
          args =list(table = saveParameters(thisEst, outDirs$data, args), name = "Arguments"), 
          plots = NULL, 
          tables = NULL))
      }
      
      ## Write the results
      fnRDS <- saveRds(asym, animal, thisEst, outDirData, filename=fn)
      
      
      ## Plot results
      plts <- savePlots(asym, animal, thisEst, outDirPlots, legend="Home Range Asymptote", filename=fn) 
      
      return(list(
        name = "asymptote",
        rds = saveRds(asym, animal, thisEst, outDirs$data, filename=fn), 
        args =list(table = saveParameters(thisEst, outDirs$data, args), name = "Arguments"), 
        msg = NULL, 
        plots = plts, 
        tables = NULL))
    }
    return(NULL)
  }

  ## core area
  rhrPCA <- function(est, estName, animal, args, outDirs, what) {
    ## check asymptote is requested
    if (estName %in% args[["rhrCoreArea"]]$include) {

      ca <- rhrCoreArea(est)
      
      ## Write the results
      fn <- paste0("animal_", animal, "_", estName, "_pCA")
      fnRDS <- saveRds(ca, animal, estName, outDirData, filename=fn)

      ## Plot results
      plts <- savePlots(est, animal, estName, outDirPlots, legend="Core Area", filename=fn, levels=ca$iso) 

      ## Spatial Data
      vcts <- saveVect(est, animal, estName, outDirs$vect, filename=fn, levels=ca$iso)

      fn <- normalizePath(file.path(outDirs$data, paste0(fn, ".shp")), mustWork=FALSE, winslash="/")
      rgdal::writeOGR(rhrIsopleths(est, levels=ca$iso), dsn=fn, layer=basename(tools::file_path_sans_ext(fn)), driver="ESRI Shapefile",
             overwrite_layer=TRUE)

      
    ## save tables
    a <- rhrArea(est, ca$iso)
    fnTbl1 <- normalizePath(
      file.path(outDirs$data, paste0("animal_", animal, "_", estName, "_tbl_ca.Rds")),
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
  defaultArgs$rhrTTSI$interval <- 6 * 60 * 60
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
  defaultArgs$rhrKDE$trast <- rhrRasterFromExt(rhrExtFromPoints(dat, extendRange=0.2), nrow=100, res=NULL)
  defaultArgs$rhrKDE$h <- as.list("href", "hlscv", "hpi", c(100, 100))

  ## BBMM
  defaultArgs$rhrBBMM <- list()
  defaultArgs$rhrBBMM$levels <- 95
  defaultArgs$rhrBBMM$rangesigma1 <- c(1, 10000)
  defaultArgs$rhrBBMM$sigma2 <- 10
  defaultArgs$rhrBBMM$trast <- rhrRasterFromExt(rhrExtFromPoints(dat, extendRange=0.2), nrow=100, res=NULL)

  ## UniNorm
  defaultArgs$rhrUniNorm <- list()
  defaultArgs$rhrUniNorm$levels <- 95
  defaultArgs$rhrUniNorm$trast <- rhrRasterFromExt(rhrExtFromPoints(dat, extendRange=0.2), nrow=100, res=NULL)
  
  ## UniCirc
  defaultArgs$rhrUniCirc <- list()
  defaultArgs$rhrUniCirc$levels <- 95
  defaultArgs$rhrUniCirc$trast <- rhrRasterFromExt(rhrExtFromPoints(dat, extendRange=0.2), nrow=100, res=NULL)

  ## BiNorm
  defaultArgs$rhrBiNorm <- list()
  defaultArgs$rhrBiNorm$levels <- 95
  defaultArgs$rhrBiNorm$trast <- rhrRasterFromExt(rhrExtFromPoints(dat, extendRange=0.2), nrow=100, res=NULL)
  
  ## UniCirc
  defaultArgs$rhrBiCirc <- list()
  defaultArgs$rhrBiCirc$levels <- 95
  defaultArgs$rhrBiCirc$trast <- rhrRasterFromExt(rhrExtFromPoints(dat, extendRange=0.2), nrow=100, res=NULL)

  ## LoCoH
  defaultArgs$rhrLoCoH <- list()
  defaultArgs$rhrLoCoH$levels <- 95
  defaultArgs$rhrLoCoH$type <- c("k")
  defaultArgs$rhrLoCoH$n <- c(10)
  defaultArgs$rhrLoCoH$autoN <- c(TRUE)

  ## Asymptote
  defaultArgs$rhrAsymptote <- list()
  defaultArgs$rhrAsymptote$minNP <- 100
  defaultArgs$rhrAsymptote$include <- "rhrMCP"
  defaultArgs$rhrAsymptote$nrep <- 10
  defaultArgs$rhrAsymptote$tolTotArea <- 0.05
  defaultArgs$rhrAsymptote$nTimes <- 2
  defaultArgs$rhrAsymptote$si <- 100
  defaultArgs$rhrAsymptote$sampling <- "random"
  
  ## Core Area
  defaultArgs$rhrCoreArea <- list()
  defaultArgs$rhrCoreArea$include <- c("rhrKDE")
  
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

  resList$summary <- list()

  resLog <- c(resLog, paste0("[", Sys.time(), "]: Preparing Data"))

  ## split data by animal
  resLog <- c(resLog, paste0("[", Sys.time(), "]: Splitting data by animal"))

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
    
    resList$est[[thisEst]]$res <- lapply(names(dat), function(animal) {
      resLog <- logProg(resLog, thisEst, animal, dat, inGUI)
      
      sf <- tryCatch(rhrSiteFidelity(dat[[animal]], n=args[[thisEst]]$n, alpha=args[[thisEst]]$alpha), error=function(e) e)
      if (inherits(sf, "error")) {
        return(list(est = rhrRes(sf, animal, thisEst, outDirs, msg = list(list(name="Error", message=sf$message)))))
      } else {
        res <-  rhrRes(sf, animal, thisEst, outDirs, figLegend = "Site fidelity plots")
        return(list(est = res))
      }
    })
    names(resList$est[[thisEst]]$res) <- names(dat)

    ## Parameters
    resList$est[[thisEst]]$parameters <- saveParameters(thisEst, outDirData, args)
  } 


  ## ------------------------------------------------------------------------------ ##  
  ## rhrTTSI

  if ("rhrTTSI" %in% what) {
    thisEst <- "rhrTTSI"
    resLog <- logStartEst(resLog, thisEst, inGUI, detail="...") 

    ## sanity check on args
    args[[thisEst]] <- checkArgs(args[[thisEst]], defaultArgs[[thisEst]], c("interval", "consec", "ntimes", "alpha"))

    ## Define possible different scenarios here
    resList$est[[thisEst]] <- list()

    resList$est[[thisEst]]$res <- lapply(names(dat), function(animal) {
      resLog <- logProg(resLog, thisEst, animal, dat, inGUI)
        ttsi <- tryCatch(rhrTTSI(dat[[animal]], 
                                 ntimes=args[[thisEst]]$ntimes, consec=args[[thisEst]]$consec, 
                                 interval = args[[thisEst]]$interval), error=function(e) e)
        if (inherits(ttsi, "error")) {
          return(list(est = rhrRes(ttsi, animal, thisEst, outDirs, msg = list(list(name="Error", message=ttsi$message)))))
        } else {
          msg <- list(
            list(name="What does it mean", 
                 msg=paste0("Time to statistical independence was ", if (ttsi$cvReached) paste0("reached after ", ttsi$cvReachedAt, " seconds.") else "not reached.")))
          res <-  rhrRes(ttsi, animal, thisEst, outDirs, msg, figLegend = "Time to statistical independence")
          return(list(est = res))
        }
    })
    names(resList$est[[thisEst]]$res) <- names(dat)

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

    resList$est[[thisEst]]$res <- lapply(names(dat), function(animal) {
      resLog <- logProg(resLog, thisEst, animal, dat, inGUI)
      mcp <- tryCatch(rhrMCP(dat[[animal]], levels=args[[thisEst]]$levels), error=function(e) e)
      
      if (inherits(mcp, "error")) {
        return(list(est = rhrRes(mcp, animal, thisEst, outDirs, msg = list(list(name="Error", message=mcp$message)))))
      } else {
        res <-  rhrRes(mcp, animal, thisEst, outDirs)
        
        ## hr area
        pArea <- rhrPArea(mcp, thisEst, animal, args, outDirs, inUnit, outUnit)
        pAsymptote <- rhrPAsymptote(mcp, thisEst, animal, args, outDirs, what)
        
        return(list(est = res,
                    properties = list(area = pArea, 
                                      asymptote = pAsymptote)))
      }
    })
    names(resList$est[[thisEst]]$res) <- names(dat)

    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- mergeIsos(resList, thisEst, names(dat))
    ## Parameters
    resList$est[[thisEst]]$parameters <- saveParameters(thisEst, outDirData, args)
  }

  ## ------------------------------------------------------------------------------ ##  
  ## rhrKDE

  if ("rhrKDE" %in% what) {
    thisEst <- "rhrKDE"
    resLog <- logStartEst(resLog, thisEst, inGUI, detail="...") 
    args[[thisEst]] <- checkArgs(args[[thisEst]], defaultArgs[[thisEst]], c("levels", "trast", "h"))
    resList$est[[thisEst]] <- list()

    resList$est[[thisEst]]$res <- lapply(names(dat), function(animal) {
      ## Log progress
      resLog <- logProg(resLog, thisEst, animal, dat, inGUI)

        ## h (its not possible yet to control h, if so you should calc a priori
        if (args[[thisEst]]$h == "user") {
          hres <- list(h=rep(args[[thisEst]]$userh, 2), name="user")
        } else if (args[[thisEst]]$h == "href") {
          hres <- rhrHref(dat[[animal]])
        } else if (args[[thisEst]]$h == "hlscv") {
          hres <- rhrHlscv(dat[[animal]], trast=args[[thisEst]]$trast)
        } else if (args[[thisEst]]$h == "hpi") {
          hres <- rhrHpi(dat[[animal]])
        } else {
          hres$msg <- "sorry, h input not understood, this calculation will be skipped"
        }

        kde <- tryCatch(rhrKDE(dat[[animal]], h=hres$h, trast=args[[thisEst]]$trast,
                               levels = args[[thisEst]]$levels), error=function(e) e)

        if (inherits(kde, "error")) {
          return(list(est = rhrRes(kde, animal, thisEst, outDirs, msg = list(list(name="Error", msg=kde$message)))))
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

          res <-  rhrRes(kde, animal, thisEst, outDirs, msgs)
          pArea <- rhrPArea(kde, thisEst, animal, args, outDirs, inUnit, outUnit)
          pAsymptote <- rhrPAsymptote(kde, thisEst, animal, args, outDirs, what)
          pCA <- rhrPCA(kde, thisEst, animal, args, outDirs, what)

          return(list(est = res, properties = list(area = pArea,
                                                   asymptote = pAsymptote, 
                                                   corearea = pCA)
          ))
        }
    })
    names(resList$est[[thisEst]]$res) <- names(dat)

    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- mergeIsos(resList, thisEst, names(dat))

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

    resList$est[[thisEst]]$res <- lapply(names(dat), function(animal) {
      resLog <- logProg(resLog, thisEst, animal, dat, inGUI)

        bbmm <- tryCatch(rhrBBMM(dat[[animal]], rangesigma1=args[[thisEst]]$rangesigma1, 
                                 sigma2=args[[thisEst]]$sigma2,
                                 trast=args[[thisEst]]$trast), error=function(e) e)

        if (inherits(bbmm, "error")) {
          return(list(est = rhrRes(bbmm, animal, thisEst, outDirs, msg = list(list(name="Error", message=bbmm$message)))))
        } else {

          ## tables
          msgs <- list(
            list(name = "Tuning parameter (Sigma 1)",
                      msg = paste0("The used value for Sigma 1 is: ", paste0(round(bbmm$sigma1, 2), collapse=","))))

          res <-  rhrRes(bbmm, animal, thisEst, outDirs, msgs)
          pArea <- rhrPArea(bbmm, thisEst, animal, args, outDirs, inUnit, outUnit)
          pCA <- rhrPCA(bbmm, thisEst, animal, args, outDirs, what)

          return(list(est = res), properties = list(area = pArea, 
                                                    corearea = pCA))
        }
    })
    names(resList$est[[thisEst]]$res) <- names(dat)

    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- mergeIsos(resList, thisEst, names(dat))

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

    resList$est[[thisEst]]$res <- lapply(names(dat), function(animal) {
      resLog <- logProg(resLog, thisEst, animal, dat, inGUI)

      est <- tryCatch(rhrUniNorm(dat[[animal]], trast=args[[thisEst]]$trast), error=function(e) e)
      
      if (inherits(est, "error")) {
        return(list(est = rhrRes(est, animal, thisEst, outDirs, msg = list(list(name="Error", msg=est$message)))))
      } else {

        ## tables
        msgs <- list(
          list(name = "Results",
               msg = paste0("Mu (", paste0(round(est$parameters$mean, 2), collapse=", "), "); Sigma (",
                            paste0(round(est$parameters$sigma, 2), collapse=", "), "); AIC (", round(est$AIC, 2), "); AICc ", round(est$AICc, 2))))
        res <-  rhrRes(est, animal, thisEst, outDirs, msgs)
        pArea <- rhrPArea(est, thisEst, animal, args, outDirs, inUnit, outUnit)
        pAsymptote <- rhrPAsymptote(est, thisEst, animal, args, outDirs, what)
        pCA <- rhrPCA(est, thisEst, animal, args, outDirs, what)
        
        return(list(est = res, properties = list(area = pArea,
                                                 asymptote = pAsymptote, 
                                                 corearea = pCA)))
      }
    })
    names(resList$est[[thisEst]]$res) <- names(dat)
    
    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- mergeIsos(resList, thisEst, names(dat))
    
    ## Parameters
    resList$est[[thisEst]]$parameters <- saveParameters(thisEst, outDirData, args)
  }
  
  ## ------------------------------------------------------------------------------ ##  
  ## rhrUniCirc

  if ("rhrUniCirc" %in% what) {
    thisEst <- "rhrUniCirc"
    resLog <- logStartEst(resLog, thisEst, inGUI, detail="...") 

    ## sanity check on args
    args[[thisEst]] <- checkArgs(args[[thisEst]], defaultArgs[[thisEst]], c("levels", "trast"))
    ## Define possible different scenarios here
    resList$est[[thisEst]] <- list()

    resList$est[[thisEst]]$res <- lapply(names(dat), function(animal) {
      resLog <- logProg(resLog, thisEst, animal, dat, inGUI)

      est <- tryCatch(rhrUniCirc(dat[[animal]], trast=args[[thisEst]]$trast), error=function(e) e)
      
      if (inherits(est, "error")) {
        return(list(est = rhrRes(est, animal, thisEst, outDirs, msg = list(list(name="Error", msg=est$message)))))
      } else {

        ## tables
        msgs <-  list(
          list(name = "Results",
               msg = paste0("Mu (", paste0(round(est$parameters$mean, 2), collapse=", "), "); a (",
                            paste0(round(est$parameters$a, 2), collapse=", "), "); AIC (", round(est$AIC, 2), "); AICc ", round(est$AICc, 2))))
        res <- rhrRes(est, animal, thisEst, outDirs, msgs)
        pArea <- rhrPArea(est, thisEst, animal, args, outDirs, inUnit, outUnit)
        pAsymptote <- rhrPAsymptote(est, thisEst, animal, args, outDirs, what)
        pCA <- rhrPCA(est, thisEst, animal, args, outDirs, what)
        
        return(list(est = res, properties = list(area = pArea,
                                                 asymptote = pAsymptote,
                                                 corearea = pCA)))
      }
    })
    names(resList$est[[thisEst]]$res) <- names(dat)
    
    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- mergeIsos(resList, thisEst, names(dat))
    
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
    
    resList$est[[thisEst]]$res <- lapply(names(dat), function(animal) {
      resLog <- logProg(resLog, thisEst, animal, dat, inGUI)

        est <- tryCatch(rhrBiNorm(dat[[animal]], trast=args[[thisEst]]$trast), error=function(e) e)

        if (inherits(est, "error")) {
          return(list(est = rhrRes(est, animal, thisEst, outDirs, msg = list(list(name="Error", msg=est$message)))))
        } else {

          ## tables
          msgs <- list(
            list(name = "Parameters",
                 msg = paste0("Mu1 (", paste0(round(est$parameters$mean1, 2), collapse=", "), "); Sigma1 (",
                              paste0(round(est$parameters$sigma1, 2), collapse="), "),
                              ") Mu2 (", paste0(round(est$parameters$mean2, 2), collapse=", "), "); Sigma2 (",
                              paste0(round(est$parameters$sigma2, 2), collapse=", "),
                              "); AIC (", round(est$AIC, 2), "); AICc ", round(est$AICc, 2))))
          res <-  rhrRes(est, animal, thisEst, outDirs, msgs)
          pArea <- rhrPArea(est, thisEst, animal, args, outDirs, inUnit, outUnit)
          pAsymptote <- rhrPAsymptote(est, thisEst, animal, args, outDirs, what)
          pCA <- rhrPCA(kde, thisEst, animal, args, outDirs, what)
          
          return(list(est = res, properties = list(area = pArea,
                                                   asymptote = pAsymptote,
                                                   corearea = pCA)))
        }
    })
    names(resList$est[[thisEst]]$res) <- names(dat)
    
    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- mergeIsos(resList, thisEst, names(dat))
    
    ## Parameters
    resList$est[[thisEst]]$parameters <- saveParameters(thisEst, outDirData, args)
  }
  
  ## ------------------------------------------------------------------------------ ##  
  ## rhrBiCirc

  if ("rhrBiCirc" %in% what) {
    thisEst <- "rhrBiCirc"
    resLog <- logStartEst(resLog, thisEst, inGUI, detail="...") 

    ## sanity check on args
    args[[thisEst]] <- checkArgs(args[[thisEst]], defaultArgs[[thisEst]], c("levels", "trast"))

    ## Define possible different scenarios here
    resList$est[[thisEst]] <- list()
    
    resList$est[[thisEst]]$res <- lapply(names(dat), function(animal) {
      resLog <- logProg(resLog, thisEst, animal, dat, inGUI)

        est <- tryCatch(rhrBiCirc(dat[[animal]], trast=args[[thisEst]]$trast), error=function(e) e)

        if (inherits(est, "error")) {
          return(list(est = rhrRes(est, animal, thisEst, outDirs, msg = list(list(name="Error", msg=est$message)))))
        } else {

          ## msg
          msgs <- list(
            list(name = "Parameters",
                 msg = paste0("Mu1 (", paste0(round(est$parameters$mean1, 2), collapse=", "), "); Sigma1 (",
                              paste0(round(est$parameters$sigma1, 2), collapse="), "),
                              ") Mu2 (", paste0(round(est$parameters$mean2, 2), collapse=", "), "); Sigma2 (",
                              paste0(round(est$parameters$sigma2, 2), collapse=", "),
                              "); AIC (", round(est$AIC, 2), "); AICc ", round(est$AICc, 2))))
          
          res <-  rhrRes(est, animal, thisEst, outDirs, msgs)
          pArea <- rhrPArea(est, thisEst, animal, args, outDirs, inUnit, outUnit)
          pAsymptote <- rhrPAsymptote(est, thisEst, animal, args, outDirs, what)
          pCA <- rhrPCA(kde, thisEst, animal, args, outDirs, what)
          
          return(list(est = res, properties = list(area = pArea,
                                                   asymptote = pAsymptote,
                                                   corearea = pCA)))
        }
    })
    names(resList$est[[thisEst]]$res) <- names(dat)
    
    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- mergeIsos(resList, thisEst, names(dat))
    
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

    resList$est[[thisEst]]$res <- lapply(names(dat), function(animal) {
      resLog <- logProg(resLog, thisEst, animal, dat, inGUI)
      locoh <- tryCatch(rhrLoCoH(dat[[animal]], type=args[[thisEst]]$type, 
                                 autoN=args[[thisEst]]$autoN, n=args[[thisEst]]$n, 
                                 levels=args[[thisEst]]$levels), error=function(e) e)
      
      if (inherits(locoh, "error")) {
        return(list(est = rhrRes(locoh, animal, thisEst, outDirs, msg = list(list(name="Error", msg=locoh$message)))))
      } else {
        
        msgs <- list(
          list(name = "Tuning Parameter",
               msg = paste0("The value of the tuning parameter (", 
                            rhrTuningParameter(locoh)$name ,") used is: ", 
                            rhrTuningParameter(locoh)$value)))
        
        
        res <- rhrRes(locoh, animal, thisEst, outDirs, msgs)
        
        ## hr area
        pArea <- rhrPArea(locoh, thisEst, animal, args, outDirs, inUnit, outUnit)
        
        return(list(est = res,
                    properties = list(area = pArea)))
      }
    })
    names(resList$est[[thisEst]]$res) <- names(dat)
    ## merge isopleths of animals along scenarios
    resList$est[[thisEst]]$allAnimals <- mergeIsos(resList, thisEst, names(dat))

    ## Parameters
    resList$est[[thisEst]]$parameters <- saveParameters(thisEst, outDirData, args)
  }
  
  runtime <- Sys.time() - startTime
  
  ## ------------------------------------------------------------------------------ ##  
  
  if (report) {
    ## html report
    files <- system.file("guiTemp", package="rhr")
    brewEnv <- list2env(list(
      config=args, 
      runtime=runtime,
      res=resList,
      baseDir=outDir,
      steps=what, 
      dat=dat,
      methodLookup=methodLookup, 
      startTime = startTime, 
      zip = zip, 
      repArgs = repArgs
    ))

    knitEnv <- list2env(list(
      config=args, 
      dat=dat
    ))

    src <- capture.output(brew::brew(file=normalizePath(file.path(files, "body.brew"), winslash="/", mustWork=FALSE), 
                                     output=stdout(), envir=brewEnv))
    
    foo <- knitr::knit(text=src, output=normalizePath(file.path(outDir, "rhrReport.Rmd"), 
                                                      mustWork=FALSE, winslash="/"), quiet=TRUE, envir=knitEnv)
    
    markdown::markdownToHTML(
      output=normalizePath(file.path(outDir, "rhrReport.html"), mustWork=FALSE, winslash="/"), 
      file=normalizePath(file.path(outDir, "rhrReport.Rmd"), mustWork=FALSE, winslash="/"), 
      stylesheet=normalizePath(file.path(files, "style.css"), mustWork=FALSE, winslash="/"),
      template=normalizePath(file.path(files, "index.html"), mustWork=FALSE, winslash="/"))
    
    
    # ------------------------------------------------------------------------------ ##  
    # Clean up
    for (file in c("rhrReport.Rnw", "rhrReport.Rmd", "rhrReport.tex")) {
      if (file.exists(normalizePath(file.path(outDir, file), mustWork = FALSE, winslash = "/"))) 
        file.remove(normalizePath(file.path(outDir, file), mustWork = FALSE, winslash = "/"))
    }
    
    
    if (zip) {
      ow <- setwd(outDir)
      zip(paste0(outDir, ".zip"), list.files(recursive=TRUE, full=TRUE))
      setwd(ow)
    }
    
    unlink(normalizePath(file.path(outDir, "figure"), mustWork=FALSE, winslash="/"), recursive=TRUE)
  }
  
  return(resList)
}
