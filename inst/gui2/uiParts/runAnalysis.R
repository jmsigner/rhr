
data4 <- reactive({
  ## Just in case some skips the subset step
  if (!is.null(data3())) {
    data4 <- data3()
  } else {
    if (!is.null(data2())) {
      data4 <- data2()
    } else {
      data4 <- NULL
    }
  }
})


observeEvent(input$rhrAnalyze, function() {
  cat("starting with analys\n")
  
  if (!is.null(data4())) {
  cat("dat4 not null\n")
    closeAlert(session, "rhrAnalyzeInfo1")
    
    runId <- paste0("rhr-run-", format(now(), "%Y%m%d%H%M%S"))
    outDir <- normalizePath(file.path(normalizePath(tempdir(), mustWork=FALSE, winslash="/"), runId), mustWork=FALSE, winslash="/")
    dir.create(outDir)
    addResourcePath("out", outDir)
    
    ## Create args
    args <- list(
      rhrSiteFidelity=list(
        n=getConfigValue("configSiteFidelityN", input, config2),
        alpha=getConfigValue("configSiteFidelityAlpha", input, config2)), 
      rhrKDE=list(
        levels=rhrCorrectLevels(getConfigValue("configGlobalLevel", input, config2)),
        trast=trast(), 
        h=getConfigValue("configKDEbandwidth", input, config2),
        userh=getConfigValue("configKDEbandwidthUser", input, config2)), 
      rhrBBMM=list(
        levels=rhrCorrectLevels(getConfigValue("configGlobalLevel", input, config2)), 
        trast=trast(), 
        sigma2=getConfigValue("configBBMMSigma2", input, config2), 
        #################################################
        ###################### TODO ADJUST SIGMA 1/2 here
        #################################################
        sigma2=getConfigValue("configBBMMRangeSigma1", input, config2)
      ), 
      rhrMCP=list(
        levels=rhrCorrectLevels(getConfigValue("configGlobalLevel", input, config2))
      ),
      rhrTTSI=list(
        init =   getConfigValue("configTTSIInt", input, config2), 
        consec = getConfigValue("configTTSISampling", input, config2), 
        ntimes = getConfigValue("configTTSINTimes", input, config2), 
        alpha =  getConfigValue("configTTSIAlpha", input, config2)
      ), 
#      rhrLoCoH=list(
#        levels=rhrCorrectLevels(getConfigValue("configGlobalLevel", input, config2)), 
#        if (any(is.null(input[[paste0("configLOCOHtype", c("A", "K", "R"))]]))) {
#          n=do.call(base::c, list(
#            switch(input$configLOCOHtypeK, "inclm" = input$configLOCOHtypeKmanN, "incla" = 10),
#            switch(input$configLOCOHtypeA, "inclm" = input$configLOCOHtypeAmanN, "incla" = 10),
#            switch(input$configLOCOHtypeR, "inclm" = input$configLOCOHtypeRmanN, "incla" = 10))), 
#        } else {
#          n = 10
#        }
#        autoN=do.call(base::c, list(
#          switch(input$configLOCOHtypeK, "incla" = TRUE, "inclm" = FALSE),
#          switch(input$configLOCOHtypeA, "incla" = TRUE, "inclm" = FALSE),
#          switch(input$configLOCOHtypeR, "incla" = TRUE, "inclm" = FALSE))),
#        type=do.call(base::c, list(
#          switch(input$configLOCOHtypeK, "incla" = "k", "inclm" = "k", "not" = NULL),
#          switch(input$configLOCOHtypeA, "incla" = "a", "inclm" = "a", "not" = NULL),
#          switch(input$configLOCOHtypeR, "incla" = "r", "inclm" = "r", "not" = NULL)))
      #), 
      rhrAsymptote=list(
         minNP =   getConfigValue("configAsymptoteMinNP", input, config2), 
         si =   getConfigValue("configAsymptoteSI", input, config2), 
         nrep =   getConfigValue("configAsymptoteNRep", input, config2), 
         tolTotArea =   getConfigValue("configAsymptoteTolToA", input, config2), 
         nTimes =   getConfigValue("configAsymptoteNTimes", input, config2), 
         sampling =   getConfigValue("configAsymptoteSampling", input, config2)
      ),
      rhrUniNorm=list(
        levels=rhrCorrectLevels(getConfigValue("configGlobalLevel", input, config2)),
        trast=trast()
      ),
      rhrBiNorm=list(
        levels=rhrCorrectLevels(getConfigValue("configGlobalLevel", input, config2)),
        trast=trast()
      )
    )
  
    
    createAlert(session, "rhrAnalyzeProgress", "rhrAnalyzeProgress1",
                "Starting Analysis",
                content = paste0("[", Sys.time(), "] Preparing the analysis"), 
                style="info", 
                dismiss=FALSE,
                append=FALSE)
    
    withProgress(message="starting analysis", {
      
      setProgress(message="Preparing calculations", detail="creating output files .....")
      files <- system.file("guiTemp", package="rhr")
      createAlert(session, "rhrAnalyzeProgress", "rhrAnalyzeProgress2",
                  "Starting Calculations",
                  content = paste0("[", Sys.time(), "] Starting with calculations, this may take some time"), 
                  style="info", 
                  dismiss=FALSE,
                  append=TRUE)
      
#      ## ------------------------------------------------------------------------------ ##  
#      ## Run the whole analysis
      starttime <- Sys.time()
      runtime <- system.time(res <- rhrHrAnalysis(data4(),
                                                  what=c(input$runSteps, input$runSteps2), 
                                                  args=args,
                                                  outDir=outDir,
                                                  inUnit=getConfigValue("configOutputInUnits", input, config2), 
                                                  outUnit=getConfigValue("configOutputOutUnits", input, config2), 
                                                  inGUI=TRUE, 
                                                  report = TRUE, 
                                                  createPDF = FALSE), gcFirst=TRUE)
      if (debug) cat(str(res))
      ## ------------------------------------------------------------------------------ ##  
      ## Brew html
      
      createAlert(session, "rhrAnalyzeProgress", "rhrAnalyzeProgress3",
                  "Report",
                  content = paste0("[", Sys.time(), "] Generating report"), 
                  style="info", 
                  dismiss=FALSE,
                  append=TRUE)
      
#      if (FALSE) {  ## this can most likely go after some testing
#        brewEnv <- list2env(list(
#          config=config(), 
#          runtime=runtime,
#          debug=TRUE, 
#          res=res,
#          baseDir=outDir,
#          steps=input$selectStep, 
#          dat=data4(),
#          subsetTable=subsetTable(),
#          methodLookup=methodLookup,
#          epsgs=list(
#            input=input$configInEpsg, 
#            output=input$configOutEpsg),
#          starttime = starttime
#        ))
#        
#        knitEnv <- list2env(list(
#          config=config(), 
#          dat=data4(),
#          data2=data2()$dat,
#          data3=data3(),
#          subsetTable=subsetTable(),
#          relocTable=missingAndDuplicated()
#        ))
#        
#        setProgress(message="Creating html file")
#        src <- capture.output(brew(file=normalizePath(file.path(files, "body.brew"), winslash="/", mustWork=FALSE), output=stdout(), envir=brewEnv))
#        
#        foo <- knit(text=src, output=normalizePath(file.path(outDir, "rhrReport.Rmd"), mustWork=FALSE, winslash="/"), quiet=TRUE,
#                    envir=knitEnv)
#        
#        setProgress(message="Just opening files", detail="new tab :) .....")
#        markdownToHTML(
#          output=normalizePath(file.path(outDir, "rhrReport.html"), mustWork=FALSE, winslash="/"), 
#          file=normalizePath(file.path(outDir, "rhrReport.Rmd"), mustWork=FALSE, winslash="/"), 
#          stylesheet=normalizePath(file.path(files, "style.css"), mustWork=FALSE, winslash="/"),
#          template=normalizePath(file.path(files, "index.html"), mustWork=FALSE, winslash="/"))
#        
#        ## ------------------------------------------------------------------------------ ##  
#        ## Clean up
#        if (!debug) {
#          file.remove(normalizePath(file.path(outDir, "rhrReport.Rnw"), mustWork=FALSE, winslash="/"))
#          file.remove(normalizePath(file.path(outDir, "rhrReport.Rmd"), mustWork=FALSE, winslash="/"))
#          file.remove(normalizePath(file.path(outDir, "rhrReport.tex"), mustWork=FALSE, winslash="/"))
#        }
#        unlink(normalizePath(file.path(outDir, "figure"), mustWork=FALSE, winslash="/"), recursive=TRUE)
#        
#      }
#      
#      if (config()$general$content$doCp) {
#        dir.create(normalizePath(file.path(config()$general$content$wd, runId), mustWork=FALSE, winslash="/"))
#        filesNames <- list.files(outDir, full.names=TRUE, recursive=FALSE, ignore.case=TRUE)
#        sapply(filesNames, function(x)
#          file.copy(from=x, to=normalizePath(file.path(config()$general$content$wd, runId), mustWork=FALSE, winslash="/")))
#        
#        for (f in c("data", "plots", "vector", "raster")) {
#          dir.create(normalizePath(file.path(config()$general$content$wd, runId, f), mustWork=FALSE, winslash="/"))
#          filesNames <- list.files(normalizePath(file.path(outDir, "results", f), mustWork=FALSE, winslash="/"), full.names=TRUE, recursive=TRUE,
#                                   ignore.case=TRUE)
#          sapply(filesNames, function(x)
#            file.copy(from=x, to=normalizePath(file.path(config()$general$content$wd, runId, f), mustWork=FALSE, winslash="/")))
#        }
#        
#      }
#      
      createAlert(session, "rhrAnalyzeProgress", "rhrAnalyzeProgress5",
                  "Finish",
                  content =paste0("[", Sys.time(), "] Finished analysis"), 
                  style="info", 
                  dismiss=FALSE,
                  append=TRUE)
      
    })
#    browseURL(normalizePath(file.path(outDir, "rhrReport.html"), mustWork=FALSE, winslash="/"))
  } else {
    createAlert(session, "rhrAnalyzeInfo", "rhrAnalyzeInfo1",
                "Not ready yet",
                content ="Please load and remap the data before you try to run an analysis", 
                style="info", 
                dismiss=TRUE,
                append=FALSE)
  }
})
