config <- list(
  pointLevel=list(
    sf=list(
      n=100,
      alpha=0.05),
    ttsi=list(
      interval=3600 * 10,
      sampling=c("random" = "FALSE", "consecutive" = "TRUE"),
      ntimes=3,
      alpha=0.25)), 
  homeRange=list(
    phr=list(
      levels=90,
      buffer=50,
      resolution=100),
    mcp=list(
      levels="90"
      ),
    kde=list(
      levels=90,
      bandwidthOptions=c("Reference Bandwidth" = "href",
        "Least Square Cross Validation" = "hlscv",
        "Plugin the equation" = "hpi",
        "User defined" = "user"), 
        ## "Scaling" = "hscale",
      bandwidth="href",
      buffer=50,
      resolution=50
      ), 
    locoh=list(
      levels="95", 
      type=c("k" = "typk", "a" = "typa", "r" = "typr", "min" = "typmin"),
      nk=40, 
      na=40, 
      nr=40 
      ),
    asymptote=list(
      minNP=100,
      SI=100, 
      NRep=10, 
      TotA=0.05,
      Alpha=0.05, 
      NTimes=4,
      sampling=c("Sequential" = "sequential", "Random" = "random")
      )
    )
  )

config2 <- list(
  ## Site fidelity
  configSiteFidelityN = 100,
  configSiteFidelityAlpha = 0.05, 
  
  ## TTSI
  configTTSIInt = 3600 * 10, 
  configTTSISampling = FALSE, 
  configTTSINTimes = 3,  
  configTTSIAlpha =  0.25, 
    
  ## KDE
  configGlobalLevel = 90, 
  configKDEbandwidth = "href", 
  configKDEbandwidthUser = 100, 
  
  ## BBMM
  configBBMMSigma2 = 10, 
  configBBMMRangeSigma1 = c(1, 1e4), 
  
  ## Asymptote
  configAsymptoteMinNP = 100,
  configAsymptoteSI = 100,
  configAsymptoteNRep = 10,
  configAsymptoteTolToA = 0.05,
  configAsymptoteNTimes = 4,
  configAsymptoteSampling = "random", 
  
  ## General
  configOutputCpWd = FALSE, 
  configOutputInUnits = "ido", 
  configOutputOutUnits = "ius"
)

