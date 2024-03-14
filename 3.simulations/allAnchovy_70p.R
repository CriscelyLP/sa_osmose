
# Packages ----------------------------------------------------------------
rm(list = ls())
require(osmose)

# Source of scripts -------------------------------------------------------

# directory
setwd("/home2/datahome/fhoungna/sensitivity_osmose")

source("internal-functions.R")
source("random-sampling.R")
source("elementary-effects.R")
source("methods.R")
source("auxiliar.R")

# 1. Parameter perturbation -----------------------------------------------

# Initial set of parameters: testing 27
parametersData = read.csv(file = "/home2/datawork/fhoungna/sensitivity/paper_protocol/c8_70p/data_perturbation/osm_parameters_allAnchovy-protocol.csv",
                          header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Estimation of min and max (limits of parameter distribution)
parametersData$percentage = rep(0.70, dim(parametersData)[1])
parametersData = rangeEstimation(parametersData, percentage = "percentage")
parametersData = get_limits(parametersData, range_min =  "range_min", range_max = "range_max")

# 2. Doe (design of experiments) ------------------------------------------
# Building the matrix with the design of experiments (doe)
doe = random_sampling(par = parametersData, r = 20, levels = 8, grid.jump = 4/7) # CHECK IT

# 3. run function ---------------------------------------------------------
# The user has to provide a function to evaluate for each parameter vector

run_model = function(par, names, ...) {
  
  # set parameter names
  names(par) = names
  
  # define the java and osmose executables
  setwd("/home2/datawork/fhoungna/sensitivity/paper_protocol/c8_70p")
  configDir  = "osmose-hum_v3"
  jarFile    = file.path(configDir, "osmose.jar")
  
  # initial configuration file
  modelConfig = read.csv(file = file.path(configDir, "osmose_hum.csv"), stringsAsFactors = FALSE, na.strings = c(""))
  
  # output directory
  outputDir = "output"
  
  # Manually changes about PREDATION ACCESSIBILITY
  predationAccessibility = read.csv(file.path(configDir, "input/predation/predation-accessibility.csv"), stringsAsFactors = FALSE, sep = ";")
  predationAccessibility$anchovy[c(2:9)] = par[c(1:8)]
  predationAccessibility$sardine[1]      = par[2]
  predationAccessibility$jurel[1]        = par[3]
  predationAccessibility$caballa[1]      = par[4]
  predationAccessibility$meso[1]         = par[5]
  predationAccessibility$pota[1]         = par[7]
  predationAccessibility$euphausidos[1]  = par[8]
  colnames(predationAccessibility)[1] = ""
  write.table(predationAccessibility, file = file.path(configDir, "newPredationAccessibility.csv"), row.names = FALSE, sep = ";")
  modelConfig[modelConfig[,1] == "predation.accessibility.file", 2] = "newPredationAccessibility.csv"
  
  # Manually changes about PREDATION SIZE RATIOS
  theta.sp0.stage1   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.theta.sp0.stage1"]) * (pi/2)
  alpha.sp0.stage1   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.alpha.sp0.stage1"]) * (pi/2)
  min.sp0.stage1 = 1/maxSlope(angle = theta.sp0.stage1, m_min = 0)
  max.sp0.stage1 = 1/maxSlope(angle = alpha.sp0.stage1, m_min = 1/min.sp0.stage1)
  
  theta.sp0.stage2   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.theta.sp0.stage2"]) * (pi/2)
  alpha.sp0.stage2   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.alpha.sp0.stage2"]) * (pi/2)
  min.sp0.stage2 = 1/maxSlope(angle = theta.sp0.stage2, m_min = 0)
  max.sp0.stage2 = 1/maxSlope(angle = alpha.sp0.stage2, m_min = 1/min.sp0.stage2)
  
  modelConfig[modelConfig[,1] == "predation.predPrey.sizeRatio.max.sp0", c(2,3)] = c(max.sp0.stage1, max.sp0.stage2)
  modelConfig[modelConfig[,1] == "predation.predPrey.sizeRatio.min.sp0", c(2,3)] = c(min.sp0.stage1, min.sp0.stage2)
  
  # PredPrey stage threshold
  Linf.sp0.per = par[names(par) == "species.lInf.sp0"]
  modelConfig[modelConfig[,1] == "predation.predPrey.stage.threshold.sp0", 2]    = par[names(par) == "predation.predPrey.stage.threshold.sp0"] * (Linf.sp0.per)
  
  # Starvation rate max, vonBertalanffy threshold
  modelConfig[modelConfig[,1] == "mortality.starvation.rate.max.sp0", 2]         = par[names(par) == "mortality.starvation.rate.max.sp0"]
  modelConfig[modelConfig[,1] == "species.vonbertalanffy.threshold.age.sp0", 2]  = par[names(par) == "species.vonbertalanffy.threshold.age.sp0"]
  
  # Manually changes about egg SIZE AND WEIGHT
  eggSize.sp0   = 0.1
  eggWeight.sp0 = 0.0005386
  meanDensity.sp0 = eggWeight.sp0 / ((4/3 * pi) * (eggSize.sp0/2)^3) 
  eggSize     = par[names(par) == "species.egg.size.sp0"]
  eggWeight   = (4/3 * pi) * (as.numeric(eggSize)/2)^3 * meanDensity.sp0
  modelConfig[modelConfig[,1] == "species.egg.weight.sp0", 2] = eggWeight
  modelConfig[modelConfig[,1] == "species.egg.size.sp0", 2]   = eggSize
  
  # Critical efficiency and predation ingestion rate 
  modelConfig[modelConfig[,1] == "predation.efficiency.critical.sp0", 2]  = par[names(par) == "predation.efficiency.critical.sp0"]
  modelConfig[modelConfig[,1] == "predation.ingestion.rate.max.sp0", 2]   = par[names(par) == "predation.ingestion.rate.max.sp0"]
  
  # Manually changes about natural mortality
  modelConfig[modelConfig[,1] == "mortality.natural.rate.sp0", 2]  = par[names(par) == "mortality.natural.rate.sp0"]
  
  # Manually changes about larval mortality: 19 par but perturbing the mean
  larvalMortality.sp0 = read.csv(file.path(configDir, "input/larval/larval_mortality-anchovy.csv"), stringsAsFactors = FALSE, sep = ";")
  lx  = log(larvalMortality.sp0$x)
  mlx = mean(lx) # perturbation using mlx: lx = exp()
  dlx = lx - mlx
  
  Lx = par[names(par) == "mortality.natural.larva.rate.Lx.sp0"]
  new_lx = dlx + log(Lx)
  new_x = exp(new_lx)
  
  newLarvalMortality.sp = larvalMortality.sp0
  newLarvalMortality.sp$x = new_x
  colnames(newLarvalMortality.sp)[1] = ""
  write.table(newLarvalMortality.sp, file = file.path(configDir, "newLavalMortality-anchovy.csv"), row.names = FALSE, sep = ";")
  modelConfig[modelConfig[,1] == "mortality.natural.larva.rate.bytDt.file.sp0", 2] = "newLavalMortality-anchovy.csv"
  
  # Manually changes about fishing mortality #### TO CHECK
  # 224 parameters: 1 (f media) + T (between years) + 12T (distribution of the fishing between years) + 2 (fishing selectivity)
  fishing_multiplier    = par[names(par) == "fishing.multiplier.sp0"]
  fishingMortality.sp0  = read.csv(file.path(configDir, "input/fishing/F-anchovy.csv"), stringsAsFactors = FALSE, sep = ";")
  fishingMortality.sp0[, c(2:45)] = fishingMortality.sp0[, c(2:45)] * fishing_multiplier
  colnames(fishingMortality.sp0) = c("", seq(from = 0, to = 21.5, by = 0.5))
  write.table(fishingMortality.sp0, file = file.path(configDir, "newFishingMortality-anchovy.csv"), row.names = FALSE, sep = ";")
  modelConfig[modelConfig[,1] == "mortality.fishing.rate.byDt.bySize.file.sp0", 2] = "newFishingMortality-anchovy.csv"
  
  # Sex ratio
  modelConfig[modelConfig[,1] == "species.sexratio.sp0", 2]  = par[names(par) == "species.sexratio.sp0"]
  
  # Von Bertalanffy parameters: l0 perturbed instead of t0
  t0.sp0   = -0.14
  K.sp0    = 0.76
  Linf.sp0 = 19.5
  
  newl0.sp0   = par[names(par) == "species.l0.sp0"]
  newt0.sp0   = t0.sp0 - (1 / K.sp0) * (log(1 - (newl0.sp0 / Linf.sp0)))
  modelConfig[modelConfig[,1] == "species.t0.sp0", 2]  = newt0.sp0
  
  # Von Bertalanffy parameters: K and lInf
  modelConfig[modelConfig[,1] == "species.K.sp0", 2]              = par[names(par) == "species.K.sp0"]
  modelConfig[modelConfig[,1] == "species.lInf.sp0", 2]           = par[names(par) == "species.lInf.sp0"]
  
  # maturity size
  modelConfig[modelConfig[,1] == "species.maturity.size.sp0", 2]  = par[names(par) == "species.maturity.size.sp0"] * (Linf.sp0.per)
  
  # Length to weight relationship: condition factor perturbed
  modelConfig[modelConfig[,1] == "species.length2weight.condition.factor.sp0", 2]  =  par[names(par) == "species.length2weight.condition.factor.sp0"]
  
  # NEW configuration file
  write.table(modelConfig, file = file.path(configDir, "config.csv"), na = "", sep = ",",
              quote = FALSE, row.names = FALSE, col.names = c(colnames(modelConfig)[c(1:2)], "", ""))
  
  # run Osmose Model
  run_osmose(input = file.path(configDir, "config.csv"), output = outputDir, osmose = jarFile, version = 3)
  
  # read Osmose outputs 
  data = read_osmose(path = file.path(outputDir))
  unlink(outputDir, recursive = TRUE) # remove outputs after read the results of simulation
  
  # extract the biomass and yields variables (monthly data)
  osmose.biomass   = data$biomass
  osmose.abundance = data$abundance
  osmose.yield     = data$yield
  osmose.yieldN    = data$yieldN
  osmose.meanTL    = data$meanTL
  
  output = list(
    
    # biomass
    anchovy.biomass       = osmose.biomass[, "anchovy",]    ,
    hake.biomass          = osmose.biomass[, "hake",]       ,
    sardine.biomass       = osmose.biomass[, "sardine",]    ,
    jurel.biomass         = osmose.biomass[, "jurel",]      ,
    caballa.biomass       = osmose.biomass[, "caballa",]    ,
    meso.biomass          = osmose.biomass[, "meso",]       ,
    munida.biomass        = osmose.biomass[, "munida",]     ,
    pota.biomass          = osmose.biomass[, "pota",]       ,
    euphausidos.biomass   = osmose.biomass[, "euphausidos",],
    
    # abundance
    anchovy.abundance       = osmose.abundance[, "anchovy",]    ,
    hake.abundance          = osmose.abundance[, "hake",]       ,
    sardine.abundance       = osmose.abundance[, "sardine",]    ,
    jurel.abundance         = osmose.abundance[, "jurel",]      ,
    caballa.abundance       = osmose.abundance[, "caballa",]    ,
    meso.abundance          = osmose.abundance[, "meso",]       ,
    munida.abundance        = osmose.abundance[, "munida",]     ,
    pota.abundance          = osmose.abundance[, "pota",]       ,
    euphausidos.abundance   = osmose.abundance[, "euphausidos",],
    
    # yield
    anchovy.yield       = osmose.yield[, "anchovy",]    ,
    hake.yield          = osmose.yield[, "hake",]       ,
    sardine.yield       = osmose.yield[, "sardine",]    ,
    jurel.yield         = osmose.yield[, "jurel",]      ,
    caballa.yield       = osmose.yield[, "caballa",]    ,
    meso.yield          = osmose.yield[, "meso",]       ,
    munida.yield        = osmose.yield[, "munida",]     ,
    pota.yield          = osmose.yield[, "pota",]       ,
    euphausidos.yield   = osmose.yield[, "euphausidos",],
    
    # yieldN
    anchovy.yieldN       = osmose.yieldN[, "anchovy",]    ,
    hake.yieldN          = osmose.yieldN[, "hake",]       ,
    sardine.yieldN       = osmose.yieldN[, "sardine",]    ,
    jurel.yieldN         = osmose.yieldN[, "jurel",]      ,
    caballa.yieldN       = osmose.yieldN[, "caballa",]    ,
    meso.yieldN          = osmose.yieldN[, "meso",]       ,
    munida.yieldN        = osmose.yieldN[, "munida",]     ,
    pota.yieldN          = osmose.yieldN[, "pota",]       ,
    euphausidos.yieldN   = osmose.yieldN[, "euphausidos",],
    
    # meanTL
    anchovy.meanTL       = osmose.meanTL[, "anchovy",]    ,
    hake.meanTL          = osmose.meanTL[, "hake",]       ,
    sardine.meanTL       = osmose.meanTL[, "sardine",]    ,
    jurel.meanTL         = osmose.meanTL[, "jurel",]      ,
    caballa.meanTL       = osmose.meanTL[, "caballa",]    ,
    meso.meanTL          = osmose.meanTL[, "meso",]       ,
    munida.meanTL        = osmose.meanTL[, "munida",]     ,
    pota.meanTL          = osmose.meanTL[, "pota",]       ,
    euphausidos.meanTL   = osmose.meanTL[, "euphausidos",]
  )
  
  return(output)
} 

# 4. save outputs ---------------------------------------------------------

start = date()
fixed70p_allAnchovy = run_experiments(X = doe, FUN = run_model, names=parametersData$parameter)
end   = date()

setwd("/home2/datawork/fhoungna/sensitivity/paper_protocol/c8_70p")
saveRDS(object = fixed70p_allAnchovy, file = "fixed70p_allAnchovy.rds")
