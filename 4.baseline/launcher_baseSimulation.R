
# Baseline simulation -----------------------------------------------------

library("osmose")

configDir  = "osmose-hum_v3"
configFile = file.path(configDir, "osmose_hum_baseLine.csv")
outputDir  = "output_baseLine"

#jarFile   = file.path(configDir, "osmose.jar")

#run_osmose(input = configFile, output = outputDir, osmose = jarFile, version=3)

# Reading outputs ---------------------------------------------------------

base_hum3 = read_osmose(path = file.path(outputDir))

# extract the biomass and yields variables (monthly data)
osmose.biomass   = base_hum3$biomass
osmose.abundance = base_hum3$abundance
osmose.yield     = base_hum3$yield
osmose.yieldN    = base_hum3$yieldN
osmose.meanTL    = base_hum3$meanTL

output_baseLine = list(
  
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

# Estimation of base line values ------------------------------------------

getBaseLine = function(x, var){
  
  assign("var", var)
  species = c("anchovy", "hake", "sardine", "jurel",
              "caballa", "meso", "munida", "pota", "euphausidos")
  
  # outpus variables
  baseLine_mean   = NULL
  baseLine_median = NULL
  baseLine_first  = NULL
  
  for(sp in seq_along(species)){
    
    # meanValue
    meanValue   = mean(apply(x[[paste(species[sp], var, sep = ".")]], 2, mean, na.rm = TRUE), na.rm =  TRUE)
    
    # medianValue
    medianValue = mean(apply(x[[paste(species[sp], var, sep = ".")]], 2, median, na.rm = TRUE), na.rm = TRUE)
    
    # firstValue
    firstValue  = mean(apply(x[[paste(species[sp], var, sep = ".")]], 2, function(x) (x[x>0 & !is.na(x)])[1]), na.rm = TRUE)
    
    baseLine_mean[sp]   = meanValue
    baseLine_median[sp] = medianValue
    baseLine_first[sp]  = firstValue
  }
  
  return(list(mean = baseLine_mean, median = baseLine_median, first = baseLine_first))
  
}

baseLine = list(biomass   = getBaseLine(x = output_baseLine, var = "biomass"),
                abundance = getBaseLine(x = output_baseLine, var = "abundance"),
                yield     = getBaseLine(x = output_baseLine, var = "yield"),
                yieldN    = getBaseLine(x = output_baseLine, var = "yieldN"),
                meanTL    = getBaseLine(x = output_baseLine, var = "meanTL"))

saveRDS(object = baseLine, file = "baseLine.rds")

# Saving outputs of baseline ----------------------------------------------

baseLine = readRDS("baseLine.rds")
