
# Processing baseline simulation to get total variables for the ecosystem
# e.g total biomass, total abundance, total yield, total yieldN


# Baseline simulation -----------------------------------------------------

library("osmose")

configDir  = "osmose-hum_v3"
configFile = file.path(configDir, "osmose_hum_baseLine.csv")
outputDir  = "output_baseLine"


# Reading outputs ---------------------------------------------------------

base_hum3 = read_osmose(path = file.path(outputDir))

outputTotal_baseLine = list(
  
  biomass       = apply(base_hum3$biomass   , c(1,3), sum) , #dim 204, 9, 10
  abundance     = apply(base_hum3$abundance , c(1,3), sum) ,
  yield         = apply(base_hum3$yield     , c(1,3), sum) ,
  yieldN        = apply(base_hum3$yieldN    , c(1,3), sum)
  
)

# Estimation of base line values ------------------------------------------

getBaseLine_total = function(x, var){
  
  assign("var", var)
  
  # dim 204 10, removing R and then t
  # meanValue
  meanValue   = mean(apply(x[[var]], 1, mean, na.rm = TRUE), na.rm =  TRUE)
    
  # medianValue
  medianValue = mean(apply(x[[var]], 1, median, na.rm = TRUE), na.rm = TRUE)
    
  # firstValue
  firstValue  = mean(apply(x[[var]], 1, function(x) (x[x>0 & !is.na(x)])[1]), na.rm = TRUE)
    
  return(list(mean = meanValue, median = medianValue, first = firstValue))
  
}

baseLine_total = list(biomass   = getBaseLine_total(x = outputTotal_baseLine, var = "biomass"   ),
                      abundance = getBaseLine_total(x = outputTotal_baseLine, var = "abundance" ),
                      yield     = getBaseLine_total(x = outputTotal_baseLine, var = "yield"     ),
                      yieldN    = getBaseLine_total(x = outputTotal_baseLine, var = "yieldN"    ))

saveRDS(object = baseLine_total, file = "output_baseLine/baseLine_totalVariables.rds")

