
# Post processing of results after running demo_osmose.R 

# Loading packages - scripts ----------------------------------------------

source("internal-functions.R")
source("random-sampling.R")
source("elementary-effects.R")
source("methods.R")

# Loading simulations outputs ---------------------------------------------

#baseLine       = readRDS("output_baseLine/baseLine.rds")                 # baseLine variables by sp
baseLineTotal  = readRDS("output_baseLine/baseLine_totalVariables.rds")   # baseLine variables total

# simulation outputs
p        = readRDS("paper_protocol/a1_protocol/outputs/protocol_allAnchovy.rds")
p_linear = readRDS("paper_protocol/b1_protocol_linear/outputs/protocol_linear_allAnchovy.rds")

fixed10 = readRDS("paper_protocol/c1_10p/outputs/fixed10p_allAnchovy.rds")
fixed15 = readRDS("paper_protocol/c2_15p/outputs/fixed15p_allAnchovy.rds")
fixed20 = readRDS("paper_protocol/c3_20p/outputs/fixed20p_allAnchovy.rds")
fixed30 = readRDS("paper_protocol/c4_30p/outputs/fixed30p_allAnchovy.rds")
fixed40 = readRDS("paper_protocol/c5_40p/outputs/fixed40p_allAnchovy.rds")
fixed50 = readRDS("paper_protocol/c6_50p/outputs/fixed50p_allAnchovy.rds")
fixed60 = readRDS("paper_protocol/c7_60p/outputs/fixed60p_allAnchovy.rds")
fixed70 = readRDS("paper_protocol/c8_70p/outputs/fixed70p_allAnchovy.rds")
fixed80 = readRDS("paper_protocol/c9_80p/outputs/fixed80p_allAnchovy.rds")
fixed85 = readRDS("paper_protocol/c10_85p/outputs/fixed85p_allAnchovy.rds")

fixed10_linear = readRDS("paper_protocol/b3_10p_linear/outputs/fixed10p_linear_allAnchovy.rds")
fixed15_linear = readRDS("paper_protocol/b3_15p_linear/outputs/fixed15p_linear_allAnchovy.rds")
fixed20_linear = readRDS("paper_protocol/b2_20p_linear/outputs/fixed20p_linear_allAnchovy.rds")
fixed30_linear = readRDS("paper_protocol/b3_30p_linear/outputs/fixed30p_linear_allAnchovy.rds")
fixed40_linear = readRDS("paper_protocol/b3_40p_linear/outputs/fixed40p_linear_allAnchovy.rds")
fixed50_linear = readRDS("paper_protocol/b3_50p_linear/outputs/fixed50p_linear_allAnchovy.rds")
fixed60_linear = readRDS("paper_protocol/b3_60p_linear/outputs/fixed60p_linear_allAnchovy.rds")
fixed70_linear = readRDS("paper_protocol/b3_70p_linear/outputs/fixed70p_linear_allAnchovy.rds")
fixed80_linear = readRDS("paper_protocol/b3_80p_linear/outputs/fixed80p_linear_allAnchovy.rds")
fixed85_linear = readRDS("paper_protocol/b3_85p_linear/outputs/fixed85p_linear_allAnchovy.rds")

# Functions to process outputs --------------------------------------------

# processing outputs by species
processing_outputs = function(x, baseLine, var, fn) {
  
  baseLine = baseLine[[var]][[fn]]
  species = c("anchovy", "hake", "sardine", "jurel",
              "caballa", "meso", "munida", "pota", "euphausidos")
  
  #by species
  out = NULL
  for(sp in seq_along(species)){
    
    stand       = x[[paste(species[sp], var, sep = ".")]] / baseLine[sp] #standardization
    deleteRT    = mean(apply(stand, 1, mean)) # removing dimentions
    
    out[sp] = deleteRT
  }
  
  return(out)
}

# removing the variables by species
getVar = function(x, var) {
  
  species = c("anchovy", "hake", "sardine", "jurel",
              "caballa", "meso", "munida", "pota", "euphausidos")
  
  newVar = array(NA, dim = c(204, 10, 9))
  
  for(sp in seq_len(length(species))) {
    newVar[,,sp] = x[[paste(species[sp], var, sep = ".")]]
  }
  
  newVar_tsr = aperm(newVar, c(1,3,2)) # dim 204, 9, 10
  totalVar   = apply(newVar_tsr, c(1,3), sum) # dim 204, 10
  
  return(totalVar)
}

# processing outputs without species (variable by system)
processing_outputs_total = function(x, baseLine, var, fn) {
  
  baseLine = baseLine[[var]][[fn]]
  
  stand       = x / baseLine #standardization
  deleteRT    = mean(apply(stand, 1, mean)) # removing dimentions
  
  return(deleteRT)
}

# Get biomass -------------------------------------------------------------

# totalBiomass of the system (removing biomass per species)
p_totalBiomass        = lapply(p        , getVar, var = "biomass")
plinear_totalBiomass  = lapply(p_linear , getVar, var = "biomass")

fixed10_totalBiomass = lapply(fixed10 , getVar, var = "biomass")
fixed15_totalBiomass = lapply(fixed15 , getVar, var = "biomass")
fixed20_totalBiomass = lapply(fixed20 , getVar, var = "biomass")
fixed30_totalBiomass = lapply(fixed30 , getVar, var = "biomass")
fixed40_totalBiomass = lapply(fixed40 , getVar, var = "biomass")
fixed50_totalBiomass = lapply(fixed50 , getVar, var = "biomass")
fixed60_totalBiomass = lapply(fixed60 , getVar, var = "biomass")
fixed70_totalBiomass = lapply(fixed70 , getVar, var = "biomass")
fixed80_totalBiomass = lapply(fixed80 , getVar, var = "biomass")
fixed85_totalBiomass = lapply(fixed85 , getVar, var = "biomass")

fixed10_linear_totalBiomass = lapply(fixed10_linear , getVar, var = "biomass")
fixed15_linear_totalBiomass = lapply(fixed15_linear , getVar, var = "biomass")
fixed20_linear_totalBiomass = lapply(fixed20_linear , getVar, var = "biomass")
fixed30_linear_totalBiomass = lapply(fixed30_linear , getVar, var = "biomass")
fixed40_linear_totalBiomass = lapply(fixed40_linear , getVar, var = "biomass")
fixed50_linear_totalBiomass = lapply(fixed50_linear , getVar, var = "biomass")
fixed60_linear_totalBiomass = lapply(fixed60_linear , getVar, var = "biomass")
fixed70_linear_totalBiomass = lapply(fixed70_linear , getVar, var = "biomass")
fixed80_linear_totalBiomass = lapply(fixed80_linear , getVar, var = "biomass")
fixed85_linear_totalBiomass = lapply(fixed85_linear , getVar, var = "biomass")

# Get DOE -----------------------------------------------------------------

doe_protocol = list(p        = attributes(p)$doe,
                    p_linear = attributes(p_linear)$doe)
rm(p, p_linear)

doe_fixed = list(fixed10 = attributes(fixed10)$doe,
                 fixed15 = attributes(fixed15)$doe,
                 fixed20 = attributes(fixed20)$doe,
                 fixed30 = attributes(fixed30)$doe, 
                 fixed40 = attributes(fixed40)$doe,
                 fixed50 = attributes(fixed50)$doe,
                 fixed60 = attributes(fixed60)$doe,
                 fixed70 = attributes(fixed70)$doe,
                 fixed80 = attributes(fixed80)$doe,
                 fixed85 = attributes(fixed85)$doe)
rm(fixed10, fixed15, fixed20, fixed30, fixed40, fixed50, fixed60, fixed70, fixed80, fixed85)

doe_fixed_linear = list(fixed10 = attributes(fixed10_linear)$doe,
                        fixed15 = attributes(fixed15_linear)$doe,
                        fixed20 = attributes(fixed20_linear)$doe,
                        fixed30 = attributes(fixed30_linear)$doe, 
                        fixed40 = attributes(fixed40_linear)$doe,
                        fixed50 = attributes(fixed50_linear)$doe,
                        fixed60 = attributes(fixed60_linear)$doe,
                        fixed70 = attributes(fixed70_linear)$doe,
                        fixed80 = attributes(fixed80_linear)$doe,
                        fixed85 = attributes(fixed85_linear)$doe)
rm(fixed10_linear, fixed15_linear, fixed20_linear, fixed30_linear, fixed40_linear,
   fixed50_linear, fixed60_linear, fixed70_linear, fixed80_linear, fixed85_linear)

# Standardization of variables --------------------------------------------

# standardization (total variables)
p_totalbiomass_mean = list(p        = lapply(p_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"),
                           p_linear = lapply(plinear_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"))
rm(p_totalBiomass, plinear_totalBiomass)

fixed_totalbiomass_mean = list(fixed10 = lapply(fixed10_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"),
                               fixed15 = lapply(fixed15_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"),
                               fixed20 = lapply(fixed20_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"),
                               fixed30 = lapply(fixed30_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"),
                               fixed40 = lapply(fixed40_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"),
                               fixed50 = lapply(fixed50_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"),
                               fixed60 = lapply(fixed60_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"),
                               fixed70 = lapply(fixed70_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"),
                               fixed80 = lapply(fixed80_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"),
                               fixed85 = lapply(fixed85_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"))
rm(fixed10_totalBiomass, fixed15_totalBiomass, fixed20_totalBiomass, fixed30_totalBiomass, fixed40_totalBiomass,
   fixed50_totalBiomass, fixed60_totalBiomass, fixed70_totalBiomass, fixed80_totalBiomass, fixed85_totalBiomass)


fixed_linear_totalbiomass_mean = list(fixed10_linear = lapply(fixed10_linear_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"),
                                      fixed15_linear = lapply(fixed15_linear_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"),
                                      fixed20_linear = lapply(fixed20_linear_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"),
                                      fixed30_linear = lapply(fixed30_linear_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"),
                                      fixed40_linear = lapply(fixed40_linear_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"),
                                      fixed50_linear = lapply(fixed50_linear_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"),
                                      fixed60_linear = lapply(fixed60_linear_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"),
                                      fixed70_linear = lapply(fixed70_linear_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"),
                                      fixed80_linear = lapply(fixed80_linear_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"),
                                      fixed85_linear = lapply(fixed85_linear_totalBiomass, processing_outputs_total, baseLine = baseLineTotal, var = "biomass", fn = "mean"))
rm(fixed10_linear_totalBiomass, fixed15_linear_totalBiomass, fixed20_linear_totalBiomass, fixed30_linear_totalBiomass, fixed40_linear_totalBiomass,
   fixed50_linear_totalBiomass, fixed60_linear_totalBiomass, fixed70_linear_totalBiomass, fixed80_linear_totalBiomass, fixed85_linear_totalBiomass)

# Estimation of SA indices ------------------------------------------------

# ee for protocol
ee_protocol = list(p        = sensitivity(X = doe_protocol$p,        Y = p_totalbiomass_mean$p,        method = "elementary_effects"),
                   p_linear = sensitivity(X = doe_protocol$p_linear, Y = p_totalbiomass_mean$p_linear, method = "elementary_effects"))
saveRDS(ee_protocol, file = "paper_protocol/ee_protocol.rds")

ee_fixed = list(fixed10 = sensitivity(X = doe_fixed$fixed10, Y = fixed_totalbiomass_mean$fixed10, method = "elementary_effects"),
                fixed15 = sensitivity(X = doe_fixed$fixed15, Y = fixed_totalbiomass_mean$fixed15, method = "elementary_effects"),
                fixed20 = sensitivity(X = doe_fixed$fixed20, Y = fixed_totalbiomass_mean$fixed20, method = "elementary_effects"),
                fixed30 = sensitivity(X = doe_fixed$fixed30, Y = fixed_totalbiomass_mean$fixed30, method = "elementary_effects"),
                fixed40 = sensitivity(X = doe_fixed$fixed40, Y = fixed_totalbiomass_mean$fixed40, method = "elementary_effects"),
                fixed50 = sensitivity(X = doe_fixed$fixed50, Y = fixed_totalbiomass_mean$fixed50, method = "elementary_effects"),
                fixed60 = sensitivity(X = doe_fixed$fixed60, Y = fixed_totalbiomass_mean$fixed60, method = "elementary_effects"),
                fixed70 = sensitivity(X = doe_fixed$fixed70, Y = fixed_totalbiomass_mean$fixed70, method = "elementary_effects"),
                fixed80 = sensitivity(X = doe_fixed$fixed80, Y = fixed_totalbiomass_mean$fixed80, method = "elementary_effects"),
                fixed85 = sensitivity(X = doe_fixed$fixed85, Y = fixed_totalbiomass_mean$fixed85, method = "elementary_effects"))
saveRDS(ee_fixed, file = "paper_protocol/ee_fixed.rds")

ee_fixed_linear = list(fixed10 = sensitivity(X = doe_fixed_linear$fixed10, Y = fixed_linear_totalbiomass_mean$fixed10, method = "elementary_effects"),
                       fixed15 = sensitivity(X = doe_fixed_linear$fixed15, Y = fixed_linear_totalbiomass_mean$fixed15, method = "elementary_effects"),
                       fixed20 = sensitivity(X = doe_fixed_linear$fixed20, Y = fixed_linear_totalbiomass_mean$fixed20, method = "elementary_effects"),
                       fixed30 = sensitivity(X = doe_fixed_linear$fixed30, Y = fixed_linear_totalbiomass_mean$fixed30, method = "elementary_effects"),
                       fixed40 = sensitivity(X = doe_fixed_linear$fixed40, Y = fixed_linear_totalbiomass_mean$fixed40, method = "elementary_effects"),
                       fixed50 = sensitivity(X = doe_fixed_linear$fixed50, Y = fixed_linear_totalbiomass_mean$fixed50, method = "elementary_effects"),
                       fixed60 = sensitivity(X = doe_fixed_linear$fixed60, Y = fixed_linear_totalbiomass_mean$fixed60, method = "elementary_effects"),
                       fixed70 = sensitivity(X = doe_fixed_linear$fixed70, Y = fixed_linear_totalbiomass_mean$fixed70, method = "elementary_effects"),
                       fixed80 = sensitivity(X = doe_fixed_linear$fixed80, Y = fixed_linear_totalbiomass_mean$fixed80, method = "elementary_effects"),
                       fixed85 = sensitivity(X = doe_fixed_linear$fixed85, Y = fixed_linear_totalbiomass_mean$fixed85, method = "elementary_effects"))
saveRDS(ee_fixed_linear, file = "paper_protocol/ee_fixed_linear.rds")
