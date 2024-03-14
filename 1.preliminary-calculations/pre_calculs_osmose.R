
source("run_up/internal-functions.R")
source("run_up/random-sampling.R")
source("run_up/elementary-effects.R")
source("run_up/methods.R")
source("run_up/auxiliar.R")

# Anchovy -----------------------------------------------------------------

# Predation size ratios: 
thetaStage1 = angleEstimation(m_min = 0, m_max = 1/800)
alphaStage1 = angleEstimation(m_min = 0, m_max = 1/8) - thetaStage1
c(thetaStage1, alphaStage1) / (pi/2) # en fracciones de pi medios (primer quadrante)

thetaStage2 = angleEstimation(m_min = 0, m_max = 1/200)
alphaStage2 = angleEstimation(m_min = 0, m_max = 1/6) - thetaStage2
c(thetaStage2, alphaStage2) / (pi/2) # en fracciones de pi medios (primer quadrante)

# Predation size threshold
s1   = 10
Linf = 19.5

s1_frac = s1/Linf # to be perturbed

# Larval mortality 
larvaMortality.sp0 = read.csv(file = "1.preliminary-calculations/config/larval/larval_mortality-anchovy.csv", header = TRUE, sep = ";")
lx  = log(larvaMortality.sp0$x)
mlx = mean(lx)
dlx = lx - mlx
# perturbation of mlx
Lx = exp(mlx)


# L0
t0.sp0   = -0.14
K.sp0    = 0.76
Linf.sp0 = 19.5
t        = 0
l0 = Linf.sp0*(1 - exp(-K.sp0 * (t - t0.sp0)))
  

# maturity size
smat = 12/19.5

