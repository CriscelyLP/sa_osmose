
# Loading packages - scripts ----------------------------------------------

source("internal-functions.R")
source("random-sampling.R")
source("elementary-effects.R")
source("methods.R")

# Loading outputs ---------------------------------------------------------

# ee using total biomass in the system
# ee_totalBiomass_mean   = readRDS("paper_protocol/ee_totalBiomass_mean.rds")

ee_protocol     = readRDS("paper_protocol/ee_protocol.rds")
ee_fixed        = readRDS("paper_protocol/ee_fixed.rds")
ee_fixed_linear = readRDS("paper_protocol/ee_fixed_linear.rds")

barColours_group = c(rep("#FF0000", 15),  #gs
                     rep("#FF9900", 3),   #er
                     rep("#DFEE04", 3),   #mc
                     rep("#228B22", 6))   #hqd


# Fig6: protocol + fixed ---------------------------------------------------

varTitle = c("P", paste0("F.", c( 20, 40, 60, 70, 80)))
var      = list()
var$protocol = ee_protocol$p
var$fixed20  = ee_fixed$fixed20
var$fixed40  = ee_fixed$fixed40
var$fixed60  = ee_fixed$fixed60
var$fixed70  = ee_fixed$fixed70
var$fixed80  = ee_fixed$fixed80
  
par(mfrow = c(2,2), mar = c(0, 0, 0, 0), oma = c(4,4,4,4))

#P
plot(x = summary(var$protocol)[[1]]$mu_star,
     y = summary(var$protocol)[[1]]$sd,
     xlab = "", ylab = "", 
     bg = barColours_group, pch = 21, cex = 1.8,
     xlim = c(0, 0.10), ylim = c(0,0.32),
     axes = FALSE, col = "black", xaxs="i",yaxs="i")
# axis(1, at = seq(from = 0, to = 0.10, by = 0.05), cex.axis = 1.8)
axis(2, at = seq(from = 0, to = 0.32, by = 0.1), cex.axis = 1.8, las = 1)
box()
mtext(text = toupper(varTitle[1]), side = 3,  cex = 1.5, adj = 0.05, line = -2)
points = summary(var$protocol)[[1]]
points = points[order(points$mu_star, decreasing = TRUE), ]
text(x = points$mu_star[c(1:3)], y = points$sd[c(1:3)],
     labels = row.names(points)[c(1:3)], cex = 1.6, adj = -0.3)


#F.20
plot(x = summary(var$fixed20)[[1]]$mu_star,
     y = summary(var$fixed20)[[1]]$sd,
     xlab = "", ylab = "", 
     bg = barColours_group, pch = 21, cex = 1.8,
     xlim = c(0, 0.10), ylim = c(0,0.32),
     axes = FALSE, col = "black", xaxs="i",yaxs="i")
axis(3, at = seq(from = 0, to = 0.10, by = 0.05), cex.axis = 1.8)
#axis(4, at = seq(from = 0, to = 0.32, by = 0.1), cex.axis = 1.8, las = 1)
box()
mtext(text = toupper(varTitle[2]), side = 3,  cex = 1.5, adj = 0.05, line = -2)
points = summary(var$fixed20)[[1]]
points = points[order(points$mu_star, decreasing = TRUE), ]
text(x = points$mu_star[c(2)], y = points$sd[c(2)],
     labels = "10 18 20", cex = 1.2, pos = 3)


#F.40
plot(x = summary(var$fixed40)[[1]]$mu_star,
     y = summary(var$fixed40)[[1]]$sd,
     xlab = "", ylab = "", 
     bg = barColours_group, pch = 21, cex = 1.8,
     xlim = c(0, 0.10), ylim = c(0,0.32),
     axes = FALSE, col = "black", xaxs="i",yaxs="i")
axis(1, at = seq(from = 0, to = 0.10, by = 0.05), cex.axis = 1.8)
#axis(2, at = seq(from = 0, to = 0.32, by = 0.1), cex.axis = 1.8, las = 1)
box()
mtext(text = toupper(varTitle[3]), side = 3,  cex = 1.5, adj = 0.05, line = -2)
points = summary(var$fixed40)[[1]]
points = points[order(points$mu_star, decreasing = TRUE), ]
text(x = points$mu_star[c(1:3)], y = points$sd[c(1:3)],
     labels = row.names(points)[c(1:3)], cex = 1.5, pos = 3)

#F.60
plot(x = summary(var$fixed60)[[1]]$mu_star,
     y = summary(var$fixed60)[[1]]$sd,
     xlab = "", ylab = "", 
     bg = barColours_group, pch = 21, cex = 1.8,
     xlim = c(0, 0.10), ylim = c(0,0.32),
     axes = FALSE, col = "black", xaxs="i",yaxs="i")
# axis(3, at = seq(from = 0, to = 0.10, by = 0.05), cex.axis = 1.8)
axis(4, at = seq(from = 0, to = 0.32, by = 0.1), cex.axis = 1.8, las = 1)
box()
mtext(text = toupper(varTitle[4]), side = 3,  cex = 1.5, adj = 0.05, line = -2)
points = summary(var$fixed60)[[1]]
points = points[order(points$mu_star, decreasing = TRUE), ]
text(x = points$mu_star[c(1:3)], y = points$sd[c(1:3)],
     labels = row.names(points)[c(1:3)], cex = 1.6, adj = -0.3)


par(mfrow = c(2,2), mar = c(0, 0, 0, 0), oma = c(4,4,4,4))
#F.70
plot(x = summary(var$fixed70)[[1]]$mu_star,
     y = summary(var$fixed70)[[1]]$sd,
     xlab = "", ylab = "", 
     bg = barColours_group, pch = 21, cex = 1.8,
     xlim = c(0, 0.5), ylim = c(0,1.5),
     axes = FALSE, col = "black", xaxs="i",yaxs="i")
axis(3, at = seq(from = 0, to = 0.5, by = 0.1), cex.axis = 1.8)
axis(2, at = seq(from = 0, to = 1.5, by = 0.5), cex.axis = 1.8, las = 1)
box()
mtext(text = toupper(varTitle[5]), side = 3,  cex = 1.5, adj = 0.05, line = -2)
points = summary(var$fixed70)[[1]]
points = points[order(points$mu_star, decreasing = TRUE), ]
text(x = points$mu_star[c(1:3)], y = points$sd[c(1:3)],
     labels = row.names(points)[c(1:3)], cex = 1.6, adj = -0.3)

segments(x0 = 0, y0 = 0.32, x1 = 0.10, y1 = 0.32, col = "black", lty = 2, lwd = 1.5)
segments(x0 = 0.10, y0 = 0.32, x1 = 0.10, y1 = 0, col = "black", lty = 2, lwd = 1.5)

plot.new()

#F.80
plot(x = summary(var$fixed80)[[1]]$mu_star,
     y = summary(var$fixed80)[[1]]$sd,
     xlab = "", ylab = "", 
     bg = barColours_group, pch = 21, cex = 1.8,
     xlim = c(0, 0.5), ylim = c(0,1.5),
     axes = FALSE, col = "black", xaxs="i",yaxs="i")
axis(1, at = seq(from = 0, to = 0.5, by = 0.1), cex.axis = 1.8)
axis(4, at = seq(from = 0, to = 1.5, by = 0.5), cex.axis = 1.8, las = 1)
box()
mtext(text = toupper(varTitle[6]), side = 3,  cex = 1.5, adj = 0.05, line = -2)
points = summary(var$fixed80)[[1]]
points = points[order(points$mu_star, decreasing = TRUE), ]
text(x = points$mu_star[c(1:3)], y = points$sd[c(1:3)],
     labels = row.names(points)[c(1:3)], cex = 1.6, adj = -0.3)

segments(x0 = 0, y0 = 0.32, x1 = 0.10, y1 = 0.32, col = "black", lty = 2, lwd = 1.5)
segments(x0 = 0.10, y0 = 0.32, x1 = 0.10, y1 = 0, col = "black", lty = 2, lwd = 1.5)

#mtext(text = "mean star", side = 1, line = 2.5, outer = TRUE, cex = 2)
#mtext(text = "sd", side = 2, line = 1, outer = TRUE, cex = 2)
#X
# plot(1:4,
#      pch = 21, cex = 4,
#      col = 'black',
#      bg = unique(barColours_group),
#      xlim = c(0, 5), ylim = c(0,5))

# Fig7: heatmap ---------------------------------------------------------

# Ordering data
var = list()
var$protocol = ee_protocol$p
var$fixed10  = ee_fixed$fixed10
var$fixed15  = ee_fixed$fixed15
var$fixed20  = ee_fixed$fixed20
var$fixed30  = ee_fixed$fixed30
var$fixed40  = ee_fixed$fixed40
var$fixed50  = ee_fixed$fixed50
var$fixed60  = ee_fixed$fixed60
var$fixed70  = ee_fixed$fixed70
var$fixed80  = ee_fixed$fixed80
var$fixed85  = ee_fixed$fixed85

protocol  = summary(var$protocol)[[1]]$mu_star
fixed10   = summary(var$fixed10)[[1]]$mu_star
fixed15   = summary(var$fixed15)[[1]]$mu_star
fixed20   = summary(var$fixed20)[[1]]$mu_star
fixed30   = summary(var$fixed30)[[1]]$mu_star
fixed40   = summary(var$fixed40)[[1]]$mu_star
fixed50   = summary(var$fixed50)[[1]]$mu_star
fixed60   = summary(var$fixed60)[[1]]$mu_star
fixed70   = summary(var$fixed70)[[1]]$mu_star
fixed80   = summary(var$fixed80)[[1]]$mu_star
fixed85   = summary(var$fixed85)[[1]]$mu_star

matrixEE = cbind(protocol, fixed10, fixed15, fixed20, fixed30, fixed40,
                 fixed50, fixed60, fixed70, fixed80, fixed85)
row.names(matrixEE) = c(1:27)

matrix_r = apply(matrixEE, 2, function(x) as.numeric(names(sort(x, decreasing = TRUE))) )
row.names(matrix_r) = c(1:27)

newMatrixEE   = matrix(rep(NA, 27*11), nrow = 27, ncol = 11)
for (i in seq_len(ncol(newMatrixEE))) {
  newMatrixEE[c(1:27),i] = match(c(1:27), matrix_r[,i])
}

colnames(newMatrixEE) = colnames(matrixEE)
rownames(newMatrixEE) = paste0("par", c(1:27))

# Plot
# ordering data
data = newMatrixEE
#varTitle = "Total Biomass"

data = apply(data, 2, rev)
data = t(data)

# colors = heat.colors(27)
# colors = colorRampPalette(c("darkred", "red", "orange", "yellow", "khaki1"))(n = 27)
colors = viridis::magma(27,alpha = 0.80)

experiments = c("P", paste0("F.", c(10,15,20,30,40,50,60,70,80,85)))
parNames = paste0("par ", c(1:27))

par(mar = c(3,4,0.5,0.8), las = 2)

image(data, axes = FALSE, col = colors)
grid(11, 27, col = "grey50")
box()
ticks = seq(from = 0, to = 1, length.out = 11)

# axis 1
axis(1, at = ticks, labels = rep("", 11))
text(seq(from = 0, to = 1, length.out = 11),
     par("usr")[3] - 0.02, labels = experiments,
     pos = 1, xpd = TRUE, cex = 1.2)

# axis 2
axis(2, at = seq(from = 0, to = 1, length.out = 27), labels = rev(parNames),
     cex.axis = 1.2)

# Legend
library(fields)
x = 1:10
y = 1:17
z = outer( x,y,"+") 

colours = viridis::magma(27,alpha = 0.80)

par(new = TRUE)
image.plot(x, y, z,
           col = rev(colours),
           legend.only = TRUE,
           legend.width = 2.5,
           axis.args=list( at=1, labels=''))
# Fig8: scales -----------------------------------------------------------

varTitle = c("P", "P.Linear", "F.20", "F.Linear.20")

var      = list()
var$p    = ee_protocol$p
var$pl   = ee_protocol$p_linear
var$f20  = ee_fixed$fixed20
var$fl20 = ee_fixed_linear$fixed20

limits = list(lim_p    = list(x = c(0, 0.30), y = c(0, 0.30)), #P
              lim_pl   = list(x = c(0, 0.30), y = c(0, 0.30)), #PL
              lim_f20  = list(x = c(0, 0.06), y = c(0, 0.15)), #F20
              lim_fl20 = list(x = c(0, 0.06), y = c(0, 0.15))) #FL20

#limitsBy = c(0.10, 0.10, 0.02, 0.05)

par(mfrow = c(2,2), mar = c(2, 0, 4, 0), oma = c(2,4,0.5,4))

# P
plot(x = summary(var$p)[[1]]$mu_star,
     y = summary(var$p)[[1]]$sd,
     xlab = "", ylab = "", 
     bg = barColours_group, pch = 21, cex = 2, col = "black",
     xlim = limits$lim_p$x, ylim = limits$lim_p$y, axes = FALSE)
axis(1, at = seq(from = 0, to = 0.30, by = 0.10), cex.axis = 1.5)
axis(2, at = seq(from = 0, to = 0.30, by = 0.10), cex.axis = 1.5, las = 1)
box()
mtext(text = varTitle[1], side = 3,  cex = 1.5, adj = 0.05, line = -2)

points = summary(var$p)[[1]]
points = points[order(points$mu_star, decreasing = TRUE), ]
text(x = points$mu_star[c(1:3)], y = points$sd[c(1:3)],
     labels = row.names(points)[c(1:3)], cex = 1.6, adj = -0.3)

# PL
plot(x = summary(var$pl)[[1]]$mu_star,
     y = summary(var$pl)[[1]]$sd,
     xlab = "", ylab = "", 
     bg = barColours_group, pch = 21, cex = 2, col = "black",
     xlim = limits$lim_pl$x, ylim = limits$lim_pl$y, axes = FALSE)
axis(3, at = seq(from = 0, to = 0.30, by = 0.10), cex.axis = 1.5)
axis(4, at = seq(from = 0, to = 0.30, by = 0.10), cex.axis = 1.5, las = 1)
box()
mtext(text = varTitle[2], side = 3,  cex = 1.5, adj = 0.05, line = -2)

points = summary(var$pl)[[1]]
points = points[order(points$mu_star, decreasing = TRUE), ]
text(x = points$mu_star[c(1:3)], y = points$sd[c(1:3)],
     labels = row.names(points)[c(1:3)], cex = 1.6, adj = -0.3)


# F20
plot(x = summary(var$f20)[[1]]$mu_star,
     y = summary(var$f20)[[1]]$sd,
     xlab = "", ylab = "", 
     bg = barColours_group, pch = 21, cex = 2, col = "black",
     xlim = limits$lim_f20$x, ylim = limits$lim_f20$y, axes = FALSE)
axis(1, at = seq(from = 0, to = 0.06, by = 0.02), cex.axis = 1.5)
axis(2, at = seq(from = 0, to = 0.15, by = 0.05), cex.axis = 1.5, las = 1)
box()
mtext(text = varTitle[3], side = 3,  cex = 1.5, adj = 0.05, line = -2)

points = summary(var$f20)[[1]]
points = points[order(points$mu_star, decreasing = TRUE), ]
text(x = points$mu_star[c(2)], y = points$sd[c(2)],
     labels = "10 18 20", cex = 1.2, pos = 3)


# FL20
plot(x = summary(var$fl20)[[1]]$mu_star,
     y = summary(var$fl20)[[1]]$sd,
     xlab = "", ylab = "", 
     bg = barColours_group, pch = 21, cex = 2, col = "black",
     xlim = limits$lim_fl20$x, ylim = limits$lim_fl20$y, axes = FALSE)
axis(3, at = seq(from = 0, to = 0.06, by = 0.02), cex.axis = 1.5)
axis(4, at = seq(from = 0, to = 0.15, by = 0.05), cex.axis = 1.5, las = 1)
box()
mtext(text = varTitle[4], side = 3,  cex = 1.5, adj = 0.05, line = -2)

points = summary(var$fl20)[[1]]
points = points[order(points$mu_star, decreasing = TRUE), ]
text(x = points$mu_star[c(1:3)], y = points$sd[c(1:3)],
     labels = row.names(points)[c(1:3)], cex = 1.6, adj = -0.5)

# Fig9: protocol ----------------------------------------------------------

varTitle = "P"
var      = ee_protocol
out      = 1

#PART A
par(mar = c(0.5,0.5,0.5,0.8), oma = c(3,3,0.5,0.5))

plot(x = summary(var[[out]])[[1]]$mu_star,
     y = summary(var[[out]])[[1]]$sd,
     xlab = "", ylab = "", col = barColours_group, pch = 19, cex = 3,
     xlim = c(0, 0.08), ylim = c(0, 0.20), axes = FALSE, xaxs="i",yaxs="i")
axis(1, at = seq(from = 0, to = 0.08, by = 0.02), cex.axis = 1.5)
axis(2, at = seq(from = 0, to = 0.20, by = 0.05), cex.axis = 1.5, las = 1)
box()

abline(coef = c(0,1), col = 'gray', lwd = 2, lty = 2.2)

text(x = summary(var[[out]])[[1]]$mu_star, y = summary(var[[out]])[[1]]$sd,
     labels = c(1:28), cex = 1.2, lwd = 1.5)

legend(legend = toupper(varTitle), 'topleft', bty = "n", cex = 2)


# Appendix: Fixed ranges using scales in Table 2 ------------------------------------

varTitle = c("P", paste0("F.", c(10,15,20,30,40,50,60,70,80,85)))

var = list()
var$protocol = ee_protocol$p
var$fixed10  = ee_fixed$fixed10
var$fixed15  = ee_fixed$fixed15
var$fixed20  = ee_fixed$fixed20
var$fixed30  = ee_fixed$fixed30
var$fixed40  = ee_fixed$fixed40
var$fixed50  = ee_fixed$fixed50
var$fixed60  = ee_fixed$fixed60
var$fixed70  = ee_fixed$fixed70
var$fixed80  = ee_fixed$fixed80
var$fixed85  = ee_fixed$fixed85

limits = list(limp  = list(x = c(0, 0.25), y = c(0,0.30)), #Protocol
              lim10 = list(x = c(0, 0.25), y = c(0,0.30)), #10P
              lim15 = list(x = c(0, 0.25), y = c(0,0.30)), #15P
              lim20 = list(x = c(0, 0.25), y = c(0,0.30)), #20P
              lim30 = list(x = c(0, 0.25), y = c(0,0.30)), #30P
              lim40 = list(x = c(0, 0.25), y = c(0,0.30)), #40P
              lim50 = list(x = c(0, 0.25), y = c(0,0.30)), #50P
              lim60 = list(x = c(0, 0.25), y = c(0,0.30)), #60P
              lim70 = list(x = c(0, 1), y = c(0,2)), #70P
              lim80 = list(x = c(0, 1), y = c(0,2)), #80
              lim85 = list(x = c(0, 1), y = c(0,2))) #85

limitsBy = list(x = c(rep(0.10, 8), rep(0.4, 3)),
                y = c(rep(0.1, 8), rep(0.5,3)))

par(mfrow = c(3,4), mar = c(2.5, 0, 1, 0), oma = c(2,4,0.5,0.5))

for (out in c(1:11)) { 
  
  plot(x = summary(var[[out]])[[1]]$mu_star,
       y = summary(var[[out]])[[1]]$sd,
       xlab = "", ylab = "", 
       col = barColours_group, pch = 19, cex = 4, cex.axis = 1.5,
       xlim = limits[[out]]$x, ylim = limits[[out]]$y, axes = FALSE)
  
  if(out %in% c(1,5,9)){
    axis(2, at = seq(from = 0, to = limits[[out]]$y[2], by = limitsBy$y[out]), cex.axis = 1.5, las = 1)  
  }
  axis(1, at = seq(from = 0, to = limits[[out]]$x[2], by = limitsBy$x[out]), cex.axis = 1.5) 
  box()
  
  text(x = summary(var[[out]])[[1]]$mu_star,
       y = summary(var[[out]])[[1]]$sd,
       labels = c(1:28), cex = 1.6)
  
  mtext(text = varTitle[out],side = 3, cex = 1.2, line = -2, adj = 0.1)
  
  if(out %in% c(9,10,11)){
    segments(x0 = -0.1   , y0 = 0.30, x1 = 0.25, y1 = 0.30, col = "black", lty = 2, lwd = 1.5)
    segments(x0 = 0.25, y0 = 0.30, x1 = 0.25, y1 = -0.1, col = "black", lty = 2, lwd = 1.5)
  }
  
}

#mtext(text = "mean star", side = 1, line = 2.5, outer = TRUE, cex = 2)
#mtext(text = "sd", side = 2, line = 1, outer = TRUE, cex = 2)

# Appendix: Fixed ranges using linear scale -----------------------------------------

varTitle = c("P", paste0("F.Linear.", c(10,15,20,30,40,50,60,70,80,85)))

var = list()
var$protocol = ee_protocol$p
var$fixed10  = ee_fixed_linear$fixed10
var$fixed15  = ee_fixed_linear$fixed15
var$fixed20  = ee_fixed_linear$fixed20
var$fixed30  = ee_fixed_linear$fixed30
var$fixed40  = ee_fixed_linear$fixed40
var$fixed50  = ee_fixed_linear$fixed50
var$fixed60  = ee_fixed_linear$fixed60
var$fixed70  = ee_fixed_linear$fixed70
var$fixed80  = ee_fixed_linear$fixed80
var$fixed85  = ee_fixed_linear$fixed85

limits = list(limp  = list(x = c(0, 0.25), y = c(0,0.30)), #Protocol
              lim10 = list(x = c(0, 0.25), y = c(0,0.30)), #10P
              lim15 = list(x = c(0, 0.25), y = c(0,0.30)), #15P
              lim20 = list(x = c(0, 0.25), y = c(0,0.30)), #20P
              lim30 = list(x = c(0, 0.25), y = c(0,0.30)), #30P
              lim40 = list(x = c(0, 0.25), y = c(0,0.30)), #40P
              lim50 = list(x = c(0, 0.25), y = c(0,0.30)), #50P
              lim60 = list(x = c(0, 0.25), y = c(0,0.30)), #60P
              lim70 = list(x = c(0, 1), y = c(0,2)), #70P
              lim80 = list(x = c(0, 1), y = c(0,2)), #80
              lim85 = list(x = c(0, 1), y = c(0,2))) #85

limitsBy = list(x = c(rep(0.10, 8), rep(0.4, 3)),
                y = c(rep(0.1, 8), rep(0.5,3)))

par(mfrow = c(3,4), mar = c(2.5, 0, 1, 0), oma = c(2,4,0.5,0.5))

for (out in c(1:11)) { 
  
  plot(x = summary(var[[out]])[[1]]$mu_star,
       y = summary(var[[out]])[[1]]$sd,
       xlab = "", ylab = "", 
       col = barColours_group, pch = 19, cex = 4, cex.axis = 1.5,
       xlim = limits[[out]]$x, ylim = limits[[out]]$y, axes = FALSE)
  
  if(out %in% c(1,5,9)){
    axis(2, at = seq(from = 0, to = limits[[out]]$y[2], by = limitsBy$y[out]), cex.axis = 1.5, las = 1)  
  }
  axis(1, at = seq(from = 0, to = limits[[out]]$x[2], by = limitsBy$x[out]), cex.axis = 1.5) 
  box()
  
  text(x = summary(var[[out]])[[1]]$mu_star,
       y = summary(var[[out]])[[1]]$sd,
       labels = c(1:28), cex = 1.6)
  
  mtext(text = varTitle[out],side = 3, cex = 1.2, line = -2, adj = 0.1)
  
  if(out %in% c(9,10,11)){
    segments(x0 = -0.1   , y0 = 0.30, x1 = 0.25, y1 = 0.30, col = "black", lty = 2, lwd = 1.5)
    segments(x0 = 0.25, y0 = 0.30, x1 = 0.25, y1 = -0.1, col = "black", lty = 2, lwd = 1.5)
  }
  
}

#mtext(text = "mean star", side = 1, line = 2.5, outer = TRUE, cex = 2)
#mtext(text = "sd", side = 2, line = 1, outer = TRUE, cex = 2)

# Table results -----------------------------------------------------------

var = list()
var$protocol = ee_protocol$p
var$fixed10  = ee_fixed$fixed10
var$fixed15  = ee_fixed$fixed15
var$fixed20  = ee_fixed$fixed20
var$fixed30  = ee_fixed$fixed30
var$fixed40  = ee_fixed$fixed40
var$fixed50  = ee_fixed$fixed50
var$fixed60  = ee_fixed$fixed60
var$fixed70  = ee_fixed$fixed70
var$fixed80  = ee_fixed$fixed80
var$fixed85  = ee_fixed$fixed85

protocol  = summary(var$protocol)[[1]]
fixed10   = summary(var$fixed10)[[1]]
fixed15   = summary(var$fixed15)[[1]]
fixed20   = summary(var$fixed20)[[1]]
fixed30   = summary(var$fixed30)[[1]]
fixed40   = summary(var$fixed40)[[1]]
fixed50   = summary(var$fixed50)[[1]]
fixed60   = summary(var$fixed60)[[1]]
fixed70   = summary(var$fixed70)[[1]]
fixed80   = summary(var$fixed80)[[1]]
fixed85   = summary(var$fixed85)[[1]]

protocol = protocol[order(protocol$mu_star, decreasing = TRUE),][c(1:10),]
fixed10  = fixed10[order(fixed10$mu_star, decreasing = TRUE),][c(1:10),]
fixed15  = fixed15[order(fixed15$mu_star, decreasing = TRUE),][c(1:10),]
fixed20  = fixed20[order(fixed20$mu_star, decreasing = TRUE),][c(1:10),]
fixed30  = fixed30[order(fixed30$mu_star, decreasing = TRUE),][c(1:10),]
fixed40  = fixed40[order(fixed40$mu_star, decreasing = TRUE),][c(1:10),]
fixed50  = fixed50[order(fixed50$mu_star, decreasing = TRUE),][c(1:10),]
fixed60  = fixed60[order(fixed60$mu_star, decreasing = TRUE),][c(1:10),]
fixed70  = fixed70[order(fixed70$mu_star, decreasing = TRUE),][c(1:10),]
fixed80  = fixed80[order(fixed80$mu_star, decreasing = TRUE),][c(1:10),]
fixed85  = fixed85[order(fixed85$mu_star, decreasing = TRUE),][c(1:10),]

table_ranking = rbind(protocol$parameter,
                      fixed10$parameter,
                      fixed15$parameter,
                      fixed20$parameter,
                      fixed30$parameter,
                      fixed40$parameter,
                      fixed50$parameter,
                      fixed60$parameter,
                      fixed70$parameter, 
                      fixed80$parameter,
                      fixed85$parameter)

colnames(table_ranking) = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
rownames(table_ranking) = c("Protocol", "10 %", "15 %", "20 %", "30 %", "40 %",
                            "50 %", "60 %", "70 %", "80 %", "85 %")
table_ranking = t(table_ranking)

