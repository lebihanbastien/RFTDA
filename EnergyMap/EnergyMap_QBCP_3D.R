# R script to handle an energy map of the QBCP around EML1,2
# The difference wrt to EnergyMap_QBCP_Order.R is the fact that the 
# data files contains 3D initial conditions in the center manifolds.
# Typically (s1, s2, s3, s4) all components being potentially different
# form zero.
#
#===============================================================================

#===============================================================================
# Init
#===============================================================================
# R ohs1s3ions
#-------------------------------------------------------------------------------
options(digits = 15)


# Load libraries
#-------------------------------------------------------------------------------
library(plyr)
library(ggplot2)
library(reshape2)
library(scales)
library(grid)

# Load Source files
#-------------------------------------------------------------------------------
source("source/init.R")


# Select Models & libration point
#-------------------------------------------------------------------------------
Li        = "L2"
MODEL     = "QBCP"
FWRK      = "EM"
Energy    = "0.1"
order     = "10"
ofs_order = "30"
t0        = "0";

#Current working folder
currentfolder = paste0(printfolder(MODEL, FWRK, Li))


#Normalized units (gamma, c1)
#-------------------------------------------------------------------------------
gamma = gamma(Li, FWRK);
L     = Ldist(FWRK);
c1    =  c1(Li, FWRK);

#===============================================================================
# Data reading
#===============================================================================

# Filename to check
#-------------------------------------------------------------------------------
fileprefix = paste0(currentfolder, "Serv/Serv_hm_Energy_", Energy, "_order_", order, "_ofs_", ofs_order, "_t0_", t0)
filename   = paste0(fileprefix, ".bin")

# Load bin source ONCE
#-------------------------------------------------------------------------------
if(!exists("icdf"))
{
  if (file.exists(filename))
  {
    names = c("label", "x", "y", "z", "px", "py", "pz",
              "xEM", "yEM", "zEM", "pxEM", "pyEM", "pzEM",
              "s1", "s2", "s3", "s4", "t", "Hz", "dHz");
    icdf = dffbinary(filename, 20, names);
    icdf$order = order;
    
  }else
  {
    icdf = data.frame()
  }
}

# Postprocessing
#-------------------------------------------------------------------------------
# From NC to EM units
icdf = NCtoC(icdf, gamma)
  
# From EM to physical units
icdf = CtoPH(icdf, L)

# Radii from Li
icdf$rNC = sqrt(icdf$x^2+icdf$y^2+icdf$z^2)
icdf$rPH = sqrt(icdf$xCPH^2+icdf$yCPH^2+icdf$zCPH^2)

#Mid point, low, high for plot
#-------------------------------------------------------------------------------
midpoint_c = mean(icdf$dHz);
high_c     = muted("red");
low_c      = muted("blue");


#===============================================================================
# Select some directions
#===============================================================================
#-------------------------------------------------------------------------------
# We select all the values of the form
# s4 == 0, s2 == a certain constant
#-------------------------------------------------------------------------------
s2u = unique(icdf$s2)
ls2u = length(s2u)
n= 10
# s4 = 0, s2 = cst
# directions = (((icdf$s2 == min(icdf$s2)| icdf$s2 == s2u[n] | icdf$s2 ==0  | 
#                icdf$s2 == s2u[ls2u - n + 1] | icdf$s2 == max(icdf$s2))) & 
#   (icdf$s4 == 0)) | ( icdf$s4 == 0 & icdf$s1 == min(icdf$s1) & icdf$s3 == max(icdf$s3) ) |
#                     ( icdf$s4 == 0 & icdf$s1 == max(icdf$s1) & icdf$s3 == min(icdf$s3) ) |
#                     ( icdf$s4 == 0 & icdf$s1 == max(icdf$s1) & icdf$s3 == max(icdf$s3) ) |
#                     ( icdf$s4 == 0 & icdf$s1 == min(icdf$s1) & icdf$s3 == min(icdf$s3) )

# s4 = cst, s2 = 0
directions = (((icdf$s4 == min(icdf$s4)| icdf$s4 == s2u[n] | icdf$s4 ==0  | 
                  icdf$s4 == s2u[ls2u - n + 1] | icdf$s4 == max(icdf$s4))) & 
                (icdf$s2 == 0)) | ( icdf$s4 == 0 & icdf$s1 == min(icdf$s1) & icdf$s3 == max(icdf$s3) ) |
  ( icdf$s2 == 0 & icdf$s1 == max(icdf$s1) & icdf$s3 == min(icdf$s3) ) |
  ( icdf$s2 == 0 & icdf$s1 == max(icdf$s1) & icdf$s3 == max(icdf$s3) ) |
  ( icdf$s2 == 0 & icdf$s1 == min(icdf$s1) & icdf$s3 == min(icdf$s3) )
icdfs1s3 = icdf[which(directions),]

icdfs1s3 = icdf[which(directions),]
icdfs1s3 = icdfs1s3[order(icdfs1s3$s1),]

#-------------------------------------------------------------------------------
# We select all the values of the form
# s1 == 0, s3 == 0
#-------------------------------------------------------------------------------
directions = (icdf$s1 == 0) & (icdf$s3 == 0)
icdfs2s4 = icdf[which(directions),]
icdfs2s4 = icdfs2s4[order(icdfs2s4$s2),]



#===============================================================================
# Plots
#===============================================================================
# New fileprefix
fileprefix = paste0(currentfolder, "Serv/hmap_plot/hm_order_", order, "_ofs_", ofs_order, "_t0_", t0)


# x0 vs y0
#-------------------------------------------------------------------------------
hxy = plotdf_point(icdfs1s3, "x", "y", "x", "y", "dHz", "dHz", 0, pointSize = 3)
hxy = hxy + scale_colour_gradient2(space="Lab", midpoint = midpoint_c, mid = "white", high = high_c, low = low_c)
hxy

# x0 vs y0 in km
#-------------------------------------------------------------------------------
hxyCPH = plotdf_point(icdfs1s3, "xCPH", "rPH", "xCPH", "rCPH", "dHz", "dHz", 0, pointSize = 3)
hxyCPH = hxyCPH + scale_colour_gradient2(space="Lab", midpoint = midpoint_c, 
                                         mid = "white", high = high_c, low = low_c)
# hxyCPH = hxyCPH + scale_x_continuous(breaks = seq(-40000, 40000, 2000))
# hxyCPH = hxyCPH + scale_y_continuous(breaks = seq(-40000, 40000, 2000))
hxyCPH


#===============================================================================
# Plots with just some directions
#===============================================================================

# x0 vs y0
#-------------------------------------------------------------------------------
pxy = ggplot()+geom_point(data = icdfs1s3, aes(x, y, colour = factor(s2)), size = 2)
pxy = pxy+custom_theme
pxy = pxy + scale_colour_discrete(guide = FALSE)
pxy

# x0 vs z0
#-------------------------------------------------------------------------------
pxz = ggplot()+geom_point(data = icdfs1s3, aes(x, z, colour = factor(s2)), size = 2)
pxz = pxz+custom_theme
pxz = pxz + scale_colour_discrete(guide = FALSE)
pxz

# y0 vs z0
#-------------------------------------------------------------------------------
pyz = ggplot()+geom_point(data = icdfs1s3, aes(y, z, colour = factor(s2)), size = 2)
pyz = pyz+custom_theme
pyz = pyz + scale_colour_discrete(guide = FALSE)
pyz

#===============================================================================
# Plots at constant energy
#===============================================================================
dHz0 = 0.01
dHzc = 1e-2;
direction = which((abs(icdf$dHz - dHz0) < dHzc) & icdf$s2 == 0 & icdf$s4 == 0)
icdfdHz = icdf[direction,]

legend = list(at = seq(0,1,0.01), side = 4, addlines = FALSE, length = 0.5, width = 0.5, font = 2)
scatter3D (icdfdHz$x, icdfdHz$y, icdfdHz$z, colvar = icdfdHz$dHz, 
           bty = "g", pch = 16,
           xlab = "$x$", ylab ="$y$", zlab = "$z$",  
           col = jet.col(100),
           clab = c("", "", "$\\delta H(\\sb, 0)$"),
           ticktype = "detailed", colkey = NULL,
           theta = 0, phi = 20)

stop()

#===============================================================================
# 3D plots
#===============================================================================

# x0 vs y0 vs z0 color is dHz
#-------------------------------------------------------------------------------
legend = list(at = seq(0,1,0.01), side = 4, addlines = FALSE, length = 0.5, width = 0.5, font = 2)
scatter3D (icdfs1s3$x, icdfs1s3$y, icdfs1s3$z, colvar = icdfs1s3$dHz, 
           bty = "g", pch = 16,
           xlab = "$x$", ylab ="$y$", zlab = "$z$",  
           col = jet.col(100),
           clab = c("", "", "$\\delta H(\\sb, 0)$"),
           ticktype = "detailed", colkey = legend,
           theta = 0, phi = 20)

stop()

# x0 vs y0 vs z0 color is dHz, second set of conditions
#-------------------------------------------------------------------------------
legend = list(at = seq(0,1,0.01), side = 4, addlines = FALSE, length = 0.5, width = 0.5, font = 2)
scatter3D (icdfs2s4$x, icdfs2s4$y, icdfs2s4$z, colvar = icdfs2s4$dHz, 
           bty = "g", pch = 16,
           xlab = "$x$", ylab ="$y$", zlab = "$z$",  
           col = jet.col(100),
           clab = c("", "", "$\\delta H(\\sb, 0)$"),
           ticktype = "detailed", colkey = legend,
           theta = -40, phi = 10)

stop()


# x0 vs y0 vs z0 color is si
#-------------------------------------------------------------------------------
legend = list(at = c(-10,0,10), labels = c("$s_2^{min}$", "0", "$s_2^{max}$"), side = 1, addlines = FALSE, length = 0.5, width = 0.5)
scatter3D (icdfs1s3$x, icdfs1s3$y, icdfs1s3$z, colvar = icdfs1s3$s2, 
           bty = "g", pch = 16, col = c("#1B9E77", "#D95F02", "#7570B3"),
           xlab = "$x$", ylab ="$y$", zlab = "$z$",
           ticktype = "detailed", colkey = legend,
           theta = 0, phi = 0)
stop()



# x0 vs y0 vs z0 in latex
#-------------------------------------------------------------------------------
scatter3D (x, y, z, ..., colvar = z, phi = 40, theta = 40,
           col = NULL, NAcol = "white", breaks = NULL,
           colkey = NULL, panel.first = NULL, 
           clim = NULL, clab = NULL, 
           bty = "b", CI = NULL, surf = NULL, 
           add = FALSE, plot = TRUE)

stop()

# x0 vs y0 vs z0 in latex: color is s4
#-------------------------------------------------------------------------------
# Common legend
legend = list(at = c(-10,0,10), labels = c("$s_2^{min}$", "0", "$s_2^{max}$"), 
              side = 1, addlines = FALSE, length = 0.5, width = 0.5)
# Common colors
colors = c("#1B9E77", "#D95F02", "#7570B3")

# Loop on theta (angle of the view)
for(theta in c(0, 60, 120, 180, 240, 300))
{
  file = paste0(fileprefix, "_pxyz_theta_",  theta, ".tex")
  
  #Print it to feed dev
  scattex3D (file, xSize, ySize, icdfs1s3$x, icdfs1s3$y, icdfs1s3$z, 
             colvar = icdfs1s3$s2, 
             bty = "g", pch = 16, col = colors,
             ticktype = "detailed", colkey = legend,
             xlab = "$x$", ylab ="$y$", zlab = "$z$",
             theta = theta, phi = 0)
}

# x0 vs y0 vs z0 in latex: energy map
#-------------------------------------------------------------------------------
legend = list(at = seq(0,1,0.01), side = 4, addlines = FALSE, length = 0.5, width = 0.5)
# Loop on theta (angle of the view)
for(theta in c(0)) #c(0, 60, 120, 180, 240, 300)
{
  file = paste0(fileprefix, "_hxyz_theta_",  theta, ".tex")
  
  #Print it to feed dev
  scattex3D (file, xSize, ySize, icdfs1s3$x, icdfs1s3$y, icdfs1s3$z, 
             colvar = icdfs1s3$dHz, 
             bty = "g", pch = 16, col = NULL,
             ticktype = "detailed", colkey = legend,
             xlab = "$x$", ylab ="$y$", zlab = "$z$",  
             clab = c("", "", "$\\delta H(\\mathbf{s}, 0)$"),
             theta = theta, phi = 20)
}

# x0 vs y0 vs z0 in latex, energy map, second set of conditions
#-------------------------------------------------------------------------------
file = paste0(fileprefix, "_hxyz_s2s4.tex")
legend = list(at = seq(0,1,0.01), side = 4, addlines = FALSE, length = 0.5, width = 0.5, font = 2)
scattex3D (file, xSize, ySize, icdfs2s4$x, icdfs2s4$y, icdfs2s4$z, colvar = icdfs2s4$dHz, 
           bty = "g", pch = 16,
           xlab = "$x$", ylab ="$y$", zlab = "$z$",  
           col = jet.col(100),
           clab = c("", "", "$\\delta H(\\mathbf{s}, 0)$"),
           ticktype = "detailed", colkey = legend,
           theta = -40, phi = 20)