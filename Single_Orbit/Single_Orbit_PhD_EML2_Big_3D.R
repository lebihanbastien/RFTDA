# Script to plot an orbit along with its associated precisions
# in the parameterization method of the QBFBP/RTBP around L1/L2 of the Earth-Moon system
# Script for the PhD manuscript.
#
# Note that the variations of the energy are no longer computed as an error, 
# but as an absolute value.
# 
# BLB Summer 2017
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Init
#-------------------------------------------------------------------------------
source("source/init.R")

#-------------------------------------------------------------------------------
# Select Models & libration point
#-------------------------------------------------------------------------------
Li    = "L2"
MODEL = "QBCP"
FWRK  = "EM"

# If from Server
currentfolder = paste0(plotfolder(MODEL, FWRK, Li), "orbits/")
size =  "30_test" #1.5 7.5 15 30 45
sizep = "30_test"
dfindex  = c(5, 10, 15, 20)

# Orders

dfnumber = 1:length(dfindex)
maxOrder = max(dfindex)


#-------------------------------------------------------------------------------
# Parameters
#-------------------------------------------------------------------------------
maxPrec = 1e-6

#-------------------------------------------------------------------------------
#Normalized units (gammaR, c1R)
#-------------------------------------------------------------------------------
muR    = muR(FWRK);
gammaR = gamma(Li, FWRK);
c1R    =  c1(Li, FWRK);
L      = Ldist(FWRK)
Period = ifelse(MODEL=="QBCP", SEMperiod(FWRK), 2*pi)

#-------------------------------------------------------------------------------
# Type of plot (can be changed at will)
#-------------------------------------------------------------------------------
if(MODEL == "QBCP") 
{
  fplot = plotdf_line;
}else
{
  fplot = plotdf_line_dashed;
}
fplot_path = plotdf_path;

#-------------------------------------------------------------------------------
# Strings
#-------------------------------------------------------------------------------
ts = "$t$ ($\\times T$)"
ns = "Order $N$"
xs = "\\textit{X} [km]"
ys = "\\textit{Y} [km]"
zs = "\\textit{Z} [km]"

#-------------------------------------------------------------------------------
#                                             vH
#-------------------------------------------------------------------------------
# Load data
#-------------------------------------------------------------------------------
dflist = list();
dftype = "eH_Order_"
for(i in dfnumber)
{
  dflist[[i]] = read.table(paste0(currentfolder, dftype,dfindex[i],"_Size_", size, ".txt"), header = F)
  colnames(dflist[[i]]) = c("t", "H")
  dflist[[i]]$order = dfindex[i]
  dflist[[i]]$VH = dflist[[i]]$H - mean(dflist[[i]]$H)
}
# Concatenate the results
vH = rbind(dflist[[1]], dflist[[2]])
for(i in 3:length(dfnumber)){vH = rbind(vH, dflist[[i]])}


#Plot
#-------------------------------------------------------------------------------
#Actual plot
pH = fplot(vH,  "t", "H", ts,  "$\\delta H$",  "order", ns, 1)
#Cont scale on the x axis 
pH = pH + scale_x_continuous(breaks = seq(0.0, Period, 0.25*Period), labels = c("0.0", "0.25 $T$", "0.5 $T$", "0.75 $T$", "$T$"))  #cont scale on the x axis 
#Display the plot
pH = pH + legend_inside_theme

pH

#-------------------------------------------------------------------------------
#                                             eI
#-------------------------------------------------------------------------------
# Load data
#-------------------------------------------------------------------------------
dflist = list();
dftype = "eI_Order_"
for(i in dfnumber)
{
  dflist[[i]] = read.table(paste0(currentfolder, dftype, dfindex[i],"_Size_", size, ".txt"), header = F)
  colnames(dflist[[i]]) = c("x", "y")
  dflist[[i]]$order = dfindex[i]
}
# Concatenate the results
eI = rbind(dflist[[1]], dflist[[2]])
for(i in 3:length(dfnumber)){eI = rbind(eI, dflist[[i]])}

#Posprocessing
#-------------------------------------------------------------------------------
eI$EI = eI$y*gammaR;

#Plot
#-------------------------------------------------------------------------------
pI = fplot(eI,  "x", "EI", ts,  "$E_I$", "order", ns, 1)
#Cont scale on the x axis 
pI = pI + scale_x_continuous(breaks = seq(0.0, Period, 0.25*Period), 
                             labels = c("0.0", "0.25 $T$", "0.5 $T$", "0.75 $T$", "$T$"))  #cont scale on the x axis 
#Logscale on the y axis
pI = pI + scale_y_log10(breaks = 10^(-8:0*2), limits=c(1e-12,1e-2))
#Theme
pI = pI+legend_inside_theme
#Display
pI


#-------------------------------------------------------------------------------
#                                             eO
#-------------------------------------------------------------------------------
# Load data
#-------------------------------------------------------------------------------
dflist = list();
dftype = "eO_Order_"
for(i in dfnumber)
{
  dflist[[i]] = read.table(paste0(currentfolder, dftype,dfindex[i],"_Size_", size, ".txt"), header = F)
  colnames(dflist[[i]]) = c("x", "y")
  dflist[[i]]$order = dfindex[i]
}
# Concatenate the results
eO = rbind(dflist[[1]], dflist[[2]])
for(i in 3:length(dfnumber)){eO = rbind(eO, dflist[[i]])}

#Plot
#-------------------------------------------------------------------------------
pO = fplot(eO,  "x", "y", ts,  "$E_O$", "order", ns, 1)
#Cont scale on the x axis 
pO = pO + scale_x_continuous(breaks = seq(0.0, Period, 0.25*Period), labels = c("0.0", "0.25 $T$", "0.5 $T$", "0.75 $T$", "$T$"))  #cont scale on the x axis 
#Logscale on the y axis
pO = pO + scale_y_log10(limits=c(1e-10,NaN), breaks = 10^(-8:0*2))
#Theme
pO = pO+legend_inside_theme
#Display
pO

#-------------------------------------------------------------------------------
#                                             XYZ
#-------------------------------------------------------------------------------
# Load data
#-------------------------------------------------------------------------------
dflist = list();
dftype = "XYZ_Order_"
for(i in dfnumber)
{
  dflist[[i]] = read.table(paste0(currentfolder, dftype,dfindex[i],"_Size_", size, ".txt"), header = F)
  colnames(dflist[[i]]) = c("t", "x", "y", "z")
  dflist[[i]]$order = dfindex[i]
}

# Concatenate the results
orbit = rbind(dflist[[1]], dflist[[2]])
for(i in 3:length(dfnumber)){orbit = rbind(orbit, dflist[[i]])}

#-------------------------------------------------------------------------------
# Load data (PM)
#-------------------------------------------------------------------------------
dflist = list();
dftype = "XYZ_PM_Order_"
for(i in dfnumber)
{
  dflist[[i]] = read.table(paste0(currentfolder, dftype,dfindex[i],"_Size_", size, ".txt"), header = F)
  colnames(dflist[[i]]) = c("t", "x", "y", "z")
  dflist[[i]]$order = dfindex[i]
}

# Concatenate the results
orbit_pm = rbind(dflist[[1]], dflist[[2]])
for(i in 3:length(dfnumber)){orbit_pm = rbind(orbit_pm, dflist[[i]])}

#-------------------------------------------------------------------------------
# Post-processing on coordinates and units
#-------------------------------------------------------------------------------
# From NC coordinates to C coordinates
orbit    = NCtoC(orbit, gammaR)
orbit    = CtoPH(orbit, L)
orbit    = NCtoSYS(orbit, gammaR, c1R)
orbit    = SYStoPH(orbit, L)
orbit_pm = NCtoC(orbit_pm, gammaR)
orbit_pm = CtoPH(orbit_pm, L)
orbit_pm = NCtoSYS(orbit_pm, gammaR, c1R)
orbit_pm = SYStoPH(orbit_pm, L)

#-------------------------------------------------------------------------------
#Select half period time
#-------------------------------------------------------------------------------
orbit_half = ddply(orbit, ~order, function(x){x[which.min(abs(x$t-0.25*Period)),]})

#-------------------------------------------------------------------------------
#Select when orbital precision is > maxPrec
#-------------------------------------------------------------------------------
eO_prec = ddply(eO, ~order, function(x){x[which.min(abs(x$y-maxPrec)),]})  #time for which eO ~ maxPrec
orbit_prec = ddply(orbit, ~order, function(orb){orb[which.min(abs(orb$t-eO_prec$x[which(eO_prec$order == maxOrder)])),]}) #select order maxOrder

#-------------------------------------------------------------------------------
#Center manifold
#-------------------------------------------------------------------------------
orbitMaxOrder = orbit_pm[which(orbit_pm$order == maxOrder),]
orbitMaxOrder_prec = orbit_prec[which(orbit_prec$order==maxOrder),]
orbitMaxOrder_start = orbitMaxOrder[which(orbitMaxOrder$t == 0.0),]
orbitMaxOrder_end = orbitMaxOrder[which(orbitMaxOrder$t == max(orbitMaxOrder$t)),]

#-------------------------------------------------------------------------------
# Plot
#-------------------------------------------------------------------------------
pxyPH = fplot_path (orbit,  "xPH", "yPH", xs,  ys, "order", ns, 1 )#, "order")
#Grey scale if needed
#pxyPH = pxyPH + scale_colour_grey(start = 0.9, end = 0.0, name="Order")
#Center manifold plot
pxyPH = pxyPH + geom_path(data  = orbitMaxOrder, aes(xPH, yPH, color = "PM"), size = linesize[["line"]], linetype = "dashed", colour = "black")
# Starting point
pxyPH = pxyPH + geom_point(data = orbitMaxOrder_start, aes(xPH, yPH), size = 4, colour = "black", fill = "white", pch = 21)
# Point at a given precision
#pxyPH = pxyPH + geom_point(data = orbitMaxOrder_end, aes(xPH, yPH), size = 4, pch = 22, colour = "black", fill = "white")
#Scaling
pxyPH = pxyPH + scale_x_continuous(limits = c(-4.47e5, -4.34e5))  #cont scale on the x axis 
pxyPH = pxyPH + scale_y_continuous(limits = c(-1.2e4,  1.1e4))   #cont scale on the y axis 
#Theme
#pxyPH = pxyPH + theme_bw()+custom_theme
#Add an arrow to give the direction of motion along the orbit
ai= 50
pxyPH = pxyPH + geom_segment(aes(x = orbit$xPH[ai], y = orbit$yPH[ai], xend = orbit$xPH[ai+1], yend = orbit$yPH[ai+1]), 
                           colour = muted("blue"), 
                           arrow = arrow(length = unit(0.4, "cm"), type = "closed"))
pxyPH = pxyPH+legend_inside_theme+theme(legend.position = c(0.28,0.02))
pxyPH

#Add the Moon
#-------------------
moonR = 1737.10       #Moon's radius
moonPos = gammaR*L     #Moon position in km wrt to Li
#pxyPH = addMoon(pxyPH, x = moonPos, y = 0, moonR, surfSize = 0.4, cratSize = 0.2)+ coord_fixed()

# Add Li
dfemli       = dflibpoint(Li, FWRK)
pxyPH = pxyPH + geom_point(data = dfemli, aes(x= x_PH, y = y_PH), size = 4, colour = "black", fill = "black", pch = 21) 
pxyPH = pxyPH + annotate("text", x = dfemli$x_PH, y = -1.5e3,  label = "\\textsc{eml}$_2$", size=10)

pxyPH


#-------------------------------------------------------------------------------
# Plot (yz)
#-------------------------------------------------------------------------------
pyzPH = fplot_path (orbit,  "yPH", "zPH", ys,  zs, "order", ns, 1 )#, "order")
#Grey scale if needed
#pyzPH = pyzPH + scale_colour_grey(start = 0.9, end = 0.0, name="Order")
#Center manifold plot
pyzPH = pyzPH + geom_path(data  = orbitMaxOrder, aes(yPH, zPH, color = "PM"), size = linesize[["line"]], linetype = "dashed", colour = "black")

# Starting point
pyzPH = pyzPH + geom_point(data = orbitMaxOrder_start, aes(yPH, zPH), size = 4, colour = "black", fill = "white", pch = 21)

# Point at a given precision
#pyzPH = pyzPH + geom_point(data = orbitMaxOrder_prec, aes(yPH, zPH), size = 4, pch = 22, colour = "black", fill = "white")

#Scaling
pyzPH = pyzPH + scale_x_continuous(limits = c(-1.2e4,  1.8e4))  #cont scale on the x axis 
pyzPH = pyzPH + scale_y_continuous(limits = c(-3.3e4, 3.1e4))  #cont scale on the y axis 

#Theme
#pyzPH = pyzPH + theme_bw()+custom_theme

#Annotation
pyzPH = pyzPH + geom_segment(aes(x = orbit$yPH[ai], y = orbit$zPH[ai], xend = orbit$yPH[ai+1], yend = orbit$zPH[ai+1]), 
                         colour = muted("blue"), 
                         arrow = arrow(length = unit(0.4, "cm"), type = "closed"))
pyzPH = pyzPH+legend_inside_theme+theme(legend.position = c(0.98,0.55))

# Add Li
pyzPH = pyzPH + geom_point(data = dfemli, aes(x= y_PH, y = z_PH), size = 4, colour = "black", fill = "black", pch = 21) 
pyzPH = pyzPH + annotate("text", x = dfemli$y_PH, y = -4e3,  label = "\\textsc{eml}$_2$", size=10)

#Display the plot
pyzPH

#===============================================================================
#                           plot XYZ (undim)
#===============================================================================
pxy = fplot_path (orbit,  "xEM", "yEM", xse,  yse, "order", ns, 1 )#, "order")

#Center manifold plot
pxy = pxy + geom_path(data  = orbitMaxOrder, aes(xEM, yEM, color = "PM"), size = linesize[["line"]], linetype = "dotted", colour = "black")

# Starting point
pxy = pxy + geom_point(data = orbitMaxOrder_start, aes(xEM, yEM), size = 4, colour = "black", fill = "white", pch = 21)

#Scaling
pxy = pxy + scale_x_continuous(limits = c(-1.165, -1.13))  #cont scale on the x axis 
pxy = pxy + scale_y_continuous(limits = c(-0.025,  0.025))   #cont scale on the y axis 

#Add an arrow to give the direction of motion along the orbit
ai= 50
pxy = pxy + geom_segment(aes(x = orbit$xEM[ai], y = orbit$yEM[ai], xend = orbit$xEM[ai+1], yend = orbit$yEM[ai+1]), 
                         colour = muted("blue"), 
                         arrow = arrow(length = unit(0.4, "cm"), type = "closed"))

# Theme
pxy = pxy+legend_inside_theme+theme(legend.position = c(0.26,0.05))

# Add Li
dfemli       = dflibpoint(Li, FWRK)
pxy = pxy + geom_point(data = dfemli, aes(x= x_SYS, y = y_SYS), size = 4, colour = "black", fill = "black", pch = 21) 
pxy = pxy + annotate("text", x = dfemli$x_SYS, y = -0.003,  label = "\\textsc{eml}$_2$", size=10)

pxy

#-------------------------------------------------------------------------------
# Plot (yz)
#-------------------------------------------------------------------------------
pyz = fplot_path (orbit,  "yEM", "zEM", yse,  zse, "order", ns, 1 )#, "order")

#Center manifold plot
pyz = pyz + geom_path(data  = orbitMaxOrder, aes(yEM, zEM, color = "PM"), size = linesize[["line"]], linetype = "dashed", colour = "black")

# Starting point
pyz = pyz + geom_point(data = orbitMaxOrder_start, aes(yEM, zEM), size = 4, colour = "black", fill = "white", pch = 21)


#Scaling
pyz = pyz + scale_x_continuous(limits = c(-0.03, 0.04))  #cont scale on the x axis 
pyz = pyz + scale_y_continuous(limits = c(-0.09, 0.08))  #cont scale on the y axis 

#Add an arrow to give the direction of motion along the orbit
pyz = pyz + geom_segment(aes(x = orbit$yEM[ai], y = orbit$zEM[ai], xend = orbit$yEM[ai+1], yend = orbit$zEM[ai+1]), 
                         colour = muted("blue"), 
                         arrow = arrow(length = unit(0.4, "cm"), type = "closed"))
# Theme
pyz = pyz+legend_inside_theme+theme(legend.position = c(0.98,0.55))


# Add Li
dfemli       = dflibpoint(Li, FWRK)
pyz = pyz + geom_point(data = dfemli, aes(x= y_SYS, y = z_SYS), size = 4, colour = "black", fill = "black", pch = 21) 
pyz = pyz + annotate("text", x = dfemli$y_SYS, y = -0.01,  label = "\\textsc{eml}$_2$", size=10)

pyz


#stop()

#-------------------------------------------------------------------------------
#Save in tex file
#-------------------------------------------------------------------------------
ggplot2tikz_phd(pxy, xSize, ySize, file = paste0(currentfolder, "R_XY_Size_", sizep,  "_EM.tex"))
ggplot2tikz_phd(pyz, xSize, ySize, file = paste0(currentfolder, "R_YZ_Size_", sizep,  "_EM.tex"))

stop()

ggplot2tikz_phd(pI, xSize, ySize, file = paste0(currentfolder, "R_eI_Size_", sizep,  ".tex"))
ggplot2tikz_phd(pO, xSize, ySize, file = paste0(currentfolder, "R_eO_Size_", sizep,  ".tex"))
ggplot2tikz_phd(pH, xSize, ySize, file = paste0(currentfolder, "R_eH_Size_", sizep,  ".tex"))
ggplot2tikz_phd(pxyPH, xSize, ySize, file = paste0(currentfolder, "R_XY_Size_", sizep,  ".tex"))
ggplot2tikz_phd(pyzPH, xSize, ySize, file = paste0(currentfolder, "R_YZ_Size_", sizep,  ".tex"))




#-------------------------------------------------------------------------------
#Save in pdf file
#-------------------------------------------------------------------------------
# ggsave(pH,   file = paste0(currentfolder, "R_eH_Size_", sizep,  ".pdf"))
# ggsave(pI,   file = paste0(currentfolder, "R_eI_Size_", sizep,  ".pdf"))
# ggsave(pO,   file = paste0(currentfolder, "R_eO_Size_", sizep,  ".pdf"))
# ggsave(pxyPH,  file = paste0(currentfolder, "R_XY_Size_", sizep, ".pdf"))
# ggsave(pyzPH,  file = paste0(currentfolder, "R_YZ_Size_", sizep, ".pdf"))

