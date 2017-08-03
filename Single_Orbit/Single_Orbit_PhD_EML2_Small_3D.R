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
# currentfolder = paste0(plotfolder(MODEL, FWRK, Li), "Serv/")
# size = "-51.0435_test" #1.5 7.5 15 30 45
# sizep = "51.0435_test"

# If local
currentfolder = paste0(plotfolder(MODEL, FWRK, Li), "orbits/")
size =  "10_test" #40_test
sizep = "10_test" #40_test

# Orders
dfindex  = c(5,10,15,20)
dfnumber = 1:length(dfindex)
maxOrder = max(dfindex)


#-------------------------------------------------------------------------------
# Parameters
#-------------------------------------------------------------------------------
maxPrec = 1e-6

#-------------------------------------------------------------------------------
#Normalized units (gamma, c1R)
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
#Logscale on the y axis
# if(MODEL == "QBCP") 
# {
#   pH = pH + scale_y_log10(limits=c(1e-8, 1e-2), breaks = 10^(-8:0*2))
# }else
# {
#   pH = pH + scale_y_log10(limits=c(1e-15, 1e-1), breaks = 10^(-8:0*2)) 
# }

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
pI = pI + scale_y_log10(breaks = 10^(-8:0*2), limits=c(1e-13,NaN))
#Theme
pI = pI+legend_inside_theme+theme(legend.position = c(1,0.4))
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
pO = pO + scale_y_log10(limits=c(1e-14,NaN), breaks = 10^(-8:0*2))
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
pxy = fplot_path (orbit,  "xPH", "yPH", xs,  ys, "order", ns, 1 )#, "order")
#Grey scale if needed
#pxy = pxy + scale_colour_grey(start = 0.9, end = 0.0, name="Order")
#Center manifold plot
pxy = pxy + geom_path(data  = orbitMaxOrder, aes(xPH, yPH, color = "PM"), size = linesize[["line"]], linetype = "dotted", colour = "black")
# Starting point
pxy = pxy + geom_point(data = orbitMaxOrder_start, aes(xPH, yPH), size = 4, colour = "black", fill = "white", pch = 21)
# Point at a given precision
#pxy = pxy + geom_point(data = orbitMaxOrder_end, aes(xPH, yPH), size = 4, pch = 22, colour = "black", fill = "white")
#Scaling
pxy = pxy + scale_x_continuous(limits = c(-4.5e5, -4.41e5))  #cont scale on the x axis 
pxy = pxy + scale_y_continuous(limits = c(-1.0e4,  1.1e4))   #cont scale on the y axis 
#Theme
#pxy = pxy + theme_bw()+custom_theme
#Annotation
#pxy = pxy + annotate("text", x = -0.3, y = -0.21, label = "CM \n Order maxOrder", parse = F, size = 8, colour = "limegreen", fontface = "bold")
#pxy = pxy + geom_segment(aes(x = -0.25, y = -0.2, xend = -0.19, yend = -0.2), arrow = arrow(length = unit(0.5, "cm")), size = psize)
#Add an arrow to give the direction of motion along the orbit
ai= 50
pxy = pxy + geom_segment(aes(x = orbit$xPH[ai], y = orbit$yPH[ai], xend = orbit$xPH[ai+1], yend = orbit$yPH[ai+1]), 
                           colour = muted("blue"), 
                           arrow = arrow(length = unit(0.4, "cm"), type = "closed"))
pxy = pxy+legend_inside_theme+theme(legend.position = c(0.3,0))
pxy

#Add the Moon
#-------------------
moonR = 1737.10       #Moon's radius
moonPos = gammaR*L     #Moon position in km wrt to Li
#pxy = addMoon(pxy, x = moonPos, y = 0, moonR, surfSize = 0.4, cratSize = 0.2)+ coord_fixed()

# Add Li
dfemli       = dflibpoint(Li, FWRK)
pxy = pxy + geom_point(data = dfemli, aes(x= x_PH, y = y_PH), size = 4, colour = "black", fill = "black", pch = 21) 
pxy = pxy + annotate("text", x = dfemli$x_PH, y = -1e3,  label = "\\textsc{eml}$_2$", size=10)

pxy


#-------------------------------------------------------------------------------
# Plot (yz)
#-------------------------------------------------------------------------------
pyz = fplot_path (orbit,  "yPH", "zPH", ys,  zs, "order", ns, 1 )#, "order")
#Grey scale if needed
#pyz = pyz + scale_colour_grey(start = 0.9, end = 0.0, name="Order")
#Center manifold plot
pyz = pyz + geom_path(data  = orbitMaxOrder, aes(yPH, zPH, color = "PM"), size = linesize[["line"]], linetype = "dashed", colour = "black")

# Starting point
pyz = pyz + geom_point(data = orbitMaxOrder_start, aes(yPH, zPH), size = 4, colour = "black", fill = "white", pch = 21)

# Point at a given precision
#pyz = pyz + geom_point(data = orbitMaxOrder_prec, aes(yPH, zPH), size = 4, pch = 22, colour = "black", fill = "white")

#Scaling
pyz = pyz + scale_x_continuous(limits = c(-5e4, 5e4))  #cont scale on the x axis 
pyz = pyz + scale_y_continuous(limits = c(-10000, 13000))  #cont scale on the y axis 
#Theme
#pyz = pyz + theme_bw()+custom_theme
#Annotation
#pyz = pyz + annotate("text", x = -0.3, y = -0.21, label = "CM \n Order maxOrder", parse = F, size = 8, colour = "limegreen", fontface = "bold")
#pyz = pyz + geom_segment(aes(x = -0.25, y = -0.2, xend = -0.19, yend = -0.2), arrow = arrow(length = unit(0.5, "cm")), size = psize)
#Display the plot
pyz
pyz = pyz + geom_segment(aes(x = orbit$yPH[ai], y = orbit$zPH[ai], xend = orbit$yPH[ai+1], yend = orbit$zPH[ai+1]), 
                         colour = muted("blue"), 
                         arrow = arrow(length = unit(0.4, "cm"), type = "closed"))
pyz = pyz+legend_inside_theme


#-------------------------------------------------------------------------------
# Plot (xz)
#-------------------------------------------------------------------------------
pxz = fplot_path (orbit,  "xPH", "zPH", xs,  zs, "order", ns, 1 )#, "order")
#Grey scale if needed
pxz = pxz + scale_colour_grey(start = 0.9, end = 0.0, name="Order")
#Center manifold plot
pxz = pxz + geom_path(data  = orbitMaxOrder, aes(xPH, zPH, color = "PM"), size = linesize[["line"]], linetype = "dashed", colour = "black")
# Starting point
pxz = pxz + geom_point(data = orbitMaxOrder_start, aes(xPH, zPH), size = 4, colour = "black", fill = "white", pch = 21)
# Point at a given precision
#pxz = pxz + geom_point(data = orbitMaxOrder_prec, aes(xPH, zPH), size = 4, pch = 22, colour = "black", fill = "white")

#Scaling
#pxz = pxz + scale_x_continuous(limits = c(-50, 40))  #cont scale on the x axis 
#pxz = pxz + scale_y_continuous(limits = c(-3500, 3500))  #cont scale on the y axis 
#Theme
#pxz = pxz + theme_bw()+custom_theme
#Annotation
#pxz = pxz + annotate("text", x = -0.3, y = -0.21, label = "CM \n Order maxOrder", parse = F, size = 8, colour = "limegreen", fontface = "bold")
#pxz = pxz + geom_segment(aes(x = -0.25, y = -0.2, xend = -0.19, yend = -0.2), arrow = arrow(length = unit(0.5, "cm")), size = psize)
#Display the plot
pxz
pxz = pxz + geom_segment(aes(x = orbit$xPH[ai], y = orbit$zPH[ai], xend = orbit$xPH[ai+1], yend = orbit$zPH[ai+1]), 
                         colour = muted("blue"), 
                         arrow = arrow(length = unit(0.4, "cm"), type = "closed"))
pxz = pxz+legend_inside_theme


#stop()
#-------------------------------------------------------------------------------
#Save in tex file
#-------------------------------------------------------------------------------
ggplot2tikz_phd(pI, xSize, ySize, file = paste0(currentfolder, "R_eI_Size_", sizep,  ".tex"))
ggplot2tikz_phd(pO, xSize, ySize, file = paste0(currentfolder, "R_eO_Size_", sizep,  ".tex"))
ggplot2tikz_phd(pH, xSize, ySize, file = paste0(currentfolder, "R_eH_Size_", sizep,  ".tex"))
ggplot2tikz_phd(pxy, xSize, ySize, file = paste0(currentfolder, "R_XY_Size_", sizep,  ".tex"))
ggplot2tikz_phd(pyz, xSize, ySize, file = paste0(currentfolder, "R_YZ_Size_", sizep,  ".tex"))


#-------------------------------------------------------------------------------
#Save in pdf file
#-------------------------------------------------------------------------------
# ggsave(pH,   file = paste0(currentfolder, "R_eH_Size_", sizep,  ".pdf"))
# ggsave(pI,   file = paste0(currentfolder, "R_eI_Size_", sizep,  ".pdf"))
# ggsave(pO,   file = paste0(currentfolder, "R_eO_Size_", sizep,  ".pdf"))
# ggsave(pxy,  file = paste0(currentfolder, "R_XY_Size_", sizep, ".pdf"))
# ggsave(pyz,  file = paste0(currentfolder, "R_YZ_Size_", sizep, ".pdf"))

