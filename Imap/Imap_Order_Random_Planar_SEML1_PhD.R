# R script to handle a precision map of the QBFBP around EML1,2
#-------------------------------------------------------------------------------

# Init
source("source/init.R")

#===============================================================================
# Select Models & libration point
#===============================================================================
Li    = "L1"
MODEL = "QBCP"
FWRK  = "SEM"
Type  = "rand" #selection or global
Energy = 0
OFS_ORDER = 30
vorders = c(5, 10, 15, 20, 25, 30);
currentfolder = paste0(printfolder(MODEL, FWRK, Li))
pS = 2
eps = 1e-7
deps = 0.2

#-------------------------------------------------------------------------------
# Primaries & Lib points for plotting
#-------------------------------------------------------------------------------
dfsemli      = dflibpoint(Li, "SEM")
dfemli       = dflibpoint(Li, "EM")

#-------------------------------------------------------------------------------
# Normalized units (gamma, c1)
#-------------------------------------------------------------------------------
muR = muR(FWRK);
gamma = gamma(Li, FWRK);
c1    =  c1(Li, FWRK);
L     = Ldist(FWRK);
if(FWRK == "EM")
{
  primaryR    =  1737.10      #m2
}else{
  primaryR    =  6378.10      #m2 
}

#-------------------------------------------------------------------------------
# Additionnal parameters
#-------------------------------------------------------------------------------
isLegendOn = 1;
legendOnly = 0;

#===============================================================================
# Building the data.frame of results
#===============================================================================
imap = data.frame()
for (i in vorders)  #loop on the orders
{
  if(Energy == 0)
  {
    fileprefix = paste0(currentfolder, "Serv/eIm_", Type, "_ofs_", toString(OFS_ORDER),"_order_",toString(i), "_planar_PhD");
  }else
  {
    fileprefix = paste0(currentfolder, "Serv/eIm_", Type, "_ofs_", toString(OFS_ORDER),"_order_",toString(i), "_energy_", toString(Energy));
  }
  filename = paste0(fileprefix, ".bin")
  
  
  # Load csv source
  #-----------------------------------------------------------------------------
  if (file.exists(filename))
  {
    names = c("label", "x", "y", "z", "px", "py", "pz",
              "xEM", "yEM", "zEM", "pxEM", "pyEM", "pzEM",
              "s1", "s2", "s3", "s4", "t", "dHz", "eIm");
    imapc = dffbinary(filename, 20, names);
    imapc$order = i;
    
  }else
  {
    imapc = data.frame()
  }
  
  #rbind in ttm_all
  imap = rbind(imap, imapc);
}



#-------------------------------------------------------------------------------
# Postprocessing
#-------------------------------------------------------------------------------
# Using EIm (EM units) instead of eIm (in NC units) 
imap$EIm = gamma*imap$eIm 
# To centered units
imap = NCtoC(imap, gamma)
# To physical units
imap = CtoPH(imap, L)
# To physical units
imap = EMtoPH(imap, L)
# Compute -log10(precision)
imap$log10eOm = log10(imap$EIm)
# Compute -log10(precision)
imap$flog10eOm = floor(log10(imap$EIm))

#-------------------------------------------------------------------------------
# Get rid of bad precision
#-------------------------------------------------------------------------------
imap = imap[which(imap$log10eOm < -2),]

#-------------------------------------------------------------------------------
# Select only positive some given value in the results
#-------------------------------------------------------------------------------
isCloseToEPS = abs(imap$EIm - eps) < deps*eps;
imapr = imap[which(isCloseToEPS),]
#Ordered version
imapr = imapr[order(imapr$order),]


#===============================================================================
# Plot 
#===============================================================================

#-------------------------------------------------------------------------------
# Only a given precision
#-------------------------------------------------------------------------------
# Version without background
pplc = plotdf_point(imapr, "xEM", "yEM", "\\textit{X}", "\\textit{Y}", pointSize = 1, colorCol = "order", colorLabel = "Order $N$", isColorFac = TRUE)
pplc = pplc + guides(colour = guide_legend(override.aes = list(size=3)))
pplc

#Add Lib point
pplc = pplc + geom_point(data = dfsemli, aes(x= x_SYS, y = y_SYS), size = 2) 

#Add m2
primaryPos  =  -(1-muR)
pplc = addPrimary(pplc, primaryPos, 0, 10*primaryR/L, 0.4, muted("blue"))

#Add the name of the Earth
pplc = pplc + annotate("text", x = primaryPos+0.001, y = 0.001,  label = "Earth", size=7)

#Ratio and limits
pplc = pplc + coord_fixed(ratio=1)
pplc = pplc + scale_y_continuous(limits = c(-0.01,0.01))

#-------------------------------------------------------------------------------
# s1-s3 maps
#-------------------------------------------------------------------------------
ppls1s3 = plotdf_point(imapr, "s1", "s3", "$s_1$", "$s_3$", pointSize = pS, colorCol = "order", colorLabel = "Order $N$", isColorFac = TRUE)
ppls1s3 = ppls1s3 + scale_x_continuous(breaks = seq(-0.5,0.5,0.1)) + scale_y_continuous(breaks = seq(-0.5,0.5,0.1))
ppls1s3



#-------------------------------------------------------------------------------
#Save in tikz
#-------------------------------------------------------------------------------
cfilename = paste0(currentfolder,  "EIm_", Type, "_ofs_", toString(OFS_ORDER),"_order_",toString(i), "_planar")
ggplot2tikz_phd(pplc, xSize, ySize, file = paste0(cfilename, ".tex"))

cfilename = paste0(currentfolder,  "EIm_", Type, "_ofs_", toString(OFS_ORDER),"_order_",toString(i), "_planar_s1s3")
ggplot2tikz_phd(ppls1s3, xSize, ySize, file = paste0(cfilename, ".tex"))

stop()


#
#
#
#

################################################################################
# Other possibilities
################################################################################
ppls = plotdf_smooth(imapr, "xEM", "yEM", "X", "Y", colorCol = "order", colorLabel = "order", isColorFac = TRUE)
ppls

ppll = plotdf_line(imapr, "xEM", "yEM", "X", "Y", colorCol = "order", colorLabel = "order", isColorFac = TRUE)
ppll



#-------------------------------------------------------------------------------
# Only a given precision, in physical units
#-------------------------------------------------------------------------------
pplcph = plotdf_point(imapr, "xCPH", "yCPH", "X [km]", "Y [km]", pointSize = pS, colorCol = "order", colorLabel = "order", isColorFac = TRUE)
pplcph

pplph = plotdf_point(imapr, "xPH", "yPH", "X [km]", "Y [km]", pointSize = pS, colorCol = "order", colorLabel = "order", isColorFac = TRUE)
#Add m2
primaryPos  =  -(1-muR) * L #Add m2
pplph = addMoon(pplph, x = primaryPos, y = 0, primaryR, surfSize = 0.4, cratSize = 0.2)
pplph

#-------------------------------------------------------------------------------
# Only a given order
#-------------------------------------------------------------------------------
ppl = plotdf_point(imap[which(imap$order == 5),], "xEM", "yEM", "X", "Y", pointSize = pS, colorCol = "flog10eOm", colorLabel = "log10eOm", isColorFac = TRUE)
ppl

#-------------------------------------------------------------------------------
# With respect to the energy
#-------------------------------------------------------------------------------
#Select only some given value in the results
eps = 0.015;
isCloseToEPS = abs(imap$dHz - eps) < 1e-4;
imape = imap[which(isCloseToEPS),]
ppe = plotdf_point(imap[which(imap$order == 20),], "xEM", "yEM", "X", "Y", pointSize = pS, colorCol = "flog10eOm", colorLabel = "log10eOm", isColorFac = TRUE)
ppe



