# R script to handle a precision map of the QBFBP around EML1,2
#-------------------------------------------------------------------------------

# Init
source("source/init.R")

#-------------------------------------------------------------------------------
# SEML1
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Select Models & libration point
#-------------------------------------------------------------------------------
Li    = "L1"
MODEL = "QBCP"
FWRK  = "SEM"
Type  = "rand" #selection or global
Energy = 0
vorders = c(5, 10, 15, 20, 25, 30);
currentfolder = paste0(printfolder(MODEL, FWRK, Li))
pS = 2
eps = 1e-7
deps = 0.2

#-------------------------------------------------------------------------------
#Normalized units (gamma_li, c1_li)
#-------------------------------------------------------------------------------
muR = muR(FWRK);
gamma_li = gamma(Li, FWRK);
c1_li    =  c1(Li, FWRK);
L     = Ldist(FWRK);
if(FWRK == "EM")
{
  primaryR    =  1737.10      #m2
}else{
  primaryR    =  6378.10      #m2 
}

#-------------------------------------------------------------------------------
#Additionnal parameters
#-------------------------------------------------------------------------------
isLegendOn = 1;
legendOnly = 0;

#-------------------------------------------------------------------------------
# Building the data.frame of results
#-------------------------------------------------------------------------------
imap = data.frame()
for (i in vorders)  #loop on the orders
{
  if(Energy == 0)
  {
    fileprefix = paste0(currentfolder, "Serv/eIm_", Type, "_ofs_30_order_",toString(i), "_planar_PhD");
  }else
  {
    fileprefix = paste0(currentfolder, "Serv/eIm_", Type, "_ofs_30_order_",toString(i), "_energy_", toString(Energy));
  }
  filename = paste0(fileprefix, ".bin")
  
  
  # Load csv source
  #-------------------------------------------------------------------------------
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
imap$EIm = gamma_li*imap$eIm 
# To centered units
imap = NCtoC(imap, gamma_li)
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
imapr_eml1 = imap[which(isCloseToEPS),]
#Ordered version
imapr_eml1 = imapr_eml1[order(imapr_eml1$order),]

#-------------------------------------------------------------------------------
# SEML2
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Select Models & libration point
#-------------------------------------------------------------------------------
Li    = "L2"
MODEL = "QBCP"
FWRK  = "SEM"
Type  = "rand" #selection or global
Energy = 0
vorders = c(5, 10, 15, 20, 25, 30);
currentfolder = paste0(printfolder(MODEL, FWRK, Li))
pS = 2

#-------------------------------------------------------------------------------
#Normalized units (gamma_li, c1_li)
#-------------------------------------------------------------------------------
gamma_li = gamma(Li, FWRK);
c1_li    =  c1(Li, FWRK);
L     = Ldist(FWRK);
if(FWRK == "EM")
{
  primaryR    =  1737.10      #m2
}else{
  primaryR    =  6378.10      #m2 
}

#-------------------------------------------------------------------------------
# Building the data.frame of results
#-------------------------------------------------------------------------------
imap = data.frame()
for (i in vorders)  #loop on the orders
{
  if(Energy == 0)
  {
    fileprefix = paste0(currentfolder, "Serv/eIm_", Type, "_ofs_30_order_",toString(i), "_planar_PhD");
  }else
  {
    fileprefix = paste0(currentfolder, "Serv/eIm_", Type, "_ofs_30_order_",toString(i), "_energy_", toString(Energy));
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
imap$EIm = gamma_li*imap$eIm 
# To centered units
imap = NCtoC(imap, gamma_li)
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
imapr_eml2 = imap[which(isCloseToEPS),]
#Ordered version
imapr_eml2 = imapr_eml2[order(imapr_eml2$order),]


#-------------------------------------------------------------------------------
# RBIND the two points
#-------------------------------------------------------------------------------
imapr = rbind(imapr_eml1, imapr_eml2);


#===============================================================================
# Plot 
#===============================================================================

#--------------------------------------------------------------------------------
# Plot 
#--------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Only a given precision
#-------------------------------------------------------------------------------
# Version without background
pplc = plotdf_point(imapr, "xEM", "yEM", "\\textit{X}", "\\textit{Y}", pointSize = 0.6, colorCol = "order", colorLabel = "Order $N$", isColorFac = TRUE)

if(isLegendOn)
{
  pplc = pplc + guides(colour = guide_legend(override.aes = list(size=3)))
}else{
  # Needed to account for the cutting of the right part of the x labels
  pplc = pplc + scale_color_discrete(guide = FALSE)
  pplc = pplc + theme(plot.margin = margin(10,40,10,10))
}

#Add lib point
dfseml1  = dflibpoint("L1", "SEM")
dfseml2  = dflibpoint("L2", "SEM")
pplc   = pplc + geom_point(data = dfseml1, aes(x= x_SYS, y = y_SYS), size = 2, shape = 21, fill="black")
pplc   = pplc + geom_point(data = dfseml2, aes(x= x_SYS, y = y_SYS), size = 2, shape = 23, fill="black")

#Add m2
primaryPos  =  -(1-muR)
pplc = addPrimary(pplc, primaryPos, 0, 10*primaryR/L, 0.4, muted("blue"))

#Add the name of the Earth
pplc = pplc + annotate("text", x = primaryPos, y = 0.0015,  label = "Earth", size=7)

#Ratio and limits
pplc = pplc + coord_fixed(ratio=1)
pplc = pplc + scale_x_continuous(limits = c(-1.013,-0.987))
pplc = pplc + scale_y_continuous(limits = c(-0.01,0.01))

pplc

#-------------------------------------------------------------------------------
#Save in tikz
#-------------------------------------------------------------------------------
ggplot2tikz_phd(pplc, 8.8,  4.6, file = paste0(currentfolder, "EIm_", Type, "_ofs_30_order_",toString(i), "_planar_bothpoints.tex"))
#ggplot2tikz_phd(pplc, 8.8,  4.6, file = paste0(currentfolder, "EIm_", Type, "_ofs_30_order_",toString(i), "_planar_bothpoints_", "eps_", eps, ".tex"))




