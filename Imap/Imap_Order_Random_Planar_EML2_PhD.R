# Imap_Order_Random_Planar_EML2_PhD.R
#
#
# R script to handle a precision map of the QBFBP around EML1,2
#--------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Init
#-------------------------------------------------------------------------------
source("source/init.R")

#-------------------------------------------------------------------------------
# EML1
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Select Models & libration point
#-------------------------------------------------------------------------------
Li    = "L2"
MODEL = "QBCP"
FWRK  = "EM"
Type  = "NF_rand" #"rand" or "NF_rand"
Energy = 0
vorders = c(5, 10, 15, 20, 25, 30);
currentfolder = paste0(printfolder(MODEL, FWRK, Li))
pS = 2
eps = 1e-7
deps = 0.2


#-------------------------------------------------------------------------------
# Normalized units (gamma_li, c1_li)
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
    if(Type  == "rand")
    {
      fileprefix = paste0(currentfolder, "Serv/eIm_", Type, "_ofs_30_order_",toString(i), "_planar_PhD");
    }else
    {
      fileprefix = paste0(currentfolder, "Serv/eIm_", Type, "_ofs_30_order_",toString(i), "_planar");
    }
    
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
imapr = imap[which(isCloseToEPS),]
#Ordered version
imapr = imapr[order(imapr$order),]


#-------------------------------------------------------------------------------
# Plot 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Only a given precision
#-------------------------------------------------------------------------------
# Version without background
pplc = plotdf_point(imapr, "xEM", "yEM", "\\textit{X}", "\\textit{Y}", pointSize = 1, colorCol = "order", colorLabel = "Order $N$", isColorFac = TRUE)
pplc = pplc + guides(colour = guide_legend(override.aes = list(size=3)))

#Add lib point
dfemli  = dflibpoint(Li, "EM")
pplc   = pplc + geom_point(data = dfemli, aes(x= x_SYS, y = y_SYS), size = 2)

#Add m2 with true apparent radius
primaryPos  =  -(1-muR)
pplc = addPrimary(pplc, primaryPos, 0, primaryR/L, 0.4, "grey50")

#Add the name of the Moon
pplc = pplc + annotate("text", x = primaryPos-0.007, y = 0.015,  label = "Moon", size=7)
pplc

#Ratio and limits
pplc = pplc + coord_fixed(ratio=1)
if(Li == "L2")
{
  pplc = pplc + scale_y_continuous(limits = c(-0.115,0.115))
  pplc = pplc + legend_inside_theme
}else{
  pplc = pplc + scale_y_continuous(limits = c(-0.115,0.115))
  pplc = pplc + legend_left_theme
}

#---------------------
# Adding the resonnances
#---------------------
if(Li == "L2")
{
  #-----------------------------------------------------------------------------
  # Precise param
  #-----------------------------------------------------------------------------
  df24 = read.table("Single_Orbit/EML2_resonant_orbit.txt", header = F)
  names(df24) = c("x", "y")
  pplc = pplc + geom_path(data=df24, aes(x=x, y=y), size=1, linetype= "dashed")
  
}else{
  eml1_res = read.table("Single_Orbit/EML1_resonant_orbit.txt", header = F)
  names(eml1_res) = c("x", "y")
  pplc = pplc + geom_path(data=eml1_res, aes(x=x, y=y), size=1, linetype= "dashed")
}


#-------------------------------------------------------------------------------
# Only a given precision, in physical units
#-------------------------------------------------------------------------------
# Centered on the lib point
pplcph = plotdf_point(imapr, "xCPH", "yCPH", "X [km]", "Y [km]", pointSize = pS, colorCol = "order", colorLabel = "order", isColorFac = TRUE)
pplcph
# Synodical frame
pplph = plotdf_point(imapr, "xPH", "yPH", "X [km]", "Y [km]", pointSize = pS, colorCol = "order", colorLabel = "order", isColorFac = TRUE)

#Add m2
primaryPos  =  -(1-muR) * L #Add m2
pplph = addMoon(pplph, x = primaryPos, y = 0, primaryR, surfSize = 0.4, cratSize = 0.2)
pplph


#-------------------------------------------------------------------------------
# s1-s3 maps
#-------------------------------------------------------------------------------
ppls1s3 = plotdf_point(imapr, "s1", "s3", "$s_1$", "$s_3$", pointSize = pS, colorCol = "order", colorLabel = "Order $N$", isColorFac = TRUE)
ppls1s3 = ppls1s3 + scale_x_continuous(breaks = seq(-50,50,10)) + scale_y_continuous(breaks = seq(-50,50,10))
ppls1s3

#-------------------------------------------------------------------------------
#Save in tikz
#-------------------------------------------------------------------------------
ggplot2tikz_phd(ppls1s3, xSize,  ySize, file = paste0(currentfolder, "EIm_", Type, "_ofs_30_order_",toString(i), "_planar_s1s3.tex"))
ggplot2tikz_phd(pplc, xSize,  ySize, file = paste0(currentfolder, "EIm_", Type, "_ofs_30_order_",toString(i), "_planar_resonant.tex"))

stop()

#
#
#
#

################################################################################
# ADDITIONAL PLOTS
################################################################################

#-------------------------------------------------------------------------------
# Adding the CR3BP zero-velocity curves
#-------------------------------------------------------------------------------
if(Energy > 0)
{
  #---------------------
  #Build vectors
  #---------------------
  if(Li == "L2")
  {
    x0   = seq(-2,-1,0.001)
  }else{
    x0   = seq(-1,-0.8,0.001)
  }
  y0   = seq(-0.15,0.15,0.001)
  
  z0   = expand.grid(x0, y0)
  potn = apply(z0, 1, cr3bpot_EM)
  
  
  #Build data frame
  df = data.frame(x1 = z0[1], y1 = z0[2], pot = potn)
  names(df) = c('x', 'y', 'pot')
  
  #---------------------
  #Energy associated with the Libration points
  #---------------------
  l1 = c(-0.836915145386502, 0, 0)
  l2 = c(-1.155682150113637, 0, 0)
  l1pot = cr3bpot(l1[1], l1[2], muR)
  l2pot = cr3bpot(l2[1], l2[2], muR)
  
  #---------------------
  #Select a given energy
  #---------------------
  if(Li == "L2")
  {
    eps = l2pot+Energy;
  }else{
    eps = l1pot+Energy;
  }
  isCloseToEPS = abs(df$pot - eps) < 1e-4;
  dfr = df[which(isCloseToEPS),]
  
  #---------------------
  #Plot
  #---------------------
  pplcp = pplc + geom_point(data=dfr, aes(x=x, y=y), size=3)
  pplcp + coord_fixed(ratio=1)
}


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