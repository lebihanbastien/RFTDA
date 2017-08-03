# R script to handle a precision map of the QBFBP around EML1,2
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Init
#-------------------------------------------------------------------------------
source("source/init.R")

#-------------------------------------------------------------------------------
# Select Models & libration point
#-------------------------------------------------------------------------------
Li      = "L2"
MODEL   = "QBCP"
FWRK    = "SEM"
Type    = "rand" #selection or global
Energy  = 0
vorders = c(5, 10, 15, 20, 25, 30);
#vorders = c(seq(3,30,2), 30)
currentfolder = paste0(printfolder(MODEL, FWRK, Li), "Serv/")

#-------------------------------------------------------------------------------
#Normalized units (gamma, c1)
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
    fileprefix = paste0(currentfolder, "eIm_", Type, "_ofs_30_order_",toString(i), "_PhD");
  }else
  {
    fileprefix = paste0(currentfolder, "eIm_", Type, "_ofs_30_order_",toString(i), "_energy_", toString(Energy));
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
#Distance to the center
imap$rC = sqrt(imap$xC^2+imap$yC^2+imap$zC^2)
imap$rCPH = sqrt(imap$xCPH^2+imap$yCPH^2+imap$zCPH^2)
imap$frCPH = floor(imap$rCPH*1e-4)
imap$sC = sqrt(imap$s1^2+imap$s2^2+imap$s3^2++imap$s4^2)
#Abs of zC
imap$zCn = abs(imap$zC)

#-------------------------------------------------------------------------------
# Get rid of bad precision
#-------------------------------------------------------------------------------
imap = imap[which(imap$log10eOm < -1),]

#-------------------------------------------------------------------------------
# Select only some given value in the results
#-------------------------------------------------------------------------------
imapsys = data.frame();
# veps = c(seq(1,8,2)*1e-5, seq(1,8,2)*1e-6, seq(1,8,2)*1e-7, seq(1,8,2)*1e-8, seq(1,8,2)*1e-9);

# Select only positive some given value in the results
if(FWRK == "EM")
{
  if(Li == "L2")
  {
    veps = 10^(-seq(5,9));
  }else{
    veps = 10^(-seq(5,9));
  }
  
  deps = 0.2
}else
{
  if(Li == "L2")
  {
    veps = 10^(-seq(5,12));
  }else{
    veps = 10^(-seq(5,12));
  } 
  
  deps = 0.5
}

for (eps in veps)  #loop on the orders
{
  isCloseToEPS = abs(imap$EIm - eps) < deps*eps;
  imapr = imap[which(isCloseToEPS),]
  
  #Ordered version
  imapr = imapr[order(imapr$order),]
  
  # Compute the mean position
  imapm = ddply(imapr, .(order), summarize, mdHz = mean(dHz), msC = mean(sC), mrCPH = mean(rCPH), mrC = mean(rC), mxC = mean(abs(xC)), myC = mean(abs(yC)),  mzC = mean(abs(zC)))
  imapm$eps = log10(eps)
  
  
  #rbind in ttm_all
  imapsys = rbind(imapsys, imapm);
}

#-------------------------------------------------------------------------------
# Scale x
#-------------------------------------------------------------------------------
if(FWRK == "EM")
{
  scale_x_dH = scale_x_continuous(breaks = seq(0,0.03,0.01))
  scale_y_dH = scale_y_continuous(breaks = seq(0,0.03,0.01), limits = c(0, 0.033))
}else
{
  scale_x_dH = scale_x_continuous(labels = scientific_format(), breaks = seq(0,12,3)*1e-5)
  scale_y_dH = scale_y_continuous(labels = scientific_format(), breaks = seq(0,12,3)*1e-5, limits = c(0, 1.4e-4))
}

#-------------------------------------------------------------------------------
# colorLab
#-------------------------------------------------------------------------------
colorLab = "logEI"
# Or
colorLab = "$\\raisebox{0.3ex}{\\scriptsize{$\\log_{10}(E_I)$}}$"

#-------------------------------------------------------------------------------
# Main plot: N vs dHz
#-------------------------------------------------------------------------------
ppme = plotdf_path(imapsys, "order", "mdHz", "Order $N$", "$\\delta \\bar{H}_0$", colorCol = "eps", colorLabel = colorLab, isColorFac = TRUE)
ppme = ppme + scale_x_continuous(breaks = seq(5,30,5))
ppme = ppme + scale_y_dH
ppme = ppme + custom_theme + legend_pos(c(0,1)) 
ppme


#-------------------------------------------------------------------------------
#Save in tikz
#-------------------------------------------------------------------------------
ggplot2tikz_phd(ppme, xSize, ySize, file = paste0(currentfolder, "EIm_", Type, "_ofs_30_order_",toString(i), "_3D.tex"))
stop()



#-------------------------------------------------------------------------------
# Main plot: dHz vs N
#
# ppme2 = geom_point_pretty(ppme2, imapsys, aes(mdHz, order, color = factor(eps)))
#-------------------------------------------------------------------------------
ppme2 = plotdf_path(imapsys, "mdHz", "order", "$\\delta \\bar{H}_0$", "Order $N$", colorCol = "eps", colorLabel = colorLab, isColorFac = TRUE)
ppme2 = ppme2 + scale_x_dH
ppme2 = ppme2 + scale_y_continuous(breaks = seq(5,30,5))
ppme2 = ppme2 + legend_inside_theme
# Needed to account for the cutting of the right part of the x labels
ppme2 = ppme2 + theme(plot.margin = margin(10,40,10,10))
ppme2


#-------------------------------------------------------------------------------
# Other plots from old implementation
#-------------------------------------------------------------------------------
Lit     = "$L_2$"
ppmd = plotdf_line(imapsys, "mrCPH", "order", paste0("mean distance to ", Lit), "Order", colorCol = "eps", colorLabel = "Precision", isColorFac = TRUE)
ppmd = ppmd + scale_x_continuous(breaks = seq(5,30,5))
ppmd

ppmer = plotdf_line(imapsys, "mdHz", "eps", "mean($\\delta H_0$)", "$log_{10}(e_I)$", colorCol = "order", colorLabel = "Order", isColorFac = TRUE)
ppmer = ppmer + scale_x_continuous(breaks = seq(-10,-1,1))
ppmer

#-------------------------------------------------------------------------------
# 3D plot
#-------------------------------------------------------------------------------
# scatter3D(imapr$xEM, imapr$yEM, imapr$zEM, colvar = imapr$order, pch = 16, cex = 1.5)
# scatter3D(imap$xEM, imap$yEM, imap$zEM, colvar = imap$flog10eOm, pch = 16, cex = 1.5)
