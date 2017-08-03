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
  
  deps = 0.8
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
  scale_y_ds = scale_y_continuous()
  legend_vec = c(0,1)
}else
{
  scale_y_ds = scale_y_continuous(limits = c(0, 0.55), breaks = seq(0, 0.5, 0.1))
  legend_vec = c(1,0)
}

#-------------------------------------------------------------------------------
# colorLab
#-------------------------------------------------------------------------------
#colorLab = "logEI"
# Or
colorLab = "$\\raisebox{0.3ex}{\\scriptsize{$\\log_{10}(E_I)$}}$"


#-------------------------------------------------------------------------------
# Main plot: N vs sC
#-------------------------------------------------------------------------------
ppms = plotdf_path(imapsys, "order", "msC", "Order $N$", "$\\bar{\\|\\mathbf{s}_0\\|}$", colorCol = "eps", colorLabel = colorLab, isColorFac = TRUE)
ppms = ppms + scale_x_continuous(breaks = seq(5,30,5))
ppms = ppms + scale_y_ds
ppms = ppms + custom_theme + legend_pos(legend_vec) 
ppms

#-------------------------------------------------------------------------------
#Save in tikz
#-------------------------------------------------------------------------------
ggplot2tikz_phd(ppms, xSize, ySize, file = paste0(currentfolder, "EIm_", Type, "_ofs_30_order_",toString(i), "_sC.tex"))
stop()

