############################################################
# R script to handle a poincare map of the QBCP
# Has to be used files such as e.g. PoincareMap_QBCP_SEML2.R
#
# 01/2016
############################################################

#-------------------------------------------------------------------------------
# Init
#-------------------------------------------------------------------------------
source("source/init.R")

#-------------------------------------------------------------------------------
#Current working folder
#-------------------------------------------------------------------------------
currentfolder = paste0(printfolder(MODEL, FWRK, Li))

#-------------------------------------------------------------------------------
#Normalized units (gamma, c1)
#-------------------------------------------------------------------------------
muR = muR(FWRK);
gamma = gamma(Li, FWRK);
c1    =  c1(Li, FWRK);
L     = Ldist(FWRK)

#-------------------------------------------------------------------------------
#Size of plots
#-------------------------------------------------------------------------------
plotW = 18
plotH = 18

#-------------------------------------------------------------------------------
# Data reading
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Filename to check
#-------------------------------------------------------------------------------
#Old version
#filesuffix = paste0("Serv_pm_", add, "Energy_", Energy, "_order_", order, "_ofs_", ofs_order, METHOD);
#filesuffixnodots = paste0("Serv_pm_", add, "Energy_", nst, "_order_", order, "_ofs_", ofs_order, METHOD);
#New version
#filesuffix = paste0("Serv_pm_", add, "Energy_", Energy, "_order_", order, "_ofs_", ofs_order, "_proj_", projFreq, "_max_events_", maxEvents, METHOD);
#filesuffixnodots = paste0("Serv_pm_", add, "Energy_", nst, "_order_", order, "_ofs_", ofs_order, "_proj_", projFreq, "_max_events_", maxEvents, METHOD);
#Direct name
filename   = paste0(currentfolder, "Serv/", fileprefix, "/", fileprefix, fileext)

#-------------------------------------------------------------------------------
# Load csv source
#-------------------------------------------------------------------------------
if (file.exists(filename))
{
  if (fileext == ".txt")
  {
    pmdf  = read.csv(filename, header = T, sep = ",")
  }else if (fileext == ".bin")
  {
    names = c("label", "x", "y", "z", "px", "py", "pz",
      "s1", "s2", "s3", "s4", "t", "dHz", "dHw",
      "dHz.dHw", "pmap.dHv", "ePm", "number")
    pmdf = dffbinary(filename, 18, names);
  }else{
    #Send an error if the file extension is unknown
    stop('The file extension is unknown (not .txt nor .bin)')
  }
}else
{
  pmdf = data.frame()
}

#-------------------------------------------------------------------------------
#New fileprefix to avoid "." (dot) in outputs
#-------------------------------------------------------------------------------
filename = paste0(currentfolder, "Serv/", fileprefix, "/", fileprefixnodots)

#-------------------------------------------------------------------------------
# Postprocessing
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# new columns
#-------------------------------------------------------------------------------
# From NC to EM units
pmdf = NCtoSYS(pmdf, gamma, c1)
# From EM to physical units
pmdf = SYStoPH(pmdf, L)
# Radii from Li
pmdf$rNC = sqrt(pmdf$x ^ 2 + pmdf$y ^ 2 + pmdf$z ^ 2)
pmdf$rPH = sqrt(pmdf$xPH ^ 2 + pmdf$yPH ^ 2 + pmdf$zPH ^ 2)
#Parity of event
pmdf$parity = pmdf$number %% 2
#log(ePm)
pmdf$log10ePm = log10(pmdf$ePm)

#-------------------------------------------------------------------------------
#Starting points
#-------------------------------------------------------------------------------
pmdf0 = pmdf[which(pmdf$number == 0),]

#-------------------------------------------------------------------------------
# Selecting only values
# for which pz > 0! (true Poincaré map)
#-------------------------------------------------------------------------------
pmdf = pmdf[which(pmdf$pz >= 0),]

#-------------------------------------------------------------------------------
#Find the halo orbits
#-------------------------------------------------------------------------------
if(Li == "L1")
{
  pmdfhaloindices = pmdf[which(abs(pmdf$yEM) > 0.06),]
}else{
  pmdfhaloindices = pmdf[which(abs(pmdf$yEM) > 0.0047),]
}

#Halo orbit isolated
pmdfhalo   = pmdf[which(pmdf$label %in% pmdfhaloindices$label),]

#Initial conditions
pmdfhalo0   = pmdfhalo[which(pmdfhalo$number == 0),]

#Just the first one
#pmdfhalo1  = pmdfhalo[which(pmdfhalo$label == min(pmdfhalo$label)),]
plotdf_point(pmdfhalo, "xEM", "yEM", "X [-]", "Y [-]", "label", "label", 1, pointSize = 2)

#-------------------------------------------------------------------------------
# Get rid of too big values of energy
# !! NEEDS IMPROVEMENT IN ORIGINAL C++ CODE !!
#-------------------------------------------------------------------------------
pmdf = pmdf[which(pmdf$dHw < 1.0),]

#-------------------------------------------------------------------------------
# Selecting particular values
#-------------------------------------------------------------------------------
#Maximum/Minimum time
maxnumber = max(pmdf$number)
minnumber = min(pmdf$number)

#-------------------------------------------------------------------------------
#Endpoints
#-------------------------------------------------------------------------------
pmdf1 = pmdf[which(pmdf$number == maxnumber),]


#-------------------------------------------------------------------------------
# Subselection
#
# We want to perform a subselection in the data so that the plots do not look 
# too crowded.
#
#-------------------------------------------------------------------------------
pmdf.mean  = ddply(pmdf, .(label), summarize, x = mean(x), y = mean(y), rNC = mean(rNC))
vecc   = pmdf.mean$rNC

if(!exists("clean.n.max"))
{
  clean.n.max = 31
}
clean.n = min(clean.n.max, length(unique(pmdf.mean$label)))

# We get a sequence of acc from min to max
clean.acc.min = min(vecc)
clean.acc.max = max(vecc)
clean.acc.v   = seq(clean.acc.min, clean.acc.max, length.out = clean.n)

# We get a sequence of labels that match the sequence of acc
pmdf.mean.min = pmdf.mean[which(vecc == min(vecc)),]
clean.acc.sub  = pmdf.mean$label[1] 
pmdf.mean.min = pmdf.mean[which(vecc == max(vecc)),]
clean.acc.sub  = c(clean.acc.sub, pmdf.mean.min$label[1] )
for(acc in clean.acc.v)
{
  pmdf.mean.min  = pmdf.mean[which(abs(vecc-acc) == min(abs(vecc-acc))),]
  clean.acc.sub  = c(clean.acc.sub, pmdf.mean.min$label[1] )
}

#We make the subselection
if(Li == "L2")
{
  condition = (pmdf$label %in% clean.acc.sub | pmdf$label %in% pmdfhaloindices$label) & !(pmdf$label %in% pmdfdiv$label)
}else{
  condition = (pmdf$label %in% clean.acc.sub   | pmdf$label %in% pmdfhaloindices$label)
}
pmdf = pmdf[which(condition),]

#-------------------------------------------------------------------------------
# Only a certain range of precision:
#-------------------------------------------------------------------------------
desiredPrec     = -6      #careful, if changed, pEM is obsolete!
desiredHardPrec = -0      #all kept solutions will have this precions

#-------------------------------------------------------------------------------
# pmdf_lab contains all complete solutions which the precision
# is better than desiredPrec  (for all points of the solution)
#-------------------------------------------------------------------------------
precFlag = pmdf$log10ePm < desiredHardPrec
wrongLabels = pmdf[which(!precFlag),]$label

#-------------------------------------------------------------------------------
# Make a difference between desiredPrec &  desiredHardPrec in pmdf_lab
#-------------------------------------------------------------------------------
pmdf_lab = pmdf[which(!(pmdf$label %in% wrongLabels)),]
precFlag = pmdf_lab$log10ePm < desiredPrec
pmdf_lab$precFlag = precFlag


#-------------------------------------------------------------------------------
# Plots
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Labels
#-------------------------------------------------------------------------------
if(!exists("pEM_lab_x"))
{
  pEM_lab_x = expression(italic(X))
  pEM_lab_y = expression(italic(Y))
}


#-------------------------------------------------------------------------------
#Lab: vs ePm
#-------------------------------------------------------------------------------
pEM = plotdf_point(pmdf_lab, "xEM", "yEM", pEM_lab_x, pEM_lab_y, "precFlag", expression(e[P](t) < 10 ^{-6}),1, pointSize = 0.3)
#Colors
pEM = pEM + scale_colour_manual(values = c("#000000", "#279B61"), guide = FALSE);#, "#CC6666", "#9999CC"), guide = FALSE) #to get the proper order!
#Theme
pEM = pEM + custom_theme
#Limits if necessary
if (pEM_limits)
{
  pEM = pEM + scale_x_continuous(limits = pEM_limits_x)
  pEM = pEM + scale_y_continuous(limits = pEM_limits_y)
}
#Change font
# pEM = pEM+theme(text=element_text(family="Times"), plot.background = element_rect(fill = "transparent",colour = NA))
#Display
pEM
#Save
ggsave(pEM, width = xSize, height = xSize,  bg = "transparent",  file = paste0(filename, "_EM.png")) #Save in png
ggsave(pEM, width = xSize, height = xSize,  bg = "transparent",  file = paste0(filename, "_EM.pdf")) #Save in pdf



#-------------------------------------------------------------------------------
# EM with prescribed precision
#-------------------------------------------------------------------------------
pEMprec = plotdf_point(pmdf_lab, "xEM", "yEM", pEM_lab_x, pEM_lab_y, pointSize = 1)
#Theme
pEMprec = pEMprec +big_font_theme
#Display
pEMprec

#-------------------------------------------------------------------------------
#Lab: labels
#-------------------------------------------------------------------------------
pLab = plotdf_point(pmdf_lab, "xEM", "yEM", pEM_lab_x, pEM_lab_y, "label", "label", 1, pointSize = 0.3)
#Colors
pLab = pLab + scale_colour_discrete(guide = FALSE)
#Theme
pLab = pLab + custom_theme
#Change font
# pLab = pLab+theme(text=element_text(family="Times"), plot.background = element_rect(fill = "transparent",colour = NA))
#Display
pLab
#Save
ggsave(pLab, width = xSize, height = xSize,  bg = "transparent",  file = paste0(filename, "_Labels.png")) #Save png
ggsave(pLab, width = xSize, height = xSize,  bg = "transparent",  file = paste0(filename, "_Labels.pdf")) #Save in pdf

#-------------------------------------------------------------------------------
#Lab: labels (tex version)
#-------------------------------------------------------------------------------
pTeX = plotdf_point(pmdf_lab, "xEM", "yEM", "$X$ $[$-$]$", "$Y$ $[$-$]$", "label", "label", 1, pointSize = 1)
#Colors
pTeX = pTeX + scale_colour_discrete(guide = FALSE)
#Theme
pTeX = pTeX + custom_theme#big_font_theme
#Display
pTeX
#Save
#ggplot2tikz(pTeX, width = xSize, height = ySize, file = paste0(filename, "_tex.tex")) #Save in tex

#-------------------------------------------------------------------------------
#Time between each projection
#-------------------------------------------------------------------------------
pmdf_proj = ddply(pmdf, .(label), summarize, projTime = mean(diff(t, lag = 1)), x = mean(abs(x)), y = mean(abs(y)))
pProj = plotdf_point(pmdf_proj, "x", "y", "x", "y", "projTime", "projTime", 0, pointSize = 3)
if(Li=="L2")
{
  pProj = pProj + scale_colour_gradient2("projTime", space="Lab", midpoint = 1.74, mid = "white", high = muted("blue"))
}else{
  pProj = pProj + scale_colour_gradient2("projTime", space="Lab", midpoint = 2.76, mid = "white", high = muted("blue"))
}
pProj

pProjX = plotdf_point(pmdf_proj, "x", "projTime", "x", "projTime", pointSize = 1)
pProjX = pProjX + scale_y_continuous(limits=c(1.3,2))
pProjX

pProjY = plotdf_point(pmdf_proj, "y", "projTime", "y", "projTime", pointSize = 1)
pProjY = pProjY + scale_y_continuous(limits=c(1.3,2))
pProjY
stop()

#-------------------------------------------------------------------------------
# PH coordinates
#-------------------------------------------------------------------------------
#Global
pPH = plotdf_point(pmdf_lab, "xPH", "yPH", "x [km]", "y [km]", "log10ePm", "ePm", 0, pointSize = 1)
#Add the moon
primaryR    =  1737.10  #Moon's radius
primaryPos  =  (muR - 1) * L  #Moon position wrt to Li
pPH = addMoon(
  pPH, x = primaryPos, y = 0, primaryR, surfSize = 0.4, cratSize = 0.2
)
pPH = pPH + annotate(
  "text", x =  primaryPos - 500, y = 4000, label = "Moon", size = 10
)
#Theme
pPH = pPH + big_font_theme + coord_fixed(ratio = 1) + scale_color_continuous(guide = FALSE)
#Display
pPH
#Save
ggsave(
  pPH, width = plotW, height = plotH, file = paste0(filename, "_Moon.eps")
) #Save


#-------------------------------------------------------------------------------
# RCM coordinates
#-------------------------------------------------------------------------------
pRCM = plotdf_point(pmdf, "s1", "s3", "s1", "s3", pointSize = 2)
#Theme
pRCM = pRCM + big_font_theme
pRCM
#Display
pRCM + scale_y_continuous(breaks = seq(-40,40,10))
pRCM + scale_y_continuous(breaks = seq(-2,2,0.1)) + scale_x_continuous(breaks = seq(-2,2,0.1))

#-------------------------------------------------------------------------------
# A particular solution
#-------------------------------------------------------------------------------
pmdf_sol = pmdf[which]
