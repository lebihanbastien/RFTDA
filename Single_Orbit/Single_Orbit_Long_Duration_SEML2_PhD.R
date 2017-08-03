################################################################################
#
# Plotting single orbits. Use for PhD manuscript
#
# BLB 2017
#
################################################################################

#-------------------------------------------------------------------------------
# Init
#-------------------------------------------------------------------------------
source("source/init.R")

#-------------------------------------------------------------------------------
# Select Models & libration point
#-------------------------------------------------------------------------------
Li    = "L2"
MODEL = "QBCP"
FWRK  = "SEM"
r0    = 0;
order = 20
currentfolder = paste0(ftincppdafolder, "plot/", MODEL, "/", FWRK, "/", Li, "/");
line.size.small = 0.6
line.size.norm  = 0.8

#Colors
Greys = rev(brewer.pal(3,"Greys"))

#-------------------------------------------------------------------------------
# Normalized units (gamma_li, c1_li)
#-------------------------------------------------------------------------------
muR      = muR(FWRK);
gamma_li = gamma(Li, FWRK);
c1_li    = c1(Li, FWRK);
L        = Ldist(FWRK);
Period = ifelse(MODEL=="QBCP", SEMperiod(FWRK), 2*pi)

if(FWRK == "EM")
{
  primaryR    =  1737.10      #m2
}else{
  primaryR    =  6378.10      #m2 
}


#-------------------------------------------------------------------------------
# Initial conditions
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Planar case, used in CONT_MULTI.R, constant phase @ Pk section
#-------------------------------------------------------------------------------
s1c = c(0.31)
s3c = c(0.31)
s2c = rep(0, length(s1c))
s4c = rep(0, length(s1c))
filename = "orbit_lyap_long_duration"


#-------------------------------------------------------------------------------
# Lissajous case
#-------------------------------------------------------------------------------
# s1c = c(0.25)
# s2c = c(0.31)
# s3c = c(0.0)
# s4c = c(0.29)
# 
# filename = "orbit_liss_long_duration"


#-------------------------------------------------------------------------------
# QHalo case
#-------------------------------------------------------------------------------
# s1c = c(0.28)
# s2c = c(0.3)
# s3c = c(0.28)
# s4c = c(0.3)
# filename = "orbit_qhalo_long_duration"

#-------------------------------------------------------------------------------
# Set ss0
#-------------------------------------------------------------------------------
ss0 = data.frame(s1 = s1c, s2 = s2c, s3 = s3c, s4 = s4c)


#-------------------------------------------------------------------------------
# Read data: orbits stroboscopic maps
#-------------------------------------------------------------------------------
# Loop on data
orbits_eml2 = data.frame();
label = 0

for(k in seq(1,nrow(ss0)))
{
  s0 = c(ss0$s1[k], ss0$s2[k], ss0$s3[k], ss0$s4[k])
  fileorbit = paste0(currentfolder, "orbit_order_", order, "_s1_", s0[1],  "_s2_", s0[2], 
                     "_s3_", s0[3],  "_s4_", s0[4], "_t0_", r0, ".txt");
  
  if (file.exists(fileorbit))
  {
    #Read table
    orbit_eml2 = read.table(file = fileorbit, header = TRUE)
    
    # Label
    orbit_eml2$label = label
    label = label +1
    
    # Energy difference
    orbit_eml2$dH_EM = orbit_eml2$H_EM - orbit_eml2$H0_EM 
    
    #Order
    orbit_eml2 = orbit_eml2[order(orbit_eml2$t_NCEM),]
    
    #Rbind
    orbits_eml2 = rbind(orbits_eml2, orbit_eml2)
  }
}

#-------------------------------------------------------------------------------
# Post-processing on coordinates and units
#-------------------------------------------------------------------------------
orbits_eml2$x =  orbits_eml2$x_CM_NCEM
orbits_eml2$y =  orbits_eml2$y_CM_NCEM
orbits_eml2$z =  orbits_eml2$z_CM_NCEM

# From NC coordinates to C coordinates
orbits_eml2    = NCtoC(orbits_eml2, gamma_li)
orbits_eml2    = CtoPH(orbits_eml2, L)
orbits_eml2    = NCtoSYS(orbits_eml2, gamma_li, c1_li)
orbits_eml2    = SYStoPH(orbits_eml2, L)


#===============================================================================
# PLOTS: only the orbits
#===============================================================================


#--------------------- xy -----------------------------------------------------#
pxy = plotdf_path(orbits_eml2,  "xEM", "yEM",  "\\textit{X}",  "\\textit{Y}", lineSize = line.size.small)

# Ratio and scaling
pxy = pxy + coord_fixed(ratio=1)
pxy = pxy + scale_x_continuous(limits = c(-1.013, NaN))
pxy = pxy + scale_y_continuous(limits = c(-0.01, 0.01))

#Add m2
primaryPos  =  -(1-muR)
pxy = addPrimary(pxy, primaryPos, 0, 10*primaryR/L, 0.4, muted("blue"))

#Add the name of the Earth
pxy = pxy + annotate("text", x = primaryPos-0.001, y = 0.001,  label = "Earth", size=7)
pxy

# Inset
# spxy = pxy + blank_theme
# spxy = spxy + scale_x_continuous(limits = c(-1.19, -1.18))
# spxy = spxy + scale_y_continuous(limits = c(-0.02, 0.02)) 
# vp <- viewport(width = 0.35, height = 0.35, x = 0.8, y = 0.12, just = c("right","bottom"))
# print(pxy)
# print(spxy, vp = vp)


#--------------------- sz -----------------------------------------------------#
pxz = plotdf_path(orbits_eml2,  "xEM", "zEM",  "\\textit{X}",  "\\textit{Z}", lineSize = line.size.small)
# Ratio and scaling
pxz = pxz + coord_fixed(ratio=1)
pxz = pxz + scale_x_continuous(limits = c(-1.013, NaN))
pxz = pxz + scale_y_continuous(limits = c(-0.01, 0.01))

#Add m2
primaryPos  =  -(1-muR)
pxz = addPrimary(pxz, primaryPos, 0, 10*primaryR/L, 0.4, muted("blue"))

#Add the name of the Earth
pxz = pxz + annotate("text", x = primaryPos-0.001, y = 0.001,  label = "Earth", size=7)
pxz

#--------------------- yz -----------------------------------------------------#
pyz = plotdf_path(orbits_eml2,  "yEM", "zEM",  "\\textit{Y}",  "\\textit{Z}", lineSize = line.size.norm)
#pyz = pyz + coord_fixed(ratio=1)
pyz

ps1s3 = plotdf_path(orbits_eml2,  "s1", "s3",  "$s_1$",  "$s_3$", lineSize = line.size)
ps1s3

ps2s4 = plotdf_path(orbits_eml2,  "s2", "s4",  "$s_2$",  "$s_4$", lineSize = line.size)
ps2s4

#===============================================================================
# PLOTS: only the energy
#===============================================================================
clabels = c("0.0", "20$T$", "40$T$", "60$T$", "80$T$", "100$T$")
cbreaks = seq(0.0, 100*Period, 20*Period)

pdH = plotdf_path(orbits_eml2,  "t_NCEM", "dH_EM",  "$t$",  "$\\delta H$", lineSize = line.size.small)
pdH = pdH + scale_x_continuous(breaks = cbreaks, labels = clabels) 
pdH


#===============================================================================
# PLOTS: only the orbits
#===============================================================================
ggplot2tikz_phd(pxy, xSize,  ySize, file = paste0(currentfolder, filename, "_xy.tex"))
ggplot2tikz_phd(pxz, xSize,  ySize, file = paste0(currentfolder, filename, "_xz.tex"))
ggplot2tikz_phd(pyz, xSize,  ySize, file = paste0(currentfolder, filename, "_yz.tex"))
ggplot2tikz_phd(pdH, xSize,  ySize, file = paste0(currentfolder, filename, "_dH.tex"))

