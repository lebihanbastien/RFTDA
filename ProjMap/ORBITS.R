################################################################################
#
# Plotting single orbits. Requires MAIN.R
#
# BLB 2017
#
################################################################################

# Subroutines
source("ProjMap/util/SUBROUTINES.R")

#-------------------------------------------------------------------------------
# Initial conditions
#-------------------------------------------------------------------------------
sl = 20;
s0 = c(6, 0, 27, 0)

#-------------------------------------------------------------------------------
# Planar case
#-------------------------------------------------------------------------------
s1c = c(6, -36, 6.01935, 6.8824, 7.32741)
s3c = c(12, 24,-30.4489, -17.3909, -10.0765)
#s1c = c(7.32741, 2.78868)
#s3c = c(-10.0765, 15.3361)
s2c = rep(0, length(s1c))
s4c = rep(0, length(s1c))

#-------------------------------------------------------------------------------
# Planar case, used in CONT_MULTI.R, constant energy
#-------------------------------------------------------------------------------
s1c = c(5.93431)
s3c = c(-33.7099)
s2c = rep(0, length(s1c))
s4c = rep(0, length(s1c))

#-------------------------------------------------------------------------------
# Planar case, used in CONT_MULTI.R, constant phase @ Pk section
#-------------------------------------------------------------------------------
s1c = c(6.99128)
s3c = c(-15.9007)
s2c = rep(0, length(s1c))
s4c = rep(0, length(s1c))

#-------------------------------------------------------------------------------
# QHalo case
#-------------------------------------------------------------------------------
# s1c = c(28)
# s2c = c(1.74347)
# s3c = c(36)
# s4c = c(1.74347)

ss0 = data.frame(s1 = s1c, s2 = s2c, s3 = s3c, s4 = s4c)


r0 = 0.96;

#-------------------------------------------------------------------------------
# Read data: orbits stroboscopic maps
#-------------------------------------------------------------------------------
# Filename
localfolder = "~/BackUpBox/PhD/FourierTaylorInCpp/plot/QBCP/EM/L2/"

# Loop on data
orbits_eml2 = data.frame();
label = 0

for(k in seq(1,nrow(ss0)))
{
  s0 = c(ss0$s1[k], ss0$s2[k], ss0$s3[k], ss0$s4[k])
  fileorbit = paste0(localfolder, "orbit_order_16_s1_", s0[1],  "_s2_", s0[2], 
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
# Read data: orbits stroboscopic maps
#-------------------------------------------------------------------------------
# Loop on data
orbits_eml2_strob = data.frame();
label = 0

for(k in seq(1,nrow(ss0)))
{
  s0 = c(ss0$s1[k], ss0$s2[k], ss0$s3[k], ss0$s4[k])
  fileorbit = paste0(localfolder, "orbit_order_16_s1_", s0[1],  "_s2_", s0[2], 
                     "_s3_", s0[3],  "_s4_", s0[4], "_t0_", r0, "_strob.txt");
  
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
    orbits_eml2_strob = rbind(orbits_eml2_strob, orbit_eml2)
  }
}

#===============================================================================
# PLOTS: only the orbits
#===============================================================================
lineSize = 0.2

pxy = plotdf_path(orbits_eml2,  "x_CM_NCEM", "y_CM_NCEM",  x_em,  y_em, "label", "Label", 1, lineSize = line.size)
pxy

pxz = plotdf_path(orbits_eml2,  "x_CM_NCEM", "z_CM_NCEM",  x_em,  z_em, "label", "Label", 1, lineSize = line.size)
pxz

pyz = plotdf_path(orbits_eml2,  "y_CM_NCEM", "z_CM_NCEM",  y_em,  z_em, "label", "Label", 1, lineSize = line.size)
pyz

ps1s3 = plotdf_path(orbits_eml2,  "s1", "s3",  s1_exp,  s3_exp, "label", "Label", 1, lineSize = line.size)
ps1s3

ps2s4 = plotdf_path(orbits_eml2,  "s2", "s4",  s2_exp,  s4_exp, "label", "Label", 1, lineSize = line.size)
ps2s4

#===============================================================================
# PLOTS: only the strob maps
#===============================================================================
# Variations of the energy
pstrop.dH = ggplot() + custom_theme
pstrop.dH = geom_point_pretty(pstrop.dH, orbits_eml2_strob, 
                                   aes(t_NCEM_T, dH_EM, color = label))
pstrop.dH = pstrop.dH + labs(x = expression(t~("%T")), y =  expression(H(t) - H[0](t)))
pstrop.dH

# Variations of s1/s3
pstrop.s1s3 = ggplot() + custom_theme
pstrop.s1s3 = geom_point_pretty(pstrop.s1s3, orbits_eml2_strob, 
                              aes(s1, s3, color = label))
pstrop.s1s3 = pstrop.s1s3 + labs(x = s1_exp, y =  s3_exp)
pstrop.s1s3


#===============================================================================
# PLOTS: superimposed on projection data
#===============================================================================

#-------------------------------------------------------------------------------
# Plot : tiles (pmin_dist_SEM) in the s1_CMU_EM/s3_CMU_EM space
#-------------------------------------------------------------------------------
pt_s1EM_s3EM_eP = plotdf_tile_1(proj_map_tem, "s1_CMU_EM", "s3_CMU_EM", 
                                s1_exp, s3_exp, "pmin_dist_SEM", "pmin_dist_SEM",
                                FALSE)
if(LIB_POINT_EM == "L2")
{
  pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + scale_x_continuous(breaks = seq(-42,42,6))
  pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + scale_y_continuous(breaks = seq(-42,42,6)) 
}else
{
  pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + scale_x_continuous(breaks = seq(-4,4,0.5))
  pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + scale_y_continuous(breaks = seq(-4,4,0.5)) 
}
pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + ggtitle_t0
pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + scg_pem

#Add the orbit
pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + geom_path(data = orbits_eml2, aes(s1,  s3, linetype = factor(label)), color = muted("black"), size = 0.2)
pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + geom_point(data = orbits_eml2_strob, aes(s1,  s3, type = factor(label)), color = muted("green"), size = 2)

#Display
pt_s1EM_s3EM_eP


#===============================================================================
# POINTS : pmin_dist_SEM in the x0_CMU_NCEM/y0_CMU_NCEM space
#===============================================================================
pp_x0EM_y0EM_eP = plotdf_point(proj_map_tem, "x0_CMU_NCEM", "y0_CMU_NCEM", "x0 (EML2)", "y0 (EML2)", "pmin_dist_SEM", "pmin_dist_SEM", 0, pointSize = 3)
pp_x0EM_y0EM_eP = pp_x0EM_y0EM_eP + ggtitle_t0
pp_x0EM_y0EM_eP = pp_x0EM_y0EM_eP + scg_pem
pp_x0EM_y0EM_eP

#Add the orbit
pp_x0EM_y0EM_eP = pp_x0EM_y0EM_eP + geom_path(data = orbits_eml2, aes(x_CM_NCEM,  y_CM_NCEM, linetype = factor(label)), color = muted("black"), size = 0.1)
pp_x0EM_y0EM_eP = pp_x0EM_y0EM_eP + geom_point(data = orbits_eml2_strob, aes(x_CM_NCEM,  y_CM_NCEM, type = factor(label)), color = muted("green"), size = 2)


#Display
pp_x0EM_y0EM_eP


