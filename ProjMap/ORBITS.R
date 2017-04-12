################################################################################
#
# Plotting single orbits. Requires MAIN.R
#
# BLB 2017
#
################################################################################

#-------------------------------------------------------------------------------
# Initial conditions
#-------------------------------------------------------------------------------
sl = 20;
s0 = c(6, 0, 27, 0)

# s1c = c(6, -36, 6.01935, 6.8824, 7.32741)
# s3c = c(12, 24,-30.4489, -17.3909, -10.0765)

s1c = c(7.32741, 2.78868)
s3c = c(-10.0765, 15.3361)
s2c = rep(0, length(s1c))
s4c = rep(0, length(s1c))

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
    
    #Rbind
    orbits_eml2_strob = rbind(orbits_eml2_strob, orbit_eml2)
  }
}

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


