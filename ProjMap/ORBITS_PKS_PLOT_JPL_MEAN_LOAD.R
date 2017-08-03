################################################################################
#
# Projection from single orbits. Requires MAIN.R, ORBITS_PROJ_POSTPROCESS.R, 
# and ORBITS_PROJ_TRAJ_LOAD.R (to get refined trajectories)
#
# Difference with ORBITS_PROJ_PLOT.R : the phase is taken on a given pk section 
# rather than at the departure point.
#
# This file focuses on the JPL transitioning results and on average estimates
# computed from these results
#
# BLB 2017
#
################################################################################

#=====  Config  ================================================================
source("ProjMap/ORBITS_PKS_PLOT_CONFIG.R")


#=============================================================================
# DDPLY - Careful, quite heavy !!
#
# Moreover, the comparison between the QBCP and the JPL result does not work 
# perfecly if the time has been left free in the JPL refinement...
#=============================================================================
compute_diff <- function(df)
{
  # Selecting the QBCP and JPL coordinates
  df0  = df[which(df$coord == 0),]
  df13 = df[which(df$coord == 13),]
  
  # Selecting only the orbit at EMLT
  df0a = df0[which(df0$t_SEM <= df0$t0_CMU_SEM[1]),]
  df13a = df13[1:length(df0a$t_SEM),]
  
  
  # Selecting only the orbit at SEMLT
  df0b = df0[which(df0$t_SEM >= df0$tf_man_SEM[1]),]
  df13b = df13[(length(df13$t_SEM)-length(df0b$t_SEM)+1):length(df13$t_SEM),]
  
  # Print
  #print(length(df0a$x_NCEM))
  #print(length(df13a$x_NCEM))
  #print(length(df0b$x_NCSEM))
  #print(length(df13b$x_NCSEM))
  
  # Merge the date for difference @EMLT
  dff = data.frame(dx_NCEM  = df0a$x_NCEM - df13a$x_NCEM, # x difference @EMLT
                   dy_NCEM  = df0a$y_NCEM - df13a$y_NCEM, # y difference @EMLT 
                   dz_NCEM  = df0a$z_NCEM - df13a$z_NCEM, # z difference @EMLT
                   x_NCEM   = df0a$x_NCEM,  # x in QBCP @EMLT
                   y_NCEM   = df0a$y_NCEM,  # y in QBCP @EMLT
                   z_NCEM   = df0a$z_NCEM)  # z in QBCP @EMLT
  
  # Merge the date for difference @SEMLT
  dff_SEM = data.frame(dx_NCSEM = df0b$x_NCSEM - df13b$x_NCSEM,   # x difference @SEMLT
                       dy_NCSEM = df0b$y_NCSEM - df13b$y_NCSEM,   # y difference @SEMLT 
                       dz_NCSEM = df0b$z_NCSEM - df13b$z_NCSEM,   # z difference @SEMLT
                       x_NCSEM  = df0b$x_NCSEM,  # x in QBCP @SEMLT
                       y_NCSEM  = df0b$y_NCSEM,  # y in QBCP @SEMLT
                       z_NCSEM  = df0b$z_NCSEM)  # z in QBCP @SEMLT
  
  # We should add the FINAL ENERGY from traj_cont, to see if the loss of the amplitude 
  # is linked to the fact that big orbits are hard to describe at SEMLT. 
  
  # Compute the norms @EMLT
  dff$dn_NCEM   = apply(X = dff[,1:3], MARGIN = 1, FUN = norm, '2')  #norm of the difference
  dff$dnxy_NCEM = apply(X = dff[,1:2], MARGIN = 1, FUN = norm, '2')  #norm of the difference only along x & y
  dff$n_NCEM    = apply(X = dff[,4:6], MARGIN = 1, FUN = norm, '2')  #norm in QBCP
  
  
  # Compute the norms @SEMLT
  dff_SEM$dn_NCSEM   = apply(X = dff_SEM[,1:3], MARGIN = 1, FUN = norm, '2')  #norm of the difference
  dff_SEM$dnxy_NCSEM = apply(X = dff_SEM[,1:2], MARGIN = 1, FUN = norm, '2')  #norm of the difference only along x & y
  dff_SEM$n_NCSEM    = apply(X = dff_SEM[,4:6], MARGIN = 1, FUN = norm, '2')  #norm in QBCP
  
  
  # Take the average in new data frame
  summarize(df, mean_dn_NCEM = mean(dff$dn_NCEM), 
                mean_dnxy_NCEM = mean(dff$dnxy_NCEM), 
                mean_n_NCEM = mean(dff$n_NCEM), 
                mean_dn_NCSEM = mean(dff_SEM$dn_NCSEM), 
                mean_dnxy_NCSEM = mean(dff_SEM$dnxy_NCSEM), 
                mean_n_NCSEM = mean(dff_SEM$n_NCSEM),
                r0_CMU_EMT = df0a$r0_CMU_EMT[1], 
                r0_CMU_EMT_mod = df0a$r0_CMU_EMT_mod[1], 
                re_CMU_EMT = df0a$re_CMU_EMT[1], 
                re_CMU_EMT_mod = df0a$re_CMU_EMT_mod[1], 
                dHf_SEM = df0a$dHf_SEM[1],
                pmin_dist_SEM = df0a$pmin_dist_SEM[1])
}

#ddply - long!
traj_from_jpl_mean = ddply(traj_from_jpl, .(label), compute_diff)

# For physical units
traj_from_jpl_mean$mean_dn_PHEM = traj_from_jpl_mean$mean_dn_NCEM*CST_DIST_PRIM_EM*CST_GAMMA_LIB_EM
traj_from_jpl_mean$mean_dnxy_PHEM = traj_from_jpl_mean$mean_dnxy_NCEM*CST_DIST_PRIM_EM*CST_GAMMA_LIB_EM
traj_from_jpl_mean$mean_dn_PHSEM = traj_from_jpl_mean$mean_dn_NCSEM*CST_DIST_PRIM_SEM*CST_GAMMA_LIB_SEM
traj_from_jpl_mean$mean_dnxy_PHSEM = traj_from_jpl_mean$mean_dnxy_NCSEM*CST_DIST_PRIM_SEM*CST_GAMMA_LIB_SEM

#-------------------------------------------------------------------------------
# A bit of test
#-------------------------------------------------------------------------------
mean_n_NCEM.sd      = sd(traj_from_jpl_mean$mean_n_NCEM)
mean_n_NCEM.sdmin   = min(traj_from_jpl_mean$mean_n_NCEM) - mean(traj_from_jpl_mean$mean_n_NCEM)
mean_n_NCEM.sdmax   = max(traj_from_jpl_mean$mean_n_NCEM) - mean(traj_from_jpl_mean$mean_n_NCEM)
mean_n_NCEM.mean    = mean(traj_from_jpl_mean$mean_n_NCEM)

mean_n_NCEM.sd/mean_n_NCEM.mean*100
mean_n_NCEM.sdmin/mean_n_NCEM.mean*100
mean_n_NCEM.sdmax/mean_n_NCEM.mean*100

#-------------------------------------------------------------------------------
# Saving in R format
#-------------------------------------------------------------------------------
traj_from_jpl_mean_Liss_s1_10_s2_5_TSPAN_SEM_40= traj_from_jpl_mean
save(traj_from_jpl_mean_Liss_s1_10_s2_5_TSPAN_SEM_40, file = "Rda/traj_from_jpl_mean_Liss_s1_10_s2_5_TSPAN_SEM_40.Rda")


