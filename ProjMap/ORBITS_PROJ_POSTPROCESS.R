################################################################################
#
# Postprocess of projection data for the specific case of single orbit projection.
# Example of data file: projcu_order_20_dest_L2_Orbit_10_40.bin
#
# Once loaded via MAIN.R and PROJMAP.R, the data are stored in the dataframe 
# called proj_map_source which can be postprocessed via the current file.
#
# Once this file is loaded, as well as ORBITS_PROJ_TRAJ_LOAD.R 
# (to get refined trajectories), the ORBIT_PROJ_PLOT.R can be used.
#
# BLB 2017
#
################################################################################

#===== Checks ==================================================================
if(!("t0_CMU_EM_seed" %in% colnames(proj_map_source)))
{
  warn.mess = paste0("t0_CMU_EM_seed is not a column in proj_map_source,", 
                     " which means that the current data was not computed",
                     "using the new implementation.")
  warning(warn.mess)
  
}
  
#===== Build t0_CMU_EM_seed_T ==================================================
proj_map_source$t0_CMU_EM_seed_T = proj_map_source$t0_CMU_EM_seed/CST_SEM_PERIOD_EM

unique_t0_CMU_EM_seed   = unique(proj_map_source$t0_CMU_EM_seed) 
unique_t0_CMU_EM_seed_T = unique(proj_map_source$t0_CMU_EM_seed_T)
unique_t0_CMU_EM_seed_T

#===== Select some values ======================================================
#
# Here, we give some condition in order to select a certain part of the data
# in proj_map_source
#
# Output: proj_map_orbit


# condition =  proj_map_source$t0_CMU_EM_seed_T >= 0.5 & 
#              proj_map_source$t0_CMU_EM_seed_T <= 1.0 &
#              proj_map_source$s1_CMU_EM_seed == 10
condition =  proj_map_source$s1_CMU_EM_seed == 30   

# Select the value that match with the condition, and order the data wrt t0
proj_map_orbit = proj_map_source[which(condition),]
proj_map_orbit = proj_map_orbit[order(proj_map_orbit$t0_CMU_EM),]

#===== Under a given precision =================================================
#
# Output: proj_map_prec

condition = proj_map_orbit$pmin_dist_SEM < 3e-4 & 
            proj_map_orbit$crossings == 2 & 
            proj_map_orbit$collision == 0
proj_map_prec = proj_map_orbit[which(condition),]
sort(unique(proj_map_prec$t0_CMU_EM_seed_T))

#===== Select the labels for which we have a connection ========================
#
# Output: proj_map_prec_all, proj_map_prec_first

connec_labels     = unique(proj_map_prec$label)
condition         = proj_map_orbit$label %in% connec_labels
proj_map_prec_all = proj_map_orbit[which(condition),]
proj_map_prec_all = proj_map_prec_all[order(proj_map_prec_all$t0_CMU_EM),]

#-------- Select the points for which  t0_CMU_EM == min(t0_CMU_EM) -------------
compute_first_point_r0 <- function(df)
{
  summarize(df,
            x0_CMU_NCEM  = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$x0_CMU_NCEM,
            y0_CMU_NCEM  = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$y0_CMU_NCEM,
            z0_CMU_NCEM  = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$z0_CMU_NCEM,
            
            x0_CMU_SI_NCEM  = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$x0_CMU_SI_NCEM,
            y0_CMU_SI_NCEM  = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$y0_CMU_SI_NCEM,
            z0_CMU_SI_NCEM  = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$z0_CMU_SI_NCEM,
            
            
            s1_CMU_EM    = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$s1_CMU_EM,
            s3_CMU_EM    = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$s3_CMU_EM,
            t0_CMU_EM_seed_T      = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$t0_CMU_EM_seed_T,
            r0_CMU_EMT   = df[which(df$pmin_dist_SEM == min(df$pmin_dist_SEM)),]$r0_CMU_EMT
  )
}
proj_map_prec_first  = ddply(proj_map_prec_all, .(label), compute_first_point_r0)

#===== r0 at the connections ===================================================
connect = unique(proj_map_prec$r0_CMU_EMT)
connect[order(connect)]

r0vec = unique(proj_map_orbit$r0_CMU_EMT)
r0vec[order(r0vec)]

#===== Comparison between proj & cont ==========================================
length(unique(proj_map_prec$label))
length(unique(proj_cont$label))