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
#
# For Qhalo: 
#
# to save: save(proj_map_source_Qhalo, file = "proj_map_source_Qhalo.Rda")
# to load: 1. load(file = "Rda/proj_map_source_Qhalo.Rda")
#          2. proj_map_source = proj_map_source_Qhalo
#
################################################################################

# Subroutines
source("ProjMap/util/SUBROUTINES.R")

#===== Checks ==================================================================
if(!("t0_CMU_EM_seed" %in% colnames(proj_map_source)))
{
  warn.mess = paste0("t0_CMU_EM_seed is not a column in proj_map_source,", 
                     " which means that the current data was not computed",
                     " using the new implementation.")
  warning(warn.mess)
  
}

#===== Do we which to impose a condition? ======================================
is.condition = F

#===== Build t0_CMU_EM_seed_T ==================================================
proj_map_source$t0_CMU_EM_seed_T = proj_map_source$t0_CMU_EM_seed/CST_SEM_PERIOD_EM


#===== Small postprocess to get other coordinates ==============================
proj_map_source$xe_CMS_SEM = -CST_GAMMA_LIB_SEM*(proj_map_source$xe_CMS_NCSEM - CST_C1_LIB_SEM)
proj_map_source$ye_CMS_SEM = -CST_GAMMA_LIB_SEM*(proj_map_source$ye_CMS_NCSEM - 0)
proj_map_source$ze_CMS_SEM = +CST_GAMMA_LIB_SEM*(proj_map_source$ze_CMS_NCSEM - 0)

proj_map_source$xe_CMS_EM = -CST_GAMMA_LIB_EM*(proj_map_source$xe_CMS_NCEM - CST_C1_LIB_EM)
proj_map_source$ye_CMS_EM = -CST_GAMMA_LIB_EM*(proj_map_source$ye_CMS_NCEM - 0)
proj_map_source$ze_CMS_EM = +CST_GAMMA_LIB_EM*(proj_map_source$ze_CMS_NCEM - 0)

# Careful, the following line is FALSE but useful
proj_map_source$pxe_CMS_EM = -CST_GAMMA_LIB_EM*proj_map_source$pxe_CMS_NCEM

proj_map_source$x0_CMU_EM = -CST_GAMMA_LIB_EM*(proj_map_source$x0_CMU_NCEM - CST_C1_LIB_EM)
proj_map_source$y0_CMU_EM = -CST_GAMMA_LIB_EM*(proj_map_source$y0_CMU_NCEM - 0)
proj_map_source$z0_CMU_EM = +CST_GAMMA_LIB_EM*(proj_map_source$z0_CMU_NCEM - 0)

#===== Select some values ======================================================
#
# Here, we give some condition in order to select a certain part of the data
# in proj_map_source
#
# Output: proj_map_orbit

# condition =  proj_map_source$t0_CMU_EM_seed_T >= 0.5 & 
#              proj_map_source$t0_CMU_EM_seed_T <= 1.0 &
#              proj_map_source$s1_CMU_EM_seed == 10

# Select the value that match with the condition, and order the data wrt t0
if(is.condition)
{
  condition =  proj_map_source$s1_CMU_EM_seed == 20 
  proj_map_orbit = proj_map_source[which(condition),]
}else{
  proj_map_orbit = proj_map_source
}

#Order in terms of t0_CMU_EM
proj_map_orbit = proj_map_orbit[order(proj_map_orbit$t0_CMU_EM),]

#===== Under a given precision =================================================

# Output: proj_map_prec
# Condition is: small projection distance + good crossing properties + no collision
condition = proj_map_orbit$pmin_dist_SEM < 5e-4 & #proj_map_orbit$tof_EM > 5.3 &
            proj_map_orbit$crossings == 2 & 
            proj_map_orbit$collision == 0

condition = proj_map_orbit$pmin_dist_SEM < 5e-4

proj_map_prec = proj_map_orbit[which(condition),]

sort(unique(proj_map_prec$t0_CMU_EM))

#==== To get r0_CMU_EMT between [0.5, 1.5] =====================================
proj_map_prec$r0_CMU_EMT_mod = r0_modulo(proj_map_prec$r0_CMU_EMT)


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
            
            label.conn  = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$label.conn,
            
            
            s1_CMU_EM             = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$s1_CMU_EM,
            s3_CMU_EM             = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$s3_CMU_EM,
            t0_CMU_EM_seed_T      = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$t0_CMU_EM_seed_T,
            t0_CMU_EM_T           = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$t0_CMU_EM/CST_SEM_PERIOD_EM,
            
            r0_CMU_EMT            = df[which(df$pmin_dist_SEM == min(df$pmin_dist_SEM)),]$r0_CMU_EMT
  )
}
proj_map_prec_first  = ddply(proj_map_prec_all, .(label), compute_first_point_r0)

compute_all_first_point <- function(df)
{
  summarize(df,
            x0_CMU_NCEM  = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$x0_CMU_NCEM,
            y0_CMU_NCEM  = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$y0_CMU_NCEM,
            z0_CMU_NCEM  = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$z0_CMU_NCEM,
            
            x0_CMU_EM  = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$x0_CMU_EM,
            y0_CMU_EM  = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$y0_CMU_EM,
            z0_CMU_EM  = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$z0_CMU_EM,
            
            s1_CMU_EM             = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$s1_CMU_EM,
            s3_CMU_EM             = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$s3_CMU_EM,
            t0_CMU_EM_seed_T      = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$t0_CMU_EM_seed_T,
            t0_CMU_EM_T           = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$t0_CMU_EM/CST_SEM_PERIOD_EM,
            r0_CMU_EMT            = df[which(df$t0_CMU_EM == min(df$t0_CMU_EM)),]$r0_CMU_EMT
  )
}
proj_map_all_first   = ddply(proj_map_orbit, .(label), compute_all_first_point) 


#===== r0 at the connections ===================================================
connect = unique(proj_map_prec$r0_CMU_EMT)
connect[order(connect)]

r0vec = unique(proj_map_orbit$r0_CMU_EMT)
r0vec[order(r0vec)]

#===== t0 seed at the connections ==============================================
unique_t0_CMU_EM_seed   = unique(proj_map_source$t0_CMU_EM_seed) 
unique_t0_CMU_EM_seed_T = unique(proj_map_source$t0_CMU_EM_seed_T)
unique_t0_CMU_EM_seed_T
