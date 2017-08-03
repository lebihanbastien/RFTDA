################################################################################
#
# Load trajectory from the refinement of projection data from single orbits. 
# Requires MAIN.R.
#
# Note: it is quite difficult to continue the solutions while keeping them close 
# to their first guesses (s1, s3) at EML2 in order to tackle this issue, 
# the problem is separated in two.  
# 
# - The data files that end with (e.g.) _Orbit_10_SINGLE contain 
# refined but NOT continued solutions. Hence, they are very close to their 
# original first guess and can be used to display the solutions from the EML2 
# point of view
#
# - The data files that end with (e.g.) _Orbit_10_CONT contain 
# refined AND continued solutions. Hence, they can be used to show beautiful 
# arrival at SEMLi
#
# All the current data (_Orbit_10,20,30,40_CONT) have been obtained by imposing 
# the initial projection distance to be below 3e-4 in NCSEM units.
# Note that, as expected, the 40 results are quite bad, because too close to 
# the end of the Dc at EML2
# 
# BLB 2017
#
################################################################################

# Do we want to reduce the number of solutions
few.solutions = F

#===============================================================================
# Get data from file
#===============================================================================
seeds = "_Liss_s1_20_s2_5_3T" #_QHalo_big" #"_Orbit_10" "_Liss_s1_10_s2_5_3T" "_Liss_s1_10_s2_5_3T"
tspan = "_TSPAN_SEM_10"
suffix_from_server = paste0(seeds, "_DIST_SEM_5e-4", tspan)#"_QHalo_CONT" #"_Orbit_40_CONT_LOOSE_eps_1e-5" #"_DIST_SEM_5e-4_TSPAN_SEM_10"

# Suffix
FILE_SUFFIX_CONT      = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = suffix_from_server ) #_Orbit_10_SINGLE/_Orbit_10_CONT/_QHalo_CONT
FILE_SUFFIX_CONT_TRAJ = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = suffix_from_server ) #_Orbit_10_SINGLE/_Orbit_10_CONT
FILE_SUFFIX_FROM_C    = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = suffix_from_server ) #_Orbit_10_SINGLE/_Orbit_10_CONT
FILE_SUFFIX_FROM_JPL  = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = suffix_from_server ) 

# Prefix
FILE_PREFIX_CONT      = paste0(ftincppdafolder, FILE_SUBFOLDER, "cont_atf_order_", ORDER, "_dest_", LIB_POINT_SEM)
FILE_PREFIX_CONT_TRAJ = paste0(ftincppdafolder, FILE_SUBFOLDER, "cont_atf_traj_order_", ORDER, "_dest_", LIB_POINT_SEM) 
FILE_PREFIX_FROM_C    = paste0(ftincppdafolder, FILE_SUBFOLDER, "traj_from_c_order_", ORDER, "_dest_", LIB_POINT_SEM) 
FILE_PREFIX_FROM_JPL  = paste0(ftincppdafolder, FILE_SUBFOLDER, "cont_jpl_order_", ORDER, "_dest_", LIB_POINT_SEM)

#-------------------------------------------------------------------------------
# Continuation: txt format
#-------------------------------------------------------------------------------
proj_cont = get_proj_cont(FILE_PREFIX_CONT, FILE_SUFFIX_CONT, FAMILY = "")

#-------------------------------------------------------------------------------
# Continuation trajectories: bin format
#-------------------------------------------------------------------------------
traj_cont = get_traj_cont(FILE_PREFIX_CONT_TRAJ, FILE_SUFFIX_CONT_TRAJ, FAMILY = "", PRINT = TRUE)

#-------------------------------------------------------------------------------
# Full trajectories: bin format
#-------------------------------------------------------------------------------
traj_from_c = get_traj_comp(FILE_PREFIX_FROM_C, FILE_SUFFIX_FROM_C, FAMILY = "", PRINT = TRUE)

#-------------------------------------------------------------------------------
# Full trajectories in JPL: bin format
#-------------------------------------------------------------------------------
traj_from_jpl = get_traj_comp(FILE_PREFIX_FROM_JPL, FILE_SUFFIX_FROM_JPL, FAMILY = "", PRINT = TRUE)



#===============================================================================
# Postprocess
#===============================================================================

#-------------------------------------------------------------------------------
# Select the right labels
#-------------------------------------------------------------------------------
connec_labels     = unique(proj_map_prec$label.conn)

condition         = traj_cont$label.conn %in% connec_labels
traj_cont         = traj_cont[which(condition),]

condition         = traj_from_c$label.conn %in% connec_labels
traj_from_c       = traj_from_c[which(condition),]

condition         = traj_from_jpl$label.conn %in% connec_labels
traj_from_jpl     = traj_from_jpl[which(condition),]


#-------------------------------------------------------------------------------
# Select every nseq labels if desired
#-------------------------------------------------------------------------------
if(few.solutions)
{
  # We select the labels that correspond to trajectories in proj_map_prec
  condition = proj_map_prec$label.conn %in% unique(traj_cont$label.conn)
  proj_map_min = proj_map_prec[which(condition),]
  
  # We get a sequence of dHf from min to max
  dHf_SEM_min = min(proj_map_min$dHf_SEM)
  dHf_SEM_max = max(proj_map_min$dHf_SEM)
  dHf_SEM_v   = seq(dHf_SEM_min, dHf_SEM_max, length.out = 11)

  # We get a sequence of labels that match the sequence of dHf
  proj_min = proj_map_min[which(proj_map_min$dHf_SEM == min(proj_map_min$dHf_SEM)),]
  connec_labels_sub  = proj_min$label.conn[1] 
  for(dHf in dHf_SEM_v)
  {
    proj_min  = proj_map_min[which(abs(proj_map_min$dHf_SEM-dHf) == min(abs(proj_map_min$dHf_SEM-dHf))),]
    connec_labels_sub  = c(connec_labels_sub, proj_min$label.conn[1] )
  }
  
  #We make the subselection
  condition         = traj_cont$label.conn %in% connec_labels_sub
  traj_cont         = traj_cont[which(condition),]
  
  condition         = traj_from_c$label.conn %in% connec_labels_sub
  traj_from_c       = traj_from_c[which(condition),]
  
  
  condition         = traj_from_jpl$label.conn %in% connec_labels_sub
  traj_from_jpl     = traj_from_jpl[which(condition),]
  
  # We update the tspan for the plots in png/pdf
  tspan = paste0(tspan, "_few")
}


#-------------------------------------------------------------------------------
# Small postprocess
#-------------------------------------------------------------------------------
proj_cont$re_CMU_EMT_mod = r0_modulo(proj_cont$re_CMU_EMT)

traj_from_c$x_SEM = -CST_GAMMA_LIB_SEM*(traj_from_c$x_NCSEM - CST_C1_LIB_SEM)
traj_from_c$y_SEM = -CST_GAMMA_LIB_SEM*(traj_from_c$y_NCSEM - 0)
traj_from_c$z_SEM = +CST_GAMMA_LIB_SEM*(traj_from_c$z_NCSEM - 0)

traj_from_c$x_EM = -CST_GAMMA_LIB_EM*(traj_from_c$x_NCEM - CST_C1_LIB_EM)
traj_from_c$y_EM = -CST_GAMMA_LIB_EM*(traj_from_c$y_NCEM - 0)
traj_from_c$z_EM = +CST_GAMMA_LIB_EM*(traj_from_c$z_NCEM - 0)

traj_from_jpl$x_SEM = -CST_GAMMA_LIB_SEM*(traj_from_jpl$x_NCSEM - CST_C1_LIB_SEM)
traj_from_jpl$y_SEM = -CST_GAMMA_LIB_SEM*(traj_from_jpl$y_NCSEM - 0)
traj_from_jpl$z_SEM = +CST_GAMMA_LIB_SEM*(traj_from_jpl$z_NCSEM - 0)

traj_from_jpl$x_EM = -CST_GAMMA_LIB_EM*(traj_from_jpl$x_NCEM - CST_C1_LIB_EM)
traj_from_jpl$y_EM = -CST_GAMMA_LIB_EM*(traj_from_jpl$y_NCEM - 0)
traj_from_jpl$z_EM = +CST_GAMMA_LIB_EM*(traj_from_jpl$z_NCEM - 0)

#-------------------------------------------------------------------------------
# Now, we cheat a little bit in traj_cont/traj_from_c: 
# We copy the value of re_CMU_EMT_mod and re_CMU_EMT
# In "original", and we copy the value from proj_map_prec
#-------------------------------------------------------------------------------

# The function to do exactly that
compute_re_CMU_EMT <- function(df)
{
  dff  = proj_map_prec[which(proj_map_prec$label.conn == df$label[1]),]
  
  mutate(df, re_CMU_EMT  = dff$re_CMU_EMT[1],                    #the modulo of the time ratio at pk section
         re_CMU_EMT_mod  = dff$re_CMU_EMT_mod[1],                #the modulo of the time ratio at pk section, in [0.5, 1.5]
         r0_CMU_EMT  = dff$r0_CMU_EMT[1],                        #the modulo of the time ratio at IC
         r0_CMU_EMT_mod  = dff$r0_CMU_EMT_mod[1],                #the modulo of the time ratio at IC, in [0.5, 1.5]
         t0_CMU_SEM  = dff$t0_CMU_EM[1]/CST_SEM_PERIOD_EM*CST_SEM_PERIOD_SEM,    #the absolute time at IC, in SEM coordinates
         tf_man_SEM  = dff$tf_man_SEM[1],                        #the absolute time at arrival, in SEM coordinates
         dHf_SEM   = dff$Hf_SEM[1] - dff$Hf_semli_SEM[1],        #the relative energy in SEM coordinates
         te_NCSEM   = dff$te_NCSEM[1],                           #the absolute time at pk section
         pmin_dist_SEM = dff$pmin_dist_SEM[1])                   #the projection error
}

# Now, on traj_cont
traj_cont$re_CMU_EMT.original     = traj_cont$re_CMU_EMT
traj_cont$re_CMU_EMT_mod.original = traj_cont$re_CMU_EMT_mod
traj_cont  = ddply(traj_cont, .(label.conn), compute_re_CMU_EMT)

# Now, on traj_from_c
traj_from_c$re_CMU_EMT.original     = traj_from_c$re_CMU_EMT
traj_from_c$re_CMU_EMT_mod.original = traj_from_c$re_CMU_EMT_mod
traj_from_c  = ddply(traj_from_c, .(label.conn), compute_re_CMU_EMT)

# Now, on traj_from_jpl
traj_from_jpl$re_CMU_EMT.original     = traj_from_jpl$re_CMU_EMT
traj_from_jpl$re_CMU_EMT_mod.original = traj_from_jpl$re_CMU_EMT_mod
traj_from_jpl  = ddply(traj_from_jpl, .(label.conn), compute_re_CMU_EMT)

# #Model equal to 1 or 2 
# traj_from_jpl$isJPL = (traj_from_jpl$coord == 13)
# traj_from_jpl$isJPL = as.integer(traj_from_jpl$isJPL) + 1

# For physical units
traj_from_jpl$x_PHEM = traj_from_jpl$x_NCEM*CST_DIST_PRIM_EM*CST_GAMMA_LIB_EM
traj_from_jpl$y_PHEM = traj_from_jpl$y_NCEM*CST_DIST_PRIM_EM*CST_GAMMA_LIB_EM
traj_from_jpl$z_PHEM = traj_from_jpl$z_NCEM*CST_DIST_PRIM_EM*CST_GAMMA_LIB_EM

traj_from_jpl$x_PHSEM = traj_from_jpl$x_NCSEM*CST_DIST_PRIM_SEM*CST_GAMMA_LIB_SEM
traj_from_jpl$y_PHSEM = traj_from_jpl$y_NCSEM*CST_DIST_PRIM_SEM*CST_GAMMA_LIB_SEM
traj_from_jpl$z_PHSEM = traj_from_jpl$z_NCSEM*CST_DIST_PRIM_SEM*CST_GAMMA_LIB_SEM

#-------------------------------------------------------------------------------
# Moreover, we kill the last part of the trajectories in traj_from_c/traj_from_jpl
# because they are usually ugly
#-------------------------------------------------------------------------------
# We get rid of the last SEM period (-1) 
traj_subset_SEM <- function(df)
{
  #subset(df, t_SEM <= df[df$t_SEM <= max(df$t_SEM)-1, "t_SEM"])
  subset(df, t_SEM <= max(df$t_SEM)-1)
}

traj_from_c   = ddply(traj_from_c, .(label.conn, coord), traj_subset_SEM)
traj_from_jpl = ddply(traj_from_jpl, .(label.conn, coord), traj_subset_SEM)

# We get rid of the first SEM period
traj_subset_EM <- function(df)
{
  subset(df, t_SEM >= min(df$t_SEM)+1)
}

traj_from_c   = ddply(traj_from_c, .(label.conn, coord), traj_subset_EM)
traj_from_jpl = ddply(traj_from_jpl, .(label.conn, coord), traj_subset_EM)



#-------------------------------------------------------------------------------
# We find "bad" solutions
#-------------------------------------------------------------------------------
# The function for seed = 10
if(seeds == "_Orbit_10")
{
  is_solution_bad <- function(df)
  {
    dff = proj_map_prec[which(proj_map_prec$label.conn == df$label[1]),]
    mutate(df, is_solution_bad  = min(df$y_NCSEM) < -0.9)
  }
  
  # Now, on traj_from_c
  traj_from_c    = ddply(traj_from_c, .(label.conn), is_solution_bad)
  traj_from_jpl  = ddply(traj_from_jpl, .(label.conn), is_solution_bad)
  
}else if(seeds == "_Orbit_20") # The function for seed = 20
{
  is_solution_bad <- function(df)
  {
    dff = proj_map_prec[which(proj_map_prec$label.conn == df$label[1]),]
    mutate(df, is_solution_bad  = min(df$y_NCSEM) < -0.9)
  }
  
  # Now, on traj_from_c
  traj_from_c    = ddply(traj_from_c, .(label.conn), is_solution_bad)
  traj_from_jpl  = ddply(traj_from_jpl, .(label.conn), is_solution_bad)
  
}else if(seeds == "_Orbit_40") # The function for seed = 20
{
  is_solution_bad <- function(df)
  {
    dff = proj_map_prec[which(proj_map_prec$label.conn == df$label[1]),]
    mutate(df, is_solution_bad  = min(df$y_NCSEM) < -1)
  }
  
  # Now, on traj_from_c
  traj_from_c    = ddply(traj_from_c, .(label.conn), is_solution_bad)
  traj_from_jpl  = ddply(traj_from_jpl, .(label.conn), is_solution_bad)
  
}else if(seeds == "_QHalo_small") # The function for seed = 20
{
  is_solution_bad <- function(df)
  {
    dff = proj_map_prec[which(proj_map_prec$label.conn == df$label[1]),]
    mutate(df, is_solution_bad  = min(df$y_NCSEM) < -0.9)
  }
  
  # Now, on traj_from_c
  traj_from_c    = ddply(traj_from_c, .(label.conn), is_solution_bad)
  traj_from_jpl  = ddply(traj_from_jpl, .(label.conn), is_solution_bad)
  
}else if(seeds == "_Liss_s1_10_s2_5_3T" || seeds == "_Liss_s1_10_s2_5" ) # The function for seed = 20
{
  is_solution_bad <- function(df)
  {
    dff = proj_map_prec[which(proj_map_prec$label.conn == df$label[1]),]
    mutate(df, is_solution_bad  = df$dHf_SEM[1] > 1.4e-4)
  }
  
  # Now, on traj_from_c
  traj_from_c    = ddply(traj_from_c, .(label.conn), is_solution_bad)
  traj_from_jpl  = ddply(traj_from_jpl, .(label.conn), is_solution_bad)
  
}else
{
  traj_from_c$is_solution_bad   = F
  traj_from_jpl$is_solution_bad = F
}

#-------------------------------------------------------------------------------
# Create data.frame with just the good JPL results
#-------------------------------------------------------------------------------
traj_from_jpl_13 = traj_from_jpl[which(traj_from_jpl$coord == 13),]
traj_from_jpl_13 = traj_from_jpl_13[which(!traj_from_jpl_13$is_solution_bad),]

#-------------------------------------------------------------------------------
# If we want to create plots
#-------------------------------------------------------------------------------
# source("ProjMap/ORBITS_PKS_PLOT_NCEM.R")
# source("ProjMap/ORBITS_PKS_PLOT_NCSE.R")
# source("ProjMap/ORBITS_PKS_PLOT_EM.R")
# source("ProjMap/ORBITS_PKS_PLOT_SE.R")
