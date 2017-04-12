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

#===============================================================================
# Get data from file
#===============================================================================
# Suffix
FILE_SUFFIX_CONT      = FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_Orbit_30_CONT" ) #_Orbit_10_SINGLE/_Orbit_10_CONT
FILE_SUFFIX_CONT_TRAJ = FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_Orbit_30_CONT" ) #_Orbit_10_SINGLE/_Orbit_10_CONT


# Prefix
FILE_PREFIX_CONT      = switch(DATA_SOURCE, "LOCAL" = paste0(ftincppdafolder, FILE_SUBFOLDER, "cont_atf_order_", ORDER, "_dest_", LIB_POINT_SEM),
                               "FROM_SERVER" = paste0(ftincppdafolder, FILE_SUBFOLDER, "cont_atf_order_", ORDER, "_dest_", LIB_POINT_SEM) )
FILE_PREFIX_CONT_TRAJ = switch(DATA_SOURCE, "LOCAL" = paste0(ftincppdafolder, FILE_SUBFOLDER, "cont_atf_traj_order_", ORDER, "_dest_", LIB_POINT_SEM),
                               "FROM_SERVER" = paste0(ftincppdafolder, FILE_SUBFOLDER, "cont_atf_traj_order_", ORDER, "_dest_", LIB_POINT_SEM) )
#-------------------------------------------------------------------------------
# Continuation
#-------------------------------------------------------------------------------
proj_cont = get_proj_cont(FILE_PREFIX_CONT, FILE_SUFFIX_CONT, FAMILY = "")

#-------------------------------------------------------------------------------
# Continuation trajectories
#-------------------------------------------------------------------------------
traj_cont = get_traj_cont(FILE_PREFIX_CONT_TRAJ, FILE_SUFFIX_CONT_TRAJ, FAMILY = "", PRINT = TRUE)

#-------------------------------------------------------------------------------
# Postprocess
#-------------------------------------------------------------------------------
proj_cont$r0_CMU_EMT = proj_cont$t0_CMU_EM/CST_SEM_PERIOD_EM
