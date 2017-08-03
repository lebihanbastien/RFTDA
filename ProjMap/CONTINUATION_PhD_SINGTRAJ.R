################################################################################
# R script to handle a projection map (connections from EML2 to SEML1,2)
# continuation case
#
# WARNING: MAIN.R must be loaded once before.
#
# This scripts basically reads datafiles and updates the following dataframes:
#
# - proj_cont : solutions of a continuation procedures (s1, s3, tf...)
# - traj_cont : the corresponding trajectories 
#
# Then, it plots the complete family as 3D trajectories. 
# It is used for the plotting of single instances of 
# planar families in the PhD manuscript.
#
# BLB 2017.
#
################################################################################

#===============================================================================
# Which specific time?
#===============================================================================
ratio_desired =  0.99
time_desired  =  ratio_desired*CST_SEM_PERIOD_EM;

#===============================================================================
# User inputs
#===============================================================================
PRINT   = FALSE
ISSAVED = TRUE

#===============================================================================
# Family 
#===============================================================================
# For old MULTFAM
FAMVEC     = c(1, 2, 3, 4, 5, 7)
NFAM       = length(FAMVEC)

# Aes
label.frequency.vec   = c(15, 15, 15, 15, 25, 20, 20)
y.position.labels.vec = c(-0.09, -1.2, -0.6, -0.6, -0.09, -0.09, -0.09)
y.position.earth.vec  = c(-0.09, -0.09, -0.6, -0.6, -0.09, -0.09, -0.09)

#-------------------------------------------------------------------------------
# ... or single family (comment if unused)
#-------------------------------------------------------------------------------
isMULTFAM  = FALSE
isFAM      = TRUE  #if FALSE, no family is used
FAMIND     = 6
FAM        = FAMVEC[FAMIND]

# Labels to get rid of at the beginning and at the end of the families
lab_min_vec = 0*seq(1,FAM)
lab_max_vec = 0*seq(1,FAM)

#-------------------------------------------------------------------------------
# Planar case
#-------------------------------------------------------------------------------
FAMILY = switch(isFAM, "TRUE" = paste0("_fam", FAM), "FALSE" = "")

#-------------------------------------------------------------------------------
# For 3D type, we can select some values of s2/s4
#-------------------------------------------------------------------------------
s2_value = 0;
s4_value = 0;

# Family for 3d computation
if(TYPE == "_3d")
{
  FAMILY = paste0("_s2_", s2_value, "_s4_", s4_value)
  if(isFAM)
  {
    FAMILY = paste0(FAMILY, "_fam", FAM)
  }
}


#===============================================================================
#Title
#===============================================================================
ggtitle_t0 = ggtitle(paste0("t = ", toString(ratio_desired), "T"))


#===============================================================================
# Which type of data?
#===============================================================================
# Prefix
FILE_PREFIX_CONT_TRAJ = switch(DATA_SOURCE, "LOCAL" = paste0(ftincppdafolder, FILE_SUBFOLDER, "phd_cont_atf_traj_order_", ORDER, "_dest_", LIB_POINT_SEM),
                          "FROM_SERVER" = paste0(ftincppdafolder, FILE_SUBFOLDER, "phd_cont_atf_traj_order_", ORDER, "_dest_", LIB_POINT_SEM) )
# Suffix
FILE_SUFFIX_CONT_TRAJ = paste0("_t0_", ratio_desired);

#===============================================================================
# Prefix and filenames to save files
#===============================================================================
filepre  = paste0(FILE_PREFIX, "_t0_0", toString(1000*ratio_desired))
filecont = paste0(FILE_PREFIX_CONT, "_t0_0", toString(1000*ratio_desired))

#===============================================================================
# Get data from file
#===============================================================================
# Continuation trajectories
traj_cont_single = get_traj_cont(FILE_PREFIX_CONT_TRAJ, FILE_SUFFIX_CONT_TRAJ, paste0(FAMILY, '_single'), PRINT)

#===============================================================================
# Plots (requires to load the POSTPROCESS.R script before)
#===============================================================================
if(!empty(traj_cont_single))
{
  source("ProjMap/PLOTS_PhD_SINGTRAJ.R")
}

