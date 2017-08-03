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
################################################################################

#===============================================================================
# Which specific time?
#===============================================================================
ratio_desired =  0.995# 0.065; # x SEML.us_em.T
time_desired  =  ratio_desired*CST_SEM_PERIOD_EM;

#===============================================================================
# User inputs
#===============================================================================
PRINT   = FALSE
ISSAVED = TRUE

#-------------------------------------------------------------------------------
# ... or single family
#-------------------------------------------------------------------------------
isMULTFAM   = FALSE
isFAM       = FALSE  #if FALSE, no family is used
FAM         = 1 #41 is cont
lab_min_vec = 0*seq(1,FAM)
lab_max_vec = 0*seq(1,FAM)

#-------------------------------------------------------------------------------
# Planar case
#-------------------------------------------------------------------------------
FAMILY = switch(isFAM, "TRUE" = paste0("_fam", FAM), "FALSE" = "")

#-------------------------------------------------------------------------------
# For 3D type, we can select some values of s2/s4
#-------------------------------------------------------------------------------
s2_value = 8;
s4_value = -12;

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
FILE_PREFIX_CONT      = switch(DATA_SOURCE, "LOCAL" = paste0(ftincppdafolder, FILE_SUBFOLDER, "phd_cont_atf_order_", ORDER, "_dest_", LIB_POINT_SEM),
                         "FROM_SERVER" = paste0(ftincppdafolder, FILE_SUBFOLDER, "phd_cont_atf_order_", ORDER, "_dest_", LIB_POINT_SEM) )
FILE_PREFIX_CONT_TRAJ = switch(DATA_SOURCE, "LOCAL" = paste0(ftincppdafolder, FILE_SUBFOLDER, "phd_cont_atf_traj_order_", ORDER, "_dest_", LIB_POINT_SEM),
                          "FROM_SERVER" = paste0(ftincppdafolder, FILE_SUBFOLDER, "phd_cont_atf_traj_order_", ORDER, "_dest_", LIB_POINT_SEM) )
FILE_PREFIX_CONT_JPL  = switch(DATA_SOURCE, "LOCAL" = paste0(ftincppdafolder, FILE_SUBFOLDER, "cont_jpl_order_", ORDER, "_dest_", LIB_POINT_SEM),
                               "FROM_SERVER" = paste0(ftincppdafolder, FILE_SUBFOLDER, "cont_jpl_order_", ORDER, "_dest_", LIB_POINT_SEM) )
# Suffix
FILE_SUFFIX_CONT      = paste0("_t0_", ratio_desired);
FILE_SUFFIX_CONT_TRAJ = paste0("_t0_", ratio_desired);

#===============================================================================
# Prefix and filenames to save files
#===============================================================================
filepre  = paste0(FILE_PREFIX, "_t0_0", toString(1000*ratio_desired))
filecont = paste0(FILE_PREFIX_CONT, "_t0_0", toString(1000*ratio_desired))

#===============================================================================
# Get data from file
#===============================================================================
#-------------------------------------------------------------------------------
# Continuation
#-------------------------------------------------------------------------------
proj_cont = get_proj_cont(FILE_PREFIX_CONT, FILE_SUFFIX_CONT, FAMILY)

#-------------------------------------------------------------------------------
# Continuation trajectories
#-------------------------------------------------------------------------------
traj_cont = get_traj_cont(FILE_PREFIX_CONT_TRAJ, FILE_SUFFIX_CONT_TRAJ, FAMILY, PRINT)

#===============================================================================
# Postprocess for continuation
#===============================================================================
if(nrow(traj_cont) > 0 && nrow(proj_cont) >0)
{
  #-----------------------------------------------------------------------------
  # For traj_cont
  #-----------------------------------------------------------------------------
  #Label only
  proj_cont_label = traj_cont[which(traj_cont$t_CMU_SEM == min(traj_cont$t_CMU_SEM)),]
  # Family
  traj_cont$family = FAM
  
  #-----------------------------------------------------------------------------
  # For proj_cont
  #-----------------------------------------------------------------------------
  proj_cont$x0_CMS_SEM = -CST_GAMMA_LIB_SEM*(proj_cont$x0_CMS_NCSEM - CST_C1_LIB_SEM)
  proj_cont$y0_CMS_SEM = -CST_GAMMA_LIB_SEM*(proj_cont$y0_CMS_NCSEM - 0)
  proj_cont$z0_CMS_SEM = +CST_GAMMA_LIB_SEM*(proj_cont$z0_CMS_NCSEM - 0)
  
  proj_cont$x0_CMU_EM = -CST_GAMMA_LIB_EM*(proj_cont$x0_CMU_NCEM - CST_C1_LIB_EM)
  proj_cont$y0_CMU_EM = -CST_GAMMA_LIB_EM*(proj_cont$y0_CMU_NCEM - 0)
  proj_cont$z0_CMU_EM = +CST_GAMMA_LIB_EM*(proj_cont$z0_CMU_NCEM - 0)
  
  #Get the time as a percentage of T
  proj_cont$t0_CMU_EMT = proj_cont$t0_CMU_EM/CST_SEM_PERIOD_EM
  #Get the time as a percentage of T
  proj_cont$tf_CMU_EMT = proj_cont$tf_CMU_EM/CST_SEM_PERIOD_EM
  #tof_EM (in T)
  proj_cont$tof_EM = (proj_cont$tf_CMU_EMT-proj_cont$t0_CMU_EMT)
  #tof_EM (days)
  proj_cont$tof_SI = (proj_cont$tf_CMU_EMT-proj_cont$t0_CMU_EMT)*SEMperiod("EM")*Tcrtbp("EM")/(2*pi*3600*24)
  #t0 in %T
  proj_cont$t0_CMU_EMT = proj_cont$t0_CMU_EM/CST_SEM_PERIOD_EM
  
  #Copy label into proj_cont
  proj_cont$label = proj_cont_label$label
  
  #---------------------------------------------------------------------------
  # We select some solutions within this family:
  # First, we get rid of some solutions at the beginning and at the end.
  # Then, we get rid of some solutions at the center, to make things clearer
  # on the plots
  #---------------------------------------------------------------------------
  # label_min = lab_min_vec[FAM]
  # label_max = max(proj_cont$label) - lab_max_vec[FAM]
  # 
  # minmaxlabel = proj_cont$label > label_min & proj_cont$label < label_max
  # proj_cont = proj_cont[which(minmaxlabel),]
  # 
  # minmaxlabel = traj_cont$label > label_min & traj_cont$label < label_max
  # traj_cont   = traj_cont[which(minmaxlabel),]
  # 
  # minmaxlabel = proj_cont_label$label > label_min & proj_cont_label$label < label_max
  # proj_cont_label   = proj_cont_label[which(minmaxlabel),]
}





#===============================================================================
# Plots (requires to load the POSTPROCESS.R script before)
#===============================================================================
source("ProjMap/PLOTS_PhD_3D.R")          # continuation results
#source("ProjMap/PLOTS_PhD_3D_TRAJ_NC.R")  # trajectories in NC coordinates
#source("ProjMap/PLOTS_PhD_3D_TRAJ_SYN.R") # same as previous, in SYNODICAL coordinates