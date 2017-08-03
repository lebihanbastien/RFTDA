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
# It is used for the plotting of planar families in the PhD manuscript.
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
# From multiple Families...  (comment if unused)
#-------------------------------------------------------------------------------
isFAM      = TRUE
isMULTFAM  = TRUE #if TRUE, we suppose that we want to display multiple families (e.g. the iconic case of t0 = 0.99T)
FAMIND     = 1 # between 1 and length(FAMVEC)
FAM        = FAMVEC[FAMIND]

# Labels to get rid of at the beginning and at the end of the families
lab_min_vec = c(10, 10, 20, 0,  0, 20, 0)
lab_max_vec = c(15, 40, 20, 0, 0, 10, 40)

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
FILE_PREFIX_CONT      = switch(DATA_SOURCE, "LOCAL" = paste0(ftincppdafolder, FILE_SUBFOLDER, "phd_cont_atf_order_", ORDER, "_dest_", LIB_POINT_SEM),
                         "FROM_SERVER" = paste0(ftincppdafolder, FILE_SUBFOLDER, "phd_cont_atf_order_", ORDER, "_dest_", LIB_POINT_SEM) )
FILE_PREFIX_CONT_TRAJ = switch(DATA_SOURCE, "LOCAL" = paste0(ftincppdafolder, FILE_SUBFOLDER, "phd_cont_atf_traj_order_", ORDER, "_dest_", LIB_POINT_SEM),
                          "FROM_SERVER" = paste0(ftincppdafolder, FILE_SUBFOLDER, "phd_cont_atf_traj_order_", ORDER, "_dest_", LIB_POINT_SEM) )
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
traj_cont_single = get_traj_cont(FILE_PREFIX_CONT_TRAJ, FILE_SUFFIX_CONT_TRAJ, paste0(FAMILY, '_single'), PRINT)

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
  #proj_cont$label = proj_cont_label$label
  
  #---------------------------------------------------------------------------
  # We select some solutions within this family:
  # First, we get rid of some solutions at the beginning and at the end.
  # Then, we get rid of some solutions at the center, to make things clearer
  # on the plots
  #---------------------------------------------------------------------------
  label_min = lab_min_vec[FAM]
  label_max = max(proj_cont$label) - lab_max_vec[FAM]
  
  minmaxlabel = proj_cont$label > label_min & proj_cont$label < label_max
  proj_cont = proj_cont[which(minmaxlabel),]
  
  minmaxlabel = traj_cont$label > label_min & traj_cont$label < label_max
  traj_cont   = traj_cont[which(minmaxlabel),]
  
  minmaxlabel = proj_cont_label$label > label_min & proj_cont_label$label < label_max
  proj_cont_label   = proj_cont_label[which(minmaxlabel),]
}



#===============================================================================
# Plots (requires to load the POSTPROCESS.R script before)
#===============================================================================
if(!empty(traj_cont))
{
  source("ProjMap/PLOTS_PhD_MULTFAM_TRAJ.R")
}

