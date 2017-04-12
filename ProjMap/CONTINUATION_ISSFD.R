################################################################################
# R script to handle a projection map (connections between EML2 and SEML1,2)
# continuation case
#
# WARNING: MAIN.R must be loaded once before.
#
# This scripts basically reads datafiles and updates the following dataframes:
#
# - proj_cont : solutions of a continuation procedures (s1, s3, tf...)
# - traj_cont : the corresponding trajectories 
#
# Notes:
# 
# I. Multiple families at t = 0.99T
#
# isMULTFAM is used for displaying the families at t0 = 0.99T, as an example 
# There are currently 7 families, that as to be read with NCOL = 8 (old storage)
# Each family N has a unique solution stored in "_famN1" (eg _fam11 for family 1)
# that has to be read with NCOL = 16 (new storage)
#
# Note: family 7 has been obtained using projcu_order_20_t0_099T.bin instead 
# of the FINAL data
#
# 2. Primary family: from t = 0.555T downwards, NCOL = 16!!
#                    for  t = 0.565 and 0.56 there is a mix of NCOL = 8 and 16... great...
#
################################################################################

#===============================================================================
# Which specific time?
#===============================================================================
ratio_desired =  0.99# 0.065; # x SEML.us_em.T
time_desired  =  ratio_desired*CST_SEM_PERIOD_EM;

#===============================================================================
#Family
#===============================================================================
isFAM  = TRUE  #if FALSE, no family is used
FAM0   = 11 #41 is cont
PRINT  = TRUE
FAMILY = switch(isFAM, "TRUE" = paste0("_fam", FAM0), "FALSE" = "")

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
    FAMILY = paste0(FAMILY, "_fam", FAM0)
  }
}


#===============================================================================
# Multiple Families
# We only select 1, 3, 5, 6 and 7
#===============================================================================
isMULTFAM  = TRUE #if TRUE, we suppose that we want to display multiple families (e.g. the iconic case of t0 = 0.99T)
NFAM       = 5
FAMVEC     = c(1, 3, 5, 6, 7)
FAMIND     = 1
FAM        = FAMVEC[FAMIND]
# Labels to get rid of at the beginning and at the end of the families
lab_min_vec = c(10, 5, 0, 0,  0, 0, 10)
lab_max_vec = c(15, 5, 0, 15, 0, 0, 15)

#===============================================================================
#Title
#===============================================================================
ggtitle_t0 = ggtitle(paste0("t = ", toString(ratio_desired), "T"))


#===============================================================================
# Which type of data?
#===============================================================================
# Prefix
FILE_PREFIX_CONT      = switch(DATA_SOURCE, "LOCAL" = paste0(ftincppdafolder, FILE_SUBFOLDER, "cont_atf_order_", ORDER, "_dest_", LIB_POINT_SEM),
                         "FROM_SERVER" = paste0(ftincppdafolder, FILE_SUBFOLDER, "cont_atf_order_", ORDER, "_dest_", LIB_POINT_SEM) )
FILE_PREFIX_CONT_TRAJ = switch(DATA_SOURCE, "LOCAL" = paste0(ftincppdafolder, FILE_SUBFOLDER, "cont_atf_traj_order_", ORDER, "_dest_", LIB_POINT_SEM),
                          "FROM_SERVER" = paste0(ftincppdafolder, FILE_SUBFOLDER, "cont_atf_traj_order_", ORDER, "_dest_", LIB_POINT_SEM) )
FILE_PREFIX_CONT_JPL  = switch(DATA_SOURCE, "LOCAL" = paste0(ftincppdafolder, FILE_SUBFOLDER, "cont_jpl_order_", ORDER, "_dest_", LIB_POINT_SEM),
                               "FROM_SERVER" = paste0(ftincppdafolder, FILE_SUBFOLDER, "cont_jpl_order_", ORDER, "_dest_", LIB_POINT_SEM) )
# Suffix
FILE_SUFFIX_CONT      = paste0("_t0_", ratio_desired);
FILE_SUFFIX_CONT_TRAJ = paste0("_t0_", ratio_desired);
FILE_SUFFIX_CONT_JPL  = paste0("_t0_", ratio_desired);


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

#-------------------------------------------------------------------------------
# All the families
#-------------------------------------------------------------------------------
if(isMULTFAM)
{
  #-----------------------------------------------------------------------------
  # Continuation results
  #-----------------------------------------------------------------------------
  proj_cont_fam = get_proj_cont(FILE_PREFIX_CONT, FILE_SUFFIX_CONT, paste0("_fam", FAM))
  proj_cont_fam$family = FAM
  
  #-----------------------------------------------------------------------------
  # Continuation results - trajectories
  #-----------------------------------------------------------------------------
  #traj_cont_fam = get_traj_cont(FILE_PREFIX_CONT_TRAJ, FILE_SUFFIX_CONT_TRAJ, paste0("_fam", FAM), PRINT)
  #traj_cont_fam$family = FAM
  # 
  # #-----------------------------------------------------------------------------
  # # Postprocess
  # #-----------------------------------------------------------------------------
  # #Label only
  # condition         = traj_cont_fam$t_CMU_SEM == min(traj_cont_fam$t_CMU_SEM)
  # proj_cont_label   = traj_cont_fam[which(condition),]
  # #Copy label into proj_cont_fam
  # proj_cont_fam$label = proj_cont_label$label
  
  #-----------------------------------------------------------------------------
  # Same on all the families
  #-----------------------------------------------------------------------------
  for(IND in seq(1, length(FAMVEC)))
  {
    FAM = FAMVEC[IND]
    #-----------------------------------------------------------------------------
    # Continuation results
    #-----------------------------------------------------------------------------
    proj_cont_temp = get_proj_cont(FILE_PREFIX_CONT, 
                                   FILE_SUFFIX_CONT, paste0("_fam", FAM))
    proj_cont_temp$family = FAM
    
    #---------------------------------------------------------------------------
    # Rbind
    #---------------------------------------------------------------------------
    proj_cont_fam = rbind_cc(proj_cont_fam, proj_cont_temp)
  }

}


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
  
  #-----------------------------------------------------------------------------
  # For proj_cont
  #-----------------------------------------------------------------------------
  proj_cont$x0_CMS_SEM = -CST_GAMMA_LIB_SEM*(proj_cont$x0_CMS_NCSEM - CST_C1_LIB_SEM)
  proj_cont$y0_CMS_SEM = -CST_GAMMA_LIB_SEM*(proj_cont$y0_CMS_NCSEM - 0)
  proj_cont$z0_CMS_SEM = +CST_GAMMA_LIB_SEM*(proj_cont$z0_CMS_NCSEM - 0)
  
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
  
  #-----------------------------------------------------------------------------
  # Max in min labels
  # LEFT TO DO: fam5, fam6
  #-----------------------------------------------------------------------------
  label_min = 0#10 for fam1 & fam7, 5 for fam2, 0 for fam3 & fam4
  label_max = max(proj_cont_label$label) - 0 #15 for fam1 & fam7, 5 for fam2, 0 for fam3 & fam4
  
  minmaxlabel = traj_cont$label >= label_min & traj_cont$label <= label_max
  traj_cont = traj_cont[which(minmaxlabel),]
  
  minmaxlabel = proj_cont$label >= label_min & proj_cont$label <= label_max
  proj_cont = proj_cont[which(minmaxlabel),]
}



#===============================================================================
# Get results from JPL file
#===============================================================================
get_traj_cont_jpl <- function(FILE_PREFIX_CONT_JPL, FILE_SUFFIX_CONT_JPL, FAMILY="")
{
  #-----------------------------------------------------------------------------
  # Column names
  #-----------------------------------------------------------------------------
  names = c(  "label",  "coord", "t_eph", 
              "t_SEM", 
              "x_NCSEM", "y_NCSEM", "z_NCSEM", 
              "px_NCSEM", "py_NCSEM", "pz_NCSEM",
              "t_EM",
              "x_NCEM", "y_NCEM", "z_NCEM", 
              "px_NCEM", "py_NCEM", "pz_NCEM")
  
  #-----------------------------------------------------------------------------
  # Build the filename 
  #-----------------------------------------------------------------------------
  filepre_cont_traj  = paste0(FILE_PREFIX_CONT_JPL, FILE_SUFFIX_CONT_JPL, FAMILY);
  #-----------------------------------------------------------------------------
  filename_cont_traj = paste0(filepre_cont_traj, ".bin");
  
  # Read data
  #-----------------------------------------------------------------------------
  if (file.exists(filename_cont_traj))
  {
    #Read table
    traj_cont = dffbinary(filename_cont_traj, 17, names)
    
  }
  
  return(traj_cont)
}

#-------------------------------------------------------------------------------
# Continuation trajectories, jpl refinement
#-------------------------------------------------------------------------------
traj_cont_jpl = get_traj_cont_jpl(FILE_PREFIX_CONT_JPL, FILE_SUFFIX_CONT_JPL, FAMILY="")

#===============================================================================
# Plots (requires to load the POSTPROCESS.R script before)
#===============================================================================
source("ProjMap/POSTPROCESS.R")
source("ProjMap/PLOTS.R")

