################################################################################
# R script to handle a projection map (connections from EML2 to SEML1,2)
# continuation case
#
# WARNING: MAIN.R must be loaded once before.
#
# This scripts basically reads datafiles and updates the following dataframes:
#
# - proj_cont_fam : solutions of a continuation procedures (s1, s3, tf...)
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
# II.  This file has been made specifically for the PhD manuscript.
# III. There is NO trajectories in this file.
#
################################################################################

#===============================================================================
# Which specific time?
#===============================================================================
ratio_desired =  0.99 # x SEML.us_em.T
time_desired  =  ratio_desired*CST_SEM_PERIOD_EM;

#===============================================================================
#Family
#===============================================================================
isFAM  = TRUE  #if FALSE, no family is used
FAM0   = 1 #41 is cont
PRINT  = FALSE
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
#===============================================================================
isMULTFAM  = TRUE #if TRUE, we suppose that we want to display multiple families (e.g. the iconic case of t0 = 0.99T)
FAMVEC     = c(1, 2, 3, 4, 5, 7)
NFAM       = length(FAMVEC)
# Labels to get rid of at the beginning and at the end of the families
lab_min_vec = c(10, 10, 20,  0,  0, 20, 0)
lab_max_vec = c(15, 40, 20, 40, 0, 10, 40)

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
# Suffix
FILE_SUFFIX_CONT      = paste0("_t0_", ratio_desired);

#===============================================================================
# Prefix and filenames to save files
#===============================================================================
filepre  = paste0(FILE_PREFIX, "_t0_0", toString(1000*ratio_desired))
filecont = paste0(FILE_PREFIX_CONT, "_t0_0", toString(1000*ratio_desired))

#===============================================================================
# Get data from file: All the families
#===============================================================================
#-----------------------------------------------------------------------------
# Same on all the families
#-----------------------------------------------------------------------------
for(IND in seq(1, length(FAMVEC)))
{
  FAM = FAMVEC[IND]
  
  #---------------------------------------------------------------------------
  # Continuation results
  #---------------------------------------------------------------------------
  proj_cont_temp = get_proj_cont(FILE_PREFIX_CONT, 
                                 FILE_SUFFIX_CONT, paste0("_fam", FAM))
  
  #---------------------------------------------------------------------------
  # Post process
  #---------------------------------------------------------------------------
  proj_cont_temp$family = FAM
  proj_cont_temp$label  = seq(1,length(proj_cont_temp$t0_CMU_EM))
  
  #---------------------------------------------------------------------------
  # We select some solutions within this family:
  # First, we get rid of some solutions at the beginning and at the end.
  # Then, we get rid of some solutions at the center, to make things clearer
  # on the plots
  #---------------------------------------------------------------------------
  label_min = lab_min_vec[FAM]
  label_max = max(proj_cont_temp$label) - lab_max_vec[FAM]
  
  minmaxlabel = proj_cont_temp$label > label_min & proj_cont_temp$label < label_max
  proj_cont_temp = proj_cont_temp[which(minmaxlabel),]
  
  
  #---------------------------------------------------------------------------
  # Rbind
  #---------------------------------------------------------------------------
  if(IND == 1)
  {
    proj_cont_fam = proj_cont_temp
  }else{
    proj_cont_fam = rbind_cc(proj_cont_fam, proj_cont_temp)
  }
}

#===============================================================================
# Postprocess for MULTFAM
#===============================================================================
proj_cont_fam$x0_CMU_NCEM_KM =  proj_cont_fam$x0_CMU_NCEM*Ldist("EM")
proj_cont_fam$y0_CMU_NCEM_KM =  proj_cont_fam$y0_CMU_NCEM*Ldist("EM")
proj_cont_fam$z0_CMU_NCEM_KM =  proj_cont_fam$z0_CMU_NCEM*Ldist("EM")

proj_cont_fam$x0_CMU_EM =  -CST_GAMMA_LIB_EM*(proj_cont_fam$x0_CMU_NCEM - CST_C1_LIB_EM)
proj_cont_fam$y0_CMU_EM =  -CST_GAMMA_LIB_EM*(proj_cont_fam$y0_CMU_NCEM)
proj_cont_fam$z0_CMU_EM =  +CST_GAMMA_LIB_EM*(proj_cont_fam$z0_CMU_NCEM)

proj_cont_fam$d0_CMU_NCEM_KM = sqrt(proj_cont_fam$x0_CMU_NCEM_KM^2   + 
                                      proj_cont_fam$y0_CMU_NCEM_KM^2 + 
                                      proj_cont_fam$z0_CMU_NCEM_KM^2)

proj_cont_fam$ds_CMU_EM = sqrt(proj_cont_fam$s1_CMU_EM^2 + 
                                 proj_cont_fam$s2_CMU_EM^2 + 
                                 proj_cont_fam$s3_CMU_EM^2 + 
                                 proj_cont_fam$s4_CMU_EM^2)


proj_cont_fam$x0_CMS_NCSEM_KM =  proj_cont_fam$x0_CMS_NCSEM*Ldist("SEM")
proj_cont_fam$y0_CMS_NCSEM_KM =  proj_cont_fam$y0_CMS_NCSEM*Ldist("SEM")
proj_cont_fam$z0_CMS_NCSEM_KM =  proj_cont_fam$z0_CMS_NCSEM*Ldist("SEM")

proj_cont_fam$d0_CMS_NCSEM_KM = sqrt(proj_cont_fam$x0_CMS_NCSEM_KM^2   + 
                                       proj_cont_fam$y0_CMS_NCSEM_KM^2 + 
                                       proj_cont_fam$z0_CMS_NCSEM_KM^2)

proj_cont_fam$ds_CMS_SEM = sqrt(proj_cont_fam$s1_CMS_SEM^2 + 
                                  proj_cont_fam$s2_CMS_SEM^2 + 
                                  proj_cont_fam$s3_CMS_SEM^2 + 
                                  proj_cont_fam$s4_CMS_SEM^2)


#===============================================================================
# Plots (requires to load the POSTPROCESS.R script before)
#===============================================================================
# For new plots
source("ProjMap/PLOTS_PhD_MULTFAM.R")

