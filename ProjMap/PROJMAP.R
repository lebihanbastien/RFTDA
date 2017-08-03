################################################################################
# R subscript of MAIN.R (to be called first)
#
# Creates the following data frames:
#
# - proj_map_source: the raw results of a multiple projection procedure.
# - proj_map_sol: the best trajectories within this set of solutions
#   (deprecated, left for consistency)
#
# Warning: proj_map_sol is deprecated and must be avoided
#
################################################################################
get_column_names <- function(FWRK)
{
  #-----------------------------------------------------------------------------
  # Column names that depend on the framework
  #-----------------------------------------------------------------------------
  if(FWRK == "EM")
  {
    names_37 = c("t0_CMU_EM", "x0_CMU_NCEM", "y0_CMU_NCEM", "z0_CMU_NCEM", "px0_CMU_NCEM", "py0_CMU_NCEM", "pz0_CMU_NCEM",
                 "x0_CM_SEM", "y0_CM_SEM",   "z0_CM_SEM",   "px0_CM_SEM",  "py0_CM_SEM",   "pz0_CM_SEM",
                 "s1_CMU_EM", "s2_CMU_EM", "s3_CMU_EM", "s4_CMU_EM",  "s5_CMU_EM",  "pmin_dist_SEM", 
                 "dv_at_projection_SEM", "tf_man_SEM",
                 "xf_CM_SEM", "yf_CM_SEM", "zf_CM_SEM", "pxf_CM_SEM", "pyf_CM_SEM", "pzf_CM_SEM",
                 "xp_CM_SEM", "yp_CM_SEM", "zp_CM_SEM", "pxp_CM_SEM", "pyp_CM_SEM", "pzp_CM_SEM",
                 "s1_CM_SEM", "s2_CM_SEM", "s3_CM_SEM", "s4_CM_SEM");

    
  }else if(FWRK == "SEM")
  {
    names_37 = c("t0_CMU_SEM", "x0_CMU_NCSEM", "y0_CMU_NCSEM", "z0_CMU_NCSEM", "px0_CMU_NCSEM", "py0_CMU_NCSEM", "pz0_CMU_NCSEM",
                 "x0_CM_EM", "y0_CM_EM",   "z0_CM_EM",   "px0_CM_EM",  "py0_CM_EM",   "pz0_CM_EM",
                 "s1_CMU_SEM", "s2_CMU_SEM", "s3_CMU_SEM", "s4_CMU_SEM",  "s5_CMU_SEM",  "pmin_dist_SEM", 
                 "dv_at_projection_EM", "tf_man_EM",
                 "xf_CM_EM", "yf_CM_EM", "zf_CM_EM", "pxf_CM_EM", "pyf_CM_EM", "pzf_CM_EM",
                 "xp_CM_EM", "yp_CM_EM", "zp_CM_EM", "pxp_CM_EM", "pyp_CM_EM", "pzp_CM_EM",
                 "s1_CM_EM", "s2_CM_EM", "s3_CM_EM", "s4_CM_EM");
    
  }else
  {
    stop("Unknown framework: FWRK must be equal to either EM or SEM.")
  }
  
  #-----------------------------------------------------------------------------
  # Column names that do not depend on the framework
  #-----------------------------------------------------------------------------
  names_39 = c(names_37, "crossings", "collision");
  names_55 = c(names_39, "H0_NCEM", "H0_NCSEM", "H0_EM", "H0_SEM",
               "H0_emli_NCEM", "H0_emli_NCSEM", "H0_emli_EM", "H0_emli_SEM",
               "Hf_NCEM", "Hf_NCSEM", "Hf_EM", "Hf_SEM",
               "Hf_semli_NCEM", "Hf_semli_NCSEM", "Hf_semli_EM", "Hf_semli_SEM");
  names_56 = c("label", names_55);
  names_61 = c(names_56, "t0_CMU_EM_seed", "s1_CMU_EM_seed", "s2_CMU_EM_seed", "s3_CMU_EM_seed", "s4_CMU_EM_seed");
  
  names_81 = c(names_61, "te_NCEM", "xe_CMS_NCEM", "ye_CMS_NCEM", "ze_CMS_NCEM", 
               "pxe_CMS_NCEM", "pye_CMS_NCEM", "pze_CMS_NCEM", 
               "vxe_CMS_NCEM", "vye_CMS_NCEM", "vze_CMS_NCEM");
  names_81 = c(names_81, "te_NCSEM", "xe_CMS_NCSEM", "ye_CMS_NCSEM", "ze_CMS_NCSEM", 
               "pxe_CMS_NCSEM", "pye_CMS_NCSEM", "pze_CMS_NCSEM", "vxe_CMS_NCSEM", 
               "vye_CMS_NCSEM", "vze_CMS_NCSEM");
  
  names_82 = c(names_81, "label.conn");
  
  #-----------------------------------------------------------------------------
  # Output in a certain order
  # Careful: the order of the name vectors may have an influence on the reading
  # of the data in dffbinaryv
  #-----------------------------------------------------------------------------
  VNAMES = list(names_82, names_81, names_61, names_56, names_37, names_39, names_55)
  VNCOL  = c(82, 81, 61, 56, 37, 39, 55)
  return(list("VNAMES" = VNAMES, "VNCOL"  = VNCOL))
}

#-------------------------------------------------------------------------------
# Projection map
#-------------------------------------------------------------------------------
filename = paste0(FILE_PREFIX, FILE_SUFFIX, ".bin");
print(paste0("PROJMAP.R: data taken from ", filename))

if (file.exists(filename))
{
  print(paste0("... that does exist."))
  Output = get_column_names(FWRK)
  VNAMES = Output$VNAMES
  VNCOL  = Output$VNCOL
  
  #proj_map_source = dffbinary(filename, 37, names_37)
  proj_map_source = dffbinaryv(filename, VNCOL, VNAMES, PRINT = TRUE)
  
  #-----------------------------------------------------------------------------
  # Postprocess (in the long run, all operations from 
  # POSTPROCESS.R should be set here)
  #-----------------------------------------------------------------------------
  # If the Pk section results are present
  if(("te_NCEM" %in% colnames(proj_map_source)))
  {
    # Time at the Pk section, given as x T
    proj_map_source$te_CMU_EMT  = proj_map_source$te_NCEM/SEMperiod("EM")
    # Time at the Pk section, given as x T, in [0, 1]
    proj_map_source$re_CMU_EMT  = round(proj_map_source$te_CMU_EMT  %% 1, digits= 4)
    # Same but between 0.5 and 1.5
    proj_map_source$re_CMU_EMT_mod = r0_modulo(proj_map_source$re_CMU_EMT)
  }
  
}else
{
  proj_map_source = data.frame()
}

#-------------------------------------------------------------------------------
# Best solution
#-------------------------------------------------------------------------------
filepre  = paste0(FILE_PREFIX_SOL, FILE_SUFFIX);
filename = paste0(filepre, ".bin");
if (file.exists(filename))
{
  names = c("label", "s1_CMU_EM", "s3_CMU_EM", "s1_CM_SEM", "s3_CM_SEM",  "pmin_dist_SEM",
            "t0_CMU_EM", "x_man_SEM", "y_man_SEM", "z_man_SEM", 
            "px_man_SEM", "py_man_SEM", "pz_man_SEM", "h_man_SEM", 
            "t_orb_eml_SEM", "x_orb_eml_SEM", "y_orb_eml_SEM", "z_orb_eml_SEM", 
            "px_orb_eml_SEM", "py_orb_eml_SEM", "pz_orb_eml_SEM", "h_orb_eml_SEM",
            "t_orb_seml_SEM", "x_orb_seml_SEM", "y_orb_seml_SEM", "z_orb_seml_SEM", 
            "px_orb_seml_SEM", "py_orb_seml_SEM", "pz_orb_seml_SEM", "h_orb_seml_SEM");
  
  # OLD VERSION (SERVER)
  # names = c("label", "t0_CMU_EM", "x_man_SEM", "y_man_SEM", "z_man_SEM", "px_man_SEM", "py_man_SEM", "pz_man_SEM",
  #           "s1_CMU_EM", "s3_CMU_EM", "s1_CM_SEM", "s3_CM_SEM", "pmin_dist_SEM", "h_man_SEM", 
  #           "t_orb_eml_SEM", "x_orb_eml_SEM", "y_orb_eml_SEM", "z_orb_eml_SEM", 
  #           "px_orb_eml_SEM", "py_orb_eml_SEM", "pz_orb_eml_SEM", "h_orb_eml_SEM",
  #           "t_orb_seml_SEM", "x_orb_seml_SEM", "y_orb_seml_SEM", "z_orb_seml_SEM", "px_orb_seml_SEM", "py_orb_seml_SEM", "pzO2", "h_orb_seml_SEM");
  
  proj_map_sol = dffbinary(filename, 30, names)
}else
{
  proj_map_sol = data.frame()
}
