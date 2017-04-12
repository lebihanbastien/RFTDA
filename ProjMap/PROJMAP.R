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

#-------------------------------------------------------------------------------
# Projection map
#-------------------------------------------------------------------------------
filename = paste0(FILE_PREFIX, FILE_SUFFIX, ".bin");
if (file.exists(filename))
{
  names_37 = c("t0_CMU_EM", "x0_CMU_NCEM", "y0_CMU_NCEM", "z0_CMU_NCEM", "px0_CMU_NCEM", "py0_CMU_NCEM", "pz0_CMU_NCEM",
               "x0_CM_SEM", "y0_CM_SEM", "z0_CM_SEM", "px0_CM_SEM", "py0_CM_SEM", "pz0_CM_SEM",
               "s1_CMU_EM", "s2_CMU_EM", "s3_CMU_EM", "s4_CMU_EM",  "s5_CMU_EM",  "pmin_dist_SEM", 
               "dv_at_projection_SEM", "tf_man_SEM",
               "xf_CM_SEM", "yf_CM_SEM", "zf_CM_SEM", "pxf_CM_SEM", "pyf_CM_SEM", "pzf_CM_SEM",
               "xp_CM_SEM", "yp_CM_SEM", "zp_CM_SEM", "pxp_CM_SEM", "pyp_CM_SEM", "pzp_CM_SEM",
               "s1_CM_SEM", "s2_CM_SEM", "s3_CM_SEM", "s4_CM_SEM");
  
  names_39 = c(names_37, "crossings", "collision");
  names_55 = c(names_39, "H0_NCEM", "H0_NCSEM", "H0_EM", "H0_SEM",
   "H0_emli_NCEM", "H0_emli_NCSEM", "H0_emli_EM", "H0_emli_SEM",
   "Hf_NCEM", "Hf_NCSEM", "Hf_EM", "Hf_SEM",
   "Hf_semli_NCEM", "Hf_semli_NCSEM", "Hf_semli_EM", "Hf_semli_SEM");
  
  names_56 = c("label", names_55);
  
  names_61 = c(names_56, "t0_CMU_EM_seed", "s1_CMU_EM_seed", "s2_CMU_EM_seed", "s3_CMU_EM_seed", "s4_CMU_EM_seed");
  
  VNAMES = list(names_61, names_37, names_39, names_55, names_56)
  
  VNCOL  = c(61, 37, 39, 55, 56)
  
  #proj_map_source = dffbinary(filename, 37, names_37)
  proj_map_source = dffbinaryv(filename, VNCOL, VNAMES)
}else
{
  proj_map_source = data.frame()
}

# #-----------------------------------------------------------------------------
# # Projection map (with variable type of data)
# #-----------------------------------------------------------------------------
# filename = paste0(FILE_PREFIX, FILE_SUFFIX, ".bin");
# if (file.exists(filename))
# {
#   names = c("t0_CMU_EM", "x0_CMU_NCEM", "y0_CMU_NCEM", "z0_CMU_NCEM", "px0_CMU_NCEM", "py0_CMU_NCEM", "pz0_CMU_NCEM",
#             "x0_CM_SEM", "y0_CM_SEM", "z0_CM_SEM", "px0_CM_SEM", "py0_CM_SEM", "pz0_CM_SEM",
#             "s1_CMU_EM", "s2_CMU_EM", "s3_CMU_EM", "s4_CMU_EM",  "s5_CMU_EM",  "pmin_dist_SEM", "crossings", "tf_man_SEM",
#             "xf_CM_SEM", "yf_CM_SEM", "zf_CM_SEM", "pxf_CM_SEM", "pyf_CM_SEM", "pzf_CM_SEM",
#             "xp_CM_SEM", "yp_CM_SEM", "zp_CM_SEM", "pxp_CM_SEM", "pyp_CM_SEM", "pzp_CM_SEM",
#             "s1_CM_SEM", "s2_CM_SEM", "s3_CM_SEM", "s4_CM_SEM");
#   
#   names_2 = c("t0_CMU_EM", "x0_CMU_NCEM", "y0_CMU_NCEM", "z0_CMU_NCEM", "px0_CMU_NCEM", "py0_CMU_NCEM", "pz0_CMU_NCEM",
#             "x0_CM_SEM", "y0_CM_SEM", "z0_CM_SEM", "px0_CM_SEM", "py0_CM_SEM", "pz0_CM_SEM",
#             "s1_CMU_EM", "s2_CMU_EM", "s3_CMU_EM", "s4_CMU_EM",  "s5_CMU_EM",  "pmin_dist_SEM", "crossings", "tf_man_SEM",
#             "xf_CM_SEM", "yf_CM_SEM", "zf_CM_SEM", "pxf_CM_SEM", "pyf_CM_SEM", "pzf_CM_SEM",
#             "xp_CM_SEM", "yp_CM_SEM", "zp_CM_SEM", "pxp_CM_SEM", "pyp_CM_SEM", "pzp_CM_SEM",
#             "s1_CM_SEM", "s2_CM_SEM", "s3_CM_SEM", "s4_CM_SEM", "francis");
#   
#   VNAMES = list(names, names_2)
#   VNCOL  = c(37, 38)
#   
#   proj_map_source = dffbinaryv(filename, VNCOL, VNAMES)
# }else
# {
#   proj_map_source = data.frame()
# }

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
