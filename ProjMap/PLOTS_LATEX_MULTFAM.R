# PLOTS_LATEX_MULTFAM.R
# Same as PLOTS_LATEX.R but for continued families at t0 = 0.99T
#


#===============================================================================
# Plot : tiles (pmin_dist_SEM) in the s1_CMU_EM/s3_CMU_EM space, 
# with continutation families
# PLOTS_LATEX.R must be loaded before, to get ppEM
#===============================================================================
if(isMULTFAM && !empty(proj_cont_fam))
{
  #Filename
  filename = paste0(filepre, "_s1_s3_pEM_cont_fam")
  
  #Adding some continuation results
  ppEM_fam = ppEM
  for(IND in seq(1, length(FAMVEC)))
  {
    FAM = FAMVEC[IND]
    #---------------------------------------------------------------------------
    # One family
    #---------------------------------------------------------------------------
    condition = proj_cont_fam$family == FAM
    one_family  = proj_cont_fam[which(condition),]
    
    #---------------------------------------------------------------------------
    # We select some solutions within this family:
    # First, we get rid of some solutions at the beginning and at the end.
    # Then, we get rid of some solutions at the center, to make things clearer
    # on the plots
    #---------------------------------------------------------------------------
    # label_min = lab_min_vec[FAM]
    # label_max = max(one_family$label) - lab_max_vec[FAM]
    # 
    # minmaxlabel = one_family$label > label_min & one_family$label < label_max
    # one_family  = one_family[which(minmaxlabel),]
    
    #---------------------------------------------------------------------------
    # Plots
    #---------------------------------------------------------------------------
    ppEM_fam = ppEM_fam + geom_point(data = one_family, aes(s1_CMU_EM, s3_CMU_EM), color = "white", size = 2.2)
    ppEM_fam = ppEM_fam + geom_point(data = one_family, aes(s1_CMU_EM, s3_CMU_EM), color = values[IND], size = 1.2)
  }
  
  
  # Limits, if necessary
  # ppEM = ppEM + scale_x_continuous(limits = c(-35, 35), breaks = seq(-42,42,6))
  # ppEM = ppEM + scale_y_continuous(limits = c(-35, 35), breaks = seq(-42,42,6)) 
  
  #Save in png
  ppEM_fam = ppEM_fam + labs(x = s1_exp, y = s3_exp)
  ppEM_fam = ppEM_fam + scg_pem_guide_false
  ggsave(ppEM_fam, width = xSize, height = ySize,  file = paste0(filename, ".png")) #Save png
  
  # Save in pdf
  ggsave(ppEM_fam, width = xSize, height = ySize,  bg = "transparent", device=cairo_pdf, file = paste0(filename, ".pdf")) #Save in pdf
}

#===============================================================================
# Plot : points (pmin_dist_SEM) in the x0_CMU_NCEM/y0_CMU_NCEM space
# with continutation families
#===============================================================================
if(isMULTFAM && !empty(proj_cont_fam))
{
  #Filename
  filename = paste0(filepre, "_x0_y0_pEM_cont_fam")
  
  #Adding some continuation results
  pp_x0y0_pEM_fam = pp_x0y0_pEM
  for(IND in seq(1, length(FAMVEC)))
  {
    FAM = FAMVEC[IND]
    #---------------------------------------------------------------------------
    # One family
    #---------------------------------------------------------------------------
    condition = proj_cont_fam$family == FAM
    one_family  = proj_cont_fam[which(condition),]
    
    #---------------------------------------------------------------------------
    # We select some solutions within this family:
    # First, we get rid of some solutions at the beginning and at the end.
    # Then, we get rid of some solutions at the center, to make things clearer
    # on the plots
    #---------------------------------------------------------------------------
    # label_min = lab_min_vec[FAM]
    # label_max = max(one_family$label) - lab_max_vec[FAM]
    # 
    # minmaxlabel = one_family$label > label_min & one_family$label < label_max
    # one_family  = one_family[which(minmaxlabel),]
    
    #---------------------------------------------------------------------------
    # Plots
    #---------------------------------------------------------------------------
    pp_x0y0_pEM_fam = pp_x0y0_pEM_fam + geom_point(data = one_family, aes(x0_CMU_NCEM, y0_CMU_NCEM), color = "white", size = 2.2)
    pp_x0y0_pEM_fam = pp_x0y0_pEM_fam + geom_point(data = one_family, aes(x0_CMU_NCEM, y0_CMU_NCEM), color = values[IND], size = 1.2)
  }
  
  #-------------------------------------------------------------------------------
  #Adding some specific points (for ISSFD)
  #-------------------------------------------------------------------------------
  x0 = c(-0.0503021637013195,  0.0570779010089832, -0.0598335194863945, -0.112560549310534, -0.0663892631216)
  y0 = c(0.173076575031959, -0.0204216068823488, -0.246786030643921, -0.550123064539671, -0.242757702271792)
  some_points = data.frame(x0 = x0, y0 = y0);
  pp_x0y0_pEM_fam = pp_x0y0_pEM_fam + geom_point(data = some_points, aes(x0, y0), color = "white", size = 6)
  # In black
  pp_x0y0_pEM_fam = pp_x0y0_pEM_fam + geom_point(data = some_points, aes(x0, y0), color = "black", size = 4)
  # or specific colors
  for(i in seq(1, NFAM, 1))
  {
    pp_x0y0_pEM_fam = pp_x0y0_pEM_fam + geom_point(data = some_points[i,], aes(x0, y0), color = values[i], size = 4)
  }
  
  # Limits, if necessary
  # pp_x0y0_pEM_fam = pp_x0y0_pEM_fam + scale_x_continuous(limits = c(-35, 35), breaks = seq(-42,42,6))
  # pp_x0y0_pEM_fam = pp_x0y0_pEM_fam + scale_y_continuous(limits = c(-35, 35), breaks = seq(-42,42,6)) 
  
  #Save in png
  pp_x0y0_pEM_fam = pp_x0y0_pEM_fam + labs(x = x_em, y = y_em)
  pp_x0y0_pEM_fam = pp_x0y0_pEM_fam + scg_pem_guide_false
  ggsave(pp_x0y0_pEM_fam, width = xSize, height = ySize,  file = paste0(filename, ".png")) #Save png
  
  # Save in pdf
  #ggsave(pp_x0y0_pEM_fam, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".pdf")) #Save in pdf
}

#===============================================================================
# CONTINUATION: Only some solutions, for all families in proj_cont_fam
#===============================================================================
label.frequency = 14
FAM        = FAMVEC[FAMIND]
if(isMULTFAM && !empty(traj_cont_fam))
{
  filename = paste0(filecont, "_x0_y0_NCSEM_cont_fam", FAM)
  
  #---------------------------------------------------------------------------
  # We select one family
  #---------------------------------------------------------------------------
  condition = traj_cont_fam$family == FAM
  one_family = traj_cont_fam[which(condition),]
  
  #---------------------------------------------------------------------------
  # We select some solutions within this family:
  # First, we get rid of some solutions at the beginning and at the end.
  # Then, we get rid of some solutions at the center, to make things clearer
  # on the plots
  #---------------------------------------------------------------------------
  label_min = lab_min_vec[FAM]
  label_max = max(one_family$label) - lab_min_vec[FAM]
  
  minmaxlabel = one_family$label > label_min & one_family$label < label_max
  one_family  = one_family[which(minmaxlabel),]
  
  condition      = (one_family$label) %% label.frequency == 0
  traj_cont_some = one_family[which(condition),]
  
  #---------------------------------------------------------------------------
  # If FAM == 4, we get rid of some solutions, because there are too much of them
  #---------------------------------------------------------------------------
  if(FAM == 4)
  {
    # First, small postprocess
    traj_cont_some$x_CMS_SEM = -CST_GAMMA_LIB_SEM*(traj_cont_some$x_CMS_NCSEM - CST_C1_LIB_SEM)
    traj_cont_some$y_CMS_SEM = -CST_GAMMA_LIB_SEM*(traj_cont_some$y_CMS_NCSEM - 0)
    traj_cont_some$z_CMS_SEM = +CST_GAMMA_LIB_SEM*(traj_cont_some$z_CMS_NCSEM - 0)
    
    # In label_cont_f, we select the trajectories that match the condition on the final x/y
    traj_cont_f  = ddply(traj_cont_some, .(label), compute_last_point)
    condition    = traj_cont_f$y_CMS_SEM_F > -0.005 &  traj_cont_f$y_CMS_SEM_F < +0.005 & 
      traj_cont_f$x_CMS_SEM_F > -1.008 &  traj_cont_f$x_CMS_SEM_F < -1.006  
    label_cont_f = traj_cont_f[which(condition),]
    
    # We isolate the solutions that are in label_cont_f solutions and select some of them
    condition      = traj_cont_some$label %in% label_cont_f$label
    traj_cont_sel = traj_cont_some[which(condition),]
    traj_cont_sel = traj_cont_sel[which(traj_cont_sel$label %% 12 == 0),]
    
    # We get rid of them in traj_cont_some
    traj_cont_some = traj_cont_some[which(!condition),]
  }
  
  #---------------------------------------------------------------------------
  # Plotting
  #---------------------------------------------------------------------------
  #fpp_path_some(traj_cont_some, filename, values[FAMIND]) (old version)
  
  fpp_path_traj(traj_cont_some, filename, "SEM", 
                limits.x = c(-1.5, 0.3), limits.y = c(-1.1, +1.1),
                xlab = x_sem, ylab = y_sem,
                xlab.tex = "$x^{sem}$", ylab.tex = "$y^{sem}$",
                colour.traj = values[FAMIND], 
                lib.point.em = LIB_POINT_EM, 
                lib.point.sem = LIB_POINT_SEM)
}
