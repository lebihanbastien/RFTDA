################################################################################
# R script used in EML2_TO_SEML.R for the plotting 
# of a projection map (connections from EML2 to SEML1,2)
#
# WARNING: MAIN.R must be loaded once before.
#
################################################################################

#-----------------------------------------------------------------------------
# Max in min labels
# Min: #10 5 0 0  0 0 10
# Max: #15 5 0 15 0 0 15
#-----------------------------------------------------------------------------
# label_min = 0 
# label_max = max(proj_cont_label$label) - 0 
# 
# minmaxlabel = traj_cont$label > label_min & traj_cont$label < label_max
# traj_cont = traj_cont[which(minmaxlabel),]
# 
# minmaxlabel = proj_cont$label > label_min & proj_cont$label < label_max
# proj_cont = proj_cont[which(minmaxlabel),]


#===============================================================================
# POINTS : pmin_dist_SEM in the x0_CMU_NCEM/y0_CMU_NCEM space
#===============================================================================
pp_x0EM_y0EM_eP = plotdf_point(proj_map_tem, "x0_CMU_NCEM", "y0_CMU_NCEM", 
                               "x0 (EML2)", "y0 (EML2)", 
                               "pmin_dist_SEM", "pmin_dist_SEM", 0, pointSize = 0.5)
pp_x0EM_y0EM_eP = pp_x0EM_y0EM_eP + ggtitle_t0
pp_x0EM_y0EM_eP = pp_x0EM_y0EM_eP + scg_pem

pp_x0EM_y0EM_eP

#Adding some continuation results
if(!empty(proj_cont))
{
  pp_x0EM_y0EM_eP = pp_x0EM_y0EM_eP + geom_path(data = proj_cont, aes(x0_CMU_NCEM,  y0_CMU_NCEM) , color = "black", size = 1)
}

#Display
pp_x0EM_y0EM_eP

#-------------------------------------------------------------------------------
# Plot : tiles (pmin_dist_SEM) in the s1_CMU_EM/s3_CMU_EM space
#-------------------------------------------------------------------------------
pt_s1EM_s3EM_eP = plotdf_tile_1(proj_map_tem, "s1_CMU_EM", "s3_CMU_EM", 
                                s1_exp, s3_exp, "pmin_dist_SEM", "pmin_dist_SEM",
                                FALSE)
if(LIB_POINT_EM == "L2")
{
  pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + scale_x_continuous(breaks = seq(-42,42,6))
  pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + scale_y_continuous(breaks = seq(-42,42,6)) 
}else
{
  pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + scale_x_continuous(breaks = seq(-4,4,0.5))
  pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + scale_y_continuous(breaks = seq(-4,4,0.5)) 
}
pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + ggtitle_t0
pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + scg_pem
pt_s1EM_s3EM_eP

# Get the collisions with the Moon
#pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + geom_point(data = proj_map_tem_coll[which(proj_map_tem_coll$collision == 301),], aes(s1_CMU_EM, s3_CMU_EM), color = "gray", size = 5)
pt_s1EM_s3EM_eP

# Get the collisions with the Earth
#pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + geom_point(data = proj_map_tem_coll[which(proj_map_tem_coll$collision == 399),], aes(s1_CMU_EM, s3_CMU_EM), color = "red", size = 5)
pt_s1EM_s3EM_eP


# Plot : tiles (pmin_dist_SEM) in the s1_CMU_EM/s3_CMU_EM space, with continuation
#-------------------------------------------------------------------------------
pt_s1EM_s3EM_eP_cont = pt_s1EM_s3EM_eP

#Adding some continuation results
pt_s1EM_s3EM_eP_cont = pt_s1EM_s3EM_eP_cont + geom_point(data = proj_cont, aes(s1_CMU_EM, s3_CMU_EM), color = "black", size = 2)
pt_s1EM_s3EM_eP_cont

#Adding some specific points
#s1 = c(-19.6734010192183, -17.1501764344739, 18.0368297229752, 8.33528384435544, 16.4521052628656)#, -3.276492877366309e+01, -3.043854834129248e+01, +3.218682800989279e+01, +3.435177734715551e+01, +2.287726449350953e+01, +2.092962158553479e+01)
#s3 = c(1.11039067757786, 11.8945781333513, -32.5007644319156, -1.19257336959027, 15.5416792554699)#, +2.693265287540921e+01, +2.879570444016836e-01, +3.303849815659568e+01, -1.033936728130126e+01, -2.731602874014531e+01, -3.446251071019438e+01)
#some_points = data.frame(s1_CMU_EM = s1, s3_CMU_EM = s3);
#pt_s1EM_s3EM_eP_cont = pt_s1EM_s3EM_eP_cont + geom_point(data = some_points, aes(s1, s3), color = "red", size = 5)
#pt_s1EM_s3EM_eP_cont

#-------------------------------------------------------------------------------
# Plot : tiles (pmin_dist_SEM) in the s1_CMU_EM/s3_CMU_EM space
#-------------------------------------------------------------------------------
pt_s1EM_t0EM_eP_comp = plotdf_tile_1(proj_map, "s1_CMU_EM", "t0_CMU_EMT", 
                                    s1_exp, "t0 (x T)", "pmin_dist_SEM", "pmin_dist_SEM",
                                    FALSE)
pt_s1EM_t0EM_eP_comp

#-------------------------------------------------------------------------------
# Plot : tiles (pmin_dist_SEM) in the s1_CMU_EM/s3_CMU_EM space
#-------------------------------------------------------------------------------
vcross  = c("2", "2.2", "3", "3.1", "4", "4.2", "5", "6") 
vcolors = brewer.pal(8,"Dark2")
vmatchs = c("2" = vcolors[1], "2.2" = vcolors[2], 
            "3" = vcolors[3], "3.1" = vcolors[4], "4" = vcolors[5],
            "4.2" = vcolors[6], "5" = vcolors[7], "6" = vcolors[8])

condition = proj_map_tem$crossings %in% vcross & proj_map_tem$collision == 0
pt_s1EM_s3EM_cross = plotdf_point(proj_map_tem[which(condition),], 
                                  "s1_CMU_EM", "s3_CMU_EM", 
                                  s1_exp, s3_exp, 
                                  "crossings", "crossings", 
                                  TRUE, pointSize = 3)
pt_s1EM_s3EM_cross = pt_s1EM_s3EM_cross + scale_colour_manual("Crossings", values = vmatchs, breaks = vcross) 

if(LIB_POINT_EM == "L2")
{
  pt_s1EM_s3EM_cross = pt_s1EM_s3EM_cross + scale_x_continuous(breaks = seq(-42,42,6))
  pt_s1EM_s3EM_cross = pt_s1EM_s3EM_cross + scale_y_continuous(breaks = seq(-42,42,6)) 
}else
{
  pt_s1EM_s3EM_cross = pt_s1EM_s3EM_cross + scale_x_continuous(breaks = seq(-4,4,0.5))
  pt_s1EM_s3EM_cross = pt_s1EM_s3EM_cross + scale_y_continuous(breaks = seq(-4,4,0.5)) 
}

#Adding some continuation results
if(!empty(proj_cont))
{
  pt_s1EM_s3EM_cross = pt_s1EM_s3EM_cross + geom_point(data = proj_cont, aes(s1_CMU_EM, s3_CMU_EM), color = "black", size = 2)
  pt_s1EM_s3EM_cross
}

#pt_s1EM_s3EM_cross = pt_s1EM_s3EM_cross + ggtitle_t0
pt_s1EM_s3EM_cross

if(ISSAVED)
{
  #Save in pdf
  filename = paste0(filepre, "_s1_s3_crossings")
  ggsave(pt_s1EM_s3EM_cross, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf")) #Save in pdf

  # Save in latex
  pt_s1EM_s3EM_cross = pt_s1EM_s3EM_cross + labs(x = "$s_1$", y = "$s_3$")
  ggplot2tikz(pt_s1EM_s3EM_cross, width = xSize, height = ySize, file = paste0(filename, "_tex.tex"))
}

#===============================================================================
# Energy
#===============================================================================
if("H0_EM" %in% colnames(proj_map_tem))
{
  #-------------------------------------------------------------------------------
  # Plot : tiles (energy) in the s1_CMU_EM/s3_CMU_EM space
  #-------------------------------------------------------------------------------
  midpoint_dH0_EM = mean(proj_map_tem$dH0_EM)
  pt_s1EM_s3EM_dH0_EM = plotdf_tile_1(proj_map_tem, "s1_CMU_EM", "s3_CMU_EM", 
                                 s1_exp, s3_exp, "dH0_EM", 
                                 "dH0_EM", FALSE, 
                                 colorLimits = projection_color_lim, na.value = "white")
  
  pt_s1EM_s3EM_dH0_EM = pt_s1EM_s3EM_dH0_EM + scale_colour_gradient2(space="Lab", midpoint= midpoint_dH0_EM, guide = FALSE)
  pt_s1EM_s3EM_dH0_EM = pt_s1EM_s3EM_dH0_EM + scale_fill_gradient2("dH0_EM", space="Lab", midpoint = midpoint_dH0_EM)
  pt_s1EM_s3EM_dH0_EM
  
  #-------------------------------------------------------------------------------
  # Plot : tiles (energy) in the s1_CMU_EM/s3_CMU_EM space
  #-------------------------------------------------------------------------------
  midpoint_dHf_EM  = mean(proj_map_tem$dHf_EM)
  pt_s1EM_s3EM_dHf_EM = plotdf_tile_1(proj_map_tem, "s1_CMU_EM", "s3_CMU_EM", 
                                      s1_exp, s3_exp, "dHf_EM", 
                                      "dHf_EM", FALSE, 
                                      colorLimits = projection_color_lim, na.value = "white")
  
  pt_s1EM_s3EM_dHf_EM = pt_s1EM_s3EM_dHf_EM + scale_colour_gradient2(space="Lab", midpoint= midpoint_dHf_EM, guide = FALSE)
  pt_s1EM_s3EM_dHf_EM = pt_s1EM_s3EM_dHf_EM + scale_fill_gradient2("dHf_EM", space="Lab", midpoint = midpoint_dHf_EM)
  pt_s1EM_s3EM_dHf_EM
}

#-------------------------------------------------------------------------------
# Plot : tiles (pmin_dist_SEM) in the s1_CMU_EM/s3_CMU_EM space
#-------------------------------------------------------------------------------
if(!empty(proj_map_tem_s2s4))
{
  pt_s2EM_s4EM_eP = plotdf_tile_1(proj_map_tem_s2s4, "s2_CMU_EM", "s4_CMU_EM", 
                                  s2_exp, s4_exp, "pmin_dist_SEM", "pmin_dist_SEM",
                                  FALSE)
  pt_s2EM_s4EM_eP = pt_s2EM_s4EM_eP + ggtitle_t0
  pt_s2EM_s4EM_eP = pt_s2EM_s4EM_eP + scg_pem
  pt_s2EM_s4EM_eP
}

#-------------------------------------------------------------------------------
# Plot : tiles (sf_CM_SEM) in the s1_CMU_EM/s3_CMU_EM space
#-------------------------------------------------------------------------------
pp_tiles_s1EM_s3EM_rf = plotdf_tile_n(proj_map_tem, "s1_CMU_EM", "s3_CMU_EM", s1_exp, s3_exp, "sf_CM_SEM", "Radius at final point", 0.4, TRUE)
pp_tiles_s1EM_s3EM_rf = pp_tiles_s1EM_s3EM_rf + ggtitle_t0
pp_tiles_s1EM_s3EM_rf = pp_tiles_s1EM_s3EM_rf + scale_x_continuous(limits = c(-35, 35), breaks = seq(-34,34,4))
pp_tiles_s1EM_s3EM_rf = pp_tiles_s1EM_s3EM_rf + scale_y_continuous(limits = c(-35, 35), breaks = seq(-34,34,4)) 
pp_tiles_s1EM_s3EM_rf

# Plot : tiles (tof_EM) in the s1_CMU_EM/s3_CMU_EM space
#-------------------------------------------------------------------------------
pt_s1EM_s3EM_tof_EM = plotdf_tile_1(proj_map_tem, "s1_CMU_EM", "s3_CMU_EM", s1_exp, s3_exp, "tof_EM", tof_exp, isLegendOn = TRUE)
pt_s1EM_s3EM_tof_EM = pt_s1EM_s3EM_tof_EM + scale_colour_gradient2(space="Lab", midpoint= 5, guide = FALSE)
pt_s1EM_s3EM_tof_EM = pt_s1EM_s3EM_tof_EM + scale_fill_gradient2(expression(TOF~(xT)), space="Lab", midpoint= 5)
pt_s1EM_s3EM_tof_EM = pt_s1EM_s3EM_tof_EM + ggtitle(paste0("t = ", toString(t0_CMU_EM_vec[time_index]/CST_SEM_PERIOD_EM), "T"))
pt_s1EM_s3EM_tof_EM = pt_s1EM_s3EM_tof_EM + scale_x_continuous(limits = c(-35, 35), breaks = seq(-34,34,4))
pt_s1EM_s3EM_tof_EM = pt_s1EM_s3EM_tof_EM + scale_y_continuous(limits = c(-35, 35), breaks = seq(-34,34,4)) 

#Adding some continuation results
if(!empty(proj_cont))
{
  pt_s1EM_s3EM_tof_EM = pt_s1EM_s3EM_tof_EM + geom_path(data = proj_cont, aes(s1_CMU_EM,  s3_CMU_EM) , color = "black", size = 1)
}

#Display
pt_s1EM_s3EM_tof_EM

#===============================================================================
# Plot : Energy, if there is such data 
#===============================================================================
if(!empty(proj_cont) && "H0_NCEM" %in% colnames(proj_cont))
{
  pH = ggplot() + geom_point(data = proj_cont, aes(s1_CMU_EM, H0_NCEM - mean(H0_NCEM)), color = "black", size = 2)
  pH = pH + custom_theme
  pH
}


#===============================================================================
# CONTINUATION: display the solution in NCSEM coordinates
#===============================================================================
if(!empty(traj_cont))
{
  # Routine to select the last point in a given dataframe (maximum time)
  #-------------------------------------------------------------------------------
  compute_last_point <- function(df)
  {
    summarize(df,
              x_CMS_SEM_F   = df[which(df$t_CMU_SEM == max(df$t_CMU_SEM)),]$x_CMS_SEM,
              y_CMS_SEM_F   = df[which(df$t_CMU_SEM == max(df$t_CMU_SEM)),]$y_CMS_SEM,
              z_CMS_SEM_F   = df[which(df$t_CMU_SEM == max(df$t_CMU_SEM)),]$z_CMS_SEM,
              x_CMS_NCSEM_F = df[which(df$t_CMU_SEM == max(df$t_CMU_SEM)),]$x_CMS_NCSEM,
              y_CMS_NCSEM_F = df[which(df$t_CMU_SEM == max(df$t_CMU_SEM)),]$y_CMS_NCSEM,
              z_CMS_NCSEM_F = df[which(df$t_CMU_SEM == max(df$t_CMU_SEM)),]$z_CMS_NCSEM,
              t_CMS_SEM_F   = max(df$t_CMU_SEM))
  }
  
  
  
  
  #=============================================================================
  # Plot : only some solution in NCSEM coordinates
  #=============================================================================
  
  # Solutions with a given label
  #-----------------------------------------------------------------------------
  solfreq = switch(LIB_POINT_EM, "L1" = 5, "L2" = 14)
  traj_cont_some =  traj_cont[which(traj_cont$label %% solfreq == 0),]
  
  # # Solutions in a given range of final x/y points
  # #---------------------------------------------------------------------------
  # # In label_cont_f, we select the trajectories that match the condition on the final x/y
  # traj_cont_f  = ddply(traj_cont_some, .(label), compute_last_point)
  # condition    = traj_cont_f$y_CMS_SEM_F > -0.005 &  traj_cont_f$y_CMS_SEM_F < +0.005 & 
  #                traj_cont_f$x_CMS_SEM_F > -1.008 &  traj_cont_f$x_CMS_SEM_F < -1.006  
  # label_cont_f = traj_cont_f[which(condition),]
  # 
  # # We isolate the solutions that are in label_cont_f solutions and select some of them
  # condition      = traj_cont_some$label %in% label_cont_f$label
  # traj_cont_sel = traj_cont_some[which(condition),]
  # traj_cont_sel = traj_cont_sel[which(traj_cont_sel$label %% 12 == 0),]
  # 
  # # We get rid of them in traj_cont_some
  # traj_cont_some = traj_cont_some[which(!condition),]
  
  #-----------------------------------------------------------------------------
  # In SEM coordinates
  #-----------------------------------------------------------------------------
  psome_SEM = ggplot() + geom_path(data = traj_cont_some, 
                                   aes(x = x_CMS_SEM, y = y_CMS_SEM, group = label), 
                                   colour = "black", size = 0.4)
  # psome_SEM = psome_SEM + geom_path(data = traj_cont_sel, 
  #               aes(x = x_CMS_SEM, y = y_CMS_SEM, group = label), 
  #               colour = "red", size = 0.4)
  psome_SEM = psome_SEM + scale_colour_gradient(space="Lab", high = "black", low = "white", guide = FALSE)
  #Theme
  psome_SEM = psome_SEM + custom_theme
  psome_SEM = psome_SEM+ coord_fixed(ratio=1)
  #Add SEMLi
  psome_SEM = psome_SEM + geom_point(data = dfsemli, aes(x= x_SYS, y = y_SYS), size = 4) 
  #Add Earth
  psome_SEM = psome_SEM + geom_point(data = dfearth_seml, aes(x= x_SYS, y = y_SYS), size = 4) 
  #Labels
  psome_SEM = psome_SEM + labs(x = "X (SEM)", y = "Y (SEM)")
  #Display
  psome_SEM
  
  #-----------------------------------------------------------------------------
  # In NCSEM coordinates
  scaleFUN <- function(x) sprintf("%.1f", x)
  #-----------------------------------------------------------------------------
  psome_NCSEM = ggplot() + geom_path(data = traj_cont_some, 
                                   aes(x = x_CMS_NCSEM, y = y_CMS_NCSEM, group = label), 
                                   colour = "black", size = 0.4)
  
  # Highlight a single solution
  #   traj_cont_red = traj_cont_some[which(traj_cont_some$label == min(traj_cont_some$label)),]
  #   psome_NCSEM = psome_NCSEM + geom_path(data = traj_cont_red, 
  #                                         aes(x = x_CMS_NCSEM, y = y_CMS_NCSEM, group = label), 
  #                                         colour = "red", size = 0.8)
  psome_NCSEM = psome_NCSEM + scale_colour_gradient(space="Lab", high = "black", low = "white", guide = FALSE)
  #Theme
  psome_NCSEM = psome_NCSEM + custom_theme
  #psome_NCSEM = psome_NCSEM+ coord_fixed(ratio=1)
  #Add SEMLi
  psome_NCSEM = psome_NCSEM + geom_point(data = dfsemli, aes(x= x_NC, y = y_NC), size = 4) 
  #Add Earth
  psome_NCSEM = psome_NCSEM + geom_point(data = dfearth_seml, aes(x= x_NC, y = y_NC), size = 4) 
  #Labels
  psome_NCSEM = psome_NCSEM + labs(x = "X (NCSEM)", y = "Y (NCSEM)")
  #Breaks
  psome_NCSEM = psome_NCSEM + scale_x_continuous(breaks = seq(-1.2, 0.6, 0.1), labels=scaleFUN) 
  #Display
  psome_NCSEM
  
  # Final solution & zoom
  psome_NCSEM_zoom = psome_NCSEM + geom_point(data = proj_cont, 
                                            aes(x0_CMS_NCSEM,  y0_CMS_NCSEM), 
                                            color = "red", size = 1)
  psome_NCSEM_zoom = psome_NCSEM_zoom + scale_x_continuous(limits = c(0.2, 0.25))
  psome_NCSEM_zoom = psome_NCSEM_zoom + scale_y_continuous(limits = c(-0.15,0.4)) 
  psome_NCSEM_zoom
  
  
  #-----------------------------------------------------------------------------
  # In NCSEM coordinates, xz
  #-----------------------------------------------------------------------------
  psome_NCSEM_xz = ggplot() + geom_path(data = traj_cont_some, 
                                     aes(x = x_CMS_NCSEM, y = z_CMS_NCSEM, group = label), 
                                     colour = "black", size = 0.2)
  
  # Highlight a single solution
  #   traj_cont_red = traj_cont_some[which(traj_cont_some$label == min(traj_cont_some$label)),]
  #   psome_NCSEM_xz = psome_NCSEM_xz + geom_path(data = traj_cont_red, 
  #                                         aes(x = x_CMS_NCSEM, y = z_CMS_NCSEM, group = label), 
  #                                         colour = "red", size = 0.8)
  psome_NCSEM_xz = psome_NCSEM_xz + scale_colour_gradient(space="Lab", high = "black", low = "white", guide = FALSE)
  #Theme
  psome_NCSEM_xz = psome_NCSEM_xz + custom_theme
  psome_NCSEM_xz = psome_NCSEM_xz+ coord_fixed(ratio=1)
  #Add SEMLi
  psome_NCSEM_xz = psome_NCSEM_xz + geom_point(data = dfsemli, aes(x= x_NC, y = z_NC), size = 4) 
  #Add Earth
  psome_NCSEM_xz = psome_NCSEM_xz + geom_point(data = dfearth_seml, aes(x= x_NC, y = z_NC), size = 4) 
  #Labels
  psome_NCSEM_xz = psome_NCSEM_xz + labs(x = "X (NCSEM)", y = "Z (NCSEM)")
  #Display
  psome_NCSEM_xz
  
  #-----------------------------------------------------------------------------
  # In NCSEM coordinates, yz
  #-----------------------------------------------------------------------------
  psome_NCSEM_yz = ggplot() + geom_path(data = traj_cont_some, 
                                        aes(x = y_CMS_NCSEM, y = z_CMS_NCSEM, group = label), 
                                        colour = "black", size = 0.2)
  
  # Highlight a single solution
  #   traj_cont_red = traj_cont_some[which(traj_cont_some$label == min(traj_cont_some$label)),]
  #   psome_NCSEM_yz = psome_NCSEM_yz + geom_path(data = traj_cont_red, 
  #                                         aes(x = y_CMS_NCSEM, y = y_CMS_NCSEM, group = label), 
  #                                         colour = "red", size = 0.8)
  psome_NCSEM_yz = psome_NCSEM_yz + scale_colour_gradient(space="Lab", high = "black", low = "white", guide = FALSE)
  #Theme
  psome_NCSEM_yz = psome_NCSEM_yz + custom_theme
  psome_NCSEM_yz = psome_NCSEM_yz+ coord_fixed(ratio=1)
  #Add SEMLi
  psome_NCSEM_yz = psome_NCSEM_yz + geom_point(data = dfsemli, aes(x= y_NC, y = z_NC), size = 4) 
  #Add Earth
  psome_NCSEM_yz = psome_NCSEM_yz + geom_point(data = dfearth_seml, aes(x= y_NC, y = z_NC), size = 4) 
  #Labels
  psome_NCSEM_yz = psome_NCSEM_yz + labs(x = "Y (NCSEM)", y = "Z (NCSEM)")
  #Display
  psome_NCSEM_yz
  
  
  #-----------------------------------------------------------------------------
  # In NCEM coordinates
  #-----------------------------------------------------------------------------
  psome_EM  = ggplot() + geom_path(data = traj_cont_some, 
                                   aes(x = x_CMS_NCEM, y = y_CMS_NCEM, 
                                       group = label), colour = "black",
                                   size = 0.4)
  psome_EM = psome_EM + scale_colour_discrete(guide = FALSE)
  #Theme
  psome_EM = psome_EM + custom_theme
  psome_EM = psome_EM + coord_fixed(ratio=1)
  #Add EMLi
  psome_EM = psome_EM + geom_point(data = dfemli, aes(x= x_NC, y = y_NC), size = 4) 
  #Add Moon
  psome_EM = psome_EM + geom_point(data = dfmoon_eml, aes(x= x_NC, y = y_NC), size = 4) 
  #Labels
  psome_EM = psome_EM + labs(x = "x (EM)", y = "y (EM)")
  #Zoom
  psome_EM = psome_EM + scale_x_continuous(limits = c(-1.5, 2.5))
  psome_EM = psome_EM + scale_y_continuous(limits = c(-1.0, 1.0))
  #Display
  psome_EM
  
  
  #===============================================================================
  # POINTS : psome_EM + pp_x0EM_y0EM_eP
  #===============================================================================
  values = rev(brewer.pal(NFAM,"Dark2"))
  # Tiles
  #pp_x0EM_y0EM_some = plotdf_point(proj_map_tem, "x0_CMU_NCEM", "y0_CMU_NCEM", x_em, y_em, "pmin_dist_SEM", "pmin_dist_SEM", 0, pointSize = 3)
  
  # Adding plots
  pp_x0EM_y0EM_some = ggplot()+ geom_path(data = traj_cont_some[which(traj_cont_some$t_CMU_SEM < 0.85),], 
                                aes(x = x_CMS_NCEM, y = z_CMS_NCEM, 
                                    group = label), colour = values[1],
                                size = 0.8)
  pp_x0EM_y0EM_some
  
  #Zoom
  pp_x0EM_y0EM_some = pp_x0EM_y0EM_some + scale_x_continuous(limits = c(-0.3, 0.3))
  pp_x0EM_y0EM_some = pp_x0EM_y0EM_some + scale_y_continuous(limits = c(-0.75, 0.7))
  
  #Theme and titles
  #pp_x0EM_y0EM_some = pp_x0EM_y0EM_some + ggtitle_t0
  pp_x0EM_y0EM_some = pp_x0EM_y0EM_some + scg_pem_guide_false
  pp_x0EM_y0EM_some = pp_x0EM_y0EM_some + custom_theme
  pp_x0EM_y0EM_some = pp_x0EM_y0EM_some + coord_fixed(ratio=1)
  
  #Labels
  pp_x0EM_y0EM_some = pp_x0EM_y0EM_some + labs(x = x_em, y = y_em)
  
  #Add EMLi
  pp_x0EM_y0EM_some = pp_x0EM_y0EM_some + geom_point(data = dfemli, aes(x= x_NC, y = y_NC), size = 4) 
  pp_x0EM_y0EM_some = pp_x0EM_y0EM_some + annotate("text", x = 0, y = -0.08,  label = "bold(EML[2])", color = "white", size = 5, parse = TRUE)
  pp_x0EM_y0EM_some = pp_x0EM_y0EM_some + annotate("text", x = 0, y = -0.08,  label = "EML[2]", size = 5, parse = TRUE)
  
  #Add Moon
  pp_x0EM_y0EM_some = pp_x0EM_y0EM_some + geom_point(data = dfmoon_eml, aes(x= x_NC, y = y_NC), size = 5) 
  pp_x0EM_y0EM_some = pp_x0EM_y0EM_some + annotate("text", x = dfmoon_eml$x_NC, y = -0.08,  label = "Moon", size = 5, parse = TRUE)
  
  #Adding some continuation results
  if(!empty(proj_cont))
  {
    #pp_x0EM_y0EM_some = pp_x0EM_y0EM_some + geom_path(data = proj_cont, aes(x0_CMU_NCEM,  y0_CMU_NCEM) , color = "white", size = 2)
    #pp_x0EM_y0EM_some = pp_x0EM_y0EM_some + geom_path(data = proj_cont, aes(x0_CMU_NCEM,  y0_CMU_NCEM) , color = "black", size = 1)
    
  }
  
  #Display
  pp_x0EM_y0EM_some
  
  # Save
  #if(ISSAVED)
  #{
    # Save in pdf, in TIMES NEW ROMAN
    filename = paste0(filepre, "_x0EM_y0EM_some")
    pp_x0EM_y0EM_some = pp_x0EM_y0EM_some + issfd_theme
    ggsave(pp_x0EM_y0EM_some, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf")) #Save in pdf
    ggsave(pp_x0EM_y0EM_some, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".png")) #Save in png
  
    pp_x0EM_y0EM_some = pp_x0EM_y0EM_some + labs(x = "$x^{em}$", y = "$y^{em}$")
    pp_x0EM_y0EM_some = pp_x0EM_y0EM_some + annotate("text", x = 0, y = -0.75,  label = "EML$_2$", size = 5)
    ggplot2tikz(pp_x0EM_y0EM_some, width = xSize, height = ySize, file = paste0(filename, "_tex.tex"))
  #}
  
  
  
  #===============================================================================
  # POINTS : psome_SEM + pp_x0SEM_y0SEM_eP
  #===============================================================================
  # Adding plots
  pp_x0SEM_y0SEM_some = ggplot() + geom_path(data = traj_cont_some, 
                                                   aes(x = x_CMS_NCSEM, y = y_CMS_NCSEM, 
                                                       group = label), colour = muted("green"),
                                                   size = 0.4)
  #Zoom
  #   pp_x0SEM_y0SEM_some = pp_x0SEM_y0SEM_some + scale_x_continuous(limits = c(-0.7, 0.24))
  #   pp_x0SEM_y0SEM_some = pp_x0SEM_y0SEM_some + scale_y_continuous(limits = c(-1.2,1.2)) 
  
  #pp_x0SEM_y0SEM_some = pp_x0SEM_y0SEM_some + scale_x_continuous(limits = c(0.221, 0.238))
  #pp_x0SEM_y0SEM_some = pp_x0SEM_y0SEM_some + scale_y_continuous(limits = c(-0.12,0.35))
  
  #Theme and titles
  #pp_x0SEM_y0SEM_some = pp_x0SEM_y0SEM_some + ggtitle_t0
  pp_x0SEM_y0SEM_some = pp_x0SEM_y0SEM_some + scg_pem_guide_false
  pp_x0SEM_y0SEM_some = pp_x0SEM_y0SEM_some + custom_theme
  #pp_x0SEM_y0SEM_some = pp_x0SEM_y0SEM_some + coord_fixed(ratio=1)
  
  #Add SEMLi
  pp_x0SEM_y0SEM_some = pp_x0SEM_y0SEM_some + geom_point(data = dfsemli, aes(x= x_NC, y = y_NC), size = 4) 
  pp_x0SEM_y0SEM_some = pp_x0SEM_y0SEM_some + annotate("text", x = 0, y = -0.1,  label = "SEML[2]", size = 5, parse = TRUE)
  
  #Adding some continuation results
  if(!empty(proj_cont))
  {
    pp_x0SEM_y0SEM_some = pp_x0SEM_y0SEM_some + geom_path(data = proj_cont, aes(x0_CMS_NCSEM,  y0_CMS_NCSEM) , color = "white", size = 2)
    pp_x0SEM_y0SEM_some = pp_x0SEM_y0SEM_some + geom_path(data = proj_cont, aes(x0_CMS_NCSEM,  y0_CMS_NCSEM) , color = "black", size = 1)
  }
  
  #Labels
  pp_x0SEM_y0SEM_some = pp_x0SEM_y0SEM_some + labs(x = x_sem, y = y_sem)
  
  
  #Display
  pp_x0SEM_y0SEM_some
  
  # Save
  if(ISSAVED)
  {
    # Save in pdf
    filename = paste0(filepre, "_x0SEM_y0SEM_some")
    ggsave(pp_x0SEM_y0SEM_some, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf")) #Save in pdf
  }
  
  
  
  #=============================================================================
  # Isolated solution
  #=============================================================================
  traj_cont_1 =  traj_cont[which(traj_cont$label == max(traj_cont$label)),]
  
  #-----------------------------------------------------------------------------
  # In SEM coordinates
  #-----------------------------------------------------------------------------
  ppsol_SEM_xy  = ggplot() + geom_path(data = traj_cont_1, 
                                    aes(x = x_CMS_SEM, y = y_CMS_SEM, 
                                        group = interaction(label, type), colour = factor(type)), 
                                    size = 1)
  
  #Theme
  ppsol_SEM_xy = ppsol_SEM_xy + custom_theme
  ppsol_SEM_xy = ppsol_SEM_xy + coord_fixed(ratio=1)
  ppsol_SEM_xy = ppsol_SEM_xy + scale_colour_discrete(guide = FALSE)
  
  #Add SEMLi
  ppsol_SEM_xy = ppsol_SEM_xy + geom_point(data = dfsemli, aes(x= x_SYS, y = y_SYS), size = 4) 
  #Add Earth
  ppsol_SEM_xy = ppsol_SEM_xy + geom_point(data = dfearth_seml, aes(x= x_SYS, y = y_SYS), size = 4) 
  #Labels
  ppsol_SEM_xy = ppsol_SEM_xy + labs(x = "X (SEM)", y = "Y (SEM)")
  #Display
  ppsol_SEM_xy
  
  
  #-----------------------------------------------------------------------------
  # In SEM coordinates, xz
  #-----------------------------------------------------------------------------
  ppsol_SEM_xz  = ggplot() + geom_path(data = traj_cont_1, 
                                       aes(x = x_CMS_SEM, y = z_CMS_SEM, 
                                           group = interaction(label, type), colour = factor(type)), 
                                       size = 1)
  
  #Theme
  ppsol_SEM_xz = ppsol_SEM_xz + custom_theme
  ppsol_SEM_xz = ppsol_SEM_xz + coord_fixed(ratio=1)
  ppsol_SEM_xz = ppsol_SEM_xz + scale_colour_discrete(guide = FALSE)
  
  #Add SEMLi
  ppsol_SEM_xz = ppsol_SEM_xz + geom_point(data = dfsemli, aes(x= x_SYS, y = z_SYS), size = 4) 
  #Add Earth
  ppsol_SEM_xz = ppsol_SEM_xz + geom_point(data = dfearth_seml, aes(x= x_SYS, y = z_SYS), size = 4) 
  #Labels
  ppsol_SEM_xz = ppsol_SEM_xz + labs(x = "X (SEM)", y = "Z (SEM)")
  #Display
  ppsol_SEM_xz
  
  
  #-----------------------------------------------------------------------------
  # In SEM coordinates, yz
  #-----------------------------------------------------------------------------
  ppsol_SEM_yz  = ggplot() + geom_path(data = traj_cont_1, 
                                       aes(x = y_CMS_SEM, y = z_CMS_SEM, 
                                           group = interaction(label, type), colour = factor(type)), 
                                       size = 1)
  
  #Theme
  ppsol_SEM_yz = ppsol_SEM_yz + custom_theme
  ppsol_SEM_yz = ppsol_SEM_yz + coord_fixed(ratio=1)
  ppsol_SEM_yz = ppsol_SEM_yz + scale_colour_discrete(guide = FALSE)
  
  #Add SEMLi
  ppsol_SEM_yz = ppsol_SEM_yz + geom_point(data = dfsemli, aes(x= y_SYS, y = z_SYS), size = 4) 
  #Add Earth
  ppsol_SEM_yz = ppsol_SEM_yz + geom_point(data = dfearth_seml, aes(x= y_SYS, y = z_SYS), size = 4) 
  #Labels
  ppsol_SEM_yz = ppsol_SEM_yz + labs(x = "Y (SEM)", y = "Z (SEM)")
  #Display
  ppsol_SEM_yz
  
  
  #-----------------------------------------------------------------------------
  # In NCEM coordinates
  #-----------------------------------------------------------------------------
  ppsol_NCEM  = ggplot() + geom_path(data = traj_cont_1, 
                                     aes(x = x_CMS_NCEM, y = y_CMS_NCEM, 
                                         group = interaction(label, type), colour = factor(type)), 
                                     size = 0.6)
  ppsol_NCEM = ppsol_NCEM + scale_colour_discrete(guide = FALSE)
  #Theme
  ppsol_NCEM = ppsol_NCEM + custom_theme
  ppsol_NCEM = ppsol_NCEM+ coord_fixed(ratio=1)
  #Add EMLi
  ppsol_NCEM = ppsol_NCEM + geom_point(data = dfemli, aes(x= x_NC, y = y_NC), size = 2) 
  #Add Earth
  ppsol_NCEM = ppsol_NCEM + geom_point(data = dfearth_eml, aes(x= x_NC, y = y_NC), size = 4) 
  #Add Moon
  ppsol_NCEM = ppsol_NCEM + geom_point(data = dfmoon_eml, aes(x= x_NC, y = y_NC), size = 4) 
  #Labels
  ppsol_NCEM = ppsol_NCEM + labs(x = "x (EM)", y = "y (EM)")
  #Zoom
  ppsol_NCEM = ppsol_NCEM + scale_x_continuous(limits = c(-10, 2)) #c(-0.25, 0.25)
  ppsol_NCEM = ppsol_NCEM + scale_y_continuous(limits = c(-2, 2)) #c(-0.25, 0.25)
  
  #Display
  ppsol_NCEM
  
  #-----------------------------------------------------------------------------
  # Energy
  #-----------------------------------------------------------------------------
  ppsol_H  = ggplot() + geom_path(data = traj_cont_some, 
                                  aes(x = t_CMU_SEM, y = H_NCSEM, 
                                      group = interaction(label, type), colour = factor(type)), 
                                  size = 0.5)
  #Theme
  ppsol_H = ppsol_H + custom_theme
  ppsol_H = ppsol_H + scale_colour_discrete(guide = FALSE)
  #Labels
  ppsol_H = ppsol_H + labs(x = "t (SEM)", y = "H (NCSEM)")
  #Display
  ppsol_H
  
  
  #=============================================================================
  # ALL found solutions as a "continuum"
  #=============================================================================
  traj_cont_some =  traj_cont#[which(traj_cont$label < 2),]
  pp_path_cont = ggplot() + geom_path(data = traj_cont_some, aes(x = x_CMS_SEM, y = y_CMS_SEM, colour = label, group = factor(label)), size = 1.5)
  pp_path_cont = pp_path_cont + scale_colour_gradient2("label", space="Lab", midpoint = max(traj_cont_some$label)/2, mid = "white", high = muted("blue"))
  pp_path_cont = pp_path_cont + custom_theme 
  #Add SEMLi
  pp_path_cont = pp_path_cont + geom_point(data = dfsemli, aes(x= x_SYS, y = y_SYS), size = 4) 
  #Labels
  pp_path_cont = pp_path_cont + labs(x = "X (SEM)", y = "Y (SEM)")
  #Display
  pp_path_cont
}

#===============================================================================
# TILES: plots that study the influence of the initial time t0
#===============================================================================
#-------------------------------------------------------------------------------
# Plot : tiles (pmin_dist_SEM) in the t0_CMU_EMT/s3_CMU_EM space
#-------------------------------------------------------------------------------
pp_tiles_t0_s3EM_eP = plotdf_tile_1(proj_map_s1, "t0_CMU_EMT", "s3_CMU_EM", "t0/T", "s3 (EML2)", "pmin_dist_SEM", "Projection \ndistance", TRUE, colorLimits = projection_color_lim)
pp_tiles_t0_s3EM_eP = pp_tiles_t0_s3EM_eP + scale_x_continuous(breaks = seq(0,1,0.01))
pp_tiles_t0_s3EM_eP = pp_tiles_t0_s3EM_eP + scale_y_continuous(limits = c(-35, 35), breaks = seq(-34,34,4))  
pp_tiles_t0_s3EM_eP


# Plot : tiles (pmin_dist_SEM) in the t0_CMU_EMT/s1_CMU_EM space
#-------------------------------------------------------------------------------
pp_tiles_t0_s1EM_eP = plotdf_tile_1(proj_map_s3, "t0_CMU_EMT", "s1_CMU_EM", "t0/T", "s1 (EML2)", "pmin_dist_SEM", "Projection \ndistance", TRUE, colorLimits = projection_color_lim)
pp_tiles_t0_s1EM_eP = pp_tiles_t0_s1EM_eP + scale_y_continuous(limits = c(-35, 35), breaks = seq(-34,34,4))  
pp_tiles_t0_s1EM_eP

#Adding some continuation results
if(!empty(proj_cont))
{
  pp_tiles_t0_s1EM_eP = pp_tiles_t0_s1EM_eP + geom_point(data = proj_cont, aes(t0_CMU_EMT,  s1_CMU_EM) , color = "black", size = 1)
}
pp_tiles_t0_s1EM_eP

#===============================================================================
# POINTS: SEML2 focus
#===============================================================================
#-------------------------------------------------------------------------------
# Plot : points (dHf_SEM) in the xf_CM_SEM/yf_CM_SEM space
#-------------------------------------------------------------------------------
midpoint_dHf_SEM  = mean(proj_map_tem$dHf_SEM)
pt_s1SEM_s3SEM_dHf_SEM  = plotdf_point(proj_map_tem, "xf_CM_SEM", "yf_CM_SEM", 
                                "xf_CM_SEM", "yf_CM_SEM", "dHf_SEM", "dHf_SEM", 
                                0, pointSize = 3)
pt_s1SEM_s3SEM_dHf_SEM = pt_s1SEM_s3SEM_dHf_SEM +scale_colour_gradient2("dHf_SEM", space="Lab", 
                                                            midpoint = midpoint_dHf_SEM, 
                                                            mid = "white", high = muted("blue"))

#Add SEMLi
pt_s1SEM_s3SEM_dHf_SEM = pt_s1SEM_s3SEM_dHf_SEM + geom_point(data = dfsemli, aes(x= x_SYS, y = y_SYS), size = 4) 

pt_s1SEM_s3SEM_dHf_SEM

#-------------------------------------------------------------------------------
# Plot : points (dH0_SEM) in the xf_CM_SEM/yf_CM_SEM space
#-------------------------------------------------------------------------------
midpoint_dH0_SEM  = mean(proj_map_tem$dH0_SEM)
pt_s1SEM_s3SEM_dH0_SEM  = plotdf_point(proj_map_tem, "xf_CM_SEM", "yf_CM_SEM", 
                                       "xf_CM_SEM", "yf_CM_SEM", "dH0_SEM", "dH0_SEM", 
                                       0, pointSize = 3)
pt_s1SEM_s3SEM_dH0_SEM = pt_s1SEM_s3SEM_dH0_SEM +scale_colour_gradient2("dHf_SEM", space="Lab", 
                                                                        midpoint = midpoint_dH0_SEM, 
                                                                        mid = "white", high = muted("blue"))

#Add SEMLi
pt_s1SEM_s3SEM_dH0_SEM = pt_s1SEM_s3SEM_dH0_SEM + geom_point(data = dfsemli, aes(x= x_SYS, y = y_SYS), size = 4) 

pt_s1SEM_s3SEM_dH0_SEM

#-------------------------------------------------------------------------------
# Plot : points (pmin_dist_SEM) in the s1_CM_SEM/s3_CM_SEM space
#-------------------------------------------------------------------------------
pt_s1SEM_s3SEM_eP = plotdf_point(proj_map_tem, "s1_CM_SEM", "s3_CM_SEM", "s1_CM_SEM", "s3_CM_SEM","pmin_dist_SEM", "pmin_dist_SEM", 0, pointSize = 3)
pt_s1SEM_s3SEM_eP = pt_s1SEM_s3SEM_eP +scale_colour_gradient2("pmin_dist_SEM", space="Lab", midpoint = projection_lim_mid, mid = "white", high = muted("blue"))
#Saturation of the constraint on s1_CM_SEM/s3_CM_SEM
pt_s1SEM_s3SEM_eP = pt_s1SEM_s3SEM_eP + geom_point(data = proj_map_tem[which(abs(proj_map_tem$ns_CM_SEM - SNMAX) < 0.001),], aes(s1_CM_SEM, s3_CM_SEM), color = "red", size = 5)
#Saturation of the constraint on s1_CMU_EM/s3_CMU_EM
pt_s1SEM_s3SEM_eP = pt_s1SEM_s3SEM_eP + geom_point(data = proj_map_tem[which(abs(proj_map_tem$ns_CMU_EM - GSIZEMAX) < 0.2),], aes(s1_CM_SEM, s3_CM_SEM), color = "green", size = 5)
pt_s1SEM_s3SEM_eP

#Adding some continuation results
if(!empty(proj_cont))
{
  pt_s1SEM_s3SEM_eP = pt_s1SEM_s3SEM_eP + geom_path(data = proj_cont, aes(s1_CMS_SEM, s3_CMS_SEM), color = "black", size = 1)
}

#Display
pt_s1SEM_s3SEM_eP

#-------------------------------------------------------------------------------
# Plot : points (pmin_dist_SEM) in the x0_CM_SEM/y0_CM_SEM space
#-------------------------------------------------------------------------------
pp_pts_x0SEM_y0SEM_eP = plotdf_point(proj_map_tem, "x0_CM_SEM", "y0_CM_SEM", "x0_CM_SEM", "y0_CM_SEM","pmin_dist_SEM", "pmin_dist_SEM", 0, pointSize = 3)
pp_pts_x0SEM_y0SEM_eP = pp_pts_x0SEM_y0SEM_eP + scale_colour_gradient2("pmin_dist_SEM", space="Lab", midpoint = projection_lim_mid, mid = "white", high = muted("blue"))
pp_pts_x0SEM_y0SEM_eP = pp_pts_x0SEM_y0SEM_eP + geom_point(data = proj_map_tem[which(abs(proj_map_tem$ns_CM_SEM - SNMAX)    < 0.001),], aes(x0_CM_SEM, y0_CM_SEM), color = "red", size = 2)
pp_pts_x0SEM_y0SEM_eP = pp_pts_x0SEM_y0SEM_eP + geom_point(data = proj_map_tem[which(abs(proj_map_tem$nf_CM_NCSEM - YNMAX) < 0.001),], aes(x0_CM_SEM, y0_CM_SEM), color = "magenta", size = 2)


#Add the points of arrival at SEML2
pp_pts_x0SEM_y0SEM_eP = pp_pts_x0SEM_y0SEM_eP + geom_point(data = proj_map_tem, aes(x= xf_CM_SEM, y = yf_CM_SEM, color = pmin_dist_SEM), size = 3)
pp_pts_x0SEM_y0SEM_eP = pp_pts_x0SEM_y0SEM_eP + geom_point(data = proj_map_tem[which(abs(proj_map_tem$ns_CM_SEM - SNMAX) < 0.001),], aes(xf_CM_SEM, yf_CM_SEM), color = "red", size = 2)
pp_pts_x0SEM_y0SEM_eP = pp_pts_x0SEM_y0SEM_eP + geom_point(data = proj_map_tem[which(abs(proj_map_tem$ns_CMU_EM - GSIZEMAX) < 0.2),], aes(xf_CM_SEM, yf_CM_SEM), color = "green", size = 2)
pp_pts_x0SEM_y0SEM_eP = pp_pts_x0SEM_y0SEM_eP + geom_point(data = proj_map_tem[which(abs(proj_map_tem$nf_CM_NCSEM - YNMAX) < 0.001),], aes(xf_CM_SEM, yf_CM_SEM), color = "magenta", size = 2)

#Add SEMLi
pp_pts_x0SEM_y0SEM_eP = pp_pts_x0SEM_y0SEM_eP + geom_point(data = dfsemli, aes(x= x_SYS, y = y_SYS), size = 4) 

#Adding some continuation results
if(!empty(proj_cont))
{
  pp_pts_x0SEM_y0SEM_eP = pp_pts_x0SEM_y0SEM_eP + geom_point(data = proj_cont, aes(x0_CMS_SEM,  y0_CMS_SEM) , color = "black", size = 1)
}

#Scaling
# pp_pts_x0SEM_y0SEM_eP = pp_pts_x0SEM_y0SEM_eP + scale_x_continuous(breaks = seq(-1.012, -1.0025, 0.001), limits = c(-1.012, -1.002)) 
# pp_pts_x0SEM_y0SEM_eP = pp_pts_x0SEM_y0SEM_eP + scale_y_continuous(limits = c(-0.005, +0.005)) 

#Display
pp_pts_x0SEM_y0SEM_eP

#===============================================================================
# Plots of points, using the event at x= cst in the continuation results
# stored in the dataframe named proj_cont
#===============================================================================
#-------------------------------------------------------------------------------
# Plot : points ye_CMS_NCSEM 
#-------------------------------------------------------------------------------
pp_pts_Pk = plotdf_point(proj_cont, "ye_CMS_NCSEM", "pye_CMS_NCSEM", 
                         "ye_CMS_NCSEM", "pye_CMS_NCSEM",  
                         "label", "label", 0, pointSize = 3)

pp_pts_Pk = pp_pts_Pk + scale_colour_gradient2("label", space="Lab", 
                                               midpoint = max(proj_cont$label)/2, 
                                               mid = "white", high = muted("blue"))

#pp_pts_Pk = pp_pts_Pk + scale_x_continuous(limits = c(8, 20)) #c(-0.25, 0.25)
# pp_pts_Pk = pp_pts_Pk + scale_y_continuous(limits = c(0.30,0.4)) #c(-0.25, 0.25)

pp_pts_Pk

#-------------------------------------------------------------------------------
# Plot : points in the x0_CMS_NCSEM/y0_CMS_NCSEM space
#-------------------------------------------------------------------------------
#Adding some continuation results
ppt_x0EM_y0EM_eP = ggplot() + geom_point(data = proj_cont, 
                                                aes(x0_CMS_NCSEM,  y0_CMS_NCSEM), 
                                                color = "black", size = 1)
ppt_x0EM_y0EM_eP


#===============================================================================
# JPL, single solution
#===============================================================================
guideJPL = FALSE
if(!empty(traj_cont_jpl))
{
  dark2 = rev(brewer.pal(3,"Dark2"))
  #-----------------------------------------------------------------------------
  # Subselection
  #
  # For QBCP and JPL separately:
  #   clab = c("0", "20", "30", "40", "50", "60", "80") + uncomment the end of condition
  #   with coord = 0 or 13
  #
  # For both models at the same time: clab = c("0", "30", "50", "80")
  #
  #-----------------------------------------------------------------------------
  clab = c("0")#, "30", "50", "80")
  condition  = traj_cont_jpl$label %in% clab #& traj_cont_jpl$coord == 13
  traj_cont_jpl_some = traj_cont_jpl[which(condition),]
  
  #-----------------------------------------------------------------------------
  # in NCSEM coordinates, xy
  #-----------------------------------------------------------------------------
  pp_jpl_NCSEM  = ggplot() + geom_path(data = traj_cont_jpl_some, 
                                       aes(x = x_NCSEM, y = y_NCSEM, 
                                           group = interaction(label, coord), 
                                           colour = factor(coord)), 
                                       size = 0.5)
  
  #Theme
  pp_jpl_NCSEM = pp_jpl_NCSEM + custom_theme
  #pp_jpl_NCSEM = pp_jpl_NCSEM + coord_fixed(ratio=1)
  pp_jpl_NCSEM = pp_jpl_NCSEM + scale_colour_manual(values=dark2, name = "Model", breaks = c(0, 13), labels = c("QBCP", "JPL"), guide = guideJPL)
  
  #Add SEMLi
  pp_jpl_NCSEM = pp_jpl_NCSEM + geom_point(data = dfsemli, aes(x= x_NC, y = y_NC), size = 4) 
  #Add Earth
  pp_jpl_NCSEM = pp_jpl_NCSEM + geom_point(data = dfearth_seml, aes(x= x_NC, y = y_NC), size = 4) 
  #Labels
  pp_jpl_NCSEM = pp_jpl_NCSEM + labs(x = x_sem, y = y_sem)
  
  #Annotate
  pp_jpl_NCSEM = pp_jpl_NCSEM + annotate("text", x = 0, y = -0.09,  
                                         label = "bold(SEML[2])", colour = "white",
                                         size = 5, parse = TRUE)
  pp_jpl_NCSEM = pp_jpl_NCSEM + annotate("text", x = 0, y = -0.09,  label = "SEML[2]", size = 5, parse = TRUE)
  # Earth
  pp_jpl_NCSEM = pp_jpl_NCSEM + annotate("text", x = -1, y = -0.09, 
                                         label = "bold(Earth)",  colour = "white",
                                         size = 5, parse = TRUE)
  
  pp_jpl_NCSEM = pp_jpl_NCSEM + annotate("text", x = -1, y = -0.09, label = "Earth", size = 5)
  
  
  #Display
  pp_jpl_NCSEM 
  
  # Save
  if(ISSAVED)
  {
    # Save in pdf
    filename = paste0(filepre, "_pp_jpl_NCSEM_single")
    ggsave(pp_jpl_NCSEM, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf")) #Save in pdf
  }
  
  
  #-----------------------------------------------------------------------------
  # in NCSEM coordinates, xz
  #-----------------------------------------------------------------------------
  pp_jpl_NCSEM_xz  = ggplot() + geom_path(data = traj_cont_jpl_some, 
                                       aes(x = x_NCSEM, y = z_NCSEM, 
                                           group = interaction(label, coord), 
                                           colour = factor(coord)),  
                                       size = 0.6)
  
  #Theme
  pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + custom_theme
  #pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + coord_fixed(ratio=1)
  pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + scale_colour_manual(values=dark2, name = "Model", breaks = c(0, 13), labels = c("QBCP", "JPL"), guide = guideJPL)
  
  #Add SEMLi
  pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + geom_point(data = dfsemli, aes(x= x_NC, y = z_NC), size = 4) 
  #Add Earth
  pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + geom_point(data = dfearth_seml, aes(x= x_NC, y = z_NC), size = 4) 
  #Labels
  pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + labs(x = x_sem, y = z_sem)
  
  #Annotate
  pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + annotate("text", x = 0, y = -0.007,  
                                               label = "bold(SEML[2])", colour = "white",
                                               size = 5, parse = TRUE)
  pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + annotate("text", x = 0, y = -0.007,  label = "SEML[2]", size = 5, parse = TRUE)
  # Earth
  pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + annotate("text", x = -1, y = -0.007, 
                                               label = "bold(Earth)",  colour = "white",
                                               size = 5, parse = TRUE)
  pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + annotate("text", x = -1, y = -0.007, label = "Earth", size = 5)
  
  #Display
  pp_jpl_NCSEM_xz 
  
  # Save
  if(ISSAVED)
  {
    # Save in pdf
    filename = paste0(filepre, "_pp_jpl_NCSEM_xz_single")
    ggsave(pp_jpl_NCSEM_xz, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf")) #Save in pdf
  }
  
  
  #-----------------------------------------------------------------------------
  # in NCSEM coordinates, yz
  #-----------------------------------------------------------------------------
  pp_jpl_NCSEM_yz  = ggplot() + geom_path(data = traj_cont_jpl_some, 
                                          aes(x = y_NCSEM, y = z_NCSEM, 
                                              group = interaction(label, coord), 
                                              colour = factor(coord)), 
                                          size = 0.6)
  
  #Theme
  pp_jpl_NCSEM_yz = pp_jpl_NCSEM_yz + custom_theme
  #pp_jpl_NCSEM_yz = pp_jpl_NCSEM_yz + coord_fixed(ratio=1)
  pp_jpl_NCSEM_yz = pp_jpl_NCSEM_yz + scale_colour_manual(values=dark2, name = "Model", breaks = c(0, 13), labels = c("QBCP", "JPL"), guide = guideJPL)
  
  #Add SEMLi
  pp_jpl_NCSEM_yz = pp_jpl_NCSEM_yz + geom_point(data = dfsemli, aes(x= y_NC, y = z_NC), size = 4) 
  #Add Earth
  pp_jpl_NCSEM_yz = pp_jpl_NCSEM_yz + geom_point(data = dfearth_seml, aes(x= y_NC, y = z_NC), size = 4) 
  #Labels
  pp_jpl_NCSEM_yz = pp_jpl_NCSEM_yz + labs(x = y_sem, y = z_sem)
  #Display
  pp_jpl_NCSEM_yz 
  
  # Save
  if(ISSAVED)
  {
    #Save in pdf
    filename = paste0(filepre, "_pp_jpl_NCSEM_yz_single")
    ggsave(pp_jpl_NCSEM_yz, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf")) #Save in pdf
  }
  
  #-----------------------------------------------------------------------------
  # in NCEM coordinates
  #-----------------------------------------------------------------------------
  pp_jpl_NCEM  = ggplot() + geom_path(data = traj_cont_jpl_some, 
                                       aes(x = x_NCEM, y = y_NCEM, 
                                           group = interaction(label, coord), 
                                           colour = factor(coord)), 
                                           size = 0.4)
  
  #Theme
  pp_jpl_NCEM = pp_jpl_NCEM + custom_theme
  #pp_jpl_NCEM = pp_jpl_NCEM + coord_fixed(ratio=1)
  pp_jpl_NCEM = pp_jpl_NCEM + scale_colour_manual(values=dark2, name = "Model", breaks = c(0, 13), labels = c("QBCP", "JPL"), guide = guideJPL)

  
  
  #Add SEMLi
  pp_jpl_NCEM = pp_jpl_NCEM + geom_point(data = dfsemli, aes(x= x_NC, y = y_NC), size = 4) 
  #Add Earth
  pp_jpl_NCEM = pp_jpl_NCEM + geom_point(data = dfearth_seml, aes(x= x_NC, y = y_NC), size = 4) 
  #Labels
  pp_jpl_NCEM = pp_jpl_NCEM + labs(x = x_em, y = y_em)
  #Zoom
  pp_jpl_NCEM = pp_jpl_NCEM + scale_x_continuous(limits = c(-0.2, 0.15)) #c(-0.25, 0.25)
  pp_jpl_NCEM = pp_jpl_NCEM + scale_y_continuous(limits = c(-0.43, 0.4)) #c(-0.25, 0.25)
  
  #Annotate
  pp_jpl_NCEM = pp_jpl_NCEM + annotate("text", x = 0, y = -0.04,  
                                       label = "bold(EML[2])", colour = "white",
                                       size = 5, parse = TRUE)
  pp_jpl_NCEM = pp_jpl_NCEM + annotate("text", x = 0, y = -0.04,  label = "EML[2]", size = 5, parse = TRUE)
  
  
  #Display
  pp_jpl_NCEM  
  
  # Save
  if(ISSAVED)
  {
    # Save in pdf
    filename = paste0(filepre, "_pp_jpl_NCEM_single")
    ggsave(pp_jpl_NCEM, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf")) #Save in pdf
  }
  
  
  
  #-----------------------------------------------------------------------------
  # in NCEM coordinates, xz
  #-----------------------------------------------------------------------------
  pp_jpl_NCEM_xz  = ggplot() + geom_path(data = traj_cont_jpl_some, 
                                      aes(x = x_NCEM, y = z_NCEM, 
                                          group = interaction(label, coord), 
                                          colour = factor(coord)), 
                                      size = 0.5)
  
  #Theme
  pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + custom_theme
  #pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + coord_fixed(ratio=1)
  pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + scale_colour_manual(values=dark2, name = "Model", breaks = c(0, 13), labels = c("QBCP", "JPL"), guide = guideJPL)
  
  #Add SEMLi
  pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + geom_point(data = dfsemli, aes(x= x_NC, y = z_NC), size = 4) 
  #Add Earth
  pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + geom_point(data = dfearth_seml, aes(x= x_NC, y = z_NC), size = 4) 
  #Labels
  pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + labs(x = x_em, y = z_em)
  #Zoom
  pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + scale_x_continuous(limits = c(-0.2, 0.2)) #c(-0.25, 0.25)
  pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + scale_y_continuous(limits = c(-0.05, 0.05)) #c(-0.25, 0.25)
  
  #Annotate
  pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + annotate("text", x = 0, y = -0.005,  
                                             label = "bold(EML[2])", colour = "white",
                                             size = 5, parse = TRUE)
  pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + annotate("text", x = 0, y = -0.005,  label = "EML[2]", size = 5, parse = TRUE)
  
  
  #Display
  pp_jpl_NCEM_xz 
  
  # Save
  if(ISSAVED)
  {
    #Save in pdf
    filename = paste0(filepre, "_pp_jpl_NCEM_xz_single")
    ggsave(pp_jpl_NCEM_xz, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf")) #Save in pdf
  }

}


#===============================================================================
# JPL, multi solutions
#===============================================================================
if(!empty(traj_cont_jpl))
{
  
  #=============================================================================
  # Subselection
  #
  # For QBCP and JPL separately:
  #   clab = c("0", "20", "30", "40", "50", "60", "80") + uncomment the end of condition
  #   with coord = 0 or 13
  #
  # For both models at the same time: clab = c("0", "30", "50", "80")
  #
  #=============================================================================
  clab = c("0", "30", "50", "80")
  condition  = traj_cont_jpl$label %in% clab #& traj_cont_jpl$coord == 13
  traj_cont_jpl_some = traj_cont_jpl[which(condition),]
  
  #=============================================================================
  # in NCSEM coordinates, xy
  #=============================================================================
  pp_jpl_NCSEM  = ggplot() + geom_path(data = traj_cont_jpl_some, 
                                       aes(x = x_NCSEM, y = y_NCSEM, 
                                           group = interaction(label, coord), 
                                           colour = factor(label),
                                           linetype = factor(coord)), 
                                       size = 0.5)
  
  #Theme
  pp_jpl_NCSEM = pp_jpl_NCSEM + custom_theme
  #pp_jpl_NCSEM = pp_jpl_NCSEM + coord_fixed(ratio=1)
  pp_jpl_NCSEM = pp_jpl_NCSEM + scale_colour_discrete(guide = FALSE)
  pp_jpl_NCSEM = pp_jpl_NCSEM + scale_linetype_manual(values=c("dashed", "solid"),name = "Model", breaks = c(0, 13), labels = c("QBCP", "JPL"), guide = guideJPL)
  
  #Add SEMLi
  pp_jpl_NCSEM = pp_jpl_NCSEM + geom_point(data = dfsemli, aes(x= x_NC, y = y_NC), size = 4) 
  #Add Earth
  pp_jpl_NCSEM = pp_jpl_NCSEM + geom_point(data = dfearth_seml, aes(x= x_NC, y = y_NC), size = 4) 
  pp_jpl_NCSEM
  
  #-----------------------------------------------------------------------------
  # For pdf, png and R display
  #-----------------------------------------------------------------------------
  pp_jpl_NCSEM_pdf = pp_jpl_NCSEM
  
  #Labels
  pp_jpl_NCSEM_pdf = pp_jpl_NCSEM_pdf + labs(x = x_sem, y = y_sem)
  
  #Annotate
  pp_jpl_NCSEM_pdf = pp_jpl_NCSEM_pdf + annotate("text", x = 0, y = -0.09,  
                                       label = "bold(SEML[2])", colour = "white",
                                       size = 5, parse = TRUE)
  pp_jpl_NCSEM_pdf = pp_jpl_NCSEM_pdf + annotate("text", x = 0, y = -0.09,  label = "SEML[2]", size = 5, parse = TRUE)
  # Earth
  pp_jpl_NCSEM_pdf = pp_jpl_NCSEM_pdf + annotate("text", x = -1, y = -0.09, 
                                       label = "bold(Earth)",  colour = "white",
                                       size = 5, parse = TRUE)
  
  pp_jpl_NCSEM_pdf = pp_jpl_NCSEM_pdf + annotate("text", x = -1, y = -0.09, label = "Earth", size = 5)
  
  #Display
  pp_jpl_NCSEM_pdf 
  
  # Save
  if(ISSAVED)
  {
    filename = paste0(filepre, "_pp_jpl_NCSEM")
    ggsave(pp_jpl_NCSEM_pdf, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf")) #Save in pdf
  }
  
  # Save for ISSFD
  if(ISSAVED)
  {
    filename = paste0(filepre, "_pp_jpl_NCSEM")
    pp_jpl_NCSEM_pdf = pp_jpl_NCSEM_pdf + issfd_theme
    ggsave(pp_jpl_NCSEM_pdf, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf, file = paste0(filename, ".pdf")) #Save in pdf
    ggsave(pp_jpl_NCSEM_pdf, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".png")) #Save in png
  }
  
  #-----------------------------------------------------------------------------
  # For tex, careful, big files!
  #-----------------------------------------------------------------------------
  # pp_jpl_NCSEM_tex = pp_jpl_NCSEM
  # 
  # #Labels
  # pp_jpl_NCSEM_tex = pp_jpl_NCSEM_tex + labs(x = "$x^{sem}$", y = "$y^{sem}$")
  # 
  # #Annotate
  # pp_jpl_NCSEM_tex = pp_jpl_NCSEM_tex + annotate("text", x = 0, y = -0.09,  label = "SEML$_2$", size = 5, parse = TRUE)
  # 
  # # Earth
  # pp_jpl_NCSEM_tex = pp_jpl_NCSEM_tex + annotate("text", x = -1, y = -0.09, label = "Earth", size = 5)
  # 
  # #Display
  # pp_jpl_NCSEM_tex 
  # 
  # # Save
  # if(ISSAVED)
  # {
  #   filename = paste0(filepre, "_pp_jpl_NCSEM")
  #   ggplot2tikz(pp_jpl_NCSEM_tex, width = xSize, height = ySize, file = paste0(filename, "_tex.tex"))
  # }
  
  
  
  #=============================================================================
  # in NCSEM coordinates, xz
  #=============================================================================
  pp_jpl_NCSEM_xz  = ggplot() + geom_path(data = traj_cont_jpl_some, 
                                          aes(x = x_NCSEM, y = z_NCSEM, 
                                              group = interaction(label, coord),
                                              colour = factor(label),
                                              linetype = factor(coord)),  
                                          size = 0.5)
  
  #Theme
  pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + custom_theme
  #pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + coord_fixed(ratio=1)
  pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + scale_colour_discrete(guide = FALSE)
  pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + scale_linetype_manual(values=c("dashed", "solid"), name = "Model", breaks = c(0, 13), labels = c("QBCP", "JPL"), guide = guideJPL)
  
  #Add SEMLi
  pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + geom_point(data = dfsemli, aes(x= x_NC, y = z_NC), size = 4) 
  #Add Earth
  pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + geom_point(data = dfearth_seml, aes(x= x_NC, y = z_NC), size = 4) 
  
  #-----------------------------------------------------------------------------
  # For pdf, png and R display
  #-----------------------------------------------------------------------------
  #Labels
  pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + labs(x = x_sem, y = z_sem)
  
  #Annotate
  pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + annotate("text", x = 0, y = -0.007,  
                                         label = "bold(SEML[2])", colour = "white",
                                         size = 5, parse = TRUE)
  pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + annotate("text", x = 0, y = -0.007,  label = "SEML[2]", size = 5, parse = TRUE)
  # Earth
  pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + annotate("text", x = -1, y = -0.007, 
                                         label = "bold(Earth)",  colour = "white",
                                         size = 5, parse = TRUE)
  
  pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + annotate("text", x = -1, y = -0.007, label = "Earth", size = 5)
  
  
  #Display
  pp_jpl_NCSEM_xz 
  
  # Save
  if(ISSAVED)
  {
    filename = paste0(filepre, "_pp_jpl_NCSEM_xz")
    ggsave(pp_jpl_NCSEM_xz, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf")) #Save in pdf
  }
  
  # Save for ISSFD
  if(ISSAVED)
  {
    filename = paste0(filepre, "_pp_jpl_NCSEM_xz")
    pp_jpl_NCSEM_xz = pp_jpl_NCSEM_xz + issfd_theme
    ggsave(pp_jpl_NCSEM_xz, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf, file = paste0(filename, ".pdf")) #Save in pdf
    ggsave(pp_jpl_NCSEM_xz, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".png")) #Save in png
  }
  
  #-----------------------------------------------------------------------------
  # in NCSEM coordinates, yz
  #-----------------------------------------------------------------------------
  pp_jpl_NCSEM_yz  = ggplot() + geom_path(data = traj_cont_jpl_some, 
                                          aes(x = y_NCSEM, y = z_NCSEM, 
                                              group = interaction(label, coord), 
                                              colour = factor(label),
                                              linetype = factor(coord)), 
                                          size = 0.5)
  
  #Theme
  pp_jpl_NCSEM_yz = pp_jpl_NCSEM_yz + custom_theme
  #pp_jpl_NCSEM_yz = pp_jpl_NCSEM_yz + coord_fixed(ratio=1)
  pp_jpl_NCSEM_yz = pp_jpl_NCSEM_yz + scale_colour_discrete(guide = FALSE)
  pp_jpl_NCSEM_yz = pp_jpl_NCSEM_yz + scale_linetype_manual(values=c("dashed", "solid"),name = "Model", breaks = c(0, 13), labels = c("QBCP", "JPL"), guide = guideJPL)
  
  #Add SEMLi
  pp_jpl_NCSEM_yz = pp_jpl_NCSEM_yz + geom_point(data = dfsemli, aes(x= y_NC, y = z_NC), size = 4) 
  #Add Earth
  pp_jpl_NCSEM_yz = pp_jpl_NCSEM_yz + geom_point(data = dfearth_seml, aes(x= y_NC, y = z_NC), size = 4) 
  #Labels
  pp_jpl_NCSEM_yz = pp_jpl_NCSEM_yz + labs(x = y_sem, y = z_sem)
  #Display
  pp_jpl_NCSEM_yz 
  
  # Save
  if(ISSAVED)
  {
    filename = paste0(filepre, "_pp_jpl_NCSEM_yz")
    ggsave(pp_jpl_NCSEM_yz, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf")) #Save in pdf
  }
  
  # Save for ISSFD
  if(ISSAVED)
  {
    filename = paste0(filepre, "_pp_jpl_NCSEM_yz")
    pp_jpl_NCSEM_yz = pp_jpl_NCSEM_yz + issfd_theme
    ggsave(pp_jpl_NCSEM_yz, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf, file = paste0(filename, ".pdf")) #Save in pdf
    ggsave(pp_jpl_NCSEM_yz, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".png")) #Save in png
  }
  
  #=============================================================================
  # in NCEM coordinates
  #=============================================================================
  pp_jpl_NCEM  = ggplot() + geom_path(data = traj_cont_jpl_some, 
                                      aes(x = x_NCEM, y = y_NCEM, 
                                          group = interaction(label, coord), 
                                          colour = factor(label),
                                          linetype = factor(coord)), 
                                      size = 0.5)
  
  #Theme
  pp_jpl_NCEM = pp_jpl_NCEM + custom_theme
  #pp_jpl_NCEM = pp_jpl_NCEM + coord_fixed(ratio=1)
  pp_jpl_NCEM = pp_jpl_NCEM + scale_colour_discrete(guide = FALSE)
  pp_jpl_NCEM = pp_jpl_NCEM + scale_linetype_manual(values=c("dashed", "solid"),name = "Model", breaks = c(0, 13), labels = c("QBCP", "JPL"), guide = guideJPL)
  
  
  #Add SEMLi
  pp_jpl_NCEM = pp_jpl_NCEM + geom_point(data = dfsemli, aes(x= x_NC, y = y_NC), size = 4) 
  #Add Earth
  pp_jpl_NCEM = pp_jpl_NCEM + geom_point(data = dfearth_seml, aes(x= x_NC, y = y_NC), size = 4) 
  #Labels
  pp_jpl_NCEM = pp_jpl_NCEM + labs(x = x_em, y = y_em)
  #Zoom
  pp_jpl_NCEM = pp_jpl_NCEM + scale_x_continuous(limits = c(-0.2, 0.15)) #c(-0.25, 0.25)
  pp_jpl_NCEM = pp_jpl_NCEM + scale_y_continuous(limits = c(-0.43, 0.4)) #c(-0.25, 0.25)
  
  #Annotate
  pp_jpl_NCEM = pp_jpl_NCEM + annotate("text", x = 0, y = -0.04,  
                                         label = "bold(EML[2])", colour = "white",
                                         size = 5, parse = TRUE)
  pp_jpl_NCEM = pp_jpl_NCEM + annotate("text", x = 0, y = -0.04,  label = "EML[2]", size = 5, parse = TRUE)
  
  
  #Display
  pp_jpl_NCEM  
  
  # Save
  if(ISSAVED)
  {
    filename = paste0(filepre, "_pp_jpl_NCEM")
    ggsave(pp_jpl_NCEM, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf")) #Save in pdf
  }
  
  # Save for ISSFD
  if(ISSAVED)
  {
    filename = paste0(filepre, "_pp_jpl_NCEM")
    pp_jpl_NCEM = pp_jpl_NCEM + issfd_theme
    ggsave(pp_jpl_NCEM, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf, file = paste0(filename, ".pdf")) #Save in pdf
    ggsave(pp_jpl_NCEM, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".png")) #Save in png
  }
  
  #=============================================================================
  # in NCEM coordinates, xz
  #=============================================================================
  pp_jpl_NCEM_xz  = ggplot() + geom_path(data = traj_cont_jpl_some, 
                                         aes(x = x_NCEM, y = z_NCEM, 
                                             group = interaction(label, coord), 
                                             colour = factor(label),
                                             linetype = factor(coord)), 
                                         size = 0.5)
  
  #Theme
  pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + custom_theme
  #pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + coord_fixed(ratio=1)
  pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + scale_colour_discrete(guide = FALSE)
  pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + scale_linetype_manual(values=c("dashed", "solid"),name = "Model", breaks = c(0, 13), labels = c("QBCP", "JPL"), guide = guideJPL)
  
  
  #Add SEMLi
  pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + geom_point(data = dfsemli, aes(x= x_NC, y = z_NC), size = 4) 
  #Add Earth
  pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + geom_point(data = dfearth_seml, aes(x= x_NC, y = z_NC), size = 4) 
  #Labels
  pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + labs(x = x_em, y = z_em)
  #Zoom
  pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + scale_x_continuous(limits = c(-0.2, 0.2)) #c(-0.25, 0.25)
  pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + scale_y_continuous(limits = c(-0.05, 0.05)) #c(-0.25, 0.25)
  
  #Annotate
  pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + annotate("text", x = 0, y = -0.005,  
                                       label = "bold(EML[2])", colour = "white",
                                       size = 5, parse = TRUE)
  pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + annotate("text", x = 0, y = -0.005,  label = "EML[2]", size = 5, parse = TRUE)
  
  
  
  #Display
  pp_jpl_NCEM_xz 
  
  # Save
  if(ISSAVED)
  {
    filename = paste0(filepre, "_pp_jpl_NCEM_xz")
    ggsave(pp_jpl_NCEM_xz, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf")) #Save in pdf
  }
  
  # Save for ISSFD
  if(ISSAVED)
  {
    filename = paste0(filepre, "_pp_jpl_NCEM_xz")
    pp_jpl_NCEM_xz = pp_jpl_NCEM_xz + issfd_theme
    ggsave(pp_jpl_NCEM_xz, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf, file = paste0(filename, ".pdf")) #Save in pdf
    ggsave(pp_jpl_NCEM_xz, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".png")) #Save in png
  }
  
}

# Stop here, the rest is DEPRECATED
#-------------------------------------------------------------------------------
#stopQuietly();

#-------------------------------------------------------------------------------
#Interpolation
#-------------------------------------------------------------------------------
# #Add an arbitrary value
# # xs1 = c(proj_map_tem$s1_CMU_EM, 35, 40)
# # xs3 = c(proj_map_tem$s3_CMU_EM, -3.5, 5.5)
# # Plot : points (pmin_dist_SEM) in the s1_CMU_EM/s3_CMU_EM space
# ppePmp = plotdf_point(proj_map_tem, "s1_CMU_EM", "s3_CMU_EM", "s1_CMU_EM", "s3_CMU_EM", pointSize = 3)
# 
# 
# #Interpolation (spline)
# splineData <- data.frame(
#   spline(xs1, xs3),
#   method = "spline()"
# )
# #Corresponding function
# splineF = splinefun(xs1, xs3)
# 
# #Interpolation (linear)
# approxData <- data.frame(
#   approx(xs1, xs3),
#   method = "approx()"
# )
# #Corresponding function
# 
# 
# #Plot again, with the interpolation
# ppePmp = ppePmp + geom_line(dat=splineData, aes(x0_CMU_EM, y0_CMU_EM), color = "red")
# ppePmp = ppePmp + geom_line(dat=approxData, aes(x0_CMU_EM, y0_CMU_EM), color = "blue")
# ppePmp
# 
#-------------------------------------------------------------------------------
# #Write interpolation data in file
#-------------------------------------------------------------------------------
# dfint = data.frame(approx(xs1, xs3))
# write.table(dfint, file = "data_interpolation.txt", sep = " ", row.names = FALSE, col.names = FALSE)
