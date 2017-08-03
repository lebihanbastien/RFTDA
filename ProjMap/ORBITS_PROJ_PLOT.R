################################################################################
#
# Projection from single orbits. Requires MAIN.R, ORBITS_PROJ_POSTPROCESS.R, 
# and ORBITS_PROJ_TRAJ_LOAD.R (to get refined trajectories)
#
# BLB 2017
#
################################################################################

# Subroutines
source("ProjMap/util/SUBROUTINES.R")

#===== Comparison between proj & cont ==========================================
length(proj_map_prec$label.conn)
#length(proj_cont$label)

#===== User inputs =============================================================

# Cuts: segment of time ratio that will be used to segregate the results on the plots.

cutf = 0.05
cut_min = max(cutf*floor(min(proj_map_prec$r0_CMU_EMT)/cutf) - cutf, 0)
cut_max = cutf*floor(max(proj_map_prec$r0_CMU_EMT)/cutf) + cutf
cutv = seq(cut_min, cut_max, cutf)
iscut = TRUE

# Plotting
is.moon.plotted = TRUE
is.orbits.plotted = FALSE

# Saving
is.saved = FALSE


#===== Colors =================================================================

# cutcolors = rev(brewer.pal(length(cutv),"YlOrRd"))
# scale.color.brewer     = scale_color_manual("%T", values = cutcolors)#, breaks = cutv) 
scale.color.brewer         = scale_color_brewer("%T",   type="seq",   palette = "YlOrRd")  
scale.color.brewer.noguide = scale_color_brewer("%T",   type="seq",   palette = "YlOrRd", guide = FALSE) 
scale.color.brewer.tex     = scale_color_brewer("\\%T", type="seq",   palette = "Set1") 




#===== Plot: x0/y0, NCEM  ======================================================
ppt_x0_y0 = plot.orbit.con(proj_map_prec, proj_map_prec_first, 
                                proj_map_prec_all, 
                                "x0_CMU_NCEM", "y0_CMU_NCEM", x_em, y_em,
                                scale.color.brewer, iscut, cutv, is.moon.plotted, 
                                is.orbits.plotted,  proj_map_orbit,
                                is.coord.one = TRUE)
ppt_x0_y0   

# Plus point from Pk section
ppt_x0_y0 + geom_point(data = proj_map_prec, aes(xe_CMS_NCEM, ye_CMS_NCEM), size = 2)

# Save
if(is.saved)
{
  # Save in pdf
  filename = paste0(filepre, "_x0_y0_orbit_connection")
  ggsave(ppt_x0_y0, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".pdf")) #Save in pdf
  
  # Save in latex
  # ppt_x0_y0_tex = ppt_x0_y0 + labs(x = x_em_tex, y = y_em_tex)
  # ppt_x0_y0_tex = ppt_x0_y0_tex + scale.color.brewer.tex
  # filename = paste0(filepre, "_x0_y0_orbit_connection_tex")
  # ggplot2tikz(ppt_x0_y0_tex, width = xSize, height = ySize, file = paste0(filename, "_tex.tex"))
}


#---- Adding continuation as points --------------------------------------------
# Adding plots
ppt_x0_y0_cont_points = ppt_x0_y0  + geom_point(data = proj_cont, aes(x0_CMU_NCEM, y0_CMU_NCEM), size = 2)

#Display
ppt_x0_y0_cont_points


#---- Adding continuation -----------------------------------------------------
# Adding plots
ppt_x0_y0_cont = ppt_x0_y0 + geom_path(data = traj_cont, aes(x = x_CMS_NCEM, 
                                                             y = y_CMS_NCEM,  
                                                             group = interaction(label, r0_CMU_EMT), 
                                                             color = conditional_cut(r0_CMU_EMT, cutv, iscut)), 
                                       size = 0.6)

# Zoom
ppt_x0_y0_cont = ppt_x0_y0_cont + scale_x_continuous(limits = c(-1,0.2))
ppt_x0_y0_cont = ppt_x0_y0_cont + scale_y_continuous(limits = c(-0.5, 0.5))

# Add the pk section 
ppt_x0_y0_cont = ppt_x0_y0_cont + geom_point(data = proj_cont, aes(xe_CMS_NCEM, ye_CMS_NCEM), size = 2)
# ppt_x0_y0_cont = geom_point_pretty(ppt_x0_y0_cont, proj_cont, aes(xe_CMS_NCEM, ye_CMS_NCEM,  
#                                                     group = interaction(label, re_CMU_EMT), 
#                                                     color = conditional_cut(re_CMU_EMT, cutv, iscut)))

# Display
ppt_x0_y0_cont


# Save
if(is.saved)
{
  # Save in pdf
  filename = paste0(filepre, "_x0_y0_orbit_connection", suffix_from_server)
  ggsave(ppt_x0_y0_cont, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".pdf")) #Save in pdf
}



#===== Plot: x0/z0, NCEM  ======================================================
ppt_x0_z0 = plot.orbit.con(proj_map_prec, proj_map_prec_first, 
                                proj_map_prec_all, 
                                "x0_CMU_NCEM", "z0_CMU_NCEM", x_em, z_em,
                                scale.color.brewer, iscut, cutv, 
                                is.moon.plotted = FALSE, 
                                is.orbits.plotted,  proj_map_orbit,
                                is.coord.one = TRUE)
ppt_x0_z0   



#---- Adding continuation -----------------------------------------------------
# Adding plots
ppt_x0_z0_cont = ppt_x0_z0 + geom_path(data = traj_cont, 
                                       aes(x = x_CMS_NCEM, y = z_CMS_NCEM,  
                                           group = interaction(label, r0_CMU_EMT), 
                                           color = conditional_cut(r0_CMU_EMT, cutv, iscut)), 
                                       size = 0.4)

# Zoom
ppt_x0_z0_cont = ppt_x0_z0_cont + scale_x_continuous(limits = c(-0.2, 0.2))
ppt_x0_z0_cont = ppt_x0_z0_cont + scale_y_continuous(limits = c(-0.15, 0.15))

# Display
ppt_x0_z0_cont

#===== Plot: y0/z0, NCEM  ======================================================
ppt_y0_z0 = plot.orbit.con(proj_map_prec, proj_map_prec_first, 
                                proj_map_prec_all, 
                                "y0_CMU_NCEM", "z0_CMU_NCEM", y_em, z_em,
                                scale.color.brewer.noguide, iscut, cutv, 
                                is.moon.plotted = FALSE, 
                                is.orbits.plotted,  proj_map_orbit,
                                is.coord.one = TRUE)
ppt_y0_z0   


# Save
if(is.saved)
{
  # Save in pdf
  filename = paste0(filepre, "_y0_z0_orbit_connection")
  ggsave(ppt_y0_z0, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".pdf")) #Save in pdf
  
  # Save in latex
  # ppt_x0_y0_tex = ppt_x0_y0 + labs(x = x_em_tex, y = y_em_tex)
  # ppt_x0_y0_tex = ppt_x0_y0_tex + scale.color.brewer.tex
  # filename = paste0(filepre, "_x0_y0_orbit_connection_tex")
  # ggplot2tikz(ppt_x0_y0_tex, width = xSize, height = ySize, file = paste0(filename, "_tex.tex"))
}


#---- Adding continuation -----------------------------------------------------
# Adding plots
ppt_y0_z0_cont = ppt_y0_z0 + geom_path(data = traj_cont, aes(x = y_CMS_NCEM, y = z_CMS_NCEM,  group = label, color = conditional_cut(r0_CMU_EMT, cutv, iscut)), size = 0.4)

# Zoom
ppt_y0_z0_cont = ppt_y0_z0_cont + scale_x_continuous(limits = c(-0.3, 0.2))
ppt_y0_z0_cont = ppt_y0_z0_cont + scale_y_continuous(limits = c(-0.15, 0.15))

# Display
ppt_y0_z0_cont


#===== Get rid of specific points ==============================================
# We get rid of some points at the end of each trajectories, to have a cleaner 
# end. The result is in traj_cont_clean.
#
# Another possibility: sign = atan2(y_CMS_NCSEM, x_CMS_NCSEM+1)*180/pi > -20

# For Qhalo
traj_cont_seq <- ddply(traj_cont, .(label, r0_CMU_EMT), mutate,
                       sign = y_CMS_NCSEM < 0.1 & y_CMS_NCSEM > 0.0 & x_CMS_NCSEM > 0.0)

# For orbit = 40
# traj_cont_seq <- ddply(traj_cont, .(label, r0_CMU_EMT), mutate,
#                        sign = atan2(y_CMS_NCSEM, x_CMS_NCSEM)*180/pi > -150 &
#                          atan2(y_CMS_NCSEM, x_CMS_NCSEM)*180/pi < -20 & x_CMS_NCSEM > -0.5)

# For others
#traj_cont_seq <- ddply(traj_cont, .(label, r0_CMU_EMT), mutate, sign = y_CMS_NCSEM > -0.5 & x_CMS_NCSEM > -0.5 )

traj_cont_seq <- ddply(traj_cont_seq, .(label, r0_CMU_EMT, sign), mutate, maxtsign = max(t_CMU_SEM))
traj_cont_seq <- ddply(traj_cont_seq, .(label, r0_CMU_EMT), mutate, smaxtsign = min(maxtsign))
traj_cont_seq <- ddply(traj_cont_seq, .(label, r0_CMU_EMT), mutate, bool = t_CMU_SEM < smaxtsign)
traj_cont_clean = traj_cont_seq[which(traj_cont_seq$bool),]
traj_cont_clean = traj_cont_clean[order(traj_cont_clean$t_CMU_SEM),]


#===== Plot: trajectories in NCSEM =============================================
# 1. Adding plots
ppt_x0_y0_SEM = ggplot() + geom_path(data = traj_cont, 
                                           aes(x = x_CMS_NCSEM, y = y_CMS_NCSEM, 
                                               group = interaction(label, r0_CMU_EMT), 
                                               color = conditional_cut(r0_CMU_EMT, cutv, iscut)),
                                               size = 0.8)

# Set here to keep the same color scale as in the previous plots... Not very flexible!!
ppt_x0_y0_SEM = geom_point_pretty(ppt_x0_y0_SEM, proj_map_prec, aes(0.001*x0_CMU_NCEM, 0.001*y0_CMU_NCEM,  
                                                          group = interaction(label, r0_CMU_EMT), 
                                                          color = conditional_cut(r0_CMU_EMT, cutv, iscut)))
# 2. Theme and titles
ppt_x0_y0_SEM = ppt_x0_y0_SEM + custom_theme
ppt_x0_y0_SEM = ppt_x0_y0_SEM + coord_fixed(ratio=1)
ppt_x0_y0_SEM = ppt_x0_y0_SEM + scale.color.brewer

# 3. Add SEMLi
ppt_x0_y0_SEM = ppt_x0_y0_SEM + geom_point(data = dfsemli, aes(x= x_NC, y = y_NC), size = 4) 
ppt_x0_y0_SEM = ppt_x0_y0_SEM + annotate("text", x = 0, y = -0.08,  label = "SEML[2]", size = 5, parse = TRUE)

# 4. Add Earth
ppt_x0_y0_SEM = ppt_x0_y0_SEM + geom_point(data = dfearth_seml, aes(x= x_NC, y = y_NC), size = 4) 
ppt_x0_y0_SEM = ppt_x0_y0_SEM + annotate("text", x = -1, y = -0.08, label = "Earth", size = 5)

# 5. Labels
ppt_x0_y0_SEM = ppt_x0_y0_SEM + labs(x = x_sem, y = y_sem)


# 6. Orbit of the Moon
# Mean radius of the Moon, in NCSEM coordinates: 2.518747349676265e-01
moon.mradius = 2.518747349676265e-01;
moon.orbit   = circleOrbit(c(-1,0), moon.mradius, npoints = 100)
ppt_x0_y0_SEM = ppt_x0_y0_SEM + geom_path(data = moon.orbit, aes(x,y), linetype = "dashed")

# 7. Add the pk section 
ppt_x0_y0_SEM = ppt_x0_y0_SEM + geom_point(data = proj_cont, aes(xe_CMS_NCSEM, ye_CMS_NCSEM), size = 2)

# 8. Zoom
#ppt_x0_y0_SEM = ppt_x0_y0_SEM + scale_x_continuous(limits = c(-0.8, -0.6))
#ppt_x0_y0_SEM = ppt_x0_y0_SEM + scale_y_continuous(limits = c(-0.2, 0.0))


# 9. Display
ppt_x0_y0_SEM


# Save
if(is.saved)
{
  # Save in pdf
  filename = paste0(filepre, "_x0_y0_SEM_connection", suffix_from_server)
  ggsave(ppt_x0_y0_SEM, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".pdf")) #Save in pdf
}

#===== Plot: trajectories in NCSEM =============================================
# 1. Adding plots
ppt_x0_z0_SEM = ggplot() + geom_path(data = traj_cont_clean, 
                                     aes(x = x_CMS_NCSEM, y = z_CMS_NCSEM, 
                                         group = interaction(label, r0_CMU_EMT), 
                                         color = conditional_cut(r0_CMU_EMT, cutv, iscut)),
                                     size = 0.8)

# Set here to keep the same color scale as in the previous plots... Not very flexible!!
ppt_x0_z0_SEM = geom_point_pretty(ppt_x0_z0_SEM, proj_map_prec, aes(0.0001*x0_CMU_NCEM, 0.0001*z0_CMU_NCEM,  
                                                                    group = interaction(label, r0_CMU_EMT), 
                                                                    color = conditional_cut(r0_CMU_EMT, cutv, iscut)))
# 2. Theme and titles
ppt_x0_z0_SEM = ppt_x0_z0_SEM + custom_theme
ppt_x0_z0_SEM = ppt_x0_z0_SEM + scale.color.brewer.noguide

# 3. Add SEMLi
ppt_x0_z0_SEM = ppt_x0_z0_SEM + geom_point(data = dfsemli, aes(x= x_NC, y = z_NC), size = 4) 
ppt_x0_z0_SEM = ppt_x0_z0_SEM + annotate("text", x = 0, y = -0.02,  label = "SEML[2]", size = 5, parse = TRUE)

# 4. Add Earth
ppt_x0_z0_SEM = ppt_x0_z0_SEM + geom_point(data = dfearth_seml, aes(x= x_NC, y = z_NC), size = 4) 
ppt_x0_z0_SEM = ppt_x0_z0_SEM + annotate("text", x = -1, y = -0.02, label = "Earth", size = 5)

# 5. Labels
ppt_x0_z0_SEM = ppt_x0_z0_SEM + labs(x = x_sem, y = z_sem)


# 7. Display
ppt_x0_z0_SEM

# Save
if(is.saved)
{
  # Save in pdf
  filename = paste0(filepre, "_x0_z0_SEM_connection", suffix_from_server)
  ggsave(ppt_x0_z0_SEM, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".pdf")) #Save in pdf
}



#===== Plot: epM in x0/y0 ======================================================
yannot = -4e-2*CST_DIST_PRIM_EM*CST_GAMMA_LIB_EM

# 1. All orbits for which a precise solutions has been found 
ppt_x0_y0_NCSI = ggplot() + geom_path(data = proj_map_prec_all, 
                                      aes(x0_CMU_SI_NCEM, y0_CMU_SI_NCEM, 
                                          group = label), 
                                      color = "black", size = 0.2)

# 2. Theme
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + custom_theme
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + coord_fixed(ratio=1)
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + scale.color.brewer

# 4. All departure positions that are precise enough
ppt_x0_y0_NCSI = geom_point_pretty(ppt_x0_y0_NCSI, proj_map_prec, 
                                   aes(x0_CMU_SI_NCEM, y0_CMU_SI_NCEM, 
                                       color = cut(r0_CMU_EMT, cutv)))
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + scale_color_discrete(name  = "%T")

# 5. All starting points for which a precise solutions has been found 
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + geom_point(data = proj_map_prec_first, 
                                             aes(x0_CMU_SI_NCEM, y0_CMU_SI_NCEM),  
                                             color= 'black', size = 2.5)

# 6. Add EMLi
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + geom_point(data = dfemli, 
                                             aes(x= x_NCPH, y = y_NCPH), size = 4) 
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + annotate("text", x = dfemli$x_NCPH, y = yannot,  
                                           label = "EML[2]", size = 5, parse = TRUE)

# 7. Add Moon
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + geom_point(data = dfmoon_eml, 
                                             aes(x= x_NCPH, y = y_NCPH), size = 4) 
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + annotate("text", x = dfmoon_eml$x_NCPH, y = yannot, 
                                           label = "Moon", size = 5)

# 8. Labels
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + labs(x = expression(italic(x)^italic(em)~" (km)"), 
                                       y = expression(italic(y)^italic(em)~" (km)"))

# 9. Display
ppt_x0_y0_NCSI


#===== Adding continuation =====================================================
# Adding plots
ppt_x0_y0_NCSI_cont = ppt_x0_y0_NCSI + geom_path(data = traj_cont, aes(x = x_CMS_SI_NCEM, y = y_CMS_SI_NCEM,  group = label, color = conditional_cut(r0_CMU_EMT, cutv, iscut)), size = 0.4)

#Zoom
ppt_x0_y0_NCSI_cont = ppt_x0_y0_NCSI_cont + scale_x_continuous(limits = c(-10e4, 5e4))
ppt_x0_y0_NCSI_cont = ppt_x0_y0_NCSI_cont + scale_y_continuous(limits = c(-5e4, 5e4))

#Display
ppt_x0_y0_NCSI_cont



#===== POINTS : psome_SEM + pp_x0SEM_y0SEM_eP ==================================
yannot = -8e-2*CST_DIST_PRIM_SEM*CST_GAMMA_LIB_SEM

# Adding plots
ppt_x0_y0_SEM_NCSI = ggplot() + geom_path(data = traj_cont_clean, 
                                       aes(x = x_CMS_SI_NCSEM, y = y_CMS_SI_NCSEM, 
                                           group = interaction(label, r0_CMU_EMT), color = conditional_cut(r0_CMU_EMT, cutv, iscut)),
                                       size = 0.4)

#Theme and titles
ppt_x0_y0_SEM_NCSI = ppt_x0_y0_SEM_NCSI + custom_theme
ppt_x0_y0_SEM_NCSI = ppt_x0_y0_SEM_NCSI + coord_fixed(ratio=1)
ppt_x0_y0_SEM_NCSI = ppt_x0_y0_SEM_NCSI + scale.color.brewer

#Add SEMLi
ppt_x0_y0_SEM_NCSI = ppt_x0_y0_SEM_NCSI + geom_point(data = dfsemli, aes(x= x_NCPH, y = y_NCPH), size = 4) 
ppt_x0_y0_SEM_NCSI = ppt_x0_y0_SEM_NCSI + annotate("text", x = dfsemli$x_NCPH, y = yannot,  label = "SEML[2]", size = 5, parse = TRUE)

#Add Earth
ppt_x0_y0_SEM_NCSI = ppt_x0_y0_SEM_NCSI + geom_point(data = dfearth_seml, aes(x= x_NCPH, y = y_NCPH), size = 4) 
ppt_x0_y0_SEM_NCSI = ppt_x0_y0_SEM_NCSI + annotate("text", x = dfearth_seml$x_NCPH, y = yannot, label = "Earth", size = 5)

#Labels
ppt_x0_y0_SEM_NCSI = ppt_x0_y0_SEM_NCSI + labs(x = "x (km)", y = "y (km)")

# Orbit of the Moon
# Mean radius of the Moon, in NCSEM coordinates: 2.518747349676265e-01
moon.mradius = 2.518747349676265e-01*Ldist("SEM")*CST_GAMMA_LIB_SEM;
moon.orbit   = circleOrbit(c(-Ldist("SEM")*CST_GAMMA_LIB_SEM,0), moon.mradius, npoints = 100)
ppt_x0_y0_SEM_NCSI = ppt_x0_y0_SEM_NCSI + geom_path(data = moon.orbit, aes(x,y), linetype = "dashed")

#Display
ppt_x0_y0_SEM_NCSI


#===== Plot: epM in s1/s3 ======================================================

# 1. All orbits for which a precise solutions has been found 
ppt_s1EM_s3EM_eP = ggplot() + geom_path(data = proj_map_prec_all, aes(s1_CMU_EM, s3_CMU_EM, group = label),  color= 'black', size = 0.3)

# 2. Theme and titles
ppt_s1EM_s3EM_eP = ppt_s1EM_s3EM_eP + custom_theme
ppt_s1EM_s3EM_eP = ppt_s1EM_s3EM_eP + labs(x= s1_exp, y = s3_exp)

# 5. All starting points for which a precise solutions has been found 
#ppt_s1EM_s3EM_eP = geom_point_pretty(ppt_s1EM_s3EM_eP, proj_map_prec_first, aes(s1_CMU_EM, s3_CMU_EM, color = conditional_cut(t0_CMU_EM_seed_T, cutv, FALSE)))#
ppt_s1EM_s3EM_eP = geom_point_pretty(ppt_s1EM_s3EM_eP, proj_map_prec_first, aes(s1_CMU_EM, s3_CMU_EM, color = factor(t0_CMU_EM_seed_T)))
ppt_s1EM_s3EM_eP = ppt_s1EM_s3EM_eP + scale_color_discrete(name  = expression(t[j]~"(x T)"))

# 6. Display
ppt_s1EM_s3EM_eP


# Save in pdf
filename = paste0(filepre, "_s1_s3_orbit_ic")
ggsave(ppt_s1EM_s3EM_eP, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf, file = paste0(filename, ".pdf")) #Save in pdf



#===== Plot: epM in x0/y0 ======================================================

# 1. All orbits for which a precise solutions has been found 
ppt_x0_y0_eP = ggplot() + geom_path(data = proj_map_prec_all, aes(x0_CMU_NCEM, y0_CMU_NCEM, group = label),  color= 'black', size = 0.05)

# 2. Theme and titles
ppt_x0_y0_eP = ppt_x0_y0_eP + custom_theme
ppt_x0_y0_eP = ppt_x0_y0_eP + labs(x= x_em, y = y_em)
#ppt_x0_y0_eP = ppt_x0_y0_eP + coord_fixed(ratio=1)

# 5. All starting points for which a precise solutions has been found 
#ppt_x0_y0_eP = geom_point_pretty(ppt_x0_y0_eP, proj_map_prec_first, aes(x0_CMU_NCEM, y0_CMU_NCEM, color = conditional_cut(t0_CMU_EM_seed_T, cutv, TRUE)))#ppt_s1EM_s3EM_eP + geom_point(data = proj_map_prec_first, aes(s1_CMU_EM, s3_CMU_EM, color = factor(t0_CMU_EM_seed_T)), size = 2)
ppt_x0_y0_eP = geom_point_pretty(ppt_x0_y0_eP, proj_map_prec_first, aes(x0_CMU_NCEM, y0_CMU_NCEM, color = factor(t0_CMU_EM_seed_T)))
ppt_x0_y0_eP = ppt_x0_y0_eP + scale_color_discrete(name  = expression(t[j]~"(x T)"))

# 6. Display
ppt_x0_y0_eP

# Save in pdf
filename = paste0(filepre, "_x0_y0_orbit_ic")
ggsave(ppt_x0_y0_eP, width = xSize, height = ySize,  bg = "transparent", file = paste0(filename, ".pdf")) #Save in pdf

#===== Plot: epM in y0/z0 ======================================================

# 1. All orbits for which a precise solutions has been found 
ppt_y0_z0_eP = ggplot() + geom_path(data = proj_map_prec_all, aes(y0_CMU_NCEM, z0_CMU_NCEM, group = label),  color= 'black', size = 0.05)

# 2. Theme and titles
ppt_y0_z0_eP = ppt_y0_z0_eP + custom_theme
ppt_y0_z0_eP = ppt_y0_z0_eP + labs(x= y_em, y = z_em)
#ppt_y0_z0_eP = ppt_y0_z0_eP + coord_fixed(ratio=1)

# 5. All starting points for which a precise solutions has been found 
ppt_y0_z0_eP = geom_point_pretty(ppt_y0_z0_eP, proj_map_prec_first, aes(y0_CMU_NCEM, z0_CMU_NCEM, color = conditional_cut(t0_CMU_EM_seed_T, cutv, TRUE)))#ppt_s1EM_s3EM_eP + geom_point(data = proj_map_prec_first, aes(s1_CMU_EM, s3_CMU_EM, color = factor(t0_CMU_EM_seed_T)), size = 2)
ppt_y0_z0_eP = ppt_y0_z0_eP + scale_color_discrete(name  = expression(t[j]~"(x T)"), guide = FALSE)

# 6. Display
ppt_y0_z0_eP

# Save in pdf
filename = paste0(filepre, "_y0_z0_orbit_ic")
ggsave(ppt_y0_z0_eP, width = xSize, height = ySize,  bg = "transparent", file = paste0(filename, ".pdf")) #Save in pdf
