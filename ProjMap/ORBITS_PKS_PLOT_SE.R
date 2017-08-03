################################################################################
#
# Projection from single orbits. Requires MAIN.R, ORBITS_PROJ_POSTPROCESS.R, 
# and ORBITS_PROJ_TRAJ_LOAD.R (to get refined trajectories)
#
# Difference with ORBITS_PROJ_PLOT.R : the phase is taken on a given pk section 
# rather than at the departure point.
#
# Final plots are in SE and EM coordinates.
#
# BLB 2017
#
################################################################################

#=====  Config  ================================================================
re.cutf = 0.02 #default: 0.05
source("ProjMap/ORBITS_PKS_PLOT_CONFIG.R")

# For saving
is.saved = T

# Local width of the trajectories
size.traj.SE = 0.5
size.traj.EM = 0.5

#=====  Local Config  ==========================================================

# Color and aesthetics to match re.string
scolor = sprintf("conditional_cut(%s, %s, is.cut)", re.string, "re.cutv")
sgroup = "label.conn"#cs_interact("label", re.string)

# For annotations
y_annotate.SE = -1e-3
z_annotate.SE = +8e-4

y_annotate.EM = -12e-2
z_annotate.EM = +6e-2

# limits:

# Zoom, for s1 = 10
# xlim.em = scale_x_continuous(limits = c(-1.2, -1.13))
# ylim.em = scale_y_continuous(limits = c(-0.03, +0.03)) 

# Zoom, for s1 = 20
# xlim.em = scale_x_continuous(limits = c(-1.2, -1.13))
# ylim.em = scale_y_continuous(limits = c(-0.055, +0.055))    

# Zoom, for s1 = 30
# xlim.em = scale_x_continuous(limits = c(-1.2, -1.12))
# ylim.em = scale_y_continuous(limits = c(-0.085, +0.085))  

xlim.em = scale_x_continuous(limits = c(-1.21, -1.1))
ylim.em = scale_y_continuous(limits = c(-0.13,0.13))    
zlim.em = scale_y_continuous(limits = c(-0.1,0.075)) 

xlim.se = scale_x_continuous(limits = c(-1.013, -0.995))
ylim.se = scale_y_continuous(limits = c(-0.01,0.01))    
zlim.se = scale_y_continuous(limits = c(-5e-3,5e-3)) 

scale.color.ncem = scale.color.noguide
scale.color.se = scale.color.noguide

# For zoom in EM plots
t_SEM_zoom = 1 #0.48 for s1_10, s1_20, 1 for s1_30 and s1_40

#===== Plot: xy trajectories in SE ===========================================

################################################################################
#
# Select only the satisfactory results in traj_from_c
#
# Either via energy
# traj_from_c_good = traj_from_c[which(traj_from_c$dHf_SEM < 1.375e-4),]
#
# Or a specific constraint set in ORBITS_PROJ_TRAJ_LOARD.R
traj_from_c_good = traj_from_c[which(!traj_from_c$is_solution_bad),]
################################################################################

# 1. Adding plots
aes.pk = aes_string("x_SEM", "y_SEM",  group = cs_interact("label.conn", "te_EM"), color = scolor)
ppt_x0_y0_SE_from_c = ggplot() + geom_path(data =  traj_from_c_good, aes.pk, size = size.traj.SE)

# Set here to keep the same color scale as in the previous plots... Not very flexible!!
aes.pk = aes_string("0.001*x0_CMU_EM", "0.001*y0_CMU_EM",  group = cs_interact("label.conn", re.string), color = scolor)
ppt_x0_y0_SE_from_c = ppt_x0_y0_SE_from_c + geom_point(data = proj_map_prec, aes.pk, size = 2) 

# 2. Theme and titles
ppt_x0_y0_SE_from_c = ppt_x0_y0_SE_from_c + theme_pgrey(color.back)
ppt_x0_y0_SE_from_c = ppt_x0_y0_SE_from_c + coord_fixed(ratio=1)
ppt_x0_y0_SE_from_c = ppt_x0_y0_SE_from_c + scale.color.se

# 3. Add SEMLi
ppt_x0_y0_SE_from_c = ppt_x0_y0_SE_from_c + geom_point(data = dfsemli, aes(x= x_SYS, y = y_SYS), size = 2) 
ppt_x0_y0_SE_from_c = ppt_x0_y0_SE_from_c + annotate("text", x = dfsemli$x_SYS, y = y_annotate.SE,  label = "SEL[2]", size = 5, parse = TRUE)

# 4. Add Earth
ppt_x0_y0_SE_from_c = ppt_x0_y0_SE_from_c + geom_point(data = dfearth_seml, aes(x= x_SYS, y = y_SYS), size = 2) 
ppt_x0_y0_SE_from_c = ppt_x0_y0_SE_from_c + annotate("text", x = dfearth_seml$x_SYS, y = y_annotate.SE, label = "Earth", size = 5)

# 5. Labels
ppt_x0_y0_SE_from_c = ppt_x0_y0_SE_from_c + labs(x = X_S, y = Y_S)

# 6. Orbit of the Moon
moon.orbit   = circleOrbit(c(-1,0), dfmoon_seml$o_SYS, npoints = 100)
ppt_x0_y0_SE_from_c = ppt_x0_y0_SE_from_c + geom_path(data = moon.orbit, aes(x,y), linetype = "dashed")

# 7. Add the pk section 
#ppt_x0_y0_SE_from_c = ppt_x0_y0_SE_from_c + geom_point(data = proj_map_prec, aes(xe_CMS_SEM, ye_CMS_SEM), size = 1)

# 8. Zoom
ppt_x0_y0_SE_from_c = ppt_x0_y0_SE_from_c + xlim.se
ppt_x0_y0_SE_from_c = ppt_x0_y0_SE_from_c + ylim.se

#  Display
ppt_x0_y0_SE_from_c

if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_y0_SE_from_c_", palette.brewer, tspan)
  ggsave(ppt_x0_y0_SE_from_c, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_y0_SE_from_c, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}


#===== Plot: xz trajectories in SE ===========================================

# 1. Adding plots
aes.pk = aes_string("x_SEM", "z_SEM",  group = cs_interact("label.conn", "te_EM"), color = scolor)
ppt_x0_z0_SE_from_c = ggplot() + geom_path(data =  traj_from_c_good,aes.pk, size = size.traj.SE)

# Set here to keep the same color scale as in the previous plots... Not very flexible!!
aes.pk = aes_string("0.001*x0_CMU_EM", "0.001*z0_CMU_EM",  group = cs_interact("label.conn", re.string), color = scolor)
ppt_x0_z0_SE_from_c = ppt_x0_z0_SE_from_c + geom_point(data = proj_map_prec, aes.pk, size = 2) 

# 2. Theme and titles
ppt_x0_z0_SE_from_c = ppt_x0_z0_SE_from_c + theme_pgrey(color.back)
#ppt_x0_z0_SE_from_c = ppt_x0_z0_SE_from_c + coord_fixed(ratio=1)
ppt_x0_z0_SE_from_c = ppt_x0_z0_SE_from_c + scale.color.se

# 3. Add SEMLi
ppt_x0_z0_SE_from_c = ppt_x0_z0_SE_from_c + geom_point(data = dfsemli, aes(x= x_SYS, y = y_SYS), size = 2) 
ppt_x0_z0_SE_from_c = ppt_x0_z0_SE_from_c + annotate("text", x = dfsemli$x_SYS, y = z_annotate.SE,  label = "SEL[2]", size = 5, parse = TRUE)

# 4. Add Earth
ppt_x0_z0_SE_from_c = ppt_x0_z0_SE_from_c + geom_point(data = dfearth_seml, aes(x= x_SYS, y = y_SYS), size = 2) 
ppt_x0_z0_SE_from_c = ppt_x0_z0_SE_from_c + annotate("text", x = dfearth_seml$x_SYS, y = z_annotate.SE, label = "Earth", size = 5)

# 5. Labels
ppt_x0_z0_SE_from_c = ppt_x0_z0_SE_from_c + labs(x = X_S, y = Z_S)

# 8. Zoom
ppt_x0_z0_SE_from_c = ppt_x0_z0_SE_from_c + xlim.se
ppt_x0_z0_SE_from_c = ppt_x0_z0_SE_from_c + zlim.se


#  Display
ppt_x0_z0_SE_from_c

if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_z0_SE_from_c_", palette.brewer, tspan)
  ggsave(ppt_x0_z0_SE_from_c, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_z0_SE_from_c, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}



#===== Plot: trajectories in EM =============================================

# 1. Adding plots
aes.pk = aes_string("x_EM", "y_EM",  group = cs_interact("label.conn", "coord"), color = scolor)
ppt_x0_y0_EM_from_c = ggplot() + geom_path(data = traj_from_c_good[which(traj_from_c_good$t_SEM < t_SEM_zoom),], aes.pk, size = size.traj.EM)

# Set here to keep the same color scale as in the previous plots... Not very flexible!!
aes.pk = aes_string("dfemli$x_SYS", "0.001*y0_CMU_EM",  group = cs_interact("label.conn", re.string), color = scolor)
ppt_x0_y0_EM_from_c = ppt_x0_y0_EM_from_c + geom_point(data = proj_map_prec, aes.pk, size = 2) 

# 2. Theme and titles
ppt_x0_y0_EM_from_c = ppt_x0_y0_EM_from_c + theme_pgrey(color.back)
ppt_x0_y0_EM_from_c = ppt_x0_y0_EM_from_c + coord_fixed(ratio=1)
ppt_x0_y0_EM_from_c = ppt_x0_y0_EM_from_c + scale.color.ncem

# 3. Add EMLi
ppt_x0_y0_EM_from_c = ppt_x0_y0_EM_from_c + geom_point(data = dfemli, aes(x= x_SYS, y = y_SYS), size = 4) 
ppt_x0_y0_EM_from_c = ppt_x0_y0_EM_from_c + annotate("text", x = dfemli$x_SYS, y = y_annotate.EM,  label = "EML[2]", size = 5, parse = TRUE)

# 5. Labels
ppt_x0_y0_EM_from_c = ppt_x0_y0_EM_from_c + labs(x = X_E, y = Y_E)

# 8. Zoom
ppt_x0_y0_EM_from_c = ppt_x0_y0_EM_from_c + xlim.em
ppt_x0_y0_EM_from_c = ppt_x0_y0_EM_from_c + ylim.em
ppt_x0_y0_EM_from_c = ppt_x0_y0_EM_from_c + coord_cartesian()

ppt_x0_y0_EM_from_c

if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_y0_EM_from_c_", palette.brewer, tspan)
  ggsave(ppt_x0_y0_EM_from_c, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_y0_EM_from_c, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}


#===== Plot: xz trajectories in EM ===========================================

# 1. Adding plots
aes.pk = aes_string("x_EM", "z_EM",  group = cs_interact("label.conn", "coord"), color = scolor)
ppt_x0_z0_EM_from_c = ggplot() + geom_path(data = traj_from_c_good[which(traj_from_c_good$t_SEM < t_SEM_zoom),], aes.pk, size = size.traj.EM)

# Set here to keep the same color scale as in the previous plots... Not very flexible!!
aes.pk = aes_string("dfemli$x_SYS", "0.001*z0_CMU_EM",  group = cs_interact("label.conn", re.string), color = scolor)
ppt_x0_z0_EM_from_c = ppt_x0_z0_EM_from_c + geom_point(data = proj_map_prec, aes.pk, size = 2) 

# 2. Theme and titles
ppt_x0_z0_EM_from_c = ppt_x0_z0_EM_from_c + theme_pgrey(color.back)
ppt_x0_z0_EM_from_c = ppt_x0_z0_EM_from_c + scale.color.ncem

# 3. Add EMLi
ppt_x0_z0_EM_from_c = ppt_x0_z0_EM_from_c + geom_point(data = dfemli, aes(x= x_SYS, y = y_SYS), size = 4) 
ppt_x0_z0_EM_from_c = ppt_x0_z0_EM_from_c + annotate("text", x = dfemli$x_SYS, y = z_annotate.EM,  label = "EML[2]", size = 5, parse = TRUE)

# 5. Labels
ppt_x0_z0_EM_from_c = ppt_x0_z0_EM_from_c + labs(x = X_E, y = Z_E)

# 8. Zoom
ppt_x0_z0_EM_from_c = ppt_x0_z0_EM_from_c + xlim.em
ppt_x0_z0_EM_from_c = ppt_x0_z0_EM_from_c + zlim.em
#ppt_x0_z0_EM_from_c = ppt_x0_z0_EM_from_c + coord_cartesian()

ppt_x0_z0_EM_from_c

if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_z0_EM_from_c_", palette.brewer, tspan)
  ggsave(ppt_x0_z0_EM_from_c, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_z0_EM_from_c, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}


#===== Plot: ALL JPL trajectories in SE =====================================

# 1. Adding plots
aes.pk = aes_string("x_SEM", "y_SEM",  group = cs_interact("label.conn", "te_NCSEM"), color = scolor)
ppt_x0_y0_SE_from_jpl = ggplot() + geom_path(data = traj_from_jpl_13, aes.pk, size = size.traj.SE)

# Set here to keep the same color scale as in the previous plots... Not very flexible!!
aes.pk = aes_string("0.001*x0_CMU_EM", "0.001*y0_CMU_EM",  group = cs_interact("label.conn", re.string), color = scolor)
ppt_x0_y0_SE_from_jpl = ppt_x0_y0_SE_from_jpl + geom_point(data = proj_map_prec, aes.pk, size = 2) 

# 2. Theme and titles
ppt_x0_y0_SE_from_jpl = ppt_x0_y0_SE_from_jpl + theme_pgrey(color.back)
ppt_x0_y0_SE_from_jpl = ppt_x0_y0_SE_from_jpl + coord_fixed(ratio=1)
ppt_x0_y0_SE_from_jpl = ppt_x0_y0_SE_from_jpl + scale.color.se

# 3. Add SEMLi
ppt_x0_y0_SE_from_jpl = ppt_x0_y0_SE_from_jpl + geom_point(data = dfsemli, aes(x= x_SYS, y = y_SYS), size = 2) 
ppt_x0_y0_SE_from_jpl = ppt_x0_y0_SE_from_jpl + annotate("text", x = dfsemli$x_SYS, y = y_annotate.SE,  label = "SEL[2]", size = 5, parse = TRUE)

# 4. Add Earth
ppt_x0_y0_SE_from_jpl = ppt_x0_y0_SE_from_jpl + geom_point(data = dfearth_seml, aes(x= x_SYS, y = y_SYS), size = 2) 
ppt_x0_y0_SE_from_jpl = ppt_x0_y0_SE_from_jpl + annotate("text", x = dfearth_seml$x_SYS, y = y_annotate.SE, label = "Earth", size = 5)

# 5. Labels
ppt_x0_y0_SE_from_jpl = ppt_x0_y0_SE_from_jpl + labs(x = X_S, y = Y_S)


# 6. Orbit of the Moon
moon.orbit   = circleOrbit(c(-1,0), dfmoon_seml$o_SYS, npoints = 100)
ppt_x0_y0_SE_from_jpl = ppt_x0_y0_SE_from_jpl + geom_path(data = moon.orbit, aes(x,y), linetype = "dashed")

# 7. Add the pk section 
#ppt_x0_y0_SE_from_jpl = ppt_x0_y0_SE_from_jpl + geom_point(data = proj_map_prec, aes(xe_CMS_SEM, ye_CMS_SEM), size = 1)

# 8. Zoom
ppt_x0_y0_SE_from_jpl = ppt_x0_y0_SE_from_jpl + xlim.se
ppt_x0_y0_SE_from_jpl = ppt_x0_y0_SE_from_jpl + ylim.se

#  Display
ppt_x0_y0_SE_from_jpl

if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_y0_SE_from_jpl_", palette.brewer, tspan)
  ggsave(ppt_x0_y0_SE_from_jpl, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_y0_SE_from_jpl, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}


#===== Plot: xz trajectories in SE ===========================================

# 1. Adding plots
aes.pk = aes_string("x_SEM", "z_SEM",  group = cs_interact("label.conn", "te_NCSEM"), color = scolor)
ppt_x0_z0_SE_from_jpl = ggplot() + geom_path(data = traj_from_jpl_13, aes.pk, size = size.traj.SE)

# Set here to keep the same color scale as in the previous plots... Not very flexible!!
aes.pk = aes_string("0.001*x0_CMU_EM", "0.001*z0_CMU_EM",  group = cs_interact("label.conn", re.string), color = scolor)
ppt_x0_z0_SE_from_jpl = ppt_x0_z0_SE_from_jpl + geom_point(data = proj_map_prec, aes.pk, size = 2) 

# 2. Theme and titles
ppt_x0_z0_SE_from_jpl = ppt_x0_z0_SE_from_jpl + theme_pgrey(color.back)
#ppt_x0_z0_SE_from_jpl = ppt_x0_z0_SE_from_jpl + coord_fixed(ratio=1)
ppt_x0_z0_SE_from_jpl = ppt_x0_z0_SE_from_jpl + scale.color.se

# 3. Add SEMLi
ppt_x0_z0_SE_from_jpl = ppt_x0_z0_SE_from_jpl + geom_point(data = dfsemli, aes(x= x_SYS, y = y_SYS), size = 2) 
ppt_x0_z0_SE_from_jpl = ppt_x0_z0_SE_from_jpl + annotate("text", x = dfsemli$x_SYS, y = z_annotate.SE,  label = "SEL[2]", size = 5, parse = TRUE)

# 4. Add Earth
ppt_x0_z0_SE_from_jpl = ppt_x0_z0_SE_from_jpl + geom_point(data = dfearth_seml, aes(x= x_SYS, y = y_SYS), size = 2) 
ppt_x0_z0_SE_from_jpl = ppt_x0_z0_SE_from_jpl + annotate("text", x = dfearth_seml$x_SYS, y = z_annotate.SE, label = "Earth", size = 5)

# 5. Labels
ppt_x0_z0_SE_from_jpl = ppt_x0_z0_SE_from_jpl + labs(x = X_S, y = Z_S)

# 8. Zoom
ppt_x0_z0_SE_from_jpl = ppt_x0_z0_SE_from_jpl + xlim.se
ppt_x0_z0_SE_from_jpl = ppt_x0_z0_SE_from_jpl + zlim.se


#  Display
ppt_x0_z0_SE_from_jpl

if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_z0_SE_from_jpl_", palette.brewer, tspan)
  ggsave(ppt_x0_z0_SE_from_jpl, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_z0_SE_from_jpl, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}



#===== Plot: ALL JPL trajectories in EM =====================================

# 1. Adding plots
aes.pk = aes_string("x_EM", "y_EM",  group = cs_interact("label.conn", "te_NCSEM"), color = scolor)
ppt_x0_y0_EM_from_jpl = ggplot() + geom_path(data = traj_from_jpl_13[which(traj_from_jpl_13$t_SEM < t_SEM_zoom),], aes.pk, size = size.traj.EM)

# Set here to keep the same color scale as in the previous plots... Not very flexible!!
aes.pk = aes_string("0.001*x0_CMU_EM", "0.001*y0_CMU_EM",  group = cs_interact("label.conn", re.string), color = scolor)
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + geom_point(data = proj_map_prec, aes.pk, size = 2) 

# 2. Theme and titles
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + theme_pgrey(color.back)
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + coord_fixed(ratio=1)
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + scale.color.ncem

# 3. Add EMLi
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + geom_point(data = dfemli, aes(x= x_SYS, y = y_SYS), size = 4) 
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + annotate("text", x = dfemli$x_SYS, y = y_annotate.EM,  label = "EML[2]", size = 5, parse = TRUE)

# 5. Labels
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + labs(x = X_E, y = Y_E)


# 8. Zoom
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + xlim.em
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + ylim.em
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + coord_cartesian()

#  Display
ppt_x0_y0_EM_from_jpl

if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_y0_EM_from_jpl_", palette.brewer, tspan)
  ggsave(ppt_x0_y0_EM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_y0_EM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}

#===== Plot: ALL JPL xz trajectories in EM ===================================

# 1. Adding plots
aes.pk = aes_string("x_EM", "z_EM",  group = cs_interact("label.conn", "te_NCSEM"), color = scolor)
ppt_x0_z0_EM_from_jpl = ggplot() + geom_path(data = traj_from_jpl_13[which(traj_from_jpl_13$t_SEM < t_SEM_zoom),], aes.pk, size = size.traj.EM)

# Set here to keep the same color scale as in the previous plots... Not very flexible!!
aes.pk = aes_string("0.001*x0_CMU_EM", "0.001*z0_CMU_EM",  group = cs_interact("label.conn", re.string), color = scolor)
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + geom_point(data = proj_map_prec, aes.pk, size = 2) 

# 2. Theme and titles
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + theme_pgrey(color.back)
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + coord_fixed(ratio=1)
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + scale.color.ncem

# 3. Add EMLi
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + geom_point(data = dfemli, aes(x= x_SYS, y = y_SYS), size = 4) 
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + annotate("text", x = dfemli$x_SYS, y = z_annotate.EM,  label = "EML[2]", size = 5, parse = TRUE)

# 5. Labels
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + labs(x = X_E, y = Z_E)

# 8. Zoom
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + xlim.em
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + zlim.em
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + coord_cartesian()

#  Display
ppt_x0_z0_EM_from_jpl

if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_z0_EM_from_jpl_", palette.brewer, tspan)
  ggsave(ppt_x0_z0_EM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_z0_EM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}


#===== Plot: trajectories in SEM (using traj_cont) ===========================

# 1. Adding plots
aes.pk = aes_string("x_CMS_SEM", "y_CMS_SEM",  group = cs_interact("label.conn", "te_NCSEM"), color = scolor)
ppt_x0_y0_SE = ggplot() + geom_path(data = traj_cont, aes.pk, size = 0.5)

# Set here to keep the same color scale as in the previous plots... Not very flexible!!
aes.pk = aes_string("0.001*x0_CMU_EM", "0.001*y0_CMU_EM",  group = cs_interact("label.conn", re.string), color = scolor)
ppt_x0_y0_SE = ppt_x0_y0_SE + geom_path(data = proj_map_prec, aes.pk, size = 0.001) 

# 2. Theme and titles
ppt_x0_y0_SE = ppt_x0_y0_SE + theme_pgrey(color.back)
ppt_x0_y0_SE = ppt_x0_y0_SE + coord_fixed(ratio=1)
ppt_x0_y0_SE = ppt_x0_y0_SE + scale.color

# 3. Add SEMLi
ppt_x0_y0_SE = ppt_x0_y0_SE + geom_point(data = dfsemli, aes(x= x_SYS, y = y_SYS), size = 4) 
ppt_x0_y0_SE = ppt_x0_y0_SE + annotate("text", x = dfsemli$x_SYS, y = y_annotate.SE,  label = "SEL[2]", size = 5, parse = TRUE)

# 4. Add Earth
ppt_x0_y0_SE = ppt_x0_y0_SE + geom_point(data = dfearth_seml, aes(x= x_SYS, y = y_SYS), size = 4) 
ppt_x0_y0_SE = ppt_x0_y0_SE + annotate("text", x = dfearth_seml$x_SYS, y = y_annotate.SE, label = B_ems, size = 5, parse = TRUE)

# 5. Labels
ppt_x0_y0_SE = ppt_x0_y0_SE + labs(x = X_S, y = Y_S)

# 6. Orbit of the Moon
moon.orbit   = circleOrbit(c(-1,0), dfmoon_seml$o_SYS, npoints = 100)
ppt_x0_y0_SE = ppt_x0_y0_SE + geom_path(data = moon.orbit, aes(x,y), linetype = "dashed")

# 7. Add the pk section 
ppt_x0_y0_SE = ppt_x0_y0_SE + geom_point(data = proj_map_prec, aes(xe_CMS_SEM, ye_CMS_SEM), size = 1)

# 8. Zoom
ppt_x0_y0_SE_zoom = ppt_x0_y0_SE + scale_x_continuous(limits = c(-0.7, -0.6))
ppt_x0_y0_SE_zoom = ppt_x0_y0_SE_zoom + scale_y_continuous(limits = c(-0.2, 0.2))
ppt_x0_y0_SE_zoom + coord_cartesian()

#  Display
ppt_x0_y0_SE
