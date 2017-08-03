################################################################################
#
# Projection from single orbits. Requires MAIN.R, ORBITS_PROJ_POSTPROCESS.R, 
# and ORBITS_PROJ_TRAJ_LOAD.R (to get refined trajectories)
#
# Difference with ORBITS_PROJ_PLOT.R : the phase is taken on a given pk section 
# rather than at the departure point.
#
# Final plots are in NCSE and NCEM coordinates.
#
# BLB 2017
#
################################################################################

#=====  Config  ================================================================
source("ProjMap/ORBITS_PKS_PLOT_CONFIG.R")

# For saving
is.saved = T

# Local width of the trajectories
size.traj = 0.15

#=====  Local Config  ==========================================================

# Color and aesthetics to match re.string
scolor = sprintf("conditional_cut(%s, %s, is.cut)", re.string, "re.cutv")
sgroup = "label.conn"#cs_interact("label", re.string)

# For annotations
y_annotate.SE = -0.1
z_annotate.SE = +0.05

# For annotations
y_annotate.EM = -0.8
z_annotate.EM = +0.2

# limits
xlim.ncem = scale_x_continuous(limits = c(-0.35, +0.35)) #for general comparison: scale_x_continuous(limits = c(-0.3, +0.3))
ylim.ncem = scale_y_continuous(limits = c(-0.8,0.8))   #for general comparison: scale_y_continuous(limits = c(-0.5,0.5))
zlim.ncem = scale_y_continuous(limits = c(-0.5,0.5)) 

xlim.ncse = scale_x_continuous(limits = c(-1.35, +0.3)) #for general comparison: scale_x_continuous(limits = c(-1.3, +0.3))
ylim.ncse = scale_y_continuous(limits = c(-1,1))        #for general comparison: scale_y_continuous(limits = c(-1,1))
zlim.ncse = scale_y_continuous(limits = c(-0.05,0.05))    

scale.color.ncem = scale.color.noguide
scale.color.ncse = scale.color.noguide

# For zoom in EM plots
t_SEM_zoom = 1 #0.48 for s1_10, s1_20, 1 for s1_30 and s1_40

#===== Plot: xy trajectories in NCSE ===========================================

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
aes.pk = aes_string("x_NCSEM", "y_NCSEM",  group = cs_interact("label.conn", "te_EM"), color = scolor)
ppt_x0_y0_NCSE_from_c = ggplot() + geom_path(data =  traj_from_c_good,aes.pk, size = size.traj)

# Set here to keep the same color scale as in the previous plots... Not very flexible!!
aes.pk = aes_string("0.001*x0_CMU_NCEM", "0.001*y0_CMU_NCEM",  group = cs_interact("label.conn", re.string), color = scolor)
ppt_x0_y0_NCSE_from_c = ppt_x0_y0_NCSE_from_c + geom_point(data = proj_map_prec, aes.pk, size = 2) 

# 2. Theme and titles
ppt_x0_y0_NCSE_from_c = ppt_x0_y0_NCSE_from_c + theme_pgrey(color.back)
ppt_x0_y0_NCSE_from_c = ppt_x0_y0_NCSE_from_c + coord_fixed(ratio=1)
ppt_x0_y0_NCSE_from_c = ppt_x0_y0_NCSE_from_c + scale.color.ncse

# 3. Add SEMLi
ppt_x0_y0_NCSE_from_c = ppt_x0_y0_NCSE_from_c + geom_point(data = dfsemli, aes(x= x_NC, y = y_NC), size = 2) 
ppt_x0_y0_NCSE_from_c = ppt_x0_y0_NCSE_from_c + annotate("text", x = 0, y = y_annotate.SE,  label = "SEL[2]", size = 5, parse = TRUE)

# 4. Add Earth
ppt_x0_y0_NCSE_from_c = ppt_x0_y0_NCSE_from_c + geom_point(data = dfearth_seml, aes(x= x_NC, y = y_NC), size = 2) 
ppt_x0_y0_NCSE_from_c = ppt_x0_y0_NCSE_from_c + annotate("text", x = -1, y = y_annotate.SE, label = "Earth", size = 5)

# 5. Labels
ppt_x0_y0_NCSE_from_c = ppt_x0_y0_NCSE_from_c + labs(x = x_sem, y = y_sem)

# 6. Orbit of the Moon
moon.orbit   = circleOrbit(c(-1,0), dfmoon_seml$o_NC, npoints = 100)
ppt_x0_y0_NCSE_from_c = ppt_x0_y0_NCSE_from_c + geom_path(data = moon.orbit, aes(x,y), linetype = "dashed")

# 7. Add the pk section 
#ppt_x0_y0_NCSE_from_c = ppt_x0_y0_NCSE_from_c + geom_point(data = proj_map_prec, aes(xe_CMS_NCSEM, ye_CMS_NCSEM), size = 1)

# 8. Zoom
ppt_x0_y0_NCSE_from_c = ppt_x0_y0_NCSE_from_c + xlim.ncse
ppt_x0_y0_NCSE_from_c = ppt_x0_y0_NCSE_from_c + ylim.ncse

# Right font
ppt_x0_y0_NCSE_from_c = set_font_cm(ppt_x0_y0_NCSE_from_c)

#  Display
ppt_x0_y0_NCSE_from_c

if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_y0_NCSE_from_c_", palette.brewer, tspan)
  #ggsave(ppt_x0_y0_NCSE_from_c, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_y0_NCSE_from_c, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}

#===== Plot: xz trajectories in NCSE ===========================================

# 1. Adding plots
aes.pk = aes_string("x_NCSEM", "z_NCSEM",  group = cs_interact("label.conn", "te_EM"), color = scolor)
ppt_x0_z0_NCSE_from_c = ggplot() + geom_path(data =  traj_from_c_good,aes.pk, size = size.traj)

# Set here to keep the same color scale as in the previous plots... Not very flexible!!
aes.pk = aes_string("0.001*x0_CMU_NCEM", "0.001*z0_CMU_NCEM",  group = cs_interact("label.conn", re.string), color = scolor)
ppt_x0_z0_NCSE_from_c = ppt_x0_z0_NCSE_from_c + geom_point(data = proj_map_prec, aes.pk, size = 2) 

# 2. Theme and titles
ppt_x0_z0_NCSE_from_c = ppt_x0_z0_NCSE_from_c + theme_pgrey(color.back)
#ppt_x0_z0_NCSE_from_c = ppt_x0_z0_NCSE_from_c + coord_fixed(ratio=1)
ppt_x0_z0_NCSE_from_c = ppt_x0_z0_NCSE_from_c + scale.color.ncse

# 3. Add SEMLi
ppt_x0_z0_NCSE_from_c = ppt_x0_z0_NCSE_from_c + geom_point(data = dfsemli, aes(x= x_NC, y = y_NC), size = 2) 
ppt_x0_z0_NCSE_from_c = ppt_x0_z0_NCSE_from_c + annotate("text", x = 0, y = z_annotate.SE,  label = "SEL[2]", size = 5, parse = TRUE)

# 4. Add Earth
ppt_x0_z0_NCSE_from_c = ppt_x0_z0_NCSE_from_c + geom_point(data = dfearth_seml, aes(x= x_NC, y = y_NC), size = 2) 
ppt_x0_z0_NCSE_from_c = ppt_x0_z0_NCSE_from_c + annotate("text", x = -1, y = z_annotate.SE, label = "Earth", size = 5)

# 5. Labels
ppt_x0_z0_NCSE_from_c = ppt_x0_z0_NCSE_from_c + labs(x = x_sem, y = z_sem)

# 8. Zoom
ppt_x0_z0_NCSE_from_c = ppt_x0_z0_NCSE_from_c + xlim.ncse
ppt_x0_z0_NCSE_from_c = ppt_x0_z0_NCSE_from_c + zlim.ncse

# Right font
ppt_x0_z0_NCSE_from_c = set_font_cm(ppt_x0_z0_NCSE_from_c)

#  Display
ppt_x0_z0_NCSE_from_c

if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_z0_NCSE_from_c_", palette.brewer, tspan)
  #ggsave(ppt_x0_z0_NCSE_from_c, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_z0_NCSE_from_c, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}



#===== Plot: trajectories in NCEM =============================================

# 1. Adding plots
aes.pk = aes_string("x_NCEM", "y_NCEM",  group = cs_interact("label.conn", "coord"), color = scolor)
ppt_x0_y0_NCEM_from_c = ggplot() + geom_path(data = traj_from_c_good[which(traj_from_c_good$t_SEM < t_SEM_zoom),], aes.pk, size = 0.2)

# Set here to keep the same color scale as in the previous plots... Not very flexible!!
aes.pk = aes_string("0.001*x0_CMU_NCEM", "0.001*y0_CMU_NCEM",  group = cs_interact("label.conn", re.string), color = scolor)
ppt_x0_y0_NCEM_from_c = ppt_x0_y0_NCEM_from_c + geom_point(data = proj_map_prec, aes.pk, size = 2) 

# 2. Theme and titles
ppt_x0_y0_NCEM_from_c = ppt_x0_y0_NCEM_from_c + theme_pgrey(color.back)
ppt_x0_y0_NCEM_from_c = ppt_x0_y0_NCEM_from_c + coord_fixed(ratio=1)
ppt_x0_y0_NCEM_from_c = ppt_x0_y0_NCEM_from_c + scale.color.ncem

# 3. Add EMLi
ppt_x0_y0_NCEM_from_c = ppt_x0_y0_NCEM_from_c + geom_point(data = dfemli, aes(x= x_NC, y = y_NC), size = 4) 
ppt_x0_y0_NCEM_from_c = ppt_x0_y0_NCEM_from_c + annotate("text", x = 0, y = y_annotate.EM,  label = "EML[2]", size = 5, parse = TRUE)

# 5. Labels
ppt_x0_y0_NCEM_from_c = ppt_x0_y0_NCEM_from_c + labs(x = x_em, y = y_em)

# Right font
ppt_x0_y0_NCEM_from_c = set_font_cm(ppt_x0_y0_NCEM_from_c)

# 8. Zoom
ppt_x0_y0_NCEM_from_c = ppt_x0_y0_NCEM_from_c + xlim.ncem
ppt_x0_y0_NCEM_from_c = ppt_x0_y0_NCEM_from_c + ylim.ncem
ppt_x0_y0_NCEM_from_c = ppt_x0_y0_NCEM_from_c + coord_cartesian()


if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_y0_NCEM_from_c_", palette.brewer, tspan)
  #ggsave(ppt_x0_y0_NCEM_from_c, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_y0_NCEM_from_c, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}


#===== Plot: xz trajectories in NCEM ===========================================

# 1. Adding plots
aes.pk = aes_string("x_NCEM", "z_NCEM",  group = cs_interact("label.conn", "coord"), color = scolor)
ppt_x0_z0_NCEM_from_c = ggplot() + geom_path(data = traj_from_c_good[which(traj_from_c_good$t_SEM < t_SEM_zoom),], aes.pk, size = 0.2)

# Set here to keep the same color scale as in the previous plots... Not very flexible!!
aes.pk = aes_string("0.001*x0_CMU_NCEM", "0.001*z0_CMU_NCEM",  group = cs_interact("label.conn", re.string), color = scolor)
ppt_x0_z0_NCEM_from_c = ppt_x0_z0_NCEM_from_c + geom_point(data = proj_map_prec, aes.pk, size = 2) 

# 2. Theme and titles
ppt_x0_z0_NCEM_from_c = ppt_x0_z0_NCEM_from_c + theme_pgrey(color.back)
ppt_x0_z0_NCEM_from_c = ppt_x0_z0_NCEM_from_c + scale.color.ncem

# 3. Add EMLi
ppt_x0_z0_NCEM_from_c = ppt_x0_z0_NCEM_from_c + geom_point(data = dfemli, aes(x= x_NC, y = y_NC), size = 4) 
ppt_x0_z0_NCEM_from_c = ppt_x0_z0_NCEM_from_c + annotate("text", x = 0, y = z_annotate.EM,  label = "EML[2]", size = 5, parse = TRUE)

# 5. Labels
ppt_x0_z0_NCEM_from_c = ppt_x0_z0_NCEM_from_c + labs(x = x_em, y = z_em)

# Right font
ppt_x0_z0_NCEM_from_c = set_font_cm(ppt_x0_z0_NCEM_from_c)

# 8. Zoom
ppt_x0_z0_NCEM_from_c = ppt_x0_z0_NCEM_from_c + xlim.ncem
ppt_x0_z0_NCEM_from_c = ppt_x0_z0_NCEM_from_c + zlim.ncem
ppt_x0_z0_NCEM_from_c = ppt_x0_z0_NCEM_from_c + coord_cartesian()

ppt_x0_z0_NCEM_from_c

if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_z0_NCEM_from_c_", palette.brewer, tspan)
  #ggsave(ppt_x0_z0_NCEM_from_c, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_z0_NCEM_from_c, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}


#===== Plot: ALL JPL trajectories in NCSE =====================================

# 1. Adding plots
aes.pk = aes_string("x_NCSEM", "y_NCSEM",  group = cs_interact("label.conn", "te_NCSEM"), color = scolor)
ppt_x0_y0_NCSE_from_jpl = ggplot() + geom_path(data = traj_from_jpl_13, aes.pk, size = size.traj)

# Set here to keep the same color scale as in the previous plots... Not very flexible!!
aes.pk = aes_string("0.001*x0_CMU_NCEM", "0.001*y0_CMU_NCEM",  group = cs_interact("label.conn", re.string), color = scolor)
ppt_x0_y0_NCSE_from_jpl = ppt_x0_y0_NCSE_from_jpl + geom_point(data = proj_map_prec, aes.pk, size = 2) 

# 2. Theme and titles
ppt_x0_y0_NCSE_from_jpl = ppt_x0_y0_NCSE_from_jpl + theme_pgrey(color.back)
ppt_x0_y0_NCSE_from_jpl = ppt_x0_y0_NCSE_from_jpl + coord_fixed(ratio=1)
ppt_x0_y0_NCSE_from_jpl = ppt_x0_y0_NCSE_from_jpl + scale.color.ncse

# 3. Add SEMLi
ppt_x0_y0_NCSE_from_jpl = ppt_x0_y0_NCSE_from_jpl + geom_point(data = dfsemli, aes(x= x_NC, y = y_NC), size = 2) 
ppt_x0_y0_NCSE_from_jpl = ppt_x0_y0_NCSE_from_jpl + annotate("text", x = 0, y = y_annotate.SE,  label = "SEL[2]", size = 5, parse = TRUE)

# 4. Add Earth
ppt_x0_y0_NCSE_from_jpl = ppt_x0_y0_NCSE_from_jpl + geom_point(data = dfearth_seml, aes(x= x_NC, y = y_NC), size = 2) 
ppt_x0_y0_NCSE_from_jpl = ppt_x0_y0_NCSE_from_jpl + annotate("text", x = -1, y = y_annotate.SE, label = "Earth", size = 5)

# 5. Labels
ppt_x0_y0_NCSE_from_jpl = ppt_x0_y0_NCSE_from_jpl + labs(x = x_sem, y = y_sem)


# 6. Orbit of the Moon
moon.orbit   = circleOrbit(c(-1,0), dfmoon_seml$o_NC, npoints = 100)
ppt_x0_y0_NCSE_from_jpl = ppt_x0_y0_NCSE_from_jpl + geom_path(data = moon.orbit, aes(x,y), linetype = "dashed")

# 7. Add the pk section 
#ppt_x0_y0_NCSE_from_jpl = ppt_x0_y0_NCSE_from_jpl + geom_point(data = proj_map_prec, aes(xe_CMS_NCSEM, ye_CMS_NCSEM), size = 1)

# 8. Zoom
ppt_x0_y0_NCSE_from_jpl = ppt_x0_y0_NCSE_from_jpl + xlim.ncse
ppt_x0_y0_NCSE_from_jpl = ppt_x0_y0_NCSE_from_jpl + ylim.ncse

# Right font
ppt_x0_y0_NCSE_from_jpl = set_font_cm(ppt_x0_y0_NCSE_from_jpl)

#  Display
ppt_x0_y0_NCSE_from_jpl

if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_y0_NCSE_from_jpl_", palette.brewer, tspan)
  #ggsave(ppt_x0_y0_NCSE_from_jpl, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_y0_NCSE_from_jpl, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}


#===== Plot: ALL JPL trajectories in NCEM =====================================

# 1. Adding plots
aes.pk = aes_string("x_NCEM", "y_NCEM",  group = cs_interact("label.conn", "te_NCSEM"), color = scolor)
ppt_x0_y0_NCEM_from_jpl = ggplot() + geom_path(data = traj_from_jpl_13[which(traj_from_jpl_13$t_SEM < t_SEM_zoom),], aes.pk, size = 0.2)

# Set here to keep the same color scale as in the previous plots... Not very flexible!!
aes.pk = aes_string("0.001*x0_CMU_NCEM", "0.001*y0_CMU_NCEM",  group = cs_interact("label.conn", re.string), color = scolor)
ppt_x0_y0_NCEM_from_jpl = ppt_x0_y0_NCEM_from_jpl + geom_point(data = proj_map_prec, aes.pk, size = 2) 

# 2. Theme and titles
ppt_x0_y0_NCEM_from_jpl = ppt_x0_y0_NCEM_from_jpl + theme_pgrey(color.back)
ppt_x0_y0_NCEM_from_jpl = ppt_x0_y0_NCEM_from_jpl + coord_fixed(ratio=1)
ppt_x0_y0_NCEM_from_jpl = ppt_x0_y0_NCEM_from_jpl + scale.color.ncem

# 3. Add EMLi
ppt_x0_y0_NCEM_from_jpl = ppt_x0_y0_NCEM_from_jpl + geom_point(data = dfemli, aes(x= x_NC, y = y_NC), size = 4) 
ppt_x0_y0_NCEM_from_jpl = ppt_x0_y0_NCEM_from_jpl + annotate("text", x = 0, y = y_annotate.EM,  label = "EML[2]", size = 5, parse = TRUE)

# 5. Labels
ppt_x0_y0_NCEM_from_jpl = ppt_x0_y0_NCEM_from_jpl + labs(x = x_em, y = y_em)

# Right font
ppt_x0_y0_NCEM_from_jpl = set_font_cm(ppt_x0_y0_NCEM_from_jpl)


# 8. Zoom
ppt_x0_y0_NCEM_from_jpl = ppt_x0_y0_NCEM_from_jpl + xlim.ncem
ppt_x0_y0_NCEM_from_jpl = ppt_x0_y0_NCEM_from_jpl + ylim.ncem
ppt_x0_y0_NCEM_from_jpl = ppt_x0_y0_NCEM_from_jpl + coord_cartesian()

#  Display
ppt_x0_y0_NCEM_from_jpl

if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_y0_NCEM_from_jpl_", palette.brewer, tspan)
  #ggsave(ppt_x0_y0_NCEM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_y0_NCEM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}

#===== Plot: ALL JPL xz trajectories in NCEM ===================================

# 1. Adding plots
aes.pk = aes_string("x_NCEM", "z_NCEM",  group = cs_interact("label.conn", "te_NCSEM"), color = scolor)
ppt_x0_z0_NCEM_from_jpl = ggplot() + geom_path(data = traj_from_jpl_13[which(traj_from_jpl_13$t_SEM < t_SEM_zoom),], aes.pk, size = 0.2)

# Set here to keep the same color scale as in the previous plots... Not very flexible!!
aes.pk = aes_string("0.001*x0_CMU_NCEM", "0.001*z0_CMU_NCEM",  group = cs_interact("label.conn", re.string), color = scolor)
ppt_x0_z0_NCEM_from_jpl = ppt_x0_z0_NCEM_from_jpl + geom_point(data = proj_map_prec, aes.pk, size = 2) 

# 2. Theme and titles
ppt_x0_z0_NCEM_from_jpl = ppt_x0_z0_NCEM_from_jpl + theme_pgrey(color.back)
ppt_x0_z0_NCEM_from_jpl = ppt_x0_z0_NCEM_from_jpl + coord_fixed(ratio=1)
ppt_x0_z0_NCEM_from_jpl = ppt_x0_z0_NCEM_from_jpl + scale.color.ncem

# 3. Add EMLi
ppt_x0_z0_NCEM_from_jpl = ppt_x0_z0_NCEM_from_jpl + geom_point(data = dfemli, aes(x= x_NC, y = y_NC), size = 4) 
ppt_x0_z0_NCEM_from_jpl = ppt_x0_z0_NCEM_from_jpl + annotate("text", x = 0, y = z_annotate.EM,  label = "EML[2]", size = 5, parse = TRUE)

# 5. Labels
ppt_x0_z0_NCEM_from_jpl = ppt_x0_z0_NCEM_from_jpl + labs(x = x_em, y = z_em)

# Right font
ppt_x0_z0_NCEM_from_jpl = set_font_cm(ppt_x0_z0_NCEM_from_jpl)

# 8. Zoom
ppt_x0_z0_NCEM_from_jpl = ppt_x0_z0_NCEM_from_jpl + xlim.ncem
ppt_x0_z0_NCEM_from_jpl = ppt_x0_z0_NCEM_from_jpl + zlim.ncem
ppt_x0_z0_NCEM_from_jpl = ppt_x0_z0_NCEM_from_jpl + coord_cartesian()

#  Display
ppt_x0_z0_NCEM_from_jpl

if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_z0_NCEM_from_jpl_", palette.brewer, tspan)
  #ggsave(ppt_x0_z0_NCEM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_z0_NCEM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}


#===== Plot: trajectories in NCSEM (using traj_cont) ===========================

# 1. Adding plots
aes.pk = aes_string("x_CMS_NCSEM", "y_CMS_NCSEM",  group = cs_interact("label.conn", "te_NCSEM"), color = scolor)
ppt_x0_y0_NCSE = ggplot() + geom_path(data = traj_cont, aes.pk, size = 0.5)

# Set here to keep the same color scale as in the previous plots... Not very flexible!!
aes.pk = aes_string("0.001*x0_CMU_NCEM", "0.001*y0_CMU_NCEM",  group = cs_interact("label.conn", re.string), color = scolor)
ppt_x0_y0_NCSE = ppt_x0_y0_NCSE + geom_path(data = proj_map_prec, aes.pk, size = 0.001) 

# 2. Theme and titles
ppt_x0_y0_NCSE = ppt_x0_y0_NCSE + theme_pgrey(color.back)
ppt_x0_y0_NCSE = ppt_x0_y0_NCSE + coord_fixed(ratio=1)
ppt_x0_y0_NCSE = ppt_x0_y0_NCSE + scale.color

# 3. Add SEMLi
ppt_x0_y0_NCSE = ppt_x0_y0_NCSE + geom_point(data = dfsemli, aes(x= x_NC, y = y_NC), size = 4) 
ppt_x0_y0_NCSE = ppt_x0_y0_NCSE + annotate("text", x = 0, y = y_annotate.SE,  label = "SEL[2]", size = 5, parse = TRUE)

# 4. Add Earth
ppt_x0_y0_NCSE = ppt_x0_y0_NCSE + geom_point(data = dfearth_seml, aes(x= x_NC, y = y_NC), size = 4) 
ppt_x0_y0_NCSE = ppt_x0_y0_NCSE + annotate("text", x = -1, y = y_annotate.SE, label = B_ems, size = 5, parse = TRUE)

# 5. Labels
ppt_x0_y0_NCSE = ppt_x0_y0_NCSE + labs(x = x_sem, y = y_sem)

# 6. Orbit of the Moon
moon.orbit   = circleOrbit(c(-1,0), dfmoon_seml$o_NC, npoints = 100)
ppt_x0_y0_NCSE = ppt_x0_y0_NCSE + geom_path(data = moon.orbit, aes(x,y), linetype = "dashed")

# 7. Add the pk section 
ppt_x0_y0_NCSE = ppt_x0_y0_NCSE + geom_point(data = proj_map_prec, aes(xe_CMS_NCSEM, ye_CMS_NCSEM), size = 1)

# 8. Zoom
ppt_x0_y0_NCSE_zoom = ppt_x0_y0_NCSE + scale_x_continuous(limits = c(-0.7, -0.6))
ppt_x0_y0_NCSE_zoom = ppt_x0_y0_NCSE_zoom + scale_y_continuous(limits = c(-0.2, 0.2))
ppt_x0_y0_NCSE_zoom + coord_cartesian()

# Right font
ppt_x0_y0_NCSE = set_font_cm(ppt_x0_y0_NCSE)

#  Display
ppt_x0_y0_NCSE
