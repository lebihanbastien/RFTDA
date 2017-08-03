################################################################################
#
# Projection from single orbits. Requires MAIN.R, ORBITS_PROJ_POSTPROCESS.R, 
# and ORBITS_PROJ_TRAJ_LOAD.R (to get refined trajectories)
#
# Difference with ORBITS_PROJ_PLOT.R : the phase is taken on a given pk section 
# rather than at the departure point.
#
# BLB 2017
#
################################################################################

################################################################################
# Reminder:saving with right fonts
#
## Loading extrafonts:
#  library(extrafont)
## Looking @ the possible fonts which have "cm" in their names
#  h = fonttable()
#  h$FamilyName[which(grepl("cm", h$FamilyName, fixed = T))]
#
## Do NOT forget device=cairo_pdf in ggsave!
#
################################################################################

#=====  Config  ================================================================
source("ProjMap/ORBITS_PKS_PLOT_CONFIG.R")

# For saving
is.saved = F

# Local width of the trajectories
size.traj = 0.2

#===== Plot: x0/y0, NCEM  ======================================================

# The projection results, on the orbit
ppt_x0_y0 = plot.orbit.pks(proj_map_prec, proj_map_prec_first, proj_map_prec_all,
                           "x0_CMU_NCEM", "y0_CMU_NCEM", re.string,
                            x_em, y_em, is.cut, "re.cutv", scale.color,
                            is.orbits.plotted,  proj_map_orbit, is.coord.one = TRUE)

# Add the "pretty Moon", or the simple one
if(is.moon.plotted)
{
  if(is.moon.pretty)
  {
    ppt_x0_y0 = addMoon(ppt_x0_y0, x = dfmoon_eml$x_NC, y = 0, dfmoon_eml$r_NC, surfSize = 0.4, cratSize = 0.2)
  }else
  {
    ppt_x0_y0 = addPrimary(ppt_x0_y0, -1, 0, primaryR/(L*gamma_li), 0.4, "grey50")
    #ppt_x0_y0 = ppt_x0_y0 + geom_point(data = dfmoon_eml, aes(x= x_NC, y = y_NC), size = 4) 
  }
  ppt_x0_y0 = ppt_x0_y0 + annotate("text", x = dfmoon_eml$x_NC, y = -0.15, label = "Moon", size = 5)
  
}

# Add EMLi
# yannot = -0.05
ppt_x0_y0 = ppt_x0_y0 + geom_point(data = dfemli, aes(x= x_NC, y = y_NC), size = 1) 
# ppt = ppt + annotate("text", x = dfemli$x_NC, y = yannot,  label = "EML[2]", size = 5, parse = TRUE)

# Plus point from the Pk section, using the same approach
aes.pk = aes_string("xe_CMS_NCEM", "ye_CMS_NCEM",  group = cs_interact("label.conn", re.string), color = scolor)
#ppt_x0_y0 = geom_point_pretty(ppt_x0_y0, proj_map_prec, aes.pk)

#New theme
ppt_x0_y0 = ppt_x0_y0 + theme_pgrey(color.back)

# Right font
ppt_x0_y0 = set_font_cm(ppt_x0_y0)

if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_ppt_x0_y0_NCEM_", palette.brewer)
  ggsave(ppt_x0_y0, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf, file = paste0(filename, ".pdf"))
  ggsave(ppt_x0_y0, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}


#===== Plot: x0/y0, NCEM, + the trajectories (refined) =========================

# The trajectories
aes.traj = aes_string("x_CMS_NCEM", "y_CMS_NCEM",  group = cs_interact("label.conn", re.string), color = scolor)
ppt_x0_y0_traj = ppt_x0_y0 + geom_path(data = traj_cont, aes.traj, size = size.traj)

# The point, again
ppt_x0_y0_traj = geom_point_pretty(ppt_x0_y0_traj, proj_map_prec, aes.pk)

# Plus 3BSOI
rTBSOI = 159198/(CST_DIST_PRIM_EM*CST_GAMMA_LIB_EM)
TBSOI.orbit   = circleOrbit(c(-1,0), rTBSOI, npoints = 100)
ppt_x0_y0_traj = ppt_x0_y0_traj + geom_path(data = TBSOI.orbit, aes(x,y), linetype = "dashed")

# Add EMLi
yannot = +0.7
ppt_x0_y0_traj = ppt_x0_y0_traj + geom_point(data = dfemli, aes(x= x_NC, y = y_NC), size = 2) 
ppt_x0_y0_traj = ppt_x0_y0_traj + annotate("text", x = dfemli$x_NC, y = -yannot,  label = "EML[2]", size = 5, parse = TRUE)

# Add 3BSOI
ppt_x0_y0_traj = ppt_x0_y0_traj + annotate("text", x = 1.6, y = 0.7,  label = "3BSOI", size = 5)

# Again, the points
#aes.pk = aes_string("x0_CMU_NCEM", "y0_CMU_NCEM",  group = cs_interact("label.conn", re.string), color = scolor)
#ppt_x0_y0_traj = geom_point_pretty(ppt_x0_y0_traj, proj_map_prec, aes.pk)

# Zoom, for 3BSOI
# ppt_x0_y0_traj = ppt_x0_y0_traj + scale_x_continuous(limits = c(-1.2,2))
# ppt_x0_y0_traj = ppt_x0_y0_traj + scale_y_continuous(limits = c(-2.02,0.8))
# ppt_x0_y0_traj

# Zoom, for closer look
ppt_x0_y0_traj = ppt_x0_y0_traj + scale_x_continuous(limits = c(-0.5,0.6))
ppt_x0_y0_traj = ppt_x0_y0_traj + scale_y_continuous(limits = c(-0.8,0.7))
ppt_x0_y0_traj

# Right font
ppt_x0_y0_traj = set_font_cm(ppt_x0_y0_traj)

# Color scale
ppt_x0_y0_traj = ppt_x0_y0_traj + scale.color.noguide

# Saving
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_ppt_x0_y0_NCEM_traj_", palette.brewer)
  ggsave(ppt_x0_y0_traj, width = xSize, height = ySize,  bg = "transparent", device=cairo_pdf, file = paste0(filename, ".pdf")) #Save in pdf
  ggsave(ppt_x0_y0_traj, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}

#===============================================================================
#stop("Stop here if only the trajectories are needed.")
#===============================================================================


#===== Plot: xe/pxe, NCEM  ======================================================

# Init
ppt_xe_pxe_NCEM = ggplot()

# Points
aes.pk = aes_string("xe_CMS_NCEM", "pxe_CMS_NCEM",  group = cs_interact("label", re.string), color = scolor)
ppt_xe_pxe_NCEM = ppt_xe_pxe_NCEM + geom_point(data = proj_map_prec, mapping = aes.pk, size = 2.2) #geom_point_pretty(ppt_xe_pxe_NCEM, proj_map_prec, aes.pk)

# Theme and titles
ppt_xe_pxe_NCEM = ppt_xe_pxe_NCEM + theme_pgrey(color.back)
ppt_xe_pxe_NCEM = ppt_xe_pxe_NCEM + scale.color.noguide

#Labels;
ppt_xe_pxe_NCEM = ppt_xe_pxe_NCEM+ xlab(x_em) + ylab(px_em)

# Right font
ppt_xe_pxe_NCEM = set_font_cm(ppt_xe_pxe_NCEM)

ppt_xe_pxe_NCEM

# Saving
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_ppt_xe_pxe_NCEM_", palette.brewer)
  ggsave(ppt_xe_pxe_NCEM, width = xSize, height = ySize,  bg = "transparent", device=cairo_pdf, file = paste0(filename, ".pdf")) #Save in pdf
  ggsave(ppt_xe_pxe_NCEM, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}


#===== Plot: xe/ye, NCSEM  ======================================================

# Init
ppt_xe_ye_NCSEM = ggplot()

# Points
aes.pk = aes_string("xe_CMS_NCSEM", "ye_CMS_NCSEM",  group = cs_interact("label", re.string), color = scolor)

ppt_xe_ye_NCSEM = geom_point_pretty(ppt_xe_ye_NCSEM, proj_map_prec, aes.pk)

# Theme and titles
ppt_xe_ye_NCSEM = ppt_xe_ye_NCSEM + theme_pgrey(color.back)
ppt_xe_ye_NCSEM = ppt_xe_ye_NCSEM + scale.color

ppt_xe_ye_NCSEM

#===== Plot: xe/ze, NCSEM  ======================================================

# Init
ppt_xe_ze_NCSEM = ggplot()

# Points
aes.pk = aes_string("xe_CMS_NCSEM", "ye_CMS_NCSEM",  group = cs_interact("label", re.string), color = scolor)

ppt_xe_ze_NCSEM = geom_point_pretty(ppt_xe_ze_NCSEM, proj_map_prec, aes.pk)

# Theme and titles
ppt_xe_ze_NCSEM = ppt_xe_ze_NCSEM + theme_pgrey(color.back)
ppt_xe_ze_NCSEM = ppt_xe_ze_NCSEM + scale.color

ppt_xe_ze_NCSEM


#===============================================================================
#stop("After this point: seeds")
is.saved = F
#===============================================================================

#===== Plot: SEEDS in s1/s3 ====================================================

# All orbits
ppt_s1EM_s3EM_eP = ggplot() + geom_path(data = proj_map_orbit, aes(s1_CMU_EM, s3_CMU_EM, group = label),  color= 'black', size = 0.3)

# Theme and titles
ppt_s1EM_s3EM_eP = ppt_s1EM_s3EM_eP + custom_theme
ppt_s1EM_s3EM_eP = ppt_s1EM_s3EM_eP + labs(x= s1_exp, y = s3_exp)
# Right font (not )
# ppt_s1EM_s3EM_eP = set_font_cm_ex(ppt_s1EM_s3EM_eP)

# All starting points
ppt_s1EM_s3EM_eP = geom_point_pretty(ppt_s1EM_s3EM_eP, proj_map_all_first, aes(s1_CMU_EM, s3_CMU_EM, color = factor(t0_CMU_EM_seed_T)))
ppt_s1EM_s3EM_eP = ppt_s1EM_s3EM_eP + scale_color_discrete(name  = expression(t[j]~"(x T)"), guide = F)

# Display
ppt_s1EM_s3EM_eP

# Save
if(is.saved)
{
  # Save in pdf
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_s1_s3_orbit_ic")
  ggsave(ppt_s1EM_s3EM_eP, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf, file = paste0(filename, ".pdf")) #Save in pdf
  
  # Save in tex
  ppt_s1EM_s3EM_eP_tex = ppt_s1EM_s3EM_eP + labs(x= s1_tex, y = s3_tex)
  ppt_s1EM_s3EM_eP_tex = ppt_s1EM_s3EM_eP_tex + scale_color_discrete(name  = tj_T_tex)
  ggplot2tikz_phd(ppt_s1EM_s3EM_eP_tex, xSize, ySize, file = paste0(filename, ".tex"))
}


#===== Plot: SEEDS in x0/y0 ====================================================

# 1. All orbits for which a precise solutions has been found 
ppt_x0_y0_eP = ggplot() + geom_path(data = proj_map_orbit, aes(x0_CMU_NCEM, y0_CMU_NCEM, group = label),  color= 'black', size = 0.05)

# 2. Theme and titles
ppt_x0_y0_eP = ppt_x0_y0_eP + custom_theme
ppt_x0_y0_eP = ppt_x0_y0_eP + labs(x= x_em, y = y_em)
#ppt_x0_y0_eP = ppt_x0_y0_eP + coord_fixed(ratio=1)

# 5. All starting points for which a precise solutions has been found 
ppt_x0_y0_eP = geom_point_pretty(ppt_x0_y0_eP, proj_map_all_first, aes(x0_CMU_NCEM, y0_CMU_NCEM, color = factor(t0_CMU_EM_seed_T)))
ppt_x0_y0_eP = ppt_x0_y0_eP + scale_color_discrete(name  = tj_T, guide = F)

# 6. Display
ppt_x0_y0_eP

# Save
if(is.saved)
{
  # Save in pdf
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_y0_NCEM_orbit_ic")
  ggsave(ppt_x0_y0_eP, width = xSize, height = ySize,  bg = "transparent", file = paste0(filename, ".pdf")) #Save in pdf
  
  # Save in tex
  ppt_x0_y0_eP_tex = ppt_x0_y0_eP + labs(x= x_em_tex, y = y_em_tex)
  ppt_x0_y0_eP_tex = ppt_x0_y0_eP_tex + scale_color_discrete(name  = tj_T_tex, guide = F)
  ggplot2tikz_phd(ppt_x0_y0_eP_tex, xSize, ySize, file = paste0(filename, ".tex"))
}
