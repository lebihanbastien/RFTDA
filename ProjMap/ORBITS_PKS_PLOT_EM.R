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
re.cutf = 0.02 #default: 0.05
source("ProjMap/ORBITS_PKS_PLOT_CONFIG.R")

# For saving
is.saved = F

# Local width of the trajectories
size.traj = 0.4


#===== Plot: x0/y0, EM  ======================================================

# The projection results, on the orbit
ppt_x0_y0 = plot.orbit.pks(proj_map_prec, proj_map_prec_first, proj_map_prec_all,
                           "x0_CMU_EM", "y0_CMU_EM", re.string,
                            X_E, Y_E, is.cut, "re.cutv", scale.color,
                            is.orbits.plotted,  proj_map_orbit, is.coord.one = TRUE)

# Add the "pretty Moon", or the simple one
if(is.moon.plotted)
{
  if(is.moon.pretty)
  {
    ppt_x0_y0 = addMoon(ppt_x0_y0, x = dfmoon_eml$x_SYS, y = 0, dfmoon_eml$r_SYS, surfSize = 0.4, cratSize = 0.2)
  }else
  {
    ppt_x0_y0 = addPrimary(ppt_x0_y0, dfmoon_eml$x_SYS, 0, primaryR/L, 0.4, "grey50")
  }
  ppt_x0_y0 = ppt_x0_y0 + annotate("text", x = dfmoon_eml$x_SYS-0.01, y = +0.012, label = "Moon", size = 5)
  
}

# Add EMLi
# yannot = -0.05
ppt_x0_y0 = ppt_x0_y0 + geom_point(data = dfemli, aes(x= x_SYS, y = y_SYS), size = 1) 
# ppt = ppt + annotate("text", x = dfemli$x_SYS, y = yannot,  label = "EML[2]", size = 5, parse = TRUE)

#New theme
ppt_x0_y0 = ppt_x0_y0 + theme_pgrey(color.back)

ppt_x0_y0

if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_ppt_x0_y0_EM_", palette.brewer)
  ggsave(ppt_x0_y0, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf, file = paste0(filename, ".pdf"))
  ggsave(ppt_x0_y0, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}

#===== Plot: y0/z0, EM  ======================================================

# The projection results, on the orbit
ppt_y0_z0 = plot.orbit.pks(proj_map_prec, proj_map_prec_first, proj_map_prec_all,
                           "y0_CMU_EM", "z0_CMU_EM", re.string,
                           Y_E, Z_E, is.cut, "re.cutv", scale.color.noguide,
                           is.orbits.plotted,  proj_map_orbit, is.coord.one = TRUE)

# Add the "pretty Moon", or the simple one
if(is.moon.plotted)
{
  if(is.moon.pretty)
  {
    ppt_y0_z0 = addMoon(ppt_y0_z0, x = dfmoon_eml$y_SYS, y = 0, dfmoon_eml$r_SYS, surfSize = 0.4, cratSize = 0.2)
  }else
  {
    ppt_y0_z0 = addPrimary(ppt_y0_z0, dfmoon_eml$y_SYS, 0, primaryR/L, 0.4, "grey50")
  }
  ppt_y0_z0 = ppt_y0_z0 + annotate("text", x = dfmoon_eml$y_SYS, y = +-0.008, label = "Moon", size = 5)
}

# Add EMLi
# yannot = -0.05
ppt_y0_z0 = ppt_y0_z0 + geom_point(data = dfemli, aes(x= y_SYS, y = z_SYS), size = 1) 
# ppt = ppt + annotate("text", x = dfemli$x_SYS, y = yannot,  label = "EML[2]", size = 5, parse = TRUE)

#New theme
ppt_y0_z0 = ppt_y0_z0 + theme_pgrey(color.back)

ppt_y0_z0

if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_ppt_y0_z0_EM_", palette.brewer)
  ggsave(ppt_y0_z0, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf, file = paste0(filename, ".pdf"))
  ggsave(ppt_y0_z0, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}


#===== Plot: x0/y0, EM, + the trajectories (refined) =========================

# The trajectories
aes.traj = aes_string("x_CMS_EM", "y_CMS_EM",  group = cs_interact("label.conn", re.string), color = scolor)
ppt_x0_y0_traj = ppt_x0_y0 + geom_path(data = traj_cont, aes.traj, size = size.traj)

# The points at pk
aes.pk = aes_string("xe_CMS_EM", "ye_CMS_EM",  group = cs_interact("label.conn", re.string), color = scolor)
ppt_x0_y0_traj = geom_point_pretty(ppt_x0_y0_traj, proj_map_prec, aes.pk)

# Plus 3BSOI
rTBSOI = 159198/CST_DIST_PRIM_EM
TBSOI.orbit   = circleOrbit(c(dfmoon_eml$x_SYS,0), rTBSOI, npoints = 100)
ppt_x0_y0_traj = ppt_x0_y0_traj + geom_path(data = TBSOI.orbit, aes(x,y), linetype = "dashed")
ppt_x0_y0_traj = ppt_x0_y0_traj + annotate("text", x = -1.45, y = -0.08,  label = "3BSOI", size = 5)

# Add EMLi
ppt_x0_y0_traj = ppt_x0_y0_traj + geom_point(data = dfemli, aes(x= x_SYS, y = y_SYS), size = 2) 
ppt_x0_y0_traj = ppt_x0_y0_traj + annotate("text", x = dfemli$x_SYS, y = 0.17,  label = "EML[2]", size = 5, parse = TRUE)


# Zoom, for 3BSOI
ppt_x0_y0_traj = ppt_x0_y0_traj + scale_x_continuous(limits = c(-1.5,-1.1))
ppt_x0_y0_traj = ppt_x0_y0_traj + scale_y_continuous(limits = c(-0.11,0.35))
ppt_x0_y0_traj

# Zoom, for closer look
# ppt_x0_y0_traj = ppt_x0_y0_traj + scale_x_continuous(limits = c(-1.28,-1.1))
# ppt_x0_y0_traj = ppt_x0_y0_traj + scale_y_continuous(limits = c(-0.11,0.15))
# ppt_x0_y0_traj

# Saving
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_ppt_x0_y0_EM_traj_", palette.brewer)
  ggsave(ppt_x0_y0_traj, width = xSize, height = ySize,  bg = "transparent", device=cairo_pdf, file = paste0(filename, ".pdf")) #Save in pdf
  ggsave(ppt_x0_y0_traj, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}





#===============================================================================
#stop("Stop here if only the trajectories are needed.")
#===============================================================================

# #===== Plot: xe/pxe, EM  =======================================================
# #
# # Made for comparison between delta tau = 1T and delta tau = 3T
# #
# # proj_map_source_3T$period = "3q" 
# # proj_map_source_1T$period = "1q" 
# # proj_map_source = rbind(proj_map_source_1T, proj_map_source_3T)
# 
# # Init
# ppt_xe_pxe_EM_comp = ggplot()
# 
# # Points
# aes.pk = aes_string("xe_CMS_EM", "pxe_CMS_EM",  color = "period")
# condition = proj_map_prec$xe_CMS_EM > -1.32 | proj_map_prec$pxe_CMS_EM > -0.4
# ppt_xe_pxe_EM_comp = ppt_xe_pxe_EM_comp + geom_point(data = proj_map_prec[which(condition),], mapping = aes.pk, size = 2.5) 
# 
# # Theme and titles
# ppt_xe_pxe_EM_comp = ppt_xe_pxe_EM_comp + theme_pgrey(color.back)
# ppt_xe_pxe_EM_comp = ppt_xe_pxe_EM_comp + scale_color_brewer(expression(delta*tau), palette = "Dark2", direction = -1, type="seq")
# 
# 
# #Labels;
# ppt_xe_pxe_EM_comp = ppt_xe_pxe_EM_comp + xlab(X_E) + ylab(PX_E)
# 
# ppt_xe_pxe_EM_comp
# 
# # Saving
# if(is.saved)
# {
#   filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_ppt_xe_pxe_EM_comp_1T_3T", palette.brewer)
#   ggsave(ppt_xe_pxe_EM_comp, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".pdf")) #Save in pdf
#   ggsave(ppt_xe_pxe_EM_comp, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
# }

#===== Plot: xe/pxe, EM  ======================================================

# Init
ppt_xe_pxe_EM = ggplot()

# Points
aes.pk = aes_string("xe_CMS_EM", "pxe_CMS_EM",  group = cs_interact("label", re.string), color = scolor)
ppt_xe_pxe_EM = ppt_xe_pxe_EM + geom_point(data = proj_map_prec, mapping = aes.pk, size = 2.2) 
#ppt_xe_pxe_EM = geom_point_pretty(ppt_xe_pxe_EM, proj_map_prec, aes.pk)
# Theme and titles
ppt_xe_pxe_EM = ppt_xe_pxe_EM + theme_pgrey(color.back)
ppt_xe_pxe_EM = ppt_xe_pxe_EM + scale.color

#Labels;
ppt_xe_pxe_EM = ppt_xe_pxe_EM+ xlab(X_E) + ylab(PX_E)

ppt_xe_pxe_EM

# Saving
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_ppt_xe_pxe_EM_", palette.brewer)
  ggsave(ppt_xe_pxe_EM, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".pdf")) #Save in pdf
  ggsave(ppt_xe_pxe_EM, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}


#===== Plot: xe/ye, SE  ======================================================

# Init
ppt_xe_ye_SE = ggplot()

# Points
aes.pk = aes_string("xe_CMS_SEM", "ye_CMS_SEM",  group = cs_interact("label", re.string), color = scolor)

ppt_xe_ye_SE = geom_point_pretty(ppt_xe_ye_SE, proj_map_prec, aes.pk)

# Theme and titles
ppt_xe_ye_SE = ppt_xe_ye_SE + theme_pgrey(color.back)
ppt_xe_ye_SE = ppt_xe_ye_SE + scale.color

ppt_xe_ye_SE

#===== Plot: xe/ze, SEM  ======================================================

# Init
ppt_xe_ze_SEM = ggplot()

# Points
aes.pk = aes_string("xe_CMS_SEM", "ze_CMS_SEM",  group = cs_interact("label", re.string), color = scolor)

ppt_xe_ze_SEM = geom_point_pretty(ppt_xe_ze_SEM, proj_map_prec, aes.pk)

# Theme and titles
ppt_xe_ze_SEM = ppt_xe_ze_SEM + theme_pgrey(color.back)
ppt_xe_ze_SEM = ppt_xe_ze_SEM + scale.color

ppt_xe_ze_SEM


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
ppt_s1EM_s3EM_eP = ppt_s1EM_s3EM_eP + scale_color_discrete(name  = expression(t[j]~"(x T)"))

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
ppt_x0_y0_eP = ggplot() + geom_path(data = proj_map_orbit, aes(x0_CMU_EM, y0_CMU_EM, group = label),  color= 'black', size = 0.05)

# 2. Theme and titles
ppt_x0_y0_eP = ppt_x0_y0_eP + custom_theme
ppt_x0_y0_eP = ppt_x0_y0_eP + labs(x= X_E, y = Y_E)
#ppt_x0_y0_eP = ppt_x0_y0_eP + coord_fixed(ratio=1)

# 5. All starting points for which a precise solutions has been found 
ppt_x0_y0_eP = geom_point_pretty(ppt_x0_y0_eP, proj_map_all_first, aes(x0_CMU_EM, y0_CMU_EM, color = factor(t0_CMU_EM_seed_T)))
ppt_x0_y0_eP = ppt_x0_y0_eP + scale_color_discrete(name  = tj_T, guide = F)

# 6. Display
ppt_x0_y0_eP

# Save
if(is.saved)
{
  # Save in pdf
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_y0_EM_orbit_ic")
  ggsave(ppt_x0_y0_eP, width = xSize, height = ySize,  bg = "transparent", file = paste0(filename, ".pdf")) #Save in pdf
  
  # Save in tex
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_y0_EM_orbit_ic_tex")
  ppt_x0_y0_eP_tex = ppt_x0_y0_eP + labs(x= X_E_tex, y = Y_E_tex)
  ppt_x0_y0_eP_tex = ppt_x0_y0_eP_tex + scale_color_discrete(name  = tj_T_tex, guide = F)
  ggplot2tikz_phd(ppt_x0_y0_eP_tex, xSize, ySize, file = paste0(filename, ".tex"))
}
