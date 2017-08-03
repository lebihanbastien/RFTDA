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

# Subroutines
source("ProjMap/util/SUBROUTINES.R")

#===== Comparison between proj & cont ==========================================
length(proj_map_prec$label)
length(proj_cont$label)
length(unique(traj_from_c$label))
length(unique(traj_from_jpl$label))

#===== Selection of the phase ==================================================

re.string = "r0_CMU_EMT_mod"
re.seed   = proj_map_prec$r0_CMU_EMT_mod

#===== User inputs =============================================================

#Cutting
is.cut = TRUE

# Plotting
is.moon.pretty    = FALSE
is.moon.plotted   = TRUE
is.orbits.plotted = FALSE
palette.brewer    = "YlOrRd"

# Saving
is.saved = FALSE



#===== Cuts ====================================================================
# segment of time ratio that will be used to segregate the results on the plots.

re.cutf = 0.05
re.cut.min = max(re.cutf*floor(min(re.seed)/re.cutf) - re.cutf, 0)
re.cut.max = re.cutf*floor(max(re.seed)/re.cutf) + re.cutf
re.cutv    = seq(re.cut.min, re.cut.max, re.cutf)


#===== Colors =================================================================

scale.color.brewer            = scale_color_brewer("%T", type="seq",   palette = palette.brewer)  
scale.color.brewer.noguide    = scale_color_brewer("%T", type="seq",   palette = palette.brewer, guide = FALSE) 

scale.color.distiller         = scale_color_distiller("%T", type="seq",   palette = palette.brewer)  
scale.color.distiller.noguide = scale_color_distiller("%T", type="seq",   palette = palette.brewer, guide = FALSE) 

if(is.cut)
{
  scale.color         = scale.color.brewer
  scale.color.noguide = scale.color.brewer.noguide  
}else
{
  scale.color         = scale.color.distiller
  scale.color.noguide = scale.color.distiller.noguide
}

# Color and aesthetics to match re.string
scolor = sprintf("conditional_cut(%s, %s, TRUE)", re.string, "re.cutv")
sgroup = cs_interact("label", re.string)


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
    ppt_x0_y0 = ppt_x0_y0 + geom_point(data = dfmoon_eml, aes(x= x_NC, y = y_NC), size = 4) 
  }
  ppt_x0_y0 = ppt_x0_y0 + annotate("text", x = dfmoon_eml$x_NC, y = -0.05, label = "Moon", size = 5)
  
}

# Plus point from the Pk section, using the same approach
aes.pk = aes_string("xe_CMS_NCEM", "ye_CMS_NCEM",  group = cs_interact("label", re.string), color = scolor)
ppt_x0_y0 = geom_point_pretty(ppt_x0_y0, proj_map_prec, aes.pk)

ppt_x0_y0

#===== Plot: x0/y0, NCEM, + the trajectories (refined) =========================

# The trajectories
aes.traj = aes_string("x_CMS_NCEM", "y_CMS_NCEM",  group = cs_interact("label", re.string), color = scolor)
ppt_x0_y0_traj = ppt_x0_y0 + geom_path(data = traj_cont, aes.traj, size = 0.6)

# The point, again
ppt_x0_y0_traj = geom_point_pretty(ppt_x0_y0_traj, proj_map_prec, aes.pk)

# Zoom
ppt_x0_y0_traj = ppt_x0_y0_traj + scale_x_continuous(limits = c(-2,2))
ppt_x0_y0_traj = ppt_x0_y0_traj + scale_y_continuous(limits = c(-2,2))

ppt_x0_y0_traj


#===== Plot: xe/pxe, NCEM  ======================================================

# Init
ppt_xe_vxe_NCEM = ggplot()

# Points
aes.pk = aes_string("xe_CMS_NCEM", "vxe_CMS_NCEM",  group = cs_interact("label", re.string), color = scolor)
ppt_xe_vxe_NCEM = geom_point_pretty(ppt_xe_vxe_NCEM, proj_map_prec, aes.pk)

# Theme and titles
ppt_xe_vxe_NCEM = ppt_xe_vxe_NCEM + custom_theme
ppt_xe_vxe_NCEM = ppt_xe_vxe_NCEM + scale.color

ppt_xe_vxe_NCEM

#===== Plot: xe/pxe, NCSEM  ======================================================

# Init
ppt_xe_ye_NCSEM = ggplot()

# Points
aes.pk = aes_string("xe_CMS_NCSEM", "ye_CMS_NCSEM",  group = cs_interact("label", re.string), color = scolor)

ppt_xe_ye_NCSEM = geom_point_pretty(ppt_xe_ye_NCSEM, proj_map_prec, aes.pk)

# Theme and titles
ppt_xe_ye_NCSEM = ppt_xe_ye_NCSEM + custom_theme
ppt_xe_ye_NCSEM = ppt_xe_ye_NCSEM + scale.color

ppt_xe_ye_NCSEM
