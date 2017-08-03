################################################################################
#
# Load trajectory from the refinement of projection data from single orbits. 
# Requires MAIN.R.
#
# Note: it is quite difficult to continue the solutions while keeping them close 
# to their first guesses (s1, s3) at EML2 in order to tackle this issue, 
# the problem is separated in two.  
# 
# - The data files that end with (e.g.) _Orbit_10_SINGLE contain 
# refined but NOT continued solutions. Hence, they are very close to their 
# original first guess and can be used to display the solutions from the EML2 
# point of view
#
# - The data files that end with (e.g.) _Orbit_10_CONT contain 
# refined AND continued solutions. Hence, they can be used to show beautiful 
# arrival at SEMLi
#
# All the current data (_Orbit_10,20,30,40_CONT) have been obtained by imposing 
# the initial projection distance to be below 3e-4 in NCSEM units.
# Note that, as expected, the 40 results are quite bad, because too close to 
# the end of the Dc at EML2
# 
# BLB 2017
#
################################################################################

#===============================================================================
# Get data from file
#===============================================================================
suffix_from_server = "_Orbit_10_DIST_SEM_5e-4_TSPAN_SEM_10" #"_Orbit_40_CONT_LOOSE_eps_1e-5"

#Suffix
FILE_SUFFIX_CONT_JPL   = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = suffix_from_server )

#Prefix
FILE_PREFIX_CONT_JPL   = paste0(ftincppdafolder, FILE_SUBFOLDER, "cont_jpl_order_", ORDER, "_dest_", LIB_POINT_SEM)

#===============================================================================
# Get results from JPL file
#===============================================================================
traj_cont_jpl = get_traj_comp(FILE_PREFIX_CONT_JPL, FILE_SUFFIX_CONT_JPL, FAMILY="")


#=============================================================================
# Subselection
#
#=============================================================================
clab = min(traj_cont_jpl$label)# "30", "50", "80")
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
pp_jpl_NCSEM = pp_jpl_NCSEM + scale_linetype_manual(values=c("dashed", "solid"),name = "Model", breaks = c(0, 13), labels = c("QBCP", "JPL"), guide = F)

#Add SEMLi
pp_jpl_NCSEM = pp_jpl_NCSEM + geom_point(data = dfsemli, aes(x= x_NC, y = y_NC), size = 4) 
#Add Earth
pp_jpl_NCSEM = pp_jpl_NCSEM + geom_point(data = dfearth_seml, aes(x= x_NC, y = y_NC), size = 4) 
pp_jpl_NCSEM


#=============================================================================
# in NCEM coordinates, xy
#=============================================================================
aes.pk = aes_string("x_NCEM", "y_NCEM",  group = cs_interact("label.conn", "coord"))
ppt_x0_y0_EM_blue = ggplot() + geom_path(data = traj_from_c_select, aes.pk, size = 0.6, color = dark2[1])

# 2. Theme and titles
ppt_x0_y0_EM_blue = ppt_x0_y0_EM_blue + custom_theme
ppt_x0_y0_EM_blue = ppt_x0_y0_EM_blue + coord_fixed(ratio=1)
ppt_x0_y0_EM_blue = ppt_x0_y0_EM_blue + scale.color.noguide

# 3. Add EMLi
ppt_x0_y0_EM_blue = ppt_x0_y0_EM_blue + geom_point(data = dfemli, aes(x= x_NC, y = y_NC), size = 4) 
ppt_x0_y0_EM_blue = ppt_x0_y0_EM_blue + annotate("text", x = 0, y = y_annotate,  label = "EML[2]", size = 5, parse = TRUE)

# 5. Labels
ppt_x0_y0_EM_blue = ppt_x0_y0_EM_blue + labs(x = x_em, y = y_em)

# Add the Moon
#ppt_x0_y0_EM_blue = addMoon(ppt_x0_y0_EM_blue, x = dfmoon_eml$x_NC, y = 0, dfmoon_eml$r_NC, surfSize = 0.4, cratSize = 0.2)

# 8. Zoom
ppt_x0_y0_EM_blue_zoom = ppt_x0_y0_EM_blue + scale_x_continuous(limits = c(-0.2, +0.3))
ppt_x0_y0_EM_blue_zoom = ppt_x0_y0_EM_blue_zoom + scale_y_continuous(limits = c(-0.38,0.38))


ppt_x0_y0_EM_blue_zoom

