################################################################################
#
# Projection from single orbits. Requires MAIN.R, ORBITS_PROJ_POSTPROCESS.R, 
# and ORBITS_PROJ_TRAJ_LOAD.R (to get refined trajectories)
#
# Difference with ORBITS_PROJ_PLOT.R : the phase is taken on a given pk section 
# rather than at the departure point.
#
# This file focuses on the JPL transitioning results.
#
# BLB 2017
#
################################################################################

#=============================================================================
# Local colors
#=============================================================================
scale.color.title.local = "Label"
scolor.local = "factor(label)"
scale.color.jpl.local           = scale_color_brewer(scale.color.title.local, type="seq",
                                                   palette = "Dark2", direction = 1)  
scale.color.jpl.local.noguide   = scale_color_brewer(scale.color.title.local, type="seq", 
                                                   palette = "Dark2", direction = 1, guide = FALSE)

scale.line.jpl.local = scale_linetype_manual(values=c("dashed", "solid"),
                                              name = "Model", breaks = c(0, 13), 
                                              labels = c("QBCP", "JPL"))
#=============================================================================
# Subselection
#=============================================================================
#-------------------------------------------------------------------------------
# One label
#-------------------------------------------------------------------------------
label_unique = unique(traj_from_jpl$label)

# With a certain condition on re_CMU_EMT_mod
condition = abs(traj_from_jpl$re_CMU_EMT_mod - 0.96) == min(abs(traj_from_jpl$re_CMU_EMT_mod - 0.96))
traj_from_jpl_sel = traj_from_jpl[which(condition),]
clab = traj_from_jpl_sel$label[1]

# Directly with a number
# clab = c(82998, 66687, 68666, 95291, 83984, 81517)        #SEM_10
# clab = c(178478, 170525, 172017, 165550, 102116, 159578)  #SEM_20
#clab = c(270440, 249887, 257408, 266432, 250388, 256405)   #SEM_30
#clab = c(379642, 388260, 359344, 384192, 388260, 357323)   #SEM_40
clab  = label_unique[1]
  
  
# Selection
condition  = traj_from_jpl$label %in% clab

# Selection in traj_cont
traj_from_jpl_some = traj_from_jpl[which(condition),]
traj_from_jpl_some$re_CMU_EMT_mod[1]

#traj_from_jpl_some_40 = traj_from_jpl_some

#-------------------------------------------------------------------------------
# Initial & final orbits
#-------------------------------------------------------------------------------
traj_from_jpl_some_emlt  = traj_from_jpl_some[which(traj_from_jpl_some$t_SEM <= traj_from_jpl_some$t0_CMU_SEM[1]),]
traj_from_jpl_some_semlt = traj_from_jpl_some[which(traj_from_jpl_some$t_SEM >= traj_from_jpl_some$tf_man_SEM[1]),]

#=============================================================================
# in NCSEM coordinates, xy
#=============================================================================
aes.pk = aes_string("x_NCSEM", "y_NCSEM",  
                    group = cs_interact("label.conn", "coord"), 
                    color = scolor.local, linetype = "factor(coord)")

ppt_x0_y0_NCSEM_from_jpl  = ggplot() + geom_path(data = traj_from_jpl_some, aes.pk, size = 0.5)

#Theme
ppt_x0_y0_NCSEM_from_jpl = ppt_x0_y0_NCSEM_from_jpl + theme_pgrey(color.back)
ppt_x0_y0_NCSEM_from_jpl = ppt_x0_y0_NCSEM_from_jpl + scale.color.jpl.local
ppt_x0_y0_NCSEM_from_jpl = ppt_x0_y0_NCSEM_from_jpl + coord_fixed(ratio=1)
ppt_x0_y0_NCSEM_from_jpl = ppt_x0_y0_NCSEM_from_jpl + scale.line.jpl.local

#Add SEMLi
ppt_x0_y0_NCSEM_from_jpl = ppt_x0_y0_NCSEM_from_jpl + geom_point(data = dfsemli, aes(x= x_NC, y = y_NC), size = 4) 
ppt_x0_y0_NCSEM_from_jpl = ppt_x0_y0_NCSEM_from_jpl + annotate("text", x = 0, y = y_annotate,  label = "SEML[2]", size = 5, parse = TRUE)

#Add Earth
ppt_x0_y0_NCSEM_from_jpl = ppt_x0_y0_NCSEM_from_jpl + geom_point(data = dfearth_seml, aes(x= x_NC, y = y_NC), size = 4) 
ppt_x0_y0_NCSEM_from_jpl = ppt_x0_y0_NCSEM_from_jpl + annotate("text", x = -1, y = y_annotate, label = "Earth", size = 5, parse = TRUE)

ppt_x0_y0_NCSEM_from_jpl

#=============================================================================
# in NCSEM coordinates, xz
#=============================================================================
aes.pk = aes_string("x_NCSEM", "z_NCSEM",  
                    group = cs_interact("label.conn", "coord"), 
                    color = scolor.local, linetype = "factor(coord)")

ppt_x0_z0_NCSEM_from_jpl  = ggplot() + geom_path(data = traj_from_jpl_some, aes.pk, size = 0.5)

#Theme
ppt_x0_z0_NCSEM_from_jpl = ppt_x0_z0_NCSEM_from_jpl + theme_pgrey(color.back)
ppt_x0_z0_NCSEM_from_jpl = ppt_x0_z0_NCSEM_from_jpl + scale.color.jpl.local
#ppt_x0_z0_NCSEM_from_jpl = ppt_x0_z0_NCSEM_from_jpl + coord_fixed(ratio=1)
ppt_x0_z0_NCSEM_from_jpl = ppt_x0_z0_NCSEM_from_jpl + scale.line.jpl.local

#Add SEMLi
ppt_x0_z0_NCSEM_from_jpl = ppt_x0_z0_NCSEM_from_jpl + geom_point(data = dfsemli, aes(x= x_NC, y = z_NC), size = 4) 
ppt_x0_z0_NCSEM_from_jpl = ppt_x0_z0_NCSEM_from_jpl + annotate("text", x = 0, y = +0.04,  label = "SEML[2]", size = 5, parse = TRUE)
#Add Earth
ppt_x0_z0_NCSEM_from_jpl = ppt_x0_z0_NCSEM_from_jpl + geom_point(data = dfearth_seml, aes(x= x_NC, y = z_NC), size = 4) 
ppt_x0_z0_NCSEM_from_jpl = ppt_x0_z0_NCSEM_from_jpl + annotate("text", x = -1, y = +0.04, label = "Earth", size = 5, parse = TRUE)

ppt_x0_z0_NCSEM_from_jpl


#=============================================================================
# in NCSEM coordinates, yz
#=============================================================================
aes.pk = aes_string("y_NCSEM", "z_NCSEM",  
                    group = cs_interact("label.conn", "coord"), 
                    color = scolor.local, linetype = "factor(coord)")

ppt_y0_z0_NCSEM_from_jpl  = ggplot() + geom_path(data = traj_from_jpl_some, aes.pk, size = 0.5)

#Theme
ppt_y0_z0_NCSEM_from_jpl = ppt_y0_z0_NCSEM_from_jpl + theme_pgrey(color.back)
ppt_y0_z0_NCSEM_from_jpl = ppt_y0_z0_NCSEM_from_jpl + scale.color.jpl.local
#ppt_y0_z0_NCSEM_from_jpl = ppt_y0_z0_NCSEM_from_jpl + coord_fixed(ratio=1)
ppt_y0_z0_NCSEM_from_jpl = ppt_y0_z0_NCSEM_from_jpl + scale.line.jpl.local

#Add SEMLi
ppt_y0_z0_NCSEM_from_jpl = ppt_y0_z0_NCSEM_from_jpl + geom_point(data = dfsemli, aes(x= y_NC, y = z_NC), size = 4) 
ppt_y0_z0_NCSEM_from_jpl = ppt_y0_z0_NCSEM_from_jpl + annotate("text", x = 0, y = -0.04,  label = "SEML[2]", size = 5, parse = TRUE)
#Add Earth
#ppt_y0_z0_NCSEM_from_jpl = ppt_y0_z0_NCSEM_from_jpl + geom_point(data = dfearth_seml, aes(x= y_NC, y = z_NC), size = 4) 
#ppt_y0_z0_NCSEM_from_jpl = ppt_y0_z0_NCSEM_from_jpl + annotate("text", x = -1, y = +0.04, label = "Earth", size = 5, parse = TRUE)

ppt_y0_z0_NCSEM_from_jpl

#=============================================================================
# in NCEM coordinates, xy
#=============================================================================
aes.pk = aes_string("x_NCEM", "y_NCEM",  group = cs_interact("label.conn", "coord"), color = scolor.local, linetype = "factor(coord)")

ppt_x0_y0_EM_from_jpl = ggplot() + geom_path(data = traj_from_jpl_some, aes.pk, size = 0.6)

# Orbit in black
#ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + geom_path(data = traj_from_jpl_some_emlt, aes.pk, size = 0.6, color = "black")

# Theme and titles
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + theme_pgrey(color.back)
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + coord_fixed(ratio=1)
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + scale.color.jpl.local
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + scale.line.jpl.local

# Add EMLi
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + geom_point(data = dfemli, aes(x= x_NC, y = y_NC), size = 4) 
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + annotate("text", x = 0, y = y_annotate,  label = "EML[2]", size = 5, parse = TRUE)

# Labels
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + labs(x = x_em, y = y_em)

# Add the Moon
#ppt_x0_y0_EM_from_jpl = addMoon(ppt_x0_y0_EM_from_jpl, x = dfmoon_eml$x_NC, y = 0, dfmoon_eml$r_NC, surfSize = 0.4, cratSize = 0.2)

# Zoom
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + scale_x_continuous(limits = c(-0.3, +0.3))
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + scale_y_continuous(limits = c(-0.6,0.6))
#ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + ggtitle(paste0(traj_from_jpl_some$re_CMU_EMT_mod[1]))

ppt_x0_y0_EM_from_jpl

# Save
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "ppt_x0_y0_EM_from_jpl")
  ggsave(ppt_x0_y0_EM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_y0_EM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}


#=============================================================================
# in NCEM coordinates, xy
#=============================================================================
aes.pk = aes_string("x_NCEM", "z_NCEM",  group = cs_interact("label.conn", "coord"), color = scolor.local, linetype = "factor(coord)")

ppt_x0_z0_EM_from_jpl = ggplot() + geom_path(data = traj_from_jpl_some, aes.pk, size = 0.6)

# Orbit in black
#ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + geom_path(data = traj_from_jpl_some_emlt, aes.pk, size = 0.6, color = "black")

# Theme and titles
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + theme_pgrey(color.back)
#ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + coord_fixed(ratio=1)
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + scale.color.jpl.local
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + scale.line.jpl.local

# Add EMLi
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + geom_point(data = dfemli, aes(x= x_NC, y = z_NC), size = 4) 
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + annotate("text", x = 0, y = y_annotate,  label = "EML[2]", size = 5, parse = TRUE)

# Labels
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + labs(x = x_em, y = z_em)

# Add the Moon
#ppt_x0_z0_EM_from_jpl = addMoon(ppt_x0_z0_EM_from_jpl, x = dfmoon_eml$x_NC, y = 0, dfmoon_eml$r_NC, surfSize = 0.4, cratSize = 0.2)

# Zoom
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + scale_x_continuous(limits = c(-0.2, +0.2))
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + scale_y_continuous(limits = c(-0.2, 0.2))
#ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + ggtitle(paste0(traj_from_jpl_some$re_CMU_EMT_mod[1]))

ppt_x0_z0_EM_from_jpl

# Save
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "ppt_x0_z0_EM_from_jpl")
  ggsave(ppt_x0_z0_EM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_z0_EM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}