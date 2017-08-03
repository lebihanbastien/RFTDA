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

#===============================================================================
# Local colors
#===============================================================================
scale.color.title.local = expression("Initial seed " * italic(s)[1])
scolor.local = "factor(label)"
scale.color.jpl.local           = scale_color_brewer(scale.color.title.local, type="seq",
                                                   palette = "Dark2", 
                                                   direction = 1, 
                                                   labels = c("10", "20", "30", "40"))
scale.color.jpl.local.noguide   = scale_color_brewer(scale.color.title.local, type="seq", 
                                                   palette = "Dark2", 
                                                   direction = 1, guide = FALSE)

#===============================================================================
# Local linetypes
#===============================================================================
scale.line.jpl.local = scale_linetype_manual(values=c("longdash", "solid"), 
                                             name = "Model", breaks = c(0, 13), 
                                             labels = c("QBCP", "JPL"))
scale.line.jpl.local.noguide = scale_linetype_manual(values=c("longdash", "solid"), 
                                             name = "Model", breaks = c(0, 13), 
                                             labels = c("QBCP", "JPL"), guide = FALSE)


line.size     = 0.6
line.size.EM  = 0.8
y_annotate    = -0.1

#===============================================================================
# Subselection
#
# save(traj_from_jpl_some, file = "traj_from_jpl_some_phd.Rda")
#
#===============================================================================

# Load from file.
load(file = "Rda/traj_from_jpl_some_phd.Rda")


# Directly with label(s)
clab_10 = c(82998, 66687, 68666, 95291, 83984, 81517)         #SEM_10
clab_20 = c(178478, 170525, 172017, 165550, 102116, 159578)   #SEM_20
clab_30 = c(270440, 249887, 257408, 266432, 250388, 256405)   #SEM_30
clab_40 = c(379642, 388260, 359344, 384192, 388260, 357323)   #SEM_40

n = 6
clab = c(clab_10[n], clab_20[n], clab_30[n], clab_40[n])

# Selection
condition  = traj_from_jpl_some$label %in% clab

# Selection in traj_cont
traj_from_jpl_some = traj_from_jpl_some[which(condition),]
traj_from_jpl_some$re_CMU_EMT_mod[1]

#traj_from_jpl_some_40 = traj_from_jpl_some


#-------------------------------------------------------------------------------
# Starting leg
#-------------------------------------------------------------------------------
traj_from_jpl_some_leg1  = traj_from_jpl_some[which(traj_from_jpl_some$t_SEM <= 1),]

#===============================================================================
# in NCSEM coordinates, xy
#===============================================================================
aes.pk = aes_string("x_NCSEM", "y_NCSEM",  
                    group = cs_interact("label.conn", "coord"), 
                    color = scolor.local, linetype = "factor(coord)")

ppt_x0_y0_NCSEM_from_jpl  = ggplot() + geom_path(data = traj_from_jpl_some, aes.pk, size = line.size)

#Theme
ppt_x0_y0_NCSEM_from_jpl = ppt_x0_y0_NCSEM_from_jpl + theme_pgrey(color.back)
ppt_x0_y0_NCSEM_from_jpl = ppt_x0_y0_NCSEM_from_jpl + scale.color.jpl.local
ppt_x0_y0_NCSEM_from_jpl = ppt_x0_y0_NCSEM_from_jpl + coord_fixed(ratio=1)
ppt_x0_y0_NCSEM_from_jpl = ppt_x0_y0_NCSEM_from_jpl + scale.line.jpl.local

# Labels
ppt_x0_y0_NCSEM_from_jpl = ppt_x0_y0_NCSEM_from_jpl + labs(x = x_sem, y = y_sem)


#Add SEMLi
ppt_x0_y0_NCSEM_from_jpl = ppt_x0_y0_NCSEM_from_jpl + geom_point(data = dfsemli, aes(x= x_NC, y = y_NC), size = 4) 
ppt_x0_y0_NCSEM_from_jpl = ppt_x0_y0_NCSEM_from_jpl + annotate("text", x = 0, y = y_annotate,  label = "SEL[2]", size = 5, parse = TRUE)

#Add Earth
ppt_x0_y0_NCSEM_from_jpl = ppt_x0_y0_NCSEM_from_jpl + geom_point(data = dfearth_seml, aes(x= x_NC, y = y_NC), size = 4) 
ppt_x0_y0_NCSEM_from_jpl = ppt_x0_y0_NCSEM_from_jpl + annotate("text", x = -1, y = y_annotate, label = "Earth", size = 5, parse = TRUE)

# Right font
ppt_x0_y0_NCSEM_from_jpl = set_font_cm(ppt_x0_y0_NCSEM_from_jpl)

ppt_x0_y0_NCSEM_from_jpl

# Save
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, "_ppt_x0_y0_NCSEM_from_jpl")
  ggsave(ppt_x0_y0_NCSEM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_y0_NCSEM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}


#===============================================================================
# in NCSEM coordinates, xz
#===============================================================================
aes.pk = aes_string("x_NCSEM", "z_NCSEM",  
                    group = cs_interact("label.conn", "coord"), 
                    color = scolor.local, linetype = "factor(coord)")

ppt_x0_z0_NCSEM_from_jpl  = ggplot() + geom_path(data = traj_from_jpl_some, aes.pk, size = line.size)

#Theme
ppt_x0_z0_NCSEM_from_jpl = ppt_x0_z0_NCSEM_from_jpl + theme_pgrey(color.back)
ppt_x0_z0_NCSEM_from_jpl = ppt_x0_z0_NCSEM_from_jpl + scale.color.jpl.local.noguide
ppt_x0_z0_NCSEM_from_jpl = ppt_x0_z0_NCSEM_from_jpl + scale.line.jpl.local.noguide
#ppt_x0_z0_NCSEM_from_jpl = ppt_x0_z0_NCSEM_from_jpl + coord_fixed(ratio=1)

# Labels
ppt_x0_z0_NCSEM_from_jpl = ppt_x0_z0_NCSEM_from_jpl + labs(x = x_sem, y = z_sem)

#Add SEMLi
ppt_x0_z0_NCSEM_from_jpl = ppt_x0_z0_NCSEM_from_jpl + geom_point(data = dfsemli, aes(x= x_NC, y = z_NC), size = 4) 
ppt_x0_z0_NCSEM_from_jpl = ppt_x0_z0_NCSEM_from_jpl + annotate("text", x = 0, y = +0.04,  label = "SEL[2]", size = 5, parse = TRUE)

#Add Earth
ppt_x0_z0_NCSEM_from_jpl = ppt_x0_z0_NCSEM_from_jpl + geom_point(data = dfearth_seml, aes(x= x_NC, y = z_NC), size = 4) 
ppt_x0_z0_NCSEM_from_jpl = ppt_x0_z0_NCSEM_from_jpl + annotate("text", x = -1, y = +0.04, label = "Earth", size = 5, parse = TRUE)

# Right font
ppt_x0_z0_NCSEM_from_jpl = set_font_cm(ppt_x0_z0_NCSEM_from_jpl)

ppt_x0_z0_NCSEM_from_jpl

# Save
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, "_ppt_x0_z0_NCSEM_from_jpl")
  ggsave(ppt_x0_z0_NCSEM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_z0_NCSEM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}


#===============================================================================
# in NCSEM coordinates, yz
#===============================================================================
aes.pk = aes_string("y_NCSEM", "z_NCSEM",  
                    group = cs_interact("label.conn", "coord"), 
                    color = scolor.local, linetype = "factor(coord)")

ppt_y0_z0_NCSEM_from_jpl  = ggplot() + geom_path(data = traj_from_jpl_some, aes.pk, size = line.size)

#Theme
ppt_y0_z0_NCSEM_from_jpl = ppt_y0_z0_NCSEM_from_jpl + theme_pgrey(color.back)
ppt_y0_z0_NCSEM_from_jpl = ppt_y0_z0_NCSEM_from_jpl + scale.color.jpl.local.noguide
ppt_y0_z0_NCSEM_from_jpl = ppt_y0_z0_NCSEM_from_jpl + scale.line.jpl.local.noguide
#ppt_y0_z0_NCSEM_from_jpl = ppt_y0_z0_NCSEM_from_jpl + coord_fixed(ratio=1)

# Labels
ppt_y0_z0_NCSEM_from_jpl = ppt_y0_z0_NCSEM_from_jpl + labs(x = y_sem, y = z_sem)


#Add SEMLi
ppt_y0_z0_NCSEM_from_jpl = ppt_y0_z0_NCSEM_from_jpl + geom_point(data = dfsemli, aes(x= y_NC, y = z_NC), size = 4) 
ppt_y0_z0_NCSEM_from_jpl = ppt_y0_z0_NCSEM_from_jpl + annotate("text", x = 0, y = -0.04,  label = "SEL[2]", size = 5, parse = TRUE)

#Add Earth
#ppt_y0_z0_NCSEM_from_jpl = ppt_y0_z0_NCSEM_from_jpl + geom_point(data = dfearth_seml, aes(x= y_NC, y = z_NC), size = 4) 
#ppt_y0_z0_NCSEM_from_jpl = ppt_y0_z0_NCSEM_from_jpl + annotate("text", x = -1, y = +0.04, label = "Earth", size = 5, parse = TRUE)

# Right font
ppt_y0_z0_NCSEM_from_jpl = set_font_cm(ppt_y0_z0_NCSEM_from_jpl)

ppt_y0_z0_NCSEM_from_jpl

# Save
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, "_ppt_y0_z0_NCSEM_from_jpl")
  ggsave(ppt_y0_z0_NCSEM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_y0_z0_NCSEM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}

#===============================================================================
# in NCEM coordinates, xy
#===============================================================================
aes.pk = aes_string("x_NCEM", "y_NCEM",  group = cs_interact("label.conn", "coord"), color = scolor.local, linetype = "factor(coord)")

ppt_x0_y0_EM_from_jpl = ggplot() + geom_path(data = traj_from_jpl_some_leg1, aes.pk, size = line.size.EM)

# Orbit in black
#ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + geom_path(data = traj_from_jpl_some_emlt, aes.pk, size = line.size.EM, color = "black")

# Theme and titles
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + theme_pgrey(color.back)
#ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + coord_fixed(ratio=1)
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + scale.color.jpl.local.noguide
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + scale.line.jpl.local.noguide

# Add EMLi
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + geom_point(data = dfemli, aes(x= x_NC, y = y_NC), size = 4) 
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + annotate("text", x = 0, y = -0.7,  label = "EML[2]", size = 5, parse = TRUE)

# Labels
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + labs(x = x_em, y = y_em)

# Add the Moon
#ppt_x0_y0_EM_from_jpl = addMoon(ppt_x0_y0_EM_from_jpl, x = dfmoon_eml$x_NC, y = 0, dfmoon_eml$r_NC, surfSize = 0.4, cratSize = 0.2)

# Zoom
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + scale_x_continuous(limits = c(-0.3, +0.3))
ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + scale_y_continuous(limits = c(-0.7,0.7))
#ppt_x0_y0_EM_from_jpl = ppt_x0_y0_EM_from_jpl + ggtitle(paste0(traj_from_jpl_some$re_CMU_EMT_mod[1]))

# Right font
ppt_x0_y0_EM_from_jpl = set_font_cm(ppt_x0_y0_EM_from_jpl)

ppt_x0_y0_EM_from_jpl

# Save
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, "_ppt_x0_y0_EM_from_jpl")
  ggsave(ppt_x0_y0_EM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_y0_EM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}


#===============================================================================
# in NCEM coordinates, xz
#===============================================================================
aes.pk = aes_string("x_NCEM", "z_NCEM",  group = cs_interact("label.conn", "coord"), color = scolor.local, linetype = "factor(coord)")

ppt_x0_z0_EM_from_jpl = ggplot() + geom_path(data = traj_from_jpl_some_leg1, aes.pk, size = line.size.EM)

# Orbit in black
#ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + geom_path(data = traj_from_jpl_some_emlt, aes.pk, size = line.size.EM, color = "black")

# Theme and titles
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + theme_pgrey(color.back)
#ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + coord_fixed(ratio=1)
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + scale.color.jpl.local.noguide
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + scale.line.jpl.local.noguide

# Add EMLi
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + geom_point(data = dfemli, aes(x= x_NC, y = z_NC), size = 4) 
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + annotate("text", x = 0, y = -0.17,  label = "EML[2]", size = 5, parse = TRUE)

# Labels
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + labs(x = x_em, y = z_em)

# Add the Moon
#ppt_x0_z0_EM_from_jpl = addMoon(ppt_x0_z0_EM_from_jpl, x = dfmoon_eml$x_NC, y = 0, dfmoon_eml$r_NC, surfSize = 0.4, cratSize = 0.2)

# Zoom
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + scale_x_continuous(limits = c(-0.3, +0.3))
ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + scale_y_continuous(limits = c(-0.2, 0.2))
#ppt_x0_z0_EM_from_jpl = ppt_x0_z0_EM_from_jpl + ggtitle(paste0(traj_from_jpl_some$re_CMU_EMT_mod[1]))

# Right font
ppt_x0_z0_EM_from_jpl = set_font_cm(ppt_x0_z0_EM_from_jpl)

ppt_x0_z0_EM_from_jpl

# Save
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, "_ppt_x0_z0_EM_from_jpl")
  ggsave(ppt_x0_z0_EM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_x0_z0_EM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}


#===============================================================================
# in NCEM coordinates, yz
#===============================================================================
aes.pk = aes_string("y_NCEM", "z_NCEM",  group = cs_interact("label.conn", "coord"), color = scolor.local, linetype = "factor(coord)")

ppt_y0_z0_EM_from_jpl = ggplot() + geom_path(data = traj_from_jpl_some_leg1, aes.pk, size = line.size.EM)

# Orbit in black
#ppt_y0_z0_EM_from_jpl = ppt_y0_z0_EM_from_jpl + geom_path(data = traj_from_jpl_some_emlt, aes.pk, size = line.size.EM, color = "black")

# Theme and titles
ppt_y0_z0_EM_from_jpl = ppt_y0_z0_EM_from_jpl + theme_pgrey(color.back)
#ppt_y0_z0_EM_from_jpl = ppt_y0_z0_EM_from_jpl + coord_fixed(ratio=1)
ppt_y0_z0_EM_from_jpl = ppt_y0_z0_EM_from_jpl + scale.color.jpl.local.noguide
ppt_y0_z0_EM_from_jpl = ppt_y0_z0_EM_from_jpl + scale.line.jpl.local.noguide

# Add EMLi
ppt_y0_z0_EM_from_jpl = ppt_y0_z0_EM_from_jpl + geom_point(data = dfemli, aes(x= y_NC, y = z_NC), size = 4) 
ppt_y0_z0_EM_from_jpl = ppt_y0_z0_EM_from_jpl + annotate("text", x = 0, y = -0.14,  label = "EML[2]", size = 5, parse = TRUE)

# Labels
ppt_y0_z0_EM_from_jpl = ppt_y0_z0_EM_from_jpl + labs(x = y_em, y = z_em)

# Add the Moon
#ppt_y0_z0_EM_from_jpl = addMoon(ppt_y0_z0_EM_from_jpl, x = dfmoon_eml$x_NC, y = 0, dfmoon_eml$r_NC, surfSize = 0.4, cratSize = 0.2)

# Zoom
ppt_y0_z0_EM_from_jpl = ppt_y0_z0_EM_from_jpl + scale_x_continuous(limits = c(-0.7, +0.7))
ppt_y0_z0_EM_from_jpl = ppt_y0_z0_EM_from_jpl + scale_y_continuous(limits = c(-0.2, 0.2))
#ppt_y0_z0_EM_from_jpl = ppt_y0_z0_EM_from_jpl + ggtitle(paste0(traj_from_jpl_some$re_CMU_EMT_mod[1]))

# Right font
ppt_y0_z0_EM_from_jpl = set_font_cm(ppt_y0_z0_EM_from_jpl)

ppt_y0_z0_EM_from_jpl

# Save
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, "_ppt_y0_z0_EM_from_jpl")
  ggsave(ppt_y0_z0_EM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  device = cairo_pdf, file = paste0(filename, ".pdf"))            #Save in pdf
  ggsave(ppt_y0_z0_EM_from_jpl, width = xSize, height = ySize,  bg = "transparent",  dpi = dpi, file = paste0(filename, ".png")) #Save in png
}