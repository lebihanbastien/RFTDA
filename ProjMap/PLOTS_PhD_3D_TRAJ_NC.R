#===============================================================================
# CONTINUATION: Only some solutions, for one family in traj_cont
#===============================================================================
label.frequency = 4

filename = paste0(filecont, "_x0_y0_NCSEM_cont")
#-------------------------------------------------------------------------------
# We select some solutions within this family:
# First, we get rid of some solutions at the beginning and at the end.
# Then, we get rid of some solutions at the center, to make things clearer
# on the plots
#-------------------------------------------------------------------------------
condition      = (traj_cont$label) %% label.frequency == 0 | traj_cont$label == max(traj_cont$label) | traj_cont$label == min(traj_cont$label)
traj_cont_some = traj_cont[which(condition),]

#===============================================================================
# Color palette
#===============================================================================
values = rev(brewer.pal(6,"Dark2"))
colour.traj = values[2]

size.traj.EM  = 0.6
size.traj.SEM = 0.4

y.position.txt = -0.09

#===============================================================================
# Plotting in SEM coordinates
#===============================================================================
#fpp_path_some(traj_cont_some, filename, values[FAMIND]) (old version)
ppNCSE = fpp_path_traj_phd(traj_cont_some, filename, "NCSE", 
                          limits.x = c(-1.3, 0.2), 
                          limits.y = c(-0.6, 0.6), 
                          xlab = x_sem, ylab = y_sem,
                          xlab.tex = "$x^{s}$", ylab.tex = "$y^{s}$",
                          colour.traj = colour.traj, 
                          lib.point.em = LIB_POINT_EM, 
                          lib.point.sem = LIB_POINT_SEM,
                          isLaTeX = FALSE, isPNG = FALSE, isPDF = FALSE, 
                          size.traj = size.traj.SEM,
                          y.position.labels = y.position.txt, 
                          y.position.earth  = y.position.txt)

# Final points
ppNCSE = ppNCSE + geom_path(data = proj_cont, aes(x0_CMS_NCSEM,  y0_CMS_NCSEM) , color = "white", size = 2)
ppNCSE = ppNCSE + geom_path(data = proj_cont, aes(x0_CMS_NCSEM,  y0_CMS_NCSEM) , color = "black", size = 1)
ppNCSE

# Moon's orbit
moon.mradius = 2.518747349676265e-01;
moon.orbit   = circleOrbit(c(-1,0), moon.mradius, npoints = 100)
ppNCSE = ppNCSE + geom_path(data = moon.orbit, aes(x,y), linetype = "dashed")
ppNCSE

# Fonts
ppNCSE = set_font_cm_ex(ppNCSE)

#-----------------------------------------------------------------------------
# Save in pdf, with annotations
#-----------------------------------------------------------------------------
filename = paste0(filecont, "_x0_y0_NCSEM_cont")
ggsave(ppNCSE, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))


#===============================================================================
# Plotting in SEM coordinates, xz view
#===============================================================================
ppNCSExz = ggplot() + geom_path(data = traj_cont_some, aes(x = x_CMS_NCSEM, y = z_CMS_NCSEM, 
                                                           group = interaction(label, type), 
                                                           colour = factor(type)), 
                              size = size.traj.SEM)

# Initial points
ppNCSExz = ppNCSExz + geom_path(data = proj_cont, aes(x0_CMS_NCSEM,  z0_CMS_NCSEM) , color = "white", size = 2)
ppNCSExz = ppNCSExz + geom_path(data = proj_cont, aes(x0_CMS_NCSEM,  z0_CMS_NCSEM) , color = "black", size = 1)
ppNCSExz

#---------------------------------------------------------------------------
# First & last solutions
#---------------------------------------------------------------------------
ppNCSExz = ppNCSExz + geom_path(data = traj_cont_some[which(traj_cont_some$label == min(traj_cont_some$label)),], 
                            aes(x = x_CMS_NCSEM, y = z_CMS_NCSEM, 
                                group = interaction(label, type)), 
                            colour = muted("red"),
                            size = 2*size.traj.SEM)

ppNCSExz = ppNCSExz + geom_path(data = traj_cont_some[which(traj_cont_some$label == max(traj_cont_some$label)),], 
                            aes(x = x_CMS_NCSEM, y = z_CMS_NCSEM, 
                                group = interaction(label, type)), 
                            colour = muted("blue"),
                            size = 2*size.traj.SEM)

#---------------------------------------------------------------------------
#Add EMLi
#---------------------------------------------------------------------------
ppNCSExz = ppNCSExz + geom_point(data = dfsemli, aes(x= x_NC, y = z_NC), size = 2) 
ppNCSExz = ppNCSExz + geom_point(data = dfearth_seml, aes(x= x_SYS, y = z_SYS), size = 3)

#---------------------------------------------------------------------------
# Annotations & labels
#---------------------------------------------------------------------------
ppNCSExz = annotate_bold(ppNCSExz, x = 0, y = -0.12, label = paste0("SEL[", 2, "]"), size = 5, parse = T)
ppNCSExz = annotate_bold(ppNCSExz, x = dfearth_seml$x_NC, y = -0.12, label = "Earth", size = 5, parse = T)
ppNCSExz = ppNCSExz + labs(x = x_sem, y = z_sem)
ppNCSExz = ppNCSExz + scale_colour_manual(values = c(colour.traj), guide = FALSE)
ppNCSExz = ppNCSExz + custom_theme
ppNCSExz = ppNCSExz + coord_fixed(ratio=1)

#---------------------------------------------------------------------------
# Limits
#---------------------------------------------------------------------------
ppNCSExz = ppNCSExz + scale_x_continuous(limits = c(-1.3, 0.2))
ppNCSExz = ppNCSExz + scale_y_continuous(limits = c(-0.15, 0.15))
ppNCSExz

# Fonts
ppNCSExz = set_font_cm_ex(ppNCSExz)

#-----------------------------------------------------------------------------
# Save in pdf, with annotations
#-----------------------------------------------------------------------------
filename = paste0(filecont, "_x0_z0_NCSEM_cont")
ggsave(ppNCSExz, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))


#===============================================================================
# Plotting in SEM coordinates, xz view
#===============================================================================
ppNCSEyz = ggplot() + geom_path(data = traj_cont_some, aes(x = y_CMS_NCSEM, y = z_CMS_NCSEM, 
                                                          group = interaction(label, type), 
                                                          colour = factor(type)), 
                               size = size.traj.SEM)

# Initial points
ppNCSEyz = ppNCSEyz + geom_path(data = proj_cont, aes(y0_CMS_NCSEM,  z0_CMS_NCSEM) , color = "white", size = 2)
ppNCSEyz = ppNCSEyz + geom_path(data = proj_cont, aes(y0_CMS_NCSEM,  z0_CMS_NCSEM) , color = "black", size = 1)
ppNCSEyz

#---------------------------------------------------------------------------
# First & last solutions
#---------------------------------------------------------------------------
ppNCSEyz = ppNCSEyz + geom_path(data = traj_cont_some[which(traj_cont_some$label == min(traj_cont_some$label)),], 
                              aes(x = y_CMS_NCSEM, y = z_CMS_NCSEM, 
                                  group = interaction(label, type)), 
                              colour = muted("red"),
                              size = 2*size.traj.SEM)

ppNCSEyz = ppNCSEyz + geom_path(data = traj_cont_some[which(traj_cont_some$label == max(traj_cont_some$label)),], 
                              aes(x = y_CMS_NCSEM, y = z_CMS_NCSEM, 
                                  group = interaction(label, type)), 
                              colour = muted("blue"),
                              size = 2*size.traj.SEM)

#---------------------------------------------------------------------------
#Add EMLi
#---------------------------------------------------------------------------
ppNCSEyz = ppNCSEyz + geom_point(data = dfsemli, aes(x= y_NC, y = z_NC), size = 2) 

#---------------------------------------------------------------------------
# Annotations & labels
#---------------------------------------------------------------------------
ppNCSEyz = annotate_bold(ppNCSEyz, x = 0, y = -0.1, label = paste0("SEL[", 2, "]"), size = 5, parse = T)
ppNCSEyz = ppNCSEyz + labs(x = y_sem, y = z_sem)
ppNCSEyz = ppNCSEyz + scale_colour_manual(values = c(colour.traj), guide = FALSE)
ppNCSEyz = ppNCSEyz + custom_theme
ppNCSEyz = ppNCSEyz + coord_fixed(ratio=1)

#---------------------------------------------------------------------------
# Limits
#---------------------------------------------------------------------------
ppNCSEyz = ppNCSEyz + scale_x_continuous(limits = c(-0.6, 0.6))
ppNCSEyz = ppNCSEyz + scale_y_continuous(limits = c(-0.15, 0.15))
ppNCSEyz

# Fonts
ppNCSEyz = set_font_cm_ex(ppNCSEyz)

#-----------------------------------------------------------------------------
# Save in pdf, with annotations
#-----------------------------------------------------------------------------
filename = paste0(filecont, "_y0_z0_NCSEM_cont")
ggsave(ppNCSEyz, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))



#===============================================================================
# Plotting in EM coordinates
#===============================================================================
limits.x = c(-0.5, 0.8)
limits.y = c(-0.8, +0.8)


ppNCEM = fpp_path_traj_phd(traj_cont_some, filename, "NCEM", 
                         limits.x = limits.x, 
                         limits.y = limits.y, 
                         xlab = x_em, ylab = y_em,
                         xlab.tex = "$x^{e}$", ylab.tex = "$y^{e}$",
                         colour.traj = colour.traj, 
                         lib.point.em = LIB_POINT_EM, 
                         lib.point.sem = LIB_POINT_SEM,
                         isLaTeX = FALSE, isPNG = FALSE, isPDF = FALSE, isPrimary = FALSE,
                         size.traj = size.traj.EM, y.position.emlt = -0.7)
# Initial points
ppNCEM = ppNCEM + geom_path(data = proj_cont, aes(x0_CMU_NCEM,  y0_CMU_NCEM) , color = "white", size = 2)
ppNCEM = ppNCEM + geom_path(data = proj_cont, aes(x0_CMU_NCEM,  y0_CMU_NCEM) , color = "black", size = 1)
ppNCEM

# Fonts
ppNCEM = set_font_cm_ex(ppNCEM)

#-----------------------------------------------------------------------------
# Save in pdf, with annotations
#-----------------------------------------------------------------------------
filename = paste0(filecont, "_x0_y0_NCEM_cont")
ggsave(ppNCEM, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))


#===============================================================================
# Plotting in EM coordinates, xz view
#===============================================================================
traj_cont_some_t = traj_cont_some[which(traj_cont_some$t_CMU_SEM < 0.95),]


ppNCEMxz = ggplot() + geom_path(data = traj_cont_some_t, aes(x = x_CMS_NCEM, y = z_CMS_NCEM, 
                                 group = interaction(label, type), 
                                 colour = factor(type)), 
                              size = size.traj.EM)

# Initial points
ppNCEMxz = ppNCEMxz + geom_path(data = proj_cont, aes(x0_CMU_NCEM,  z0_CMU_NCEM) , color = "white", size = 2)
ppNCEMxz = ppNCEMxz + geom_path(data = proj_cont, aes(x0_CMU_NCEM,  z0_CMU_NCEM) , color = "black", size = 1)
ppNCEMxz

#---------------------------------------------------------------------------
# First & last solutions
#---------------------------------------------------------------------------
ppNCEMxz = ppNCEMxz + geom_path(data = traj_cont_some_t[which(traj_cont_some_t$label == min(traj_cont_some_t$label)),], 
                          aes(x = x_CMS_NCEM, y = z_CMS_NCEM, 
                              group = interaction(label, type)), 
                          colour = muted("red"),
                          size = 2*size.traj.EM)

ppNCEMxz = ppNCEMxz + geom_path(data = traj_cont_some_t[which(traj_cont_some_t$label == max(traj_cont_some_t$label)),], 
                          aes(x = x_CMS_NCEM, y = z_CMS_NCEM, 
                              group = interaction(label, type)), 
                          colour = muted("blue"),
                          size = 2*size.traj.EM)

#---------------------------------------------------------------------------
#Add EMLi
#---------------------------------------------------------------------------
ppNCEMxz = ppNCEMxz + geom_point(data = dfemli, aes(x= x_NC, y = z_NC), size = 2)

#---------------------------------------------------------------------------
# Annotations & labels
#---------------------------------------------------------------------------
ppNCEMxz = annotate_bold(ppNCEMxz, x = 0, y = 0.75, label = paste0("EML[", 2, "]"), size = 5, parse = T)
ppNCEMxz = ppNCEMxz + labs(x = x_em, y = z_em)
ppNCEMxz = ppNCEMxz + scale_colour_manual(values = c(colour.traj), guide = FALSE)
ppNCEMxz = ppNCEMxz + custom_theme
ppNCEMxz = ppNCEMxz + coord_fixed(ratio=1)

#---------------------------------------------------------------------------
# Limits
#---------------------------------------------------------------------------
ppNCEMxz = ppNCEMxz + scale_x_continuous(limits = c(-0.5, 0.8))
ppNCEMxz = ppNCEMxz + scale_y_continuous(limits = c(-1.1, +0.8))
ppNCEMxz

# Fonts
ppNCEMxz = set_font_cm_ex(ppNCEMxz)

#-----------------------------------------------------------------------------
# Save in pdf, with annotations
#-----------------------------------------------------------------------------
filename = paste0(filecont, "_x0_z0_NCEM_cont")
ggsave(ppNCEMxz, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))


#===============================================================================
# Plotting in EM coordinates, xz view
#===============================================================================
traj_cont_some_t = traj_cont_some[which(traj_cont_some$t_CMU_SEM < 1),]


ppNCEMyz = ggplot() + geom_path(data = traj_cont_some_t, aes(x = y_CMS_NCEM, y = z_CMS_NCEM, 
                                                           group = interaction(label, type), 
                                                           colour = factor(type)), 
                              size = size.traj.EM)

# Initial points
ppNCEMyz = ppNCEMyz + geom_path(data = proj_cont, aes(y0_CMU_NCEM,  z0_CMU_NCEM) , color = "white", size = 2)
ppNCEMyz = ppNCEMyz + geom_path(data = proj_cont, aes(y0_CMU_NCEM,  z0_CMU_NCEM) , color = "black", size = 1)
ppNCEMyz

#---------------------------------------------------------------------------
# First & last solutions
#---------------------------------------------------------------------------
ppNCEMyz = ppNCEMyz + geom_path(data = traj_cont_some_t[which(traj_cont_some_t$label == min(traj_cont_some_t$label)),], 
                            aes(x = y_CMS_NCEM, y = z_CMS_NCEM, 
                                group = interaction(label, type)), 
                            colour = muted("red"),
                            size = 2*size.traj.EM)

ppNCEMyz = ppNCEMyz + geom_path(data = traj_cont_some_t[which(traj_cont_some_t$label == max(traj_cont_some_t$label)),], 
                            aes(x = y_CMS_NCEM, y = z_CMS_NCEM, 
                                group = interaction(label, type)), 
                            colour = muted("blue"),
                            size = 2*size.traj.EM)

#---------------------------------------------------------------------------
#Add EMLi
#---------------------------------------------------------------------------
ppNCEMyz = ppNCEMyz + geom_point(data = dfemli, aes(x= y_NC, y = z_NC), size = 2) 

#---------------------------------------------------------------------------
# Annotations & labels
#---------------------------------------------------------------------------
ppNCEMyz = annotate_bold(ppNCEMyz, x = 0, y = 0.75, label = paste0("EML[", 2, "]"), size = 5, parse = T)
ppNCEMyz = ppNCEMyz + labs(x = y_em, y = z_em)
ppNCEMyz = ppNCEMyz + scale_colour_manual(values = c(colour.traj), guide = FALSE)
ppNCEMyz = ppNCEMyz + custom_theme
ppNCEMyz = ppNCEMyz + coord_fixed(ratio=1)

#---------------------------------------------------------------------------
# Limits
#---------------------------------------------------------------------------
ppNCEMyz = ppNCEMyz + scale_x_continuous(limits = c(-1, 0.8))
ppNCEMyz = ppNCEMyz + scale_y_continuous(limits = c(-1.1, +0.8))
ppNCEMyz

# Fonts
ppNCEMyz = set_font_cm_ex(ppNCEMyz)

#-----------------------------------------------------------------------------
# Save in pdf, with annotations
#-----------------------------------------------------------------------------
filename = paste0(filecont, "_y0_z0_NCEM_cont")
ggsave(ppNCEMyz, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))