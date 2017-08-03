#===============================================================================
# CONTINUATION: Only some solutions, for one family in traj_cont
#===============================================================================
label.frequency = 4

#-------------------------------------------------------------------------------
# We select some solutions within this family:
# First, we get rid of some solutions at the beginning and at the end.
# Then, we get rid of some solutions at the center, to make things clearer
# on the plots
#-------------------------------------------------------------------------------
condition      = (traj_cont$label) %% label.frequency == 0 | 
  traj_cont$label == max(traj_cont$label) | 
  traj_cont$label == min(traj_cont$label)
traj_cont_some = traj_cont[which(condition),]

#===============================================================================
# Color palette
#===============================================================================
colour.pal  = rev(brewer.pal(6,"Dark2"))
colour.traj = colour.pal[2]

size.traj.EM  = 0.6
size.traj.SEM = 0.4

y.position.txt = +9e-4

limits.x.SE = c(-1.012, -0.9972)
limits.y.SE = c(-0.006, 0.0061)
limits.z.SE = c(-1.5e-3, 1.5e-3)

limits.x.EM = c(-1.3, -1.07)
limits.y.EM = c(-0.13, +0.13)
limits.z.EM = c(-0.185, 0.12)

#===============================================================================
# Plotting in SEM coordinates
#===============================================================================
#fpp_path_some(traj_cont_some, filename, colour.pal[FAMIND]) (old version)
ppSE = fpp_path_traj_phd(traj_cont_some, filename, "SE", 
                         limits.x = limits.x.SE, 
                         limits.y = limits.y.SE,
                         xlab = X_E, ylab = Y_E,
                         xlab.tex = "$X^{E}$", ylab.tex = "$Y^{E}$",
                         colour.traj = colour.traj, 
                         lib.point.em = LIB_POINT_EM, 
                         lib.point.sem = LIB_POINT_SEM,
                         isLaTeX = FALSE, isPNG = FALSE, isPDF = FALSE, 
                         size.traj = size.traj.SEM,
                         y.position.labels = y.position.txt, 
                         y.position.earth  = y.position.txt)

# Final points
ppSE = ppSE + geom_path(data = proj_cont, aes(x0_CMS_SEM,  y0_CMS_SEM) , color = "white", size = 2)
ppSE = ppSE + geom_path(data = proj_cont, aes(x0_CMS_SEM,  y0_CMS_SEM) , color = "black", size = 1)
ppSE

# Moon's orbit
moon.mradius = 2.518747349676265e-01*CST_GAMMA_LIB_SEM;
moon.orbit   = circleOrbit(c(-1,0), moon.mradius, npoints = 100)
ppSE = ppSE + geom_path(data = moon.orbit, aes(x,y), linetype = "dashed")
ppSE


#-------------------------------------------------------------------------------
# Save in pdf, with annotations
#-------------------------------------------------------------------------------
filename = paste0(filecont, "_x0_y0_SE_cont")
ggsave(ppSE, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))


#===============================================================================
# Plotting in SEM coordinates, xz view
#===============================================================================
ppSExz = ggplot() + geom_path(data = traj_cont_some, 
                                aes(x = x_CMS_SEM, y = z_CMS_SEM, 
                                    group = interaction(label, type), 
                                    colour = factor(type)), 
                                size = size.traj.SEM)

# Initial points
ppSExz = ppSExz + geom_path(data = proj_cont, aes(x0_CMS_SEM,  z0_CMS_SEM) , color = "white", size = 2)
ppSExz = ppSExz + geom_path(data = proj_cont, aes(x0_CMS_SEM,  z0_CMS_SEM) , color = "black", size = 1)
ppSExz

#---------------------------------------------------------------------------
# First & last solutions
#---------------------------------------------------------------------------
ppSExz = ppSExz + geom_path(data = traj_cont_some[which(traj_cont_some$label == min(traj_cont_some$label)),], 
                            aes(x = x_CMS_SEM, y = z_CMS_SEM, 
                                group = interaction(label, type)), 
                            colour = muted("red"),
                            size = 2*size.traj.SEM)

ppSExz = ppSExz + geom_path(data = traj_cont_some[which(traj_cont_some$label == max(traj_cont_some$label)),], 
                            aes(x = x_CMS_SEM, y = z_CMS_SEM, 
                                group = interaction(label, type)), 
                            colour = muted("blue"),
                            size = 2*size.traj.SEM)

#---------------------------------------------------------------------------
#Add EMLi & Earth
#---------------------------------------------------------------------------
ppSExz = ppSExz + geom_point(data = dfsemli, aes(x= x_SYS, y = z_SYS), size = 2) 
ppSExz = ppSExz + geom_point(data = dfearth_seml, aes(x= x_SYS, y = z_SYS), size = 3) 

#---------------------------------------------------------------------------
# Annotations & labels
#---------------------------------------------------------------------------
ppSExz = annotate_bold(ppSExz, x = dfsemli$x_SYS,      y = -1.1e-3, label = paste0("SEL[", 2, "]"), size = 5, parse = T)
ppSExz = annotate_bold(ppSExz, x = dfearth_seml$x_SYS, y = -1.1e-3, label = "Earth", size = 5, parse = T)

ppSExz = ppSExz + labs(x = X_S, y = Z_S)
ppSExz = ppSExz + scale_colour_manual(values = c(colour.traj), guide = FALSE)
ppSExz = ppSExz + custom_theme
ppSExz = ppSExz + coord_fixed(ratio=1)

#---------------------------------------------------------------------------
# Limits
#---------------------------------------------------------------------------
ppSExz = ppSExz + scale_x_continuous(limits = limits.x.SE)
ppSExz = ppSExz + scale_y_continuous(limits = limits.z.SE)
ppSExz

#-----------------------------------------------------------------------------
# Save in pdf, with annotations
#-----------------------------------------------------------------------------
filename = paste0(filecont, "_x0_z0_SE_cont")
ggsave(ppSExz, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))


#===============================================================================
# Plotting in SEM coordinates, xz view
#===============================================================================
ppSEyz = ggplot() + geom_path(data = traj_cont_some, aes(x = y_CMS_SEM, y = z_CMS_SEM, 
                                                          group = interaction(label, type), 
                                                          colour = factor(type)), 
                               size = size.traj.SEM)

# Initial points
ppSEyz = ppSEyz + geom_path(data = proj_cont, aes(y0_CMS_SEM,  z0_CMS_SEM) , color = "white", size = 2)
ppSEyz = ppSEyz + geom_path(data = proj_cont, aes(y0_CMS_SEM,  z0_CMS_SEM) , color = "black", size = 1)
ppSEyz

#---------------------------------------------------------------------------
# First & last solutions
#---------------------------------------------------------------------------
ppSEyz = ppSEyz + geom_path(data = traj_cont_some[which(traj_cont_some$label == min(traj_cont_some$label)),], 
                              aes(x = y_CMS_SEM, y = z_CMS_SEM, 
                                  group = interaction(label, type)), 
                              colour = muted("red"),
                              size = 2*size.traj.SEM)

ppSEyz = ppSEyz + geom_path(data = traj_cont_some[which(traj_cont_some$label == max(traj_cont_some$label)),], 
                              aes(x = y_CMS_SEM, y = z_CMS_SEM, 
                                  group = interaction(label, type)), 
                              colour = muted("blue"),
                              size = 2*size.traj.SEM)

#---------------------------------------------------------------------------
#Add EMLi
#---------------------------------------------------------------------------
ppSEyz = ppSEyz + geom_point(data = dfsemli, aes(x= y_SYS, y = z_SYS), size = 2) 

#---------------------------------------------------------------------------
# Annotations & labels
#---------------------------------------------------------------------------
#ppSEyz = annotate_bold(ppSEyz, x = dfsemli$y_SYS, y = +1.2e-3, label = paste0("SEL[", 2, "]"), size = 5, parse = T)
ppSEyz = ppSEyz + labs(x = Y_S, y = Z_S)
ppSEyz = ppSEyz + scale_colour_manual(values = c(colour.traj), guide = FALSE)
ppSEyz = ppSEyz + custom_theme
ppSEyz = ppSEyz + coord_fixed(ratio=1)

#---------------------------------------------------------------------------
# Limits
#---------------------------------------------------------------------------
ppSEyz = ppSEyz + scale_x_continuous(limits = limits.y.SE)
ppSEyz = ppSEyz + scale_y_continuous(limits = limits.z.SE)
ppSEyz


#-----------------------------------------------------------------------------
# Save in pdf, with annotations
#-----------------------------------------------------------------------------
filename = paste0(filecont, "_y0_z0_SE_cont")
ggsave(ppSEyz, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))



#===============================================================================
# Plotting in EM coordinates
#===============================================================================
ppEM = fpp_path_traj_phd(traj_cont_some, filename, "EM", 
                         limits.x = limits.x.EM, 
                         limits.y = limits.y.EM, 
                         xlab = X_E, ylab = Y_E,
                         xlab.tex = "$X^{E}$", ylab.tex = "$Y^{E}$",
                         colour.traj = colour.traj, 
                         lib.point.em = LIB_POINT_EM, 
                         lib.point.sem = LIB_POINT_SEM,
                         isLaTeX = FALSE, isPNG = FALSE, isPDF = FALSE, isPrimary = FALSE,
                         size.traj = size.traj.EM, y.position.emlt = +0.115)
# Initial points
ppEM = ppEM + geom_path(data = proj_cont, aes(x0_CMU_EM,  y0_CMU_EM) , color = "white", size = 2)
ppEM = ppEM + geom_path(data = proj_cont, aes(x0_CMU_EM,  y0_CMU_EM) , color = "black", size = 1)
ppEM

#-----------------------------------------------------------------------------
# Save in pdf
#-----------------------------------------------------------------------------
filename = paste0(filecont, "_x0_y0_EM_cont")
ggsave(ppEM, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))


#===============================================================================
# Plotting in EM coordinates, xz view
#===============================================================================
traj_cont_some_t = traj_cont_some[which(traj_cont_some$t_CMU_SEM < 0.95),]

ppEMxz = ggplot() + geom_path(data = traj_cont_some_t, aes(x = x_CMS_EM, y = z_CMS_EM, 
                                 group = interaction(label, type), 
                                 colour = factor(type)), 
                              size = size.traj.EM)

# Initial points
ppEMxz = ppEMxz + geom_path(data = proj_cont, aes(x0_CMU_EM,  z0_CMU_EM) , color = "white", size = 2)
ppEMxz = ppEMxz + geom_path(data = proj_cont, aes(x0_CMU_EM,  z0_CMU_EM) , color = "black", size = 1)
ppEMxz

#---------------------------------------------------------------------------
# First & last solutions
#---------------------------------------------------------------------------
ppEMxz = ppEMxz + geom_path(data = traj_cont_some_t[which(traj_cont_some_t$label == min(traj_cont_some_t$label)),], 
                          aes(x = x_CMS_EM, y = z_CMS_EM, 
                              group = interaction(label, type)), 
                          colour = muted("red"),
                          size = 2*size.traj.EM)

ppEMxz = ppEMxz + geom_path(data = traj_cont_some_t[which(traj_cont_some_t$label == max(traj_cont_some_t$label)),], 
                          aes(x = x_CMS_EM, y = z_CMS_EM, 
                              group = interaction(label, type)), 
                          colour = muted("blue"),
                          size = 2*size.traj.EM)

#---------------------------------------------------------------------------
#Add EMLi
#---------------------------------------------------------------------------
ppEMxz = ppEMxz + geom_point(data = dfemli, aes(x= x_SYS, y = z_SYS), size = 2) 

#---------------------------------------------------------------------------
# Annotations & labels
#---------------------------------------------------------------------------
ppEMxz = annotate_bold(ppEMxz, x = dfemli$x_SYS, y = 0.118, label = paste0("EML[", 2, "]"), size = 5, parse = T)
ppEMxz = ppEMxz + labs(x = X_E, y = Z_E)
ppEMxz = ppEMxz + scale_colour_manual(values = c(colour.traj), guide = FALSE)
ppEMxz = ppEMxz + custom_theme
ppEMxz = ppEMxz + coord_fixed(ratio=1)

#---------------------------------------------------------------------------
# Limits
#---------------------------------------------------------------------------
ppEMxz = ppEMxz + scale_x_continuous(limits = limits.x.EM)
ppEMxz = ppEMxz + scale_y_continuous(limits = limits.z.EM)
ppEMxz

#-----------------------------------------------------------------------------
# Save in pdf, with annotations
#-----------------------------------------------------------------------------
filename = paste0(filecont, "_x0_z0_EM_cont")
ggsave(ppEMxz, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))


#===============================================================================
# Plotting in EM coordinates, xz view
#===============================================================================
traj_cont_some_t = traj_cont_some[which(traj_cont_some$t_CMU_SEM < 1),]


ppEMyz = ggplot() + geom_path(data = traj_cont_some_t, aes(x = y_CMS_EM, y = z_CMS_EM, 
                                                           group = interaction(label, type), 
                                                           colour = factor(type)), 
                              size = size.traj.EM)

# Initial points
ppEMyz = ppEMyz + geom_path(data = proj_cont, aes(y0_CMU_EM,  z0_CMU_EM) , color = "white", size = 2)
ppEMyz = ppEMyz + geom_path(data = proj_cont, aes(y0_CMU_EM,  z0_CMU_EM) , color = "black", size = 1)
ppEMyz

#---------------------------------------------------------------------------
# First & last solutions
#---------------------------------------------------------------------------
ppEMyz = ppEMyz + geom_path(data = traj_cont_some_t[which(traj_cont_some_t$label == min(traj_cont_some_t$label)),], 
                            aes(x = y_CMS_EM, y = z_CMS_EM, 
                                group = interaction(label, type)), 
                            colour = muted("red"),
                            size = 2*size.traj.EM)

ppEMyz = ppEMyz + geom_path(data = traj_cont_some_t[which(traj_cont_some_t$label == max(traj_cont_some_t$label)),], 
                            aes(x = y_CMS_EM, y = z_CMS_EM, 
                                group = interaction(label, type)), 
                            colour = muted("blue"),
                            size = 2*size.traj.EM)

#---------------------------------------------------------------------------
#Add EMLi
#---------------------------------------------------------------------------
ppEMyz = ppEMyz + geom_point(data = dfemli, aes(x= y_SYS, y = z_SYS), size = 2) 

#---------------------------------------------------------------------------
# Annotations & labels
#---------------------------------------------------------------------------
ppEMyz = annotate_bold(ppEMyz, x = dfemli$y_SYS, y = -0.185, label = paste0("EML[", 2, "]"), size = 5, parse = T)
ppEMyz = ppEMyz + labs(x = Y_E, y = Z_E)
ppEMyz = ppEMyz + scale_colour_manual(values = c(colour.traj), guide = FALSE)
ppEMyz = ppEMyz + custom_theme
ppEMyz = ppEMyz + coord_fixed(ratio=1)

#---------------------------------------------------------------------------
# Limits
#---------------------------------------------------------------------------
ppEMyz = ppEMyz + scale_x_continuous(limits = limits.y.EM)
ppEMyz = ppEMyz + scale_y_continuous(limits = limits.z.EM)
ppEMyz

#-----------------------------------------------------------------------------
# Save in pdf, with annotations
#-----------------------------------------------------------------------------
filename = paste0(filecont, "_y0_z0_EM_cont")
ggsave(ppEMyz, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))