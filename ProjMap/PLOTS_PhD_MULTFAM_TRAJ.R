#===============================================================================
# CONTINUATION: Only some solutions, for one family in traj_cont
#===============================================================================
label.frequency = label.frequency.vec[FAM]

#===============================================================================
# Color palette for families
#===============================================================================
values = rev(brewer.pal(NFAM,"Dark2"))

#-------------------------------------------------------------------------------
# We select some solutions within this family:
# First, we get rid of some solutions at the beginning and at the end.
# Then, we get rid of some solutions at the center, to make things clearer
# on the plots
#-------------------------------------------------------------------------------
condition      = (traj_cont$label) %% label.frequency == 0 | traj_cont$label == max(traj_cont$label) | traj_cont$label == min(traj_cont$label)
traj_cont_some = traj_cont[which(condition),]


#-------------------------------------------------------------------------------
# Aes
#-------------------------------------------------------------------------------
limits.x.NCSE = c(-1.5, 0.3)
limits.y.NCSE = c(-1.3, 1.3)

limits.x.NCEM = c(-0.5, 0.8)
limits.y.NCEM = c(-0.8, +0.8)

limits.x.SE = c(-1.015, -0.995) 
limits.y.SE = c(-0.013, 0.013)

limits.x.EM = c(-1.25, -1.1)
limits.y.EM = c(-0.15, +0.15)


#===============================================================================
# Plotting in NCSE coordinates
#===============================================================================
#fpp_path_some(traj_cont_some, filename, values[FAMIND]) (old version)
ppNCSE = fpp_path_traj_phd(traj_cont_some, filename, "NCSE", 
                          limits.x = limits.x.NCSE, 
                          limits.y = limits.y.NCSE, 
                          xlab = x_sem, ylab = y_sem,
                          xlab.tex = "$x^{s}$", ylab.tex = "$y^{s}$",
                          colour.traj = values[FAMIND], 
                          lib.point.em = LIB_POINT_EM, 
                          lib.point.sem = LIB_POINT_SEM,
                          isLaTeX = FALSE, isPNG = FALSE, isPDF = FALSE, 
                          size.traj = 0.6,
                          y.position.labels = y.position.labels.vec[FAM], 
                          y.position.earth = y.position.earth.vec[FAM])

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

#-------------------------------------------------------------------------------
# Save in pdf, with annotations
#-------------------------------------------------------------------------------
filename = paste0(filecont, "_x0_y0_NCSEM_cont_fam", FAM)
ggsave(ppNCSE, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))


#===============================================================================
# Zoom in NCSE coordinates
#===============================================================================
# Initial points
ppNCSEz = ppNCSE + geom_path(data = proj_cont_label, aes(x_CMS_NCSEM,  y_CMS_NCSEM) , color = "white", size = 1)
ppNCSEz = ppNCSEz + geom_path(data = proj_cont_label, aes(x_CMS_NCSEM,  y_CMS_NCSEM) , color = "black", size = 0.5)

ppNCSEz = ppNCSEz + scale_x_continuous(limits = c(-0.78, -0.6))
ppNCSEz = ppNCSEz + scale_y_continuous(limits = c(-0.05, 0.05))
ppNCSEz

#-----------------------------------------------------------------------------
# Save in pdf, with annotations
#-----------------------------------------------------------------------------
ggsave(ppNCSEz, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, "_zoom.pdf"))

#===============================================================================
# Plotting in NCEM coordinates
#===============================================================================
ppNCEM = fpp_path_traj_phd(traj_cont_some, filename, "NCEM", 
                          limits.x = limits.x.NCEM, 
                          limits.y = limits.y.NCEM, 
                          xlab = x_em, ylab = y_em,
                          xlab.tex = "$x^{e}$", ylab.tex = "$y^{e}$",
                          colour.traj = values[FAMIND], 
                          lib.point.em = LIB_POINT_EM, 
                          lib.point.sem = LIB_POINT_SEM,
                          isLaTeX = FALSE, isPNG = FALSE, 
                          isPDF = FALSE, isPrimary = FALSE,
                          size.traj = 0.6)
# Initial points
ppNCEM = ppNCEM + geom_path(data = proj_cont, aes(x0_CMU_NCEM,  y0_CMU_NCEM) , color = "white", size = 2)
ppNCEM = ppNCEM + geom_path(data = proj_cont, aes(x0_CMU_NCEM,  y0_CMU_NCEM) , color = "black", size = 1)
ppNCEM

# Fonts
ppNCEM = set_font_cm_ex(ppNCEM)

#-----------------------------------------------------------------------------
# Save in pdf, with annotations
#-----------------------------------------------------------------------------
filename = paste0(filecont, "_x0_y0_NCEM_cont_fam", FAM)
ggsave(ppNCEM, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))


#===============================================================================
# Plotting in SE coordinates
#===============================================================================
ppSE = fpp_path_traj_phd(traj_cont_some, filename, "SE", 
                         limits.x = limits.x.SE, 
                         limits.y = limits.y.SE, 
                         xlab = X_S, ylab = Y_S,
                         xlab.tex = "$X^{S}$", ylab.tex = "$Y^{S}$",
                         colour.traj = values[FAMIND], 
                         lib.point.em = LIB_POINT_EM, 
                         lib.point.sem = LIB_POINT_SEM,
                         isLaTeX = FALSE, isPNG = FALSE, isPDF = FALSE, 
                         size.traj = 0.6,
                         y.position.labels = -y.position.labels.vec[FAM]*CST_GAMMA_LIB_SEM, 
                         y.position.earth = -y.position.earth.vec[FAM]*CST_GAMMA_LIB_SEM)

# Final points
ppSE = ppSE + geom_path(data = proj_cont, aes(x0_CMS_SEM,  y0_CMS_SEM) , color = "white", size = 2)
ppSE = ppSE + geom_path(data = proj_cont, aes(x0_CMS_SEM,  y0_CMS_SEM) , color = "black", size = 1)

# Moon's orbit
moon.mradius = 2.518747349676265e-01*CST_GAMMA_LIB_SEM;
moon.orbit   = circleOrbit(c(-1,0), moon.mradius, npoints = 100)
ppSE = ppSE + geom_path(data = moon.orbit, aes(x,y), linetype = "dashed")
ppSE

#-----------------------------------------------------------------------------
# Save in pdf, with annotations
#-----------------------------------------------------------------------------
filename = paste0(filecont, "_x0_y0_SE_cont_fam", FAM)
ggsave(ppSE, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))


#===============================================================================
# Plotting in EM coordinates
#===============================================================================
ppEM = fpp_path_traj_phd(traj_cont_some, filename, "EM", 
                         limits.x = limits.x.EM, 
                         limits.y = limits.y.EM, 
                         xlab = X_E, ylab = Y_E,
                         xlab.tex = "$X^{E}$", ylab.tex = "$Y^{E}$",
                         colour.traj = values[FAMIND], 
                         lib.point.em = LIB_POINT_EM, 
                         lib.point.sem = LIB_POINT_SEM,
                         isLaTeX = FALSE, isPNG = FALSE, 
                         isPDF = FALSE, isPrimary = FALSE,
                         y.position.emlt = -0.75*CST_GAMMA_LIB_EM,
                         size.traj = 0.6)
# Initial points
ppEM = ppEM + geom_path(data = proj_cont, aes(x0_CMU_EM,  y0_CMU_EM) , color = "white", size = 2)
ppEM = ppEM + geom_path(data = proj_cont, aes(x0_CMU_EM,  y0_CMU_EM) , color = "black", size = 1)
ppEM

#-----------------------------------------------------------------------------
# Save in pdf, with annotations
#-----------------------------------------------------------------------------
filename = paste0(filecont, "_x0_y0_EM_cont_fam", FAM)
ggsave(ppEM, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))

