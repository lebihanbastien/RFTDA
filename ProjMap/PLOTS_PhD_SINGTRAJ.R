#===============================================================================
# CONTINUATION: Only some solutions, for one family in traj_cont
#===============================================================================
label.frequency = 14
#-------------------------------------------------------------------------------
# We select some solutions within this family:
# First, we get rid of some solutions at the beginning and at the end.
# Then, we get rid of some solutions at the center, to make things clearer
# on the plots
#-------------------------------------------------------------------------------
label_min = lab_min_vec[FAM]
label_max = max(traj_cont$label) - lab_min_vec[FAM]

minmaxlabel = traj_cont$label > label_min & traj_cont$label < label_max
traj_cont  = traj_cont[which(minmaxlabel),]

condition      = (traj_cont$label) %% label.frequency == 0
traj_cont_some = traj_cont[which(condition),]

#-------------------------------------------------------------------------------
# Aes
#-------------------------------------------------------------------------------
limits.x.NCSE = c(-1.5, 0.3)
limits.y.NCSE = c(-1.05, 1.05)

limits.x.SE = c(-1.013, -0.996) 
limits.y.SE = c(-0.0101, 0.0101)

limits.x.NCEM = c(-0.5, 0.8)
limits.y.NCEM = c(-0.8, +0.8)

limits.x.EM = c(-1.25, -1.1)
limits.y.EM = c(-0.15, +0.15)

#-------------------------------------------------------------------------------
# Labels (additionnal)
#-------------------------------------------------------------------------------
xncse  = "$x^\\mathsctiny{s}$"
yncse  = "$y^\\mathsctiny{s}$"

xse  = "$X^\\mathsctiny{S}$"
yse  = "$Y^\\mathsctiny{S}$"


#===============================================================================
# Single in NCSE coordinates
#===============================================================================
filename = paste0(filecont, "_x0_y0_NCSE_single", FAM)

ppNCSEs = fpp_path_traj_phd(traj_cont_single, filename, "NCSE", 
                            limits.x = limits.x.NCSE, 
                            limits.y = limits.y.NCSE, 
                            xlab = x_sem, ylab = y_sem,
                            xlab.tex = xncse, ylab.tex = yncse,
                            colour.traj = values[FAMIND], 
                            lib.point.em = LIB_POINT_EM, 
                            lib.point.sem = LIB_POINT_SEM,
                            isLaTeX = TRUE, isPNG = FALSE, isPDF = FALSE, 
                            size.traj = 0.6, isSingle = TRUE)

# Fonts
ppNCSEs = set_font_cm_ex(ppNCSEs)

# Save in pdf, with annotations
ggsave(ppNCSEs, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))


#===============================================================================
# Single in SE coordinates
#===============================================================================
filename = paste0(filecont, "_x0_y0_SE_single", FAM)

ppSEs = fpp_path_traj_phd(traj_cont_single, filename, "SE", 
                            limits.x = limits.x.SE, 
                            limits.y = limits.y.SE, 
                            xlab = X_S, ylab = Y_S,
                            xlab.tex = xse, ylab.tex = yse,
                            colour.traj = values[FAMIND], 
                            lib.point.em = LIB_POINT_EM, 
                            lib.point.sem = LIB_POINT_SEM,
                            isLaTeX = TRUE, isPNG = FALSE, isPDF = FALSE, 
                            size.traj = 0.6, isSingle = TRUE,
                            y.position.labels = -9e-4, 
                            y.position.earth = -9e-4)


# Save in pdf, with annotations
ggsave(ppSEs, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))



#===============================================================================
# Single in EM coordinates
#===============================================================================
filename = paste0(filecont, "_x0_y0_EM_single", FAM)

ppEMs = fpp_path_traj_phd(traj_cont_single, filename, "EM", 
                          limits.x = limits.x.EM, 
                          limits.y = limits.y.EM, 
                          xlab = X_S, ylab = Y_S,
                          xlab.tex = xse, ylab.tex = yse,
                          colour.traj = values[FAMIND], 
                          lib.point.em = LIB_POINT_EM, 
                          lib.point.sem = LIB_POINT_SEM,
                          isLaTeX = TRUE, isPNG = FALSE, isPDF = FALSE, 
                          size.traj = 0.6, isSingle = TRUE)


# Save in pdf, with annotations
ggsave(ppEMs, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))
