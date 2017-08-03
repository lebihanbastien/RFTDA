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
limits.y.NCSE = c(-1.3, 1.3)

limits.x.NCEM = c(-0.5, 0.8)
limits.y.NCEM = c(-0.8, +0.8)

limits.x.SE = c(-1.015, -0.995) 
limits.y.SE = c(-0.013, 0.013)

limits.x.EM = c(-1.25, -1.1)
limits.y.EM = c(-0.15, +0.15)

#-------------------------------------------------------------------------------
# Labels (additionnal)
#-------------------------------------------------------------------------------
xncse  = "$x^\\mathsctiny{s}$"
yncse  = "$y^\\mathsctiny{s}$"


#-------------------------------------------------------------------------------
# Plotting
#-------------------------------------------------------------------------------
#fpp_path_some(traj_cont_some, filename, values[FAMIND]) (old version)
fpp_path_traj(traj_cont_some, filename, "SEM", 
              limits.x = c(-1.5, 0.3), 
              limits.y = c(-1.3, +1.3), 
              xlab = x_sem, ylab = y_sem,
              xlab.tex = "$x^{sem}$", ylab.tex = "$y^{sem}$",
              colour.traj = "black", 
              lib.point.em = LIB_POINT_EM, 
              lib.point.sem = LIB_POINT_SEM,
              isLaTeX = FALSE, isPNG = FALSE)

#===============================================================================
# Single in NCSE coordinates
#===============================================================================
if(!empty(traj_cont_single))
{
  
  #-----------------------------------------------------------------------------
  # NCSE coordinates
  #-----------------------------------------------------------------------------
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
  filename = paste0(filecont, "_x0_y0_NCSE_cont_single", FAM)
  ggsave(ppNCSEs, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))
  
}