################################################################################
#
# Visualization of some trajectories in 3D
#
# Configuration file, sourced in ORBITS_PKS_PLOT_3D_*.R
#
# BLB 2017
#
################################################################################

#===== Color and aesthetics  ===================================================

# To match re.string
scolor = sprintf("conditional_cut(%s, %s, is.cut)", re.string, "re.cutv")
sgroup = "label.conn"#cs_interact("label", re.string)

# Dark colors
color.pal = rev(brewer.pal(3,"Dark2"))
color.pal = color.pal[c(1,2)]

# For annotations
y_annotate = -0.08

# Choice of colors for the 3D plot
if(color.3d == "dark")
{
  color.bg  = "#202020" #"#23373b"
  color.ax1 = "white"
  color.ax2 = "white"
}else{
  color.bg  = "white"
  color.ax1 = "black"
  color.ax2 = "black"
}

#===== Select ONE label ========================================================
#
# For Quasi-Halo small : 
#  - selab = 1068/82192 for good results
#  - selab = 79624 for bad result
#
# For Lissajous s1 = 10, s2 = 5
#  - selab = 25015 for good results with TSEM = 20
#  - selab = 24548 for good results with TSEM = 40
# ==============================================================================
label.conn.unique = unique(traj_from_jpl$label.conn)
selab = label.conn.unique[1]
traj_cont_0 = traj_from_jpl[which(traj_from_jpl$label.conn == selab),]

# Model equal to 1 or 2 
traj_cont_0$isJPL = (traj_cont_0$coord == 13)
traj_cont_0$isJPL = as.integer(traj_cont_0$isJPL) + 1

#QBCP
condition = traj_cont_0$coord == 0
traj_cont_0_QBCP = traj_cont_0[which(condition),] 

#JPL
condition = traj_cont_0$coord == 13
traj_cont_0_JPL = traj_cont_0[which(condition),] 


#===== Select SOME labels ======================================================
traj_from_c_select = traj_from_c[which(traj_from_c$label %% 5 == 0),]


#===== Legend ==================================================================
legend = list(at = c(1.25, 1.75), side = 1, addlines = TRUE, 
              length = 0.5, width = 0.5, font = 2, labels = c("QBCP", "JPL"))
