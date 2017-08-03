################################################################################
#
# Projection from single orbits. Requires MAIN.R, ORBITS_PROJ_POSTPROCESS.R, 
# and ORBITS_PROJ_TRAJ_LOAD.R (to get refined trajectories)
#
# Difference with ORBITS_PROJ_PLOT.R : the phase is taken on a given pk section 
# rather than at the departure point. Note the ORBITS_PKS_PLOT_1.R must be 
# loaded once before
#
# BLB 2017
#
################################################################################

#===== Color and aesthetics  ===================================================

# To match re.string
scolor = sprintf("conditional_cut(%s, %s, is.cut)", re.string, "re.cutv")
sgroup = "label.conn"#cs_interact("label", re.string)

# Dark colors
dark2 = rev(brewer.pal(3,"Dark2"))
dark2 = dark2[c(1,2)]

# For annotations
y_annotate = -0.08

# Choice of colors for the 3D plot
color.3d = ""#"dark"

if(color.3d == "dark")
{
  color.bg  = "#23373b"
  color.ax1 = "black"
  color.ax2 = "white"
}else{
  color.bg  = "white"
  color.ax1 = "black"
  color.ax2 = "black"
}

#===== Select ONE label ========================================================
selab = 82192#max(traj_from_jpl$label.conn)
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


#===== Selection @ EML2 ========================================================

# With zoom
xlim = c(-0.4, 0.4)*CST_DIST_PRIM_EM*CST_GAMMA_LIB_EM
ylim = c(-0.9, 0.9)*CST_DIST_PRIM_EM*CST_GAMMA_LIB_EM
zlim = c(-0.2, 0.2)*CST_DIST_PRIM_EM*CST_GAMMA_LIB_EM

condition = traj_cont_0$x_PHEM > xlim[1] & traj_cont_0$x_PHEM < xlim[2] &
  traj_cont_0$y_PHEM > ylim[1] & traj_cont_0$y_PHEM < ylim[2] &
  traj_cont_0$z_PHEM > zlim[1] & traj_cont_0$z_PHEM < zlim[2]

traj_zoom_0 = traj_cont_0[which(condition),] 

condition = traj_zoom_0$isJPL == 1
traj_zoom_QBCP = traj_zoom_0[which(condition),] 

condition = traj_zoom_0$isJPL == 2 
traj_zoom_JPL = traj_zoom_0[which(condition),] 

#===== using RGL ===============================================================
ratio = 1
rgl_init(bg = color.bg)


lines3d(x = traj_zoom_QBCP$x_PHEM*ratio, 
        z = -traj_zoom_QBCP$y_PHEM*ratio,
        y = traj_zoom_QBCP$z_PHEM*ratio, 
        lwd = 2,
        fog = TRUE,
        color = dark2[1])

lines3d(x = traj_zoom_JPL$x_PHEM*ratio, 
        z = -traj_zoom_JPL$y_PHEM*ratio,
        y = traj_zoom_JPL$z_PHEM*ratio, 
        lwd = 2,
        smooth = TRUE,
        color = dark2[2])

# The moon
#rgl_moon(dfmoon_eml, ratio, add = TRUE) 

# Lights (if Moon is on)
clear3d(type = "lights")
light3d(theta = 20, phi = 0, viewpoint.rel = FALSE)


# EML2
rgl_emli(dfsemli, ratio = ratio, add = TRUE, color = color.ax2) 
#text3d(x = dfsemli$x_NCPH, y = dfsemli$y_NCPH + 1e3, z = dfsemli$z_NCPH, "EML2", color = color.ax2)


# Setup
lim <- function(x){c(min(x), max(x)) * 1.6}
xlim <- lim(traj_zoom_JPL$x_PHEM*ratio); 
zlim <- lim(traj_zoom_JPL$z_PHEM*ratio); 
ylim <- lim(traj_zoom_JPL$y_PHEM*ratio)


# Box
axes3d(color = c(color.ax1, color.ax2))

# Labels

# Easy way
#title3d(xlab = 'x (km)', ylab = 'z (km)', zlab = 'y (km)', color = color.ax2)

# Or hard way
mtext3d('x (km)', 'x', line = 0, at = NULL, pos = c(0, 0, ylim[2]), color = color.ax2) 
mtext3d('y (km)', 'z', line = 0, at = NULL, pos = c(xlim[2], 0, 0), color = color.ax2) 
mtext3d('z (km)', 'y', line = 0, at = NULL, pos = c(xlim[1], 0, ylim[2]), color = color.ax2) 
#axis3d('x--', labels = TRUE, xlab = 'XXX', pos = c(NA, 0, 0), color = color.ax2)

rgl.bringtotop()

#aspect3d(1,1,1)

# Viewpoint
rgl.viewpoint(theta = 30, phi = 30)

# Snapshot
filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_y0_EM_rgl")
rgl.snapshot( paste0(filename, '.png'), fmt = "png", top = TRUE )
#rgl.postscript(paste0(filename, '.pdf'),fmt='pdf')
stop()



#===== Plot:3D in SEM coordinates ==============================================
ratio = 1
rgl_init(bg = color.bg)


lines3d(x =  traj_cont_0_QBCP$x_PHSEM*ratio, 
        z = -traj_cont_0_QBCP$y_PHSEM*ratio,
        y =  traj_cont_0_QBCP$z_PHSEM*ratio, 
        lwd = 2,
        fog = TRUE,
        color = dark2[1])

lines3d(x =  traj_cont_0_JPL$x_PHSEM*ratio, 
        z = -traj_cont_0_JPL$y_PHSEM*ratio,
        y =  traj_cont_0_JPL$z_PHSEM*ratio, 
        lwd = 2,
        smooth = TRUE,
        color = dark2[2])


#Earth
rgl_earth(dfearth_seml, radius.ratio = 4, ratio = ratio, add = TRUE) 

# Lights
clear3d(type = "lights")
light3d(theta = -90, phi = 0, viewpoint.rel = FALSE)


# EML2
rgl_emli(dfemli, radius = 5e4, ratio = ratio, add = TRUE, color = color.ax2) 
text3d(x = dfsemli$x_NCPH, y = dfsemli$y_NCPH + 1e5, z = dfsemli$z_NCPH, "SEML2", color = color.ax2)


# Setup
axes3d(color = c(color.ax1, color.ax2), labels = T)

# Easy way
#title3d(xlab = 'x (km)', ylab = 'z (km)', zlab = 'y (km)', color = color.ax2)

#Or hard way
lim <- function(x){c(min(x), max(x)) * 1.3}
xlim <- lim(traj_cont_0_JPL$x_PHSEM*ratio); 
zlim <- lim(traj_cont_0_JPL$z_PHSEM*ratio); 
ylim <- lim(traj_cont_0_JPL$y_PHSEM*ratio)

mtext3d('x (km)', 'x', line = 0, at = NULL, pos = c(0, 0, ylim[2]*1.2)) 
mtext3d('y (km)', 'z', line = 0, at = NULL, pos = c(xlim[2]*2.5, 0, 0)) 
mtext3d('z (km)', 'y', line = 0, at = NULL, pos = c(xlim[1], 0, ylim[2])) 

#aspect3d(1,1,1)

# Viewpoint
rgl.viewpoint(theta = 30, phi = 30)

# Just to play
#play3d(spin3d(axis = c(0, 1, 0)), duration = 10)

## Movies
# filemovie  = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_y0_SEM_movie")
# movie3d(spin3d(axis = c(0, 1, 0)), 
#         duration = 10, 
#         movie = "filemovie",
#         dir = getwd())

# Snapshot
filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_y0_SEM_rgl")
rgl.snapshot( paste0(filename, '.png'), fmt = "png", top = TRUE )


stop()



#===== Old code using plot3D @ EML2 ============================================

# Ratio and labels
sratio = "1e3"
ratio = 1e-3

xlab = paste0("x (", sratio, " km)")
ylab = paste0("y (", sratio, " km)")
zlab = paste0("z (", sratio, " km)")

# lines3D for R plot (with file = file) linex3D for latex plot
file = paste0(FILE_PREFIX, "_3Dtraj_label_",  selab, ".tex")

lines3D (x = traj_zoom_JPL$x_PHEM*ratio, y = traj_zoom_JPL$y_PHEM*ratio,
         z = traj_zoom_JPL$z_PHEM*ratio, colvar = traj_zoom_JPL$isJPL,
         bty = "g", xlab = xlab,ylab = ylab, zlab = zlab,
         col = dark2, clab = c("Model"), ticktype = "detailed", colkey = legend,
         clim = c(1, 2),
         xlim = xlim*ratio,
         ylim = ylim*ratio,
         zlim = zlim*ratio,
         phi = 30, theta = 25, d = 3, font = 5)



lines3D (x = traj_zoom_QBCP$x_PHEM*ratio, y = traj_zoom_QBCP$y_PHEM*ratio,
         z = traj_zoom_QBCP$z_PHEM*ratio, colvar = traj_zoom_QBCP$isJPL,
         bty = "g", xlab = xlab,ylab = ylab, zlab = zlab,
         col = dark2, clab = c("Model"), ticktype = "detailed", colkey = legend,
         clim = c(1, 2),
         xlim = xlim*ratio,
         ylim = ylim*ratio,
         zlim = zlim*ratio,
         phi = 30, theta = 25, d = 3, font = 5, add = TRUE)


stop()


#===== Old code using plot3D @ SEML2 ===========================================

# Ratio and labels
sratio = "1e5"
ratio = 1e-5

xlab = paste0("x (", sratio, " km)")
ylab = paste0("y (", sratio, " km)")
zlab = paste0("z (", sratio, " km)")

# The Earth

# Create a sphere
M <- mesh(seq(0, 2*pi, length.out = 50), seq(0,   pi, length.out = 50))
u <- M$x ; v  <- M$y
x_s = 1 + cos(u)*sin(v)
y_s = sin(u)*sin(v)
z_s = cos(v)

# Trajectories
lines3D (traj_cont_0$x_PHSEM*ratio, 
         traj_cont_0$y_PHSEM*ratio,
         traj_cont_0$z_PHSEM*ratio,
         colvar = traj_cont_0$isJPL, 
         bty = "g", pch = 16,
         xlab = xlab, ylab = ylab, zlab = zlab,  
         col = dark2,
         clab = c("Model"),
         ticktype = "detailed", 
         clim = c(1, 2),
         colkey = legend,
         d = 3,
         theta = 40, phi = 20)
# Earth
scatter3D(x = dfearth_seml$x_NCPH*ratio, y = dfearth_seml$y_NCPH*ratio, z = dfearth_seml$z_NCPH*ratio, colvar = NULL, col = "#483D8B", pch = 19, cex = 1, add = TRUE)
#surf3D(x = x_s, y = y_s, z = z_s,  colvar = NULL, lighting = TRUE, col = "#7570B3", lwd = 5)

# SEML2
with(dfsemli, scatter3D(x = x_NCPH*ratio, y = y_NCPH*ratio, z = z_NCPH*ratio, colvar = NULL, col = "black", pch = 19, cex = 0.5, add = TRUE))


