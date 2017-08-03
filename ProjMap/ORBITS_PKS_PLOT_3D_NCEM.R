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

color.3d = "white"#"dark"
source("ProjMap/ORBITS_PKS_PLOT_3D_CONFIG.R")

is.moon.on = F

#===============================================================================
# Selection @ EML2
#===============================================================================
# Zoom
xlim = c(-0.4, 0.4)#*CST_GAMMA_LIB_EM
ylim = c(-0.9, 0.9)#*CST_GAMMA_LIB_EM
zlim = c(-0.25, 0.25)#*CST_GAMMA_LIB_EM

condition = traj_cont_0$x_NCEM > xlim[1] & traj_cont_0$x_NCEM < xlim[2] &
  traj_cont_0$y_NCEM > ylim[1] & traj_cont_0$y_NCEM < ylim[2] &
  traj_cont_0$z_NCEM > zlim[1] & traj_cont_0$z_NCEM < zlim[2]

traj_zoom_0 = traj_cont_0[which(condition),] 

# Only QBCP results
condition = traj_zoom_0$isJPL == 1
traj_zoom_QBCP = traj_zoom_0[which(condition),] 

# Only JPL results
condition = traj_zoom_0$isJPL == 2 
traj_zoom_JPL = traj_zoom_0[which(condition),] 


#===============================================================================
# RGL plot
#===============================================================================
ratio = 1

#-------------------------------------------------------------------------------
# Initialization
#-------------------------------------------------------------------------------
rgl_init(bg = color.bg)
par3d(cex=1.5)

#-------------------------------------------------------------------------------
# Plotting
#-------------------------------------------------------------------------------
# QBCP
lines3d(x = traj_zoom_QBCP$y_NCEM*ratio,
        y = traj_zoom_QBCP$z_NCEM*ratio,
        z = traj_zoom_QBCP$x_NCEM*ratio,
        lwd = 2,
        fog = TRUE,
        color = color.pal[1])
# JPL
lines3d(x = traj_zoom_JPL$y_NCEM*ratio,
        y = traj_zoom_JPL$z_NCEM*ratio, 
        z = traj_zoom_JPL$x_NCEM*ratio,
        lwd = 2,
        smooth = TRUE,
        color = color.pal[2])


# EML2
rgl_emli(dfsemli, ratio = ratio, add = TRUE, color = color.ax2) 
#text3d(x = dfsemli$x_NC, y = dfsemli$y_NC + 1e3, z = dfsemli$z_NC, "EML2", color = color.ax2)

# The moon
if(is.moon.on)
{
  rgl_moon(dfmoon_eml, ratio, add = TRUE) 
  
  # Lights (if Moon is on)
  # clear3d(type = "lights")
  light3d(theta = 20, phi = 0, viewpoint.rel = FALSE)
}

#-------------------------------------------------------------------------------
# Axes & Grid
#-------------------------------------------------------------------------------
# Axes and Box
axes3d(color = c(color.ax2,color.ax2,color.ax2), marklen=25, marklen.rel = T)
#box3d(color = c(color.ax2))

# Grid
grid3d(c("x+", "y-", "z"))

#-------------------------------------------------------------------------------
# Labels
#-------------------------------------------------------------------------------
# Easy way
#title3d(xlab = 'y (km)', ylab = 'z (km)', zlab = 'x (km)', color = color.ax2)

# Or hard way
lim <- function(x){c(min(x), max(x)) * 1.3}

if(is.moon.on)
{
  xlim = lim(traj_zoom_JPL$y_NCEM*ratio); 
  ylim = lim(traj_zoom_JPL$x_NCEM*ratio);
  zlim = lim(traj_zoom_JPL$z_NCEM*ratio);
  
  ylim = c(-1.1, ylim[2])
}else{
  xlim = lim(traj_zoom_JPL$y_NCEM*ratio); 
  ylim = lim(traj_zoom_JPL$x_NCEM*ratio);
  zlim = lim(traj_zoom_JPL$z_NCEM*ratio); 
}

mtext3d('x', 'z', line = 0, at = NULL, pos = c(xlim[1]*1.1, zlim[1], 0),       color = color.ax2, cex = 2)
mtext3d('y', 'x', line = 0, at = NULL, pos = c(0, zlim[1], ylim[2]*1.2),       color = color.ax2, cex = 2)
mtext3d('z', 'y', line = 0, at = NULL, pos = c(xlim[1]*1.05, 0, ylim[1]*1.05), color = color.ax2, cex = 2)


#-------------------------------------------------------------------------------
# Aspect & View
#-------------------------------------------------------------------------------
# Aspect
#aspect3d(1,0.5,1)

# Viewpoint
rgl.viewpoint(theta = -55, phi = 20, fov = 10)

#-------------------------------------------------------------------------------
# Saving
#-------------------------------------------------------------------------------
# Bring window to top
rgl.bringtotop()

# Snapshot
filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_y0_NCEM_rgl")
rgl.snapshot( paste0(filename, '.png'), fmt = "png", top = TRUE )
#rgl.postscript(paste0(filename, '.svg'),fmt='svg')
#rgl.postscript(paste0(filename, '.tex'),fmt='tex', drawText= FALSE)

stop()

#-------------------------------------------------------------------------------
# Movies and shit
#-------------------------------------------------------------------------------
# Just to play
play3d(spin3d(axis = c(0, 1, 0)), duration = 10)

## Movies
# filemovie  = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_y0_NCEM_movie")
# movie3d(spin3d(axis = c(0, 1, 0)),
#         duration = 10,
#         movie = filemovie,
#         dir = getwd())

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

lines3D (x = traj_zoom_JPL$x_NCEM*ratio, y = traj_zoom_JPL$y_NCEM*ratio,
         z = traj_zoom_JPL$z_NCEM*ratio, colvar = traj_zoom_JPL$isJPL,
         bty = "g", xlab = xlab,ylab = ylab, zlab = zlab,
         col = color.pal, clab = c("Model"), ticktype = "detailed", colkey = legend,
         clim = c(1, 2),
         xlim = xlim*ratio,
         ylim = ylim*ratio,
         zlim = zlim*ratio,
         phi = 30, theta = 25, d = 3, font = 5)



lines3D (x = traj_zoom_QBCP$x_NCEM*ratio, y = traj_zoom_QBCP$y_NCEM*ratio,
         z = traj_zoom_QBCP$z_NCEM*ratio, colvar = traj_zoom_QBCP$isJPL,
         bty = "g", xlab = xlab,ylab = ylab, zlab = zlab,
         col = color.pal, clab = c("Model"), ticktype = "detailed", colkey = legend,
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
         traj_cont_0$z_PHSEM*ratio,
         traj_cont_0$y_PHSEM*ratio,
         colvar = traj_cont_0$isJPL, 
         bty = "g", pch = 16,
         xlab = xlab, ylab = ylab, zlab = zlab,  
         col = color.pal,
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


