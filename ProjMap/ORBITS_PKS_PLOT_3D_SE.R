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

color.3d = "white"
source("ProjMap/ORBITS_PKS_PLOT_3D_CONFIG.R")


#===============================================================================
# RGL plot
#===============================================================================
ratio = 1

#-------------------------------------------------------------------------------
# Initialization
#-------------------------------------------------------------------------------
rgl_init(bg = color.bg)
par3d(cex=1.4)

#-------------------------------------------------------------------------------
# Plotting
#-------------------------------------------------------------------------------
# QBCP
lines3d(x =  traj_cont_0_QBCP$y_SEM*ratio, 
        y =  traj_cont_0_QBCP$z_SEM*ratio, 
        z =  traj_cont_0_QBCP$x_SEM*ratio,
        lwd = 2,
        fog = TRUE,
        color = color.pal[1])
# JPL
lines3d(x =  traj_cont_0_JPL$y_SEM*ratio, 
        y =  traj_cont_0_JPL$z_SEM*ratio,
        z =  traj_cont_0_JPL$x_SEM*ratio,
        lwd = 2,
        smooth = TRUE,
        color = color.pal[2])



#Earth
rgl_earth(dfearth_seml, 
          radius.ratio = 3, 
          ratio = ratio, 
          add = TRUE,
          re = dfearth_seml$r_SYS,
          xe = dfearth_seml$x_SYS,
          ye = dfearth_seml$y_SYS,
          ze = dfearth_seml$z_SYS) 

# Lights
clear3d(type = "lights")
light3d(theta = -90, phi = 0, viewpoint.rel = FALSE)


# SEML2
rgl_emli(dfsemli, 
         radius = 1e-4, 
         ratio = ratio, 
         add = TRUE, 
         color = color.ax2,
         x = dfsemli$y_SYS, 
         y = dfsemli$z_SYS, 
         z = dfsemli$x_SYS) 


#-------------------------------------------------------------------------------
# Axes & Grid
#-------------------------------------------------------------------------------
# Axes and Box
axes3d(color = c(color.ax2,color.ax2,color.ax2), marklen=25, marklen.rel = T)
#bbox3d(color = c(color.ax2,color.ax2,color.ax2), yat = c(-4e-4, 0, 4e-4), marklen=25, marklen.rel = T)
#box3d(color = c(color.ax2))

# Grid
grid3d(c("x-", "y-", "z+"))

#-------------------------------------------------------------------------------
# Labels
#-------------------------------------------------------------------------------
# 1. Easy way
#title3d(xlab = 'x (km)', ylab = 'z (km)', zlab = 'y (km)', color = color.ax2)

# 2. Or hard way
lim <- function(x){c(min(x), max(x)) * 1.3}

xlim.NC =  lim(traj_zoom_JPL$y_NCEM); 
ylim.NC =  lim(traj_zoom_JPL$x_NCEM);
zlim.NC =  lim(traj_zoom_JPL$z_NCEM);

xlim = rev(-CST_GAMMA_LIB_SEM*(xlim.NC - 0))
ylim = rev(-CST_GAMMA_LIB_SEM*(ylim.NC - CST_C1_LIB_SEM))
zlim = +CST_GAMMA_LIB_SEM*(zlim.NC - 0)

# mtext3d('X', 'z', line = 0, at = NULL, pos = c(-xlim[1]*1.5, zlim[1], 0),       color = color.ax2, cex = 2)
# mtext3d('Y', 'x', line = 0, at = NULL, pos = c(0, zlim[1], ylim[1]*1.),      color = color.ax2, cex = 2)
# mtext3d('Z', 'y', line = 0, at = NULL, pos = c(xlim[2]*0.70, 0, ylim[2]*0.988),  color = color.ax2, cex = 2)

 # 3. Or Manual
mtext3d('X', 'z', line = 0, at = NULL, pos = c(0.011, -0.0034, 0),       color = color.ax2, cex = 2)
mtext3d('Y', 'x', line = 0, at = NULL, pos = c(0, -0.0034, -1.014),      color = color.ax2, cex = 2)
mtext3d('Z', 'y', line = 0, at = NULL, pos = c(0.01, 0, -0.995),  color = color.ax2, cex = 2)


# 
# mtext3d('x', 'z', line = 0, at = NULL, pos = c(xlim[1]*1.2, zlim[1], 0),  color = color.ax2, cex = 2)
# mtext3d('y', 'x', line = 0, at = NULL, pos = c(0, zlim[1], ylim[2]*2.1),  color = color.ax2, cex = 2)
# mtext3d('z', 'y', line = 0, at = NULL, pos = c(xlim[1], 0, ylim[1]*0.95), color = color.ax2, cex = 2)

#-------------------------------------------------------------------------------
# Aspect & View
#-------------------------------------------------------------------------------
# Aspect
#aspect3d(1,0.5,1)

# Viewpoint
rgl.viewpoint(theta = 130, phi = 18, fov = 10, zoom = 0.6)

#-------------------------------------------------------------------------------
# Saving
#-------------------------------------------------------------------------------
# Bring window to top
rgl.bringtotop()

# Snapshot
filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_y0_NCSEM_rgl")
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

lines3D (x = traj_zoom_JPL$x_PHEM*ratio, y = traj_zoom_JPL$y_PHEM*ratio,
         z = traj_zoom_JPL$z_PHEM*ratio, colvar = traj_zoom_JPL$isJPL,
         bty = "g", xlab = xlab,ylab = ylab, zlab = zlab,
         col = color.pal, clab = c("Model"), ticktype = "detailed", colkey = legend,
         clim = c(1, 2),
         xlim = xlim*ratio,
         ylim = ylim*ratio,
         zlim = zlim*ratio,
         phi = 30, theta = 25, d = 3, font = 5)



lines3D (x = traj_zoom_QBCP$x_PHEM*ratio, y = traj_zoom_QBCP$y_PHEM*ratio,
         z = traj_zoom_QBCP$z_PHEM*ratio, colvar = traj_zoom_QBCP$isJPL,
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
lines3D (traj_cont_0$x_NCSEM*ratio, 
         traj_cont_0$y_NCSEM*ratio,
         traj_cont_0$z_NCSEM*ratio,
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


