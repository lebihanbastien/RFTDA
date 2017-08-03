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

is.moon.on = F

#===============================================================================
# Selection @ EML2
#===============================================================================
# Zoom
xlim = c(-0.5, 0.4)   #*CST_GAMMA_LIB_EM
ylim = c(-2,2)        #*CST_GAMMA_LIB_EM
zlim = c(-0.5, 0.5)   #*CST_GAMMA_LIB_EM

condition = traj_cont_0$x_NCEM > xlim[1] & traj_cont_0$x_NCEM < xlim[2] &
  traj_cont_0$y_NCEM > ylim[1] & traj_cont_0$y_NCEM < ylim[2] &
  traj_cont_0$z_NCEM > zlim[1] & traj_cont_0$z_NCEM < zlim[2]

traj_zoom_0 = traj_cont_0[which(condition),] 

#===============================================================================
# Subselection
#===============================================================================

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
lines3d(x = traj_zoom_QBCP$y_EM,
        y = traj_zoom_QBCP$z_EM,
        z = traj_zoom_QBCP$x_EM,
        lwd = 2,
        fog = TRUE,
        color = color.pal[1])
# JPL
lines3d(x = traj_zoom_JPL$y_EM,
        y = traj_zoom_JPL$z_EM, 
        z = traj_zoom_JPL$x_EM,
        lwd = 2,
        smooth = TRUE,
        color = color.pal[2])


# EML2
rgl_emli(dfeml = dfemli, 
         ratio = ratio, 
         add = TRUE, 
         radius = 2e-3,
         color = color.ax2,
         x = dfemli$y_SYS, 
         y = dfemli$z_SYS, 
         z = dfemli$x_SYS)

#text3d(x = dfsemli$x_NC, y = dfsemli$y_NC + 1e3, z = dfsemli$z_NC, "EML2", color = color.ax2)

# The moon
if(is.moon.on)
{
  rgl_moon(dfmoon_eml, ratio, add = TRUE,
           rm = dfmoon_eml$r_SYS, 
           xm = dfmoon_eml$x_SYS, 
           ym = dfmoon_eml$y_SYS,
           zm = dfmoon_eml$z_SYS) 
  
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
grid3d(c("x-", "y-", "z+"))

#-------------------------------------------------------------------------------
# Labels
#-------------------------------------------------------------------------------
# 1. Easy way
#title3d(xlab = 'y (km)', ylab = 'z (km)', zlab = 'x (km)', color = color.ax2)

# 2. Or hard way
lim <- function(x){c(min(x), max(x)) * 1.3}

xlim.NC =  lim(traj_zoom_JPL$y_NCEM); 
ylim.NC =  lim(traj_zoom_JPL$x_NCEM);
zlim.NC =  lim(traj_zoom_JPL$z_NCEM);

xlim = rev(-CST_GAMMA_LIB_EM*(xlim.NC - 0))
ylim = rev(-CST_GAMMA_LIB_EM*(ylim.NC - CST_C1_LIB_EM))
zlim = +CST_GAMMA_LIB_EM*(zlim.NC - 0)

if(is.moon.on)
{
  ylim = c(-0.98, ylim[2])
}else{
}

# mtext3d('X', 'z', line = 0, at = NULL, pos = c(-xlim[1]*1.3, zlim[1], 0),       color = color.ax2, cex = 2)
# mtext3d('Y', 'x', line = 0, at = NULL, pos = c(0, zlim[1], ylim[1]*1.01),       color = color.ax2, cex = 2)
# mtext3d('Z', 'y', line = 0, at = NULL, pos = c(xlim[2]*1.05, 0, ylim[2]*0.998),  color = color.ax2, cex = 2)

#3. or manual
mtext3d('X', 'z', line = 0, at = NULL, pos = c(0.13, -0.06, 0),       color = color.ax2, cex = 2)
mtext3d('Y', 'x', line = 0, at = NULL, pos = c(0, -0.06, -1.25),       color = color.ax2, cex = 2)
mtext3d('Z', 'y', line = 0, at = NULL, pos = c(0.12, 0, -1.13), color = color.ax2, cex = 2)

#-------------------------------------------------------------------------------
# Aspect & View
#-------------------------------------------------------------------------------
# Aspect
#aspect3d(1,0.5,1)

# Viewpoint
rgl.viewpoint(theta = 130, phi = 20, fov = 10, zoom = 0.85)

#-------------------------------------------------------------------------------
# Saving
#-------------------------------------------------------------------------------
# Bring window to top
rgl.bringtotop()

# Snapshot
filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_y0_EM_rgl")
rgl.snapshot( paste0(filename, '.png'), fmt = "png", top = TRUE )
rgl.postscript(paste0(filename, '.pdf'),fmt='pdf')
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
