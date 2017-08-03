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

color.3d = "dark"
source("ProjMap/ORBITS_PKS_PLOT_3D_CONFIG.R")

#===============================================================================
# Plotting the Earth and the Moon + stars
#===============================================================================
is.moon.on  = T
is.earth.on = T
is.stars.on = T


#===============================================================================
# Selection @ EML2
#===============================================================================
# Zoom
xlim = c(-5, 8)   #*CST_GAMMA_LIB_EM
ylim = c(-5, 5)   #*CST_GAMMA_LIB_EM
zlim = c(-0.25, 0.25) #*CST_GAMMA_LIB_EM

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
lines3d(x = traj_zoom_QBCP$y_EM*ratio,
        y = traj_zoom_QBCP$z_EM*ratio,
        z = traj_zoom_QBCP$x_EM*ratio,
        lwd = 2,
        fog = TRUE,
        color = color.pal[1], ambient = "grey25")
# JPL
lines3d(x = traj_zoom_JPL$y_EM*ratio,
        y = traj_zoom_JPL$z_EM*ratio, 
        z = traj_zoom_JPL$x_EM*ratio,
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

#-------------------------------------------------------------------------------
# Stars background
#-------------------------------------------------------------------------------
r = 3

if(is.earth.on)
{
  lat.mid  =  0.1
  lat.amp  =  0.5
  long.amp =  0.2
  rad.amp  = 5e-3
}else
{
  lat.mid  =  0.4
  lat.amp  =  0.3 
  long.amp =  0.1
  rad.amp  = 2e-3
}

if(is.stars.on)
{
  for(IND in seq(1, 220))
  {
    
    
    lat  = runif(1, lat.mid, lat.amp + lat.mid)
    long = runif(1, -long.amp, long.amp)
    
    xr <- r*cos(lat)*cos(long) + dfmoon_eml$y_SYS
    yr <- r*cos(lat)*sin(long) + dfmoon_eml$z_SYS
    zr <- r*sin(lat)           + dfmoon_eml$x_SYS
    
    rgl_star(dfeml = dfemli, 
           ratio = ratio, 
           add = TRUE, 
           radius = rad.amp*runif(1, 0, 1),
           color = color.ax2,
           x = xr , 
           y = yr, 
           z = zr)
  }
}

#-------------------------------------------------------------------------------
# The moon
#-------------------------------------------------------------------------------
if(is.moon.on)
{
  rgl_moon(dfmoon_eml, ratio, add = TRUE,
           rm = dfmoon_eml$r_SYS, 
           xm = dfmoon_eml$x_SYS, 
           ym = dfmoon_eml$y_SYS,
           zm = dfmoon_eml$z_SYS)
}

#-------------------------------------------------------------------------------
# The Earth
#-------------------------------------------------------------------------------
if(is.earth.on)
{
  rgl_earth(dfearth_eml, ratio, add = TRUE, radius.ratio = 1,
            re = dfearth_eml$r_SYS, 
            xe = dfearth_eml$x_SYS, 
            ye = dfearth_eml$y_SYS,
            ze = dfearth_eml$z_SYS) 
}

#-------------------------------------------------------------------------------
# Axes & Grid
#-------------------------------------------------------------------------------
# Axes and Box
#axes3d(color = c(color.ax2,color.ax2,color.ax2), marklen=25, marklen.rel = T)
#box3d(color = c(color.ax2))

# Grid
#grid3d(c("x-", "y-", "z+"))

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

#mtext3d('x', 'z', line = 0, at = NULL, pos = c(xlim[1]*1.1, zlim[1], 0),       color = color.ax2, cex = 2)
#mtext3d('y', 'x', line = 0, at = NULL, pos = c(0, zlim[1], ylim[2]*1.2),       color = color.ax2, cex = 2)
#mtext3d('z', 'y', line = 0, at = NULL, pos = c(xlim[1]*1.05, 0, ylim[1]*1.05), color = color.ax2, cex = 2)


#-------------------------------------------------------------------------------
# Aspect & View
#-------------------------------------------------------------------------------
# Aspect
#aspect3d(1,0.5,1)

# Viewpoint
if(is.earth.on)
{
  rgl.viewpoint(theta = -102, phi = 0, fov = 10, zoom = 0.22)
}else
{
  rgl.viewpoint(theta = -125, phi = 0, fov = 10, zoom = 0.08)
}



# Lights (if Moon is on)
clear3d(type = "lights")
light3d(theta = -60, phi = 0, viewpoint.rel = FALSE)

#-------------------------------------------------------------------------------
# Saving
#-------------------------------------------------------------------------------
# Bring window to top
rgl.bringtotop()

# Snapshot
filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_y0_EM_rgl_dark")
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
