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
# Plotting stars
#===============================================================================
is.stars.on = T

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
          radius.ratio = 2, 
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
# Stars background
#-------------------------------------------------------------------------------
r = 0.35
lat.mid  =  0.1
lat.amp  =  0.3
long.amp =  0.2
long.mid = -0.2
rad.amp  =  2e-4

if(is.stars.on)
{
  for(IND in seq(1, 350))
  {
    
    
    lat  = runif(1, lat.mid, lat.amp + lat.mid)
    long = runif(1, -long.amp + long.mid, long.amp + long.mid)
    
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
# Aspect & View
#-------------------------------------------------------------------------------
# Aspect
#aspect3d(1,0.5,1)

# Viewpoint
if(is.stars.on)
{
  rgl.viewpoint(theta = -113.5, phi = 20, fov = 40, zoom = 0.04)
}


#-------------------------------------------------------------------------------
# Saving
#-------------------------------------------------------------------------------
# Bring window to top
rgl.bringtotop()

# Snapshot
filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_x0_y0_NCSEM_rgl_dark")
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


