#' @param new.device a logical value. If TRUE, creates a new device
#' @param bg the background color of the device
#' @param width the width of the device
rgl_init <- function(new.device = FALSE, bg = "black", width = 640, sphere = F) { 
  if( new.device | rgl.cur() == 0 ) {
    rgl.open(rgl.antialias = T)
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg, sphere = sphere)#, texture=system.file("textures/perseus2.png", package="rgl"), back = "filled")
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}


#-------------------------------------------------------------------------------
# Plot the Moon on a RGL plot, in NC/SYS coordinates
#-------------------------------------------------------------------------------
rgl_moon <- function(dfmoon, ratio = 1, add = TRUE, 
                     rm = dfmoon$r_NC, 
                     xm = dfmoon$x_NC, 
                     ym = dfmoon$y_NC,
                     zm = dfmoon$z_NC) 
{
  
  # Latitude and longitude
  lat  <- matrix(seq(90,-90, len=50)*pi/180, 50, 50, byrow=TRUE)
  long <- matrix(seq(-180, 180, len=50)*pi/180, 50, 50)
  
  # Building the cartesian coordinates
  r <- rm # 6378.1 radius of Earth in km
  x <- r*cos(lat)*cos(long) + xm
  y <- r*cos(lat)*sin(long) + ym
  z <- r*sin(lat) + zm
  
  
  # Plot using persp3d
  persp3d(x = y*ratio, 
          y = z*ratio,
          z = x*ratio, 
          col="grey",
          specular="black",
          ambient = "grey25",
          point_antialias = TRUE,
          shininess = 30,
          texture=system.file("textures/moon.png",package="rgl"), 
          axes=FALSE, box=FALSE, xlab="", ylab="", zlab="", add = add)
}

#-------------------------------------------------------------------------------
# Plot the Eartth on a RGL plot, in NC/SYS coordinates
#-------------------------------------------------------------------------------
rgl_earth <- function(dfearth, 
                      radius.ratio = 1, 
                      ratio = 1, 
                      add = TRUE,
                      re = dfearth$r_NC,
                      xe = dfearth$x_NC,
                      ye = dfearth$y_NC,
                      ze = dfearth$z_NC) 
{
  
  # Latitude and longitude
  lat  <- matrix(seq(90,-90, len=50)*pi/180, 50, 50, byrow=TRUE)
  long <- matrix(seq(-180, 180, len=50)*pi/180, 50, 50)
  
  # Building the cartesian coordinates
  r <- re*radius.ratio # 6378.1 radius of Earth in km
  x <- r*cos(lat)*cos(long) + xe
  y <- r*cos(lat)*sin(long) + ye
  z <- r*sin(lat) + ze
  
  
  # Plot using persp3d
  persp3d(x = y*ratio, 
          y = z*ratio, 
          z = x*ratio, 
          col="grey",
          specular="grey20",
          ambient = "white",
          point_antialias = TRUE,
          shininess = 10,
          texture=system.file("textures/worldmapnasa.png",package="rgl"), 
          axes=FALSE, box=FALSE, xlab="", ylab="", zlab="", add = add)
}

#-------------------------------------------------------------------------------
# Plot the EMLi on a RGL plot, in NC/SYS coordinates
#-------------------------------------------------------------------------------
rgl_emli <- function(dfeml, 
                     radius = 1e-2, 
                     ratio = 1, 
                     add = TRUE, 
                     color = "white", 
                     x = dfeml$y_NC, 
                     y = dfeml$z_NC, 
                     z = dfeml$x_NC) 
{
  sprites3d(x = x*ratio, 
            y = y*ratio, 
            z = z*ratio, 
            r = radius*ratio, alpha= 1,
            shape = shade3d(dodecahedron3d(), col = color), 
            lit = FALSE)
}



rgl_star <- function(dfeml, 
                     radius = 1e-2, 
                     ratio = 1, 
                     add = TRUE, 
                     color = "white", 
                     x = dfeml$y_NC, 
                     y = dfeml$z_NC, 
                     z = dfeml$x_NC) 
{
  sprites3d(x = x*ratio, 
            y = y*ratio, 
            z = z*ratio, 
            r = radius*ratio, 
            color = color, 
            ambient = color, emission = color, 
            shininess   = 1000,
            lit = FALSE, alpha = 1,
            textype = "alpha", 
            texture = system.file("textures/sparkle.png", package = "rgl"))
            #texture = system.file("textures/particle.png", package = "rgl"))
}