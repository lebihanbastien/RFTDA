#' Print a scatter3D plot from plot3D library to in standalone tex format
#' 
#' @description The goal of this function is only to print a given scatter3D plot
#' to tex format. The input parameters are the same as scatter3D except for
#' file, width, and height that are inputs for the tex file. Namely, the name of 
#' the file, its width and height.
#' 
#' @author BLB
scattex3D <- function(file, width = 9.01, heigh = 7.66, x, y, z, 
                      colvar = z, phi = 40, theta = 40,
                      col = NULL, NAcol = "white", breaks = NULL,
                      colkey = NULL, panel.first = NULL, 
                      clim = NULL, clab = NULL, 
                      bty = "g", CI = NULL, surf = NULL, 
                      add = FALSE, plot = TRUE, pch = 16,
                      xlab = "$x$", ylab ="$y$", zlab = "$z$",
                      ticktype = "detailed")
{
  #-----------------------------------------------------------------------------
  # Init the tikz device
  #-----------------------------------------------------------------------------
  tikz(file, 
       width = xSize, 
       height = ySize, 
       standAlone=TRUE, 
       documentDeclaration="\\documentclass{standalone}\n",
       packages = c("\\usepackage[utf8]{inputenc}",
                    "\\usepackage[T1]{fontenc}",
                    "\\usepackage{tikz}", 
                    "\\usepackage{pgf}", 
                    "\\usetikzlibrary{calc}", 
                    "\\usepackage{amssymb}", 
                    "\\usepackage{amsfonts}\n",
                    "\\tikzset{font = {\\fontsize{18pt}{12}\\selectfont}}\n"))
  
  #-----------------------------------------------------------------------------
  #Print the plot using original scatter3D to feed dev
  #-----------------------------------------------------------------------------
  scatter3D (x = x, y = y, z = z, colvar = colvar, phi = phi, theta = theta, 
             col = col, NAcol = NAcol, breaks = breaks, colkey = colkey, 
             panel.first = panel.first, clim = clim, clab = clab, bty = bty, 
             CI = CI, surf = surf, add = add, plot = plot, 
             pch = pch, xlab = xlab, ylab = ylab, zlab = zlab,
             ticktype = ticktype)
  
  #-----------------------------------------------------------------------------
  # Necessary to close or the tikxDevice .tex file will not be written
  #-----------------------------------------------------------------------------
  dev.off()
}


#' Print a lines3D plot from plot3D library to in standalone tex format
#' 
#' @description The goal of this function is only to print a given lines3D plot
#' to tex format. The input parameters are the same as lines3D except for
#' file, width, and height that are inputs for the tex file. Namely, the name of 
#' the file, its width and height.
#' 
#' @author BLB
linex3D <- function(file, width = 9.01, heigh = 7.66, x, y, z, 
                      colvar = z, phi = 40, theta = 40,
                      col = NULL, NAcol = "white", breaks = NULL,
                      colkey = NULL, panel.first = NULL, 
                      clim = NULL, clab = NULL, 
                      bty = "g", CI = NULL, surf = NULL, 
                      add = FALSE, plot = TRUE, pch = 16,
                      xlab = "$x$", ylab ="$y$", zlab = "$z$",
                      xlim = NULL,
                      ylim = NULL,
                      zlim = NULL,
                      d = 3,
                      scale = NULL,
                      ticktype = "detailed")
{
  #-----------------------------------------------------------------------------
  # Init the tikz device
  #-----------------------------------------------------------------------------
  tikz(file, 
       width = xSize, 
       height = ySize, 
       standAlone=TRUE, 
       documentDeclaration="\\documentclass{standalone}\n",
       packages = c("\\usepackage[utf8]{inputenc}",
                    "\\usepackage[T1]{fontenc}",
                    "\\usepackage{tikz}", 
                    "\\usepackage{pgf}", 
                    "\\usetikzlibrary{calc}", 
                    "\\usepackage{amssymb}", 
                    "\\usepackage{amsfonts}\n",
                    "\\tikzset{font = {\\fontsize{18pt}{12}\\selectfont}}\n"))
  
  #-----------------------------------------------------------------------------
  #Print the plot using original scatter3D to feed dev
  #-----------------------------------------------------------------------------
  lines3D (x = x, y = y, z = z, colvar = colvar, phi = phi, theta = theta, 
             col = col, NAcol = NAcol, breaks = breaks, colkey = colkey, 
             panel.first = panel.first, clim = clim, clab = clab, bty = bty, 
             CI = CI, surf = surf, add = add, plot = plot, 
             pch = pch, xlab = xlab, ylab = ylab, zlab = zlab,
             ticktype = ticktype,
             xlim = xlim,
             ylim = ylim,
             zlim = zlim,
             d = d,
             scale = scale)
  
  #-----------------------------------------------------------------------------
  # Necessary to close or the tikxDevice .tex file will not be written
  #-----------------------------------------------------------------------------
  dev.off()
}
