#
# ggplot2tikz. Save a ggplot in tex format using tikz package for R.
#
# The output format is a standalone document of size width0 x height0, save as
# filename 
# BLB 2016.
ggplot2tikz <- function(plot, width0, height0, filename)
{
  #Create a .tex filename that will contain the plot
  tikz(filename, 
       width = width0, 
       height = height0, 
       standAlone=TRUE, 
       documentDeclaration="\\documentclass{standalone}\n",
       packages = c("\\usepackage[utf8]{inputenc}",
                    "\\usepackage[T1]{fontenc}",
                    "\\usepackage{tikz}", 
                    "\\usepackage{pgf}", 
                    "\\usetikzlibrary{calc}", 
                    "\\usepackage{amssymb}", 
                    "\\usepackage{amsfonts}\n"))
  
  
  #Print it to feed dev
  print(plot)
  
  #Necessary to close or the tikxDevice .tex filename will not be written
  dev.off()

}