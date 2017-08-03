################################################################################
# Initialization script for R session.
#
# BLB 2017
# 
# Note: to instale a package locally:
# install.packages(pkgs ='/home/b.le-bihan/R/tikzDevice_0.9.tar.gz', repos = NULL)
#
################################################################################

#-------------------------------------------------------------------------------
# R options
#-------------------------------------------------------------------------------
options(digits = 15)

#-------------------------------------------------------------------------------
# Load libraries
#-------------------------------------------------------------------------------
library(plyr)
library(ggplot2)
library(reshape2)
library(scales)
library(grid)
library(tikzDevice)
library(latex2exp)
library(RColorBrewer)
library(Rgnuplot)
#library(plot3D)
library(rgl)
library(gtable)

#-------------------------------------------------------------------------------
# Load Source files
#-------------------------------------------------------------------------------
source("source/rgl_init.R")
source("source/folder.R")
source("source/plot.R")
source("source/env.R")
source("source/userFriendly.R")
source("source/multiplot.R")
source("source/dffbinary.R")
source("source/parameters.R")
source("source/RGnuplot.R")
source("source/addgrids3d.R")
source("source/scattex3D.R")
source("source/ggplot2tikz.R")
source("source/rbind_cc.R")
source("source/get_cont.R")
source("source/fpp_path_traj.R")
source("source/fpp_path_traj_phd.R")