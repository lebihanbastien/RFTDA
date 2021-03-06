######################################################################################
# R script used in EML2_TO_SEML.R for the video
# of a projection map (connections between EML2 and SEML1,2)
######################################################################################
#===============================================================================
# Define limit of projection
#===============================================================================
#hard
if(LIB_POINT_EM == "L2")
{
  projection_lim_max = 5e-3; #1e-1
  projection_lim_mid = 5e-4;
  projection_color_lim = c(0, 1e-2);
}else
{
  projection_lim_max = 1e-2; #1e-1
  projection_lim_mid = 5e-4;
  projection_color_lim = c(0, 1e-2);
}


#===============================================================================
#Keep only values with a projection distance below projection_lim_max.
#===============================================================================
proj_map = proj_map_source[which(proj_map_source$pmin_dist_SEM <= projection_lim_max),] 

#===============================================================================
# Functions for titles 
#===============================================================================
title1 <- function(t0)
{
  str = paste0("t = ", toString(t0/CST_SEM_PERIOD_EM), "T")
  return(str)
}

title2 <- function(s3)
{
  str = paste0("s3 = ", toString(s3))
  return(str)
}

title3 <- function(s1)
{
  str = paste0("s1 = ", toString(s1))
  return(str)
}

#===============================================================================
# Functions for filename 
#===============================================================================z
filename1 <- function(t0, index_pdf)
{
  str = paste0(getwd(), "/ProjMap/VIDEO/", "pp_tiles_", 
               "from_EM", LIB_POINT_EM, "_to_SEM", LIB_POINT_SEM, 
               "_s1EM_s3EM_eP_t0_", toString(1000*t0/CST_SEM_PERIOD_EM), '.pdf')
  return(str)
}

filename2 <- function(s3, index_pdf)
{
  str = paste0(getwd(), "/ProjMap/VIDEO/", "pp_tiles_", 
               "from_EM", LIB_POINT_EM, "_to_SEM", LIB_POINT_SEM, 
               "_t0EM_s1EM_eP_s3_", toString(s3), '.pdf')
  return(str)
}


filename3 <- function(s1, index_pdf)
{
  str = paste0(getwd(), "/ProjMap/VIDEO/", "pp_tiles_", 
               "from_EM", LIB_POINT_EM, "_to_SEM", LIB_POINT_SEM, 
               "_t0EM_s3EM_eP_s1_", toString(s1), '.pdf')
  return(str)
}

filename4 <- function(t0, index_pdf)
{
  str = paste0(getwd(), "/ProjMap/VIDEO/", "pp_tiles_", 
               "from_EM", LIB_POINT_EM, "_to_SEM", LIB_POINT_SEM, 
               "_s1EM_s3EM_tof_t0_", toString(1000*t0/CST_SEM_PERIOD_EM), '.pdf')
  return(str)
}

filename5 <- function(t0, index_pdf)
{
  str = paste0(getwd(), "/ProjMap/VIDEO/", "pp_tiles_", 
               "from_EM", LIB_POINT_EM, "_to_SEM", LIB_POINT_SEM, 
               "_s1EM_s3EM_crossings_t0_", toString(1000*t0/CST_SEM_PERIOD_EM), '.pdf')
  return(str)
}

filename6 <- function(t0, index_pdf)
{
  str = paste0(getwd(), "/ProjMap/VIDEO/", "pp_tiles_", 
               "from_EM", LIB_POINT_EM, "_to_SEM", LIB_POINT_SEM, 
               "_s1EM_s3EM_crossings_t0_", toString(1000*t0/CST_SEM_PERIOD_EM), '.pdf')
  return(str)
}

#===============================================================================
# User-defined video type 
#===============================================================================
splash1 = paste0("The different possibilities of video are the following:\n",
                "A. Displaying the projection distance:\n",
                "  1. s1_CMU_EM vs s3_CMU_EM as t0_CMU_EM increases;\n",
                "  2. t0_CMU_EM vs s1_CMU_EM as s3_CMU_EM increases;\n",
                "  3. t0_CMU_EM vs s3_CMU_EM as s1_CMU_EM increases;\n",
                "B. Displaying the time of flight:\n",
                "  4. s1_CMU_EM vs s3_CMU_EM as t0_CMU_EM increases;\n",
                "C. Displaying the crossings:\n",
                "  5. s1_CMU_EM vs s3_CMU_EM as t0_CMU_EM increases;\n",
                "  6. Same, but only the primary family;")
splash2 = paste0("Enter the corresponding value (1, 2, 3, 4, 5, or 6):")

writeLines(splash1)
video_type = readline(prompt=splash2)



#===============================================================================
# Select the parameters for the rest of the computation, 
# depending on the user choice
#===============================================================================
if(video_type == 1)
{
  #=============================================================================
  # s1_CMU_EM vs s3_CMU_EM as t0_CMU_EM increases
  #=============================================================================
  # Select all unique values of time
  all_vec    = proj_map$t0_CMU_EM
  unique_vec = sort(unique(proj_map$t0_CMU_EM))
  # x and y values
  xx = "s1_CMU_EM"
  yy = "s3_CMU_EM"
  # Corresponding strings to display on plots
  xxs = expression("s"[1])
  yys = expression("s"[3])
  # Corresponding limits
  xxl = c(-40, 40)
  yyl = c(-40, 40)
  # Corresponding breaks
  xxb = seq(-42,42,6)
  yyb = seq(-42,42,6)
  # Corresponding title function
  titlef = title1
  # Corresponding filename function
  filenamef = filename1
}else if(video_type == 2)
{
  #=============================================================================
  # t0_CMU_EM vs s1_CMU_EM as s3_CMU_EM increases
  #=============================================================================
  # Select all unique values of time
  all_vec    = proj_map$s3_CMU_EM
  unique_vec = sort(unique(proj_map$s3_CMU_EM))
  # x and y values
  xx = "t0_CMU_EM"
  yy = "s1_CMU_EM"
  # Corresponding strings to display on plots
  xxs = expression("t"[0])
  yys = expression("s"[1])
  # Corresponding limits
  xxl = c(0, 1)
  yyl = c(-35, 35)
  # Corresponding breaks
  xxb = seq(0, 1, 0.1)
  yyb = seq(-42,42,6)
  # Corresponding title function
  titlef = title2
  # Corresponding filename function
  filenamef = filename2
}else if(video_type == 3)
{
  #=============================================================================
  # t0_CMU_EM vs s3_CMU_EM as s1_CMU_EM
  #=============================================================================
  # Select all unique values of time
  all_vec    = proj_map$s1_CMU_EM
  unique_vec = sort(unique(proj_map$s1_CMU_EM))
  # x and y values
  xx = "t0_CMU_EM"
  yy = "s3_CMU_EM"
  # Corresponding strings to display on plots
  xxs = expression("t"[0])
  yys = expression("s"[3])
  # Corresponding limits
  xxl = c(0, 1)
  yyl = c(-35, 35)
  # Corresponding breaks
  xxb = seq(0, 1, 0.1)
  yyb = seq(-42,42,6)
  # Corresponding title function
  titlef = title3
  # Corresponding filename function
  filenamef = filename3
  
}else if(video_type == 4)
{
  #=============================================================================
  # s1_CMU_EM vs s3_CMU_EM as t0_CMU_EM increases
  #=============================================================================
  # Select all unique values of time
  all_vec    = proj_map$t0_CMU_EM
  unique_vec = sort(unique(proj_map$t0_CMU_EM))
  # x and y values
  xx = "s1_CMU_EM"
  yy = "s3_CMU_EM"
  # Corresponding strings to display on plots
  xxs = expression("s"[1])
  yys = expression("s"[3])
  # Corresponding limits
  xxl = c(-35, 35)
  yyl = c(-35, 35)
  # Corresponding breaks
  xxb = seq(-42,42,6)
  yyb = seq(-42,42,6)
  # Corresponding title function
  titlef = title1
  # Corresponding filename function
  filenamef = filename4
}else if(video_type == 5 || video_type == 6)
{
  #=============================================================================
  # s1_CMU_EM vs s3_CMU_EM as t0_CMU_EM increases
  #=============================================================================
  # Select all unique values of time
  all_vec    = proj_map$t0_CMU_EM
  unique_vec = sort(unique(proj_map$t0_CMU_EM))
  # x and y values
  xx = "s1_CMU_EM"
  yy = "s3_CMU_EM"
  # Corresponding strings to display on plots
  xxs = expression("s"[1])
  yys = expression("s"[3])
  # Corresponding limits
  xxl = c(-40, 40)
  yyl = c(-40, 40)
  # Corresponding breaks
  xxb = seq(-42,42,6)
  yyb = seq(-42,42,6)
  # Corresponding title function
  titlef = title1
  # Corresponding filename function
  filenamef = filename5
}else{
  #=============================================================================
  # otherwise...
  #=============================================================================
  stop("unknown video-type. Must between 1 and 6")
}


#===============================================================================
# Loop
#===============================================================================
index_pdf = 0
for(zz in unique_vec)
{
  #=============================================================================
  # Select the data
  #=============================================================================
  if(video_type == 6)
  {
    # Selecting the primary family
    condition = all_vec == zz & proj_map$crossings == 2 & proj_map$collision == 0
    proj_map_sub = proj_map[which(condition),] 
  }else
  {
    condition = all_vec == zz
    proj_map_sub = proj_map[which(condition),] 
  }
  

  #=============================================================================
  # Plot
  #=============================================================================
  if(video_type == 1 || video_type == 2 || video_type == 3)
  {
    #Plotting the projection distance
    pp_tiles_eP = plotdf_tile_1(proj_map_sub, xx, yy, xxs, yys, "pmin_dist_SEM", "Projection \ndistance", FALSE, colorLimits = projection_color_lim, na.value = "white")
  }else if(video_type == 4)
  {
    #Plotting the tof 
    pp_tiles_eP = plotdf_tile_1(proj_map_sub, xx, yy, xxs, yys, "tof_EM", tof_exp, isLegendOn = TRUE)
    pp_tiles_eP = pp_tiles_eP + scale_colour_gradient2(space="Lab", midpoint= 6, guide = FALSE)
    pp_tiles_eP = pp_tiles_eP + scale_fill_gradient2(expression(TOF~(xT)), space="Lab", midpoint = 6)
  }else 
  {
    #---------------------------------------------------------------------------
    # Colors
    #---------------------------------------------------------------------------
    if(video_type == 5)
    {
      # A selected set of crossings
      vcross  = c("2", "2.2", "3", "3.1", "4", "4.2", "5, 6") 
    }else
    {
      # Only the primary
      vcross  = c("2")
    }
    
    vcolors = brewer.pal(8,"Dark2")
    vmatchs = c("2" = vcolors[1], "2.2" = vcolors[2], 
                "3" = vcolors[3], "3.1" = vcolors[4], "4" = vcolors[5],
                "4.2" = vcolors[6], "5" = vcolors[7], "6" = vcolors[8])
    
    #---------------------------------------------------------------------------
    # Condition on crossings
    #---------------------------------------------------------------------------
    condition = proj_map_sub$crossings %in% vcross & proj_map_sub$collision == 0
    proj_map_sub = proj_map_sub[which(condition),]
    
    #---------------------------------------------------------------------------
    # Plot
    #---------------------------------------------------------------------------
    pp_tiles_eP = plotdf_point(proj_map_sub, 
                                      "s1_CMU_EM", "s3_CMU_EM", 
                                      s1_exp, s3_exp, 
                                      "crossings", "crossings", 
                                      TRUE, pointSize = 1)
    pp_tiles_eP = pp_tiles_eP + scale_colour_manual("Crossings", values = vmatchs, breaks = vcross, guide = FALSE) 
  }
  
  pp_tiles_eP = pp_tiles_eP + scale_x_continuous(limits = xxl, breaks = xxb) 
  pp_tiles_eP = pp_tiles_eP + scale_y_continuous(limits = yyl, breaks = yyb) 
  #pp_tiles_eP =  pp_tiles_eP + theme_bw()
  
  #=============================================================================
  # Title
  #=============================================================================
  pp_tiles_eP   = pp_tiles_eP + ggtitle(titlef(zz))
  
  #=============================================================================
  # White background
  #=============================================================================
  pp_tiles_eP   = pp_tiles_eP + theme(plot.background = element_rect(fill = "white", colour = "white"))
  
  #=============================================================================
  # Save
  #=============================================================================
  if(!empty(proj_map_sub))
  {
    filename = filenamef(zz, index_pdf)
    ggsave(pp_tiles_eP, width = xSize, height = ySize, file = filename)
  }
  index_pdf = index_pdf+1
}