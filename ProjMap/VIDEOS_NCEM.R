######################################################################################
# R script used in EML2_TO_SEML.R for the video
# of a projection map (connections between EML2 and SEML1,2)
######################################################################################

#===============================================================================
# With or without the Moon
#===============================================================================
is.moon.plotted = TRUE

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
#NCEM to EM
#===============================================================================
proj_map$x0_CMU_EM <- -CST_GAMMA_LIB_EM * (proj_map$x0_CMU_NCEM-CST_C1_LIB_EM)
proj_map$y0_CMU_EM <- -CST_GAMMA_LIB_EM * (proj_map$y0_CMU_NCEM)
proj_map$z0_CMU_EM <- +CST_GAMMA_LIB_EM * (proj_map$z0_CMU_NCEM)

#===============================================================================
#L2
#===============================================================================
eml2 = data.frame(x0_CMU_NCEM = 0.0, y0_CMU_NCEM = 0.0, z0_CMU_NCEM = 0.0);
eml2 = NCtoSYS(eml2, CST_GAMMA_LIB_EM, CST_C1_LIB_EM);

#===============================================================================
# Functions for titles 
#===============================================================================
title1 <- function(t0)
{
  str = paste0("t = ", toString(t0/CST_SEM_PERIOD_EM), "T")
  return(str)
}

#===============================================================================
# Functions for filename 
#===============================================================================
filename1 <- function(t0, index_pdf)
{
  str = paste0(getwd(), "/ProjMap/VIDEO/", "pp_", 
               "from_EM", LIB_POINT_EM, "_to_SEM", LIB_POINT_SEM, 
               "_x0EM_y0EM_eP_t0_", toString(1000*t0/CST_SEM_PERIOD_EM), '.png')
  return(str)
}

filename2 <- function(t0, index_pdf)
{
  str = paste0(getwd(), "/ProjMap/VIDEO/", "pp_tiles_", 
               "from_EM", LIB_POINT_EM, "_to_SEM", LIB_POINT_SEM, 
               "_x0EM_y0EM_crossings_t0_", toString(1000*t0/CST_SEM_PERIOD_EM), '.png')
  return(str)
}


#===============================================================================
# User-defined video type 
#===============================================================================
splash1 = paste0("The different possibilities of video are the following:\n",
                 "A. Displaying the projection distance:\n",
                 "  1. x0_CMU_NCEM vs y0_CMU_NCEM as t0_CMU_EM increases;\n",
                 "B. Displaying the crossings:\n",
                 "  2. x0_CMU_NCEM vs y0_CMU_NCEM as t0_CMU_EM increases;")
splash2 = paste0("Enter the corresponding value (1 or 2):")

writeLines(splash1)
video_type = readline(prompt=splash2)

#===============================================================================
# Select the parameters for the rest of the computation, 
# depending on the user choice
#===============================================================================
if(video_type == 1)
{
  #=============================================================================
  # x0_CMU_NCEM vs y0_CMU_NCEM as t0_CMU_EM increases (proj distance)
  #=============================================================================
  # Select all unique values of time
  all_vec    = proj_map$t0_CMU_EM
  unique_vec = sort(unique(proj_map$t0_CMU_EM))
  # x and y values
  xx = "x0_CMU_NCEM"
  yy = "y0_CMU_NCEM"
  # Corresponding strings to display on plots
  xxs = x_em
  yys = y_em
  # Corresponding limits & breaks
  if(is.moon.plotted)
  {
    scale_x = scale_x_continuous(limits = c(-1, +0.25), breaks = seq(-1, 1, 0.2)) 
    scale_y = scale_y_continuous(limits = c(-0.7, 0.7), breaks = seq(-1, 1, 0.2))
    
  }else{
    
    scale_x = scale_x_continuous(limits = c(-0.25, +0.25)) 
    scale_y = scale_y_continuous(limits = c(-0.65, 0.65))
    
  }
  # Corresponding title function
  titlef = title1
  # Corresponding filename function
  filenamef = filename1
}else
{
  #=============================================================================
  # x0_CMU_NCEM vs y0_CMU_NCEM as t0_CMU_EM increases (crossings)
  #=============================================================================
  # Select all unique values of time
  all_vec    = proj_map$t0_CMU_EM
  unique_vec = sort(unique(proj_map$t0_CMU_EM))
  # x and y values
  xx = "x0_CMU_NCEM"
  yy = "y0_CMU_NCEM"
  # Corresponding strings to display on plots
  xxs = x_em
  yys = y_em
  # Corresponding limits & breaks
  if(is.moon.plotted)
  {
    scale_x = scale_x_continuous(limits = c(-1, +0.25), breaks = seq(-1, 1, 0.2)) 
    scale_y = scale_y_continuous(limits = c(-0.7, 0.7), breaks = seq(-1, 1, 0.2))
    
  }else{
    
    scale_x = scale_x_continuous(limits = c(-0.25, +0.25)) 
    scale_y = scale_y_continuous(limits = c(-0.65, 0.65))
    
  }
  # Corresponding title function
  titlef = title1
  # Corresponding filename function
  filenamef = filename2
}


#===============================================================================
# Loop
#===============================================================================
index = 0;
for(t0 in unique_vec)
{
  #=============================================================================
  # Select the data
  #=============================================================================
  condition = all_vec == t0
  proj_map_sub = proj_map[which(condition),] 
  
  #=============================================================================
  # Select the continuation data if they exist
  #=============================================================================
  # ratio_desired = t0/CST_SEM_PERIOD_EM;
  # FILE_SUFFIX_CONT      = paste0("_t0_", ratio_desired);
  # proj_map_cont = get_proj_cont(FILE_PREFIX_CONT, FILE_SUFFIX_CONT)
  # 
  # #NC to EM
  # proj_map_cont$x0_CMU_EM <- -CST_GAMMA_LIB_EM * (proj_map_cont$x0_CMU_NCEM-CST_C1_LIB_EM)
  # proj_map_cont$y0_CMU_EM <- -CST_GAMMA_LIB_EM * (proj_map_cont$y0_CMU_NCEM)
  # proj_map_cont$z0_CMU_EM <- +CST_GAMMA_LIB_EM * (proj_map_cont$z0_CMU_NCEM)

  #=============================================================================
  # Plot
  #=============================================================================
  if(video_type == 1)
  {
    ppt = plotdf_point(proj_map_sub, xx, yy, xxs, yys, "pmin_dist_SEM", "pmin_dist_SEM", 0, pointSize = 1)
    ppt = ppt + scg_pem_guide_false
  }else{
    #---------------------------------------------------------------------------
    # Colors
    #---------------------------------------------------------------------------
    # A selected set of crossings
    vcross  = c("2", "2.2", "3", "3.1", "4", "4.2", "5, 6") 
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
    ppt = plotdf_point(proj_map_sub, xx, yy, xxs, yys, "crossings", "crossings", TRUE, pointSize = 1)
    ppt = ppt + scale_colour_manual("Crossings", values = vmatchs, breaks = vcross, guide = FALSE) 
  }
  
  
  #Continuation
  # if(nrow(proj_map_cont) >0)
  # {
  #   ppt = ppt + geom_path(data = proj_map_cont, aes(x0_CMU_EM,  y0_CMU_EM) , color = "red", size = 1)
  #   
  # }
  
  #Position of L2
  ppt = ppt + geom_point(data = eml2, aes(x= x0_CMU_NCEM, y = y0_CMU_NCEM), size = 3) 
  
  #Add Moon
  if(is.moon.plotted)
  {
    ppt = ppt + geom_point(data = dfmoon_eml, aes(x= x_NC, y = y_NC), size = 4) 
    ppt = ppt + annotate("text", x = dfmoon_eml$x_NC[1], y = -0.1, label = "Moon", size = 5)
  }
   
  #Limits
  ppt = ppt + scale_x
  ppt = ppt + scale_y
  
  #=============================================================================
  # Title
  #=============================================================================
  ppt   = ppt + ggtitle(titlef(t0))
  
  #=============================================================================
  # Save
  #=============================================================================
  if(!empty(proj_map_sub))
  {
    filename = filenamef(t0, index_pdf)
    ggsave(ppt, width = xSize, height = ySize, file = filename, dpi=100)
  }
  index_pdf = index_pdf+1
}