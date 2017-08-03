################################################################################
# R script used in EML2_TO_SEML.R for the plotting 
# of a projection map (connections between EML2 and SEML1,2)
# The plot are given in latex, pdf and png format, whenever necessary
#
# Used in the PhD manuscript.
#
################################################################################

#===============================================================================
# Color palette for families
#===============================================================================
values = rev(brewer.pal(6,"Dark2"))
colour.traj = values[2]

#===============================================================================
# Sizes
#===============================================================================
size_prim = 3;
size_lib  = 2;

#===============================================================================
# Plot : points (pmin_dist_SEM) in the x0_CMU_NCEM/y0_CMU_NCEM space
#===============================================================================
#-------------------------------------------------------------------------------
#Plot
#-------------------------------------------------------------------------------
pp_x0y0_pEM = plotdf_point(proj_map_tem, "x0_CMU_NCEM", "y0_CMU_NCEM", x_em, y_em, 
                           "pmin_dist_SEM", "pmin_dist_SEM", 0, pointSize = 3)

#-------------------------------------------------------------------------------
#Title & theme
#-------------------------------------------------------------------------------
pp_x0y0_pEM = pp_x0y0_pEM + scg_pem_guide_false 
pp_x0y0_pEM = pp_x0y0_pEM + custom_theme


#-------------------------------------------------------------------------------
# Add objects
#-------------------------------------------------------------------------------
#Add EMLi
pp_x0y0_pEM = pp_x0y0_pEM + geom_point(data = dfemli, aes(x= x_NC, y = y_NC), size = size_lib) 

#-------------------------------------------------------------------------------
#Ratio
#-------------------------------------------------------------------------------
#pp_x0y0_pEM = pp_x0y0_pEM + coord_fixed(ratio=1)

#-------------------------------------------------------------------------------
# Labels
#-------------------------------------------------------------------------------
pp_x0y0_pEM = pp_x0y0_pEM + labs(x = x_em, y = y_em)

#-------------------------------------------------------------------------------
#Save
#-------------------------------------------------------------------------------
#Filename
filename = paste0(filepre, "_x0_y0_pEM")
#Save in png
ggsave(pp_x0y0_pEM, width = xSize, height = ySize,  file = paste0(filename, ".png")) 

#===============================================================================
# Plot : points (pmin_dist_SEM) in the x0_CMU_NCEM/y0_CMU_NCEM space
# with continutation 
#===============================================================================
#---------------------------------------------------------------------------
# Plots
#---------------------------------------------------------------------------
pp_x0y0_pEM_fam = pp_x0y0_pEM
pp_x0y0_pEM_fam = pp_x0y0_pEM_fam + geom_path(data = proj_cont, aes(x0_CMU_NCEM, y0_CMU_NCEM), color = "white", size = 2.2)
pp_x0y0_pEM_fam = pp_x0y0_pEM_fam + geom_path(data = proj_cont, aes(x0_CMU_NCEM, y0_CMU_NCEM), color = colour.traj, size = 1.2)

#-------------------------------------------------------------------------------
#Aes
#-------------------------------------------------------------------------------
pp_x0y0_pEM_fam = pp_x0y0_pEM_fam + labs(x = x_em, y = y_em)
pp_x0y0_pEM_fam = pp_x0y0_pEM_fam + scg_pem_guide_false

#-------------------------------------------------------------------------------
#Save
#-------------------------------------------------------------------------------
#Filename
filename = paste0(filepre, "_x0_y0_pEM_cont_fam")

#Save in png
ggsave(pp_x0y0_pEM_fam, width = xSize, height = ySize,  file = paste0(filename, ".png")) #Save png

# Save in pdf - quite heavy!
# ggsave(pp_x0y0_pEM_fam, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".pdf")) #Save in pdf


#===============================================================================
# Plot : tiles (pmin_dist_SEM) in the s1_CMU_EM/s3_CMU_EM space
#===============================================================================
#-------------------------------------------------------------------------------
#Plot
#-------------------------------------------------------------------------------
ppEM = plotdf_tile_1(proj_map_tem, "s1_CMU_EM", "s3_CMU_EM", s1_exp, s3_exp, "pmin_dist_SEM", "pmin_dist_SEM", FALSE)

#-------------------------------------------------------------------------------
# Aes
#-------------------------------------------------------------------------------
ppEM = ppEM + custom_theme
ppEM = ppEM + scg_pem_guide_false
if(LIB_POINT_EM == "L2")
{
  ppEM = ppEM + scale_x_continuous(breaks = seq(-42,42,6))
  ppEM = ppEM + scale_y_continuous(breaks = seq(-42,42,6)) 
}else
{
  ppEM = ppEM + scale_x_continuous(breaks = seq(-4,4,0.5))
  ppEM = ppEM + scale_y_continuous(breaks = seq(-4,4,0.5)) 
}

#-------------------------------------------------------------------------------
# Add EMLi
#-------------------------------------------------------------------------------
ppEM = ppEM + geom_point(data = dfemli, aes(x= x_NC, y = y_NC), size = size_lib)  

#-------------------------------------------------------------------------------
# Fonts
#-------------------------------------------------------------------------------
ppEM = set_font_cm_ex(ppEM)


#-------------------------------------------------------------------------------
# Add Continuation
#-------------------------------------------------------------------------------
ppEM = ppEM + geom_path(data = proj_cont, aes(s1_CMU_EM, s3_CMU_EM), color = "white", size = 2.2)
ppEM = ppEM + geom_path(data = proj_cont, aes(s1_CMU_EM, s3_CMU_EM), color = colour.traj, size = 1.2)

#-------------------------------------------------------------------------------
# Save
#-------------------------------------------------------------------------------
#Filename
filename = paste0(filepre, "_s1_s3_pEM")
#Save png
ggsave(ppEM, width = xSize, height = ySize,  file = paste0(filename, ".png")) 
