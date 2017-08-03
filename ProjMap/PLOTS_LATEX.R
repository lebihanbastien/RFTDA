################################################################################
# R script used in EML2_TO_SEML.R for the plotting 
# of a projection map (connections between EML2 and SEML1,2)
# The plot are given in latex, pdf and png format, whenever necessary
#
################################################################################

#===============================================================================
# Color palette for families
#===============================================================================
values = rev(brewer.pal(NFAM,"Dark2"))

#===============================================================================
# Sizes
#===============================================================================
size_prim = 3;
size_lib  = 2;

#===============================================================================
# Plot : tiles (pmin_dist_SEM) in the s1_CMU_EM/s3_CMU_EM space
#===============================================================================
#Filename
filename = paste0(filepre, "_s1_s3_pEM")

#-------------------------------------------------------------------------------
#Plot
#-------------------------------------------------------------------------------
ppEM = plotdf_tile_1(proj_map_tem, "s1_CMU_EM", "s3_CMU_EM", s1_exp, s3_exp, "pmin_dist_SEM", "pmin_dist_SEM", FALSE)

#-------------------------------------------------------------------------------
# Aes
#-------------------------------------------------------------------------------
#ppEM = ppEM + ggtitle_t0
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
# Adding some specific points EML1 case
#-------------------------------------------------------------------------------
# s1 = c(-1.86434234616972,    1.26790749047835, -0.912619607338164)
# s3 = c(0.692929085525797,   -0.146997107762611, -0.2978187605094)   
# some_points = data.frame(s1 = s1, s3 = s3);
# ppEM = ppEM + geom_point(data = some_points, aes(s1, s3), color = "white", size = 6)
# ppEM = ppEM + geom_point(data = some_points, aes(s1, s3), color = "black", size = 4)   

#-------------------------------------------------------------------------------
# Adding some specific points EML2 case (last one is optional, it's the 81 solution)
#-------------------------------------------------------------------------------
# s1 = c(-6.33657336447702, -17.4967559841643, -7.89562473141036, 
#        -34.1699947879281, 21.3502514532090, 34.049549194161, 22.0624920802144)#, 21.8521515545601 )
# s3 = c(-16.0433671144055,  11.975160638613, 13.2363424410166, 
#         17.2350980451565, 7.00000095578531, 35.0529704977418, 5.36507500514161)#, 5.29561909750371 )
#some_points = data.frame(s1 = s1, s3 = s3, values = values);
# In white...
#ppEM = ppEM + geom_point(data = some_points, aes(s1, s3), color = "white", size = 6)
# In black
#ppEM = ppEM + geom_point(data = some_points, aes(s1, s3), color = "black", size = 4)
# or specific colors
# for(i in seq(1, NFAM, 1))
# {
#   ppEM = ppEM + geom_point(data = some_points[i,], aes(s1, s3), color = values[i], size = 4)
# }

#-------------------------------------------------------------------------------
# Colors
#-------------------------------------------------------------------------------
ppEM = ppEM + scale_colour_gradient("pmin_dist_SEM", space="Lab", high = "white", low = muted("blue"), limits = projection_color_lim, guide = FALSE)
ppEM = ppEM + scale_fill_gradient("pmin_dist_SEM", space="Lab", high = "white", low = muted("blue"), limits = projection_color_lim, guide = FALSE)


#-------------------------------------------------------------------------------
# Add EMLi
#-------------------------------------------------------------------------------
ppEM = ppEM + geom_point(data = dfemli, aes(x= x_NC, y = y_NC), size = size_lib)  

#-------------------------------------------------------------------------------
# Display
#-------------------------------------------------------------------------------
ppEM


#-------------------------------------------------------------------------------
# Save, for issfd
#-------------------------------------------------------------------------------
# Save in png
ppEM = ppEM + scg_pem + theme(legend.position="top")
ppEM = ppEM + labs(x = s1_exp, y = s3_exp)
ppEM = ppEM + issfd_theme
ggsave(ppEM, width = xSize, height = ySize,  file = paste0(filename, ".png")) #Save png


#-------------------------------------------------------------------------------
# Save, classic
#-------------------------------------------------------------------------------
# Save png
# ppEM = ppEM + labs(x = s1_exp, y = s3_exp)
# ppEM = ppEM + scg_pem_guide_false
# ggsave(ppEM, width = xSize, height = ySize,  file = paste0(filename, ".png")) 

# Save in pdf
# ppEM = ppEM + sfg_pem_guide_false
# ggsave(ppEM, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".pdf")) 

#-------------------------------------------------------------------------------
# Save in tex
#-------------------------------------------------------------------------------
# ppEM = ppEM + labs(x = "$s_1$", y = "$s_3$")
# ppEM = ppEM + scg_pem_tex


#===============================================================================
# Plot : tiles (pmin_dist_SEM) in the s1_CMU_EM/s3_CMU_EM space, 
# with continutation
#===============================================================================
if(!empty(proj_cont))
{
  #Filename
  filename = paste0(filepre, "_s1_s3_pEM_cont")
  
  #Adding some continuation results
  ppEM_cont = ppEM + geom_point(data = proj_cont, aes(s1_CMU_EM, s3_CMU_EM), color = "white", size = 3)
  ppEM_cont = ppEM_cont + geom_point(data = proj_cont, aes(s1_CMU_EM, s3_CMU_EM), color = "black", size = 1)
  
  # Limits, if necessary
  #   ppEM = ppEM + scale_x_continuous(limits = c(-35, 35), breaks = seq(-42,42,6))
  #   ppEM = ppEM + scale_y_continuous(limits = c(-35, 35), breaks = seq(-42,42,6)) 
  
  #Save in png
  ppEM_cont = ppEM_cont + labs(x = s1_exp, y = s3_exp)
  ppEM_cont = ppEM_cont + scg_pem_guide_false
  ggsave(ppEM_cont, width = xSize, height = ySize,  file = paste0(filename, ".png")) #Save png
  
  # Save in pdf
  #ggsave(ppEM_cont, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".pdf")) #Save in pdf
}


#===============================================================================
# Plot : points (pmin_dist_SEM) in the x0_CMU_NCEM/y0_CMU_NCEM space
#===============================================================================

#-------------------------------------------------------------------------------
#Filename
#-------------------------------------------------------------------------------
filename = paste0(filepre, "_x0_y0_pEM")

#-------------------------------------------------------------------------------
#Plot
#-------------------------------------------------------------------------------
pp_x0y0_pEM = plotdf_point(proj_map_tem, "x0_CMU_NCEM", "y0_CMU_NCEM", "x", "y", 
                           "pmin_dist_SEM", "pmin_dist_SEM", 0, pointSize = 1)

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

#Add Moon
#pp_x0y0_pEM = pp_x0y0_pEM + geom_point(data = dfmoon_eml, aes(x= x_NC, y = y_NC), size = size_prim)

#-------------------------------------------------------------------------------
#Adding some specific points (last one is optional)
#-------------------------------------------------------------------------------
# x0 = c(-0.0503021637013195, 0.0720745778172508, 0.0570779010089832,
#        0.108390915510616, -0.0598335194863945, -0.112560549310534, -0.0663892631216)#, -0.0654641553422555)
# y0 = c(0.173076575031959, 0.0716211151952854, -0.0204216068823488,
#        0.177116299110083, -0.246786030643921, -0.550123064539671, -0.242757702271792)#, -0.240252085712626)
# some_points = data.frame(x0 = x0, y0 = y0);
# pp_x0y0_pEM = pp_x0y0_pEM + geom_point(data = some_points, aes(x0, y0), color = "white", size = 6)
# # In black
# pp_x0y0_pEM = pp_x0y0_pEM + geom_point(data = some_points, aes(x0, y0), color = "black", size = 4)
# # or specific colors
# for(i in seq(1, NFAM, 1))
# {
#   pp_x0y0_pEM = pp_x0y0_pEM + geom_point(data = some_points[i,], aes(x0, y0), color = values[i], size = 4)
# }

#-------------------------------------------------------------------------------
#Adding some specific points (for ISSFD)
#-------------------------------------------------------------------------------
x0 = c(-0.0503021637013195,  0.0570779010089832, -0.0598335194863945, -0.112560549310534, -0.0663892631216)#, -0.0654641553422555)
y0 = c(0.173076575031959, -0.0204216068823488, -0.246786030643921, -0.550123064539671, -0.242757702271792)#, -0.240252085712626)
some_points = data.frame(x0 = x0, y0 = y0);
pp_x0y0_pEM = pp_x0y0_pEM + geom_point(data = some_points, aes(x0, y0), color = "white", size = 6)
# In black
pp_x0y0_pEM = pp_x0y0_pEM + geom_point(data = some_points, aes(x0, y0), color = "black", size = 4)
# or specific colors
for(i in seq(1, NFAM, 1))
{
  pp_x0y0_pEM = pp_x0y0_pEM + geom_point(data = some_points[i,], aes(x0, y0), color = values[i], size = 4)
}

#-------------------------------------------------------------------------------
#Ratio
#-------------------------------------------------------------------------------
#pp_x0y0_pEM = pp_x0y0_pEM + coord_fixed(ratio=1)

#-------------------------------------------------------------------------------
# Labels
#-------------------------------------------------------------------------------
pp_x0y0_pEM = pp_x0y0_pEM + labs(x = x_em, y = y_em)

#-------------------------------------------------------------------------------
#Save for issfd
#-------------------------------------------------------------------------------
#Save in png
pp_x0y0_pEM = pp_x0y0_pEM  + issfd_theme
pp_x0y0_pEM = pp_x0y0_pEM + scg_pem #+ theme(legend.position="top")
ggsave(pp_x0y0_pEM, width = xSize, height = ySize,  file = paste0(filename, ".png")) 

stop()
#-------------------------------------------------------------------------------
#Save, classic
#-------------------------------------------------------------------------------
# #Save in png
# ggsave(pp_x0y0_pEM, width = xSize, height = ySize,  file = paste0(filename, ".png")) 
# #Save in pdf
# ggsave(pp_x0y0_pEM, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".pdf")) 


#-------------------------------------------------------------------------------
# Save in tex
#-------------------------------------------------------------------------------
#pp_x0y0_pEM = pp_x0y0_pEM + labs(x = "$x$", y = "$y$")
#pp_x0y0_pEM = pp_x0y0_pEM + scg_pem_tex


#===============================================================================
# Plot : tiles (pmin_dist_SEM) in the s1_CMU_EM/s3_CMU_EM space, with continutation
#===============================================================================
if(!empty(proj_cont))
{
  #Filename
  filename = paste0(filepre, "_x0_y0_pEM_cont")
  
  #Adding some continuation results
  pp_x0y0_pEM = pp_x0y0_pEM + geom_path(data = proj_cont, aes(x0_CMU_NCEM, y0_CMU_NCEM), color = "white", size = size_lib)
  pp_x0y0_pEM = pp_x0y0_pEM + geom_path(data = proj_cont, aes(x0_CMU_NCEM, y0_CMU_NCEM), color = "black", size = 1)
  
  #Save in png
  pp_x0y0_pEM = pp_x0y0_pEM + scg_pem_guide_false
  ggsave(pp_x0y0_pEM, width = xSize, height = ySize,  file = paste0(filename, ".png")) #Save png
  
  # Save in pdf
  #pp_x0y0_pEM = pp_x0y0_pEM + labs(x = "$x$", y = "$y$")
  #pp_x0y0_pEM = pp_x0y0_pEM + scg_pem_tex
  #ggsave(pp_x0y0_pEM, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".pdf")) #Save in pdf
  
}


#===============================================================================
# After this point: we stop if no values in traj_cont
#===============================================================================
if(empty(traj_cont))
{
  stop("No continuation results. Stop.")
}

#===============================================================================
# else, continuation plot: Only certain values
#===============================================================================
maxlabel = Inf

# Select the value for which the label is under a certain value
proj_map_cont_traj_in =  traj_cont[which(traj_cont$label < maxlabel),]
proj_map_cont_in      =  proj_cont[which(proj_cont$label < maxlabel),]

# Only one solution (the first)
constraint = proj_map_cont_traj_in$label == 50#min(proj_map_cont_traj_in$label)
proj_map_cont_traj_one =  proj_map_cont_traj_in[which(constraint),]

#Only some solution
constraint = proj_map_cont_traj_in$label %% 6 == 0
proj_map_cont_traj_some =  proj_map_cont_traj_in[which(constraint),]

#===============================================================================
# continuation plot: Only one solution
#===============================================================================
#Filename
filename = paste0(filecont, "_x0_y0_NCSEM")

#Plot
pp_path_sol = ggplot() + geom_path(data = proj_map_cont_traj_one, aes(x = x_CMS_NCSEM, y = y_CMS_NCSEM, colour = factor(label), group = factor(label)), colour = "black", size = 1.5)
pp_path_sol = pp_path_sol + scale_colour_brewer(palette = "Set1", direction = -1, guide = FALSE) #scale_colour_manual(values = brewer.pal(2,"Set1"), guide = FALSE)


#Add SEMLi
pp_path_sol = pp_path_sol + geom_point(data = dfseml2, aes(x= x_NC, y = x_NC), size = size_lib)

#Add Earth
pp_path_sol = pp_path_sol + geom_point(data = dfearth_seml, aes(x= x_NC, y = y_NC), size = size_prim) 

#Theme
pp_path_sol = pp_path_sol + custom_bw_theme
pp_path_sol = pp_path_sol + coord_fixed(ratio=1)

#Save in Latex, with annotations
pp_path_sol = pp_path_sol + labs(x = "$x$", y = "$y$")
pp_path_sol_tex = pp_path_sol + annotate("text", x = 0, y = -0.08, label = "SEML$_2$", size = 5)
pp_path_sol_tex = pp_path_sol_tex + annotate("text", x = -1, y = -0.08, label = "Earth", size = 5)

#ggplot2tikz(pp_path_sol_tex, width = xSize, height = ySize, file = paste0(filename, "_tex.tex"))

# Save in pdf and png with annotations
pp_path_sol = pp_path_sol + labs(x = "x", y = "y")
pp_path_sol = pp_path_sol + annotate("text", x = 0, y = -0.08,  label = "SEML[2]", size = 5, parse = TRUE)
pp_path_sol = pp_path_sol + annotate("text", x = -1, y = -0.08, label = "Earth", size = 5)
pp_path_sol

ggsave(pp_path_sol, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".pdf")) #Save in pdf
ggsave(pp_path_sol, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".png")) #Save in pdf

#===============================================================================
# CONTINUATION: Only some solutions
#===============================================================================
fpp_path_some <- function(proj_map_cont_traj_some, filename, colour_fam, x_dim = "x_CMS_NCSEM", y_dim = "y_CMS_NCSEM")
{
  #Plot
  pp_path_some = ggplot() + geom_path(data = proj_map_cont_traj_some, 
                                      aes_string(x = x_dim, y = y_dim, group = "label"), 
                                      colour = colour_fam, size = 0.4)
  
  #Theme
  pp_path_some = pp_path_some + custom_theme
  pp_path_some = pp_path_some + coord_fixed(ratio=1)
  
  if(x_dim == "x_CMS_NCSEM" && y_dim == "y_CMS_NCSEM")
  {
    #Add SEMLi
    pp_path_some = pp_path_some + geom_point(data = dfseml2, aes(x= x_NC, y = x_NC), size = size_lib)
    
    #Add Earth
    pp_path_some = pp_path_some + geom_point(data = dfearth_seml, aes(x= x_NC, y = y_NC), size = size_prim) 
    
    #In Latex, with annotations
    pp_path_some_tex = pp_path_some + labs(x = "$x$", y = "$y$")
    pp_path_some_tex = pp_path_some_tex + annotate("text", x = 0,  y = -0.08, label = "SEML$_2$", size = 5)
    pp_path_some_tex = pp_path_some_tex + annotate("text", x = -1, y = -0.08, label = "Earth", size = 5)
    
    
    # In pdf, with annotations
    pp_path_some = pp_path_some + labs(x = "x", y = "y")
    pp_path_some = pp_path_some + annotate("text", x = 0, y = -0.08,  
                                           label = "bold(SEML[2])", colour = "white",
                                           size = 5, parse = TRUE)
    pp_path_some = pp_path_some + annotate("text", x = -1, y = -0.08, 
                                           label = "bold(Earth)",  colour = "white",
                                           size = 5, parse = TRUE)
    pp_path_some = pp_path_some + annotate("text", x = 0, y = -0.08,  label = "SEML[2]", size = 5, parse = TRUE)
    pp_path_some = pp_path_some + annotate("text", x = -1, y = -0.08, label = "Earth", size = 5)
    
  }
  
  if(x_dim == "x_CMS_NCSEM" && y_dim == "z_CMS_NCSEM")
  {
    #Add SEMLi
    pp_path_some = pp_path_some + geom_point(data = dfseml2, aes(x= x_NC, y = z_NC), size = size_lib)
    #Add Earth
    pp_path_some = pp_path_some + geom_point(data = dfearth_seml, aes(x= x_NC, y = z_NC), size = size_prim) 
    
    #In Latex, with annotations
    pp_path_some_tex = pp_path_some + labs(x = "$x$", y = "$z$")
    pp_path_some_tex = pp_path_some_tex + annotate("text", x = 0, y = -0.08, label = "SEML$_2$", size = 5)
    pp_path_some_tex = pp_path_some_tex + annotate("text", x = -1, y = -0.08, label = "Earth", size = 5)
    
    
    # In pdf, with annotations
    pp_path_some = pp_path_some + labs(x = "x", y = "z")
    pp_path_some = pp_path_some + annotate("text", x = 0, y = -0.08,  
                                           label = "bold(SEML[2])", colour = "white",
                                           size = 5, parse = TRUE)
    pp_path_some = pp_path_some + annotate("text", x = -1, y = -0.08, 
                                           label = "bold(Earth)",  colour = "white",
                                           size = 5, parse = TRUE)
    pp_path_some = pp_path_some + annotate("text", x = 0, y = -0.08,  label = "SEML[2]", size = 5, parse = TRUE)
    pp_path_some = pp_path_some + annotate("text", x = -1, y = -0.08, label = "Earth", size = 5)
    
    
  }
  
  if(x_dim == "y_CMS_NCSEM" && y_dim == "z_CMS_NCSEM")
  {
    #Add SEMLi
    pp_path_some = pp_path_some + geom_point(data = dfseml2, aes(x= y_NC, y = z_NC), size = size_lib)
    #Add Earth
    pp_path_some = pp_path_some + geom_point(data = dfearth_seml, aes(x= y_NC, y = z_NC), size = size_prim) 
    
    #In Latex, with annotations
    pp_path_some_tex = pp_path_some + labs(x = "$y$", y = "$z$")
    
    # In pdf, with annotations
    pp_path_some = pp_path_some + labs(x = "y", y = "z")
  }
  
  if(x_dim == "x_CMS_NCEM" && y_dim == "y_CMS_NCEM")
  {
    #Constants
    ydown = -0.1
    
    #Add EML1
    pp_path_some = pp_path_some + geom_point(data = dfemli, aes(x= x_NC, y = x_NC), size = size_lib)
    
    #Add EML2
    pp_path_some = pp_path_some + geom_point(data = dfeml2, aes(x= x_NCC, y = y_NCC), size = size_lib)
    
    #Add Moon
    pp_path_some = pp_path_some + geom_point(data = dfmoon_eml, aes(x= x_NC, y = y_NC), size = size_prim) 
    
    # Limit
    pp_path_some = pp_path_some + scale_x_continuous(limits = c(-0.3, 3)) #c(-0.25, 0.25)
    pp_path_some = pp_path_some + scale_y_continuous(limits = c(-0.8, 0.8)) #c(-0.25, 0.25)
    
    #In Latex, with annotations
    pp_path_some_tex = pp_path_some + labs(x = "$x$", y = "$y$")
    pp_path_some_tex = pp_path_some_tex + annotate("text", x = 0,  y = ydown, label = "EML$_1$", size = 5)
    pp_path_some_tex = pp_path_some_tex + annotate("text", x = dfeml2$x_NCC[1],  y = ydown, label = "EML$_2$", size = 5)
    pp_path_some_tex = pp_path_some_tex + annotate("text", x = dfmoon_eml$x_NC[1], y = ydown, label = "Moon", size = 5)
    
    
    # In pdf, with annotations
    pp_path_some = pp_path_some + labs(x = "x", y = "y")
    pp_path_some = pp_path_some + annotate("text", x = 0, y = ydown,  
                                           label = "bold(EML[1])", colour = "white",
                                           size = 5, parse = TRUE)
    pp_path_some = pp_path_some + annotate("text", x = dfeml2$x_NCC[1], y = ydown,  
                                           label = "bold(EML[2])", colour = "white",
                                           size = 5, parse = TRUE)
    pp_path_some = pp_path_some + annotate("text", x = dfmoon_eml$x_NC[1], y = ydown, 
                                           label = "bold(Moon)",  colour = "white",
                                           size = 5, parse = TRUE)
    pp_path_some = pp_path_some + annotate("text", x = 0, y = ydown,  label = "EML[1]", size = 5, parse = TRUE)
    pp_path_some = pp_path_some + annotate("text", x = dfeml2$x_NCC[1], y = ydown,  label = "EML[2]", size = 5, parse = TRUE)
    pp_path_some = pp_path_some + annotate("text", x = dfmoon_eml$x_NC[1], y = ydown, label = "Moon", size = 5)
    
  }
  
  #Limits
  #pp_path_some = pp_path_some + scale_x_continuous(limits = c(-1.013, -0.997))
  #pp_path_some = pp_path_some + scale_y_continuous(limits = c(-0.012, 0.013)) 
  
  
  ggplot2tikz(pp_path_some_tex, width = xSize, height = ySize, file = paste0(filename, "_tex.tex"))          #Save in tex
  ggsave(pp_path_some, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".pdf")) #Save in pdf
  ggsave(pp_path_some, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".png")) #Save in png
  
  return(pp_path_some)
}

#-------------------------------------------------------------------------------
# Plot NCEM
#-------------------------------------------------------------------------------
#Filename
filename = paste0(filecont, "_x0_y0_NCEM_cont")
fpp_path_some(proj_map_cont_traj_some, filename, "black", x_dim = "x_CMS_NCEM", y_dim = "y_CMS_NCEM")

#-------------------------------------------------------------------------------
# Plot NCSEM
#-------------------------------------------------------------------------------
#Filename
filename = paste0(filecont, "_x0_y0_NCSEM_cont")
fpp_path_some(proj_map_cont_traj_some, filename, "black", x_dim = "x_CMS_NCSEM", y_dim = "y_CMS_NCSEM")

# filename = paste0(filecont, "_x0_z0_NCSEM_cont")
# fpp_path_some(proj_map_cont_traj_some, filename, "black", x_dim = "x_CMS_NCSEM", y_dim = "z_CMS_NCSEM")
# 
# filename = paste0(filecont, "_y0_z0_NCSEM_cont")
# fpp_path_some(proj_map_cont_traj_some, filename, "black", x_dim = "y_CMS_NCSEM", y_dim = "z_CMS_NCSEM")


#===============================================================================
# Isolated solution
#===============================================================================

#-------------------------------------------------------------------------------
# Prefix and filenames to save files, again, if not defined...
#-------------------------------------------------------------------------------
filepre  = paste0(FILE_PREFIX, "_t0_0", toString(100*ratio_desired))
filecont = paste0(FILE_PREFIX_CONT, "_t0_0", toString(100*ratio_desired))

#-------------------------------------------------------------------------------
# Data
#-------------------------------------------------------------------------------
condition = traj_cont$label == max(traj_cont$label) #& traj_cont$type < 3
traj_cont_1 =  traj_cont[which(condition),]
set1 = rev(brewer.pal(3,"Set1"))

#-------------------------------------------------------------------------------
# In SEM coordinates
#-------------------------------------------------------------------------------
filename     = paste0(filecont, "_x0_y0_NCSEM_single", FAM0)
ppsol_NCSEM  = fpp_path_traj(traj_cont_1, filename, "SEM", 
                             xlab = x_sem, ylab = y_sem,
                             xlab.tex = "$x^{sem}$", ylab.tex = "$y^{sem}$",
                             colour.traj = set1[1],
                             lib.point.em = LIB_POINT_EM, 
                             lib.point.sem = LIB_POINT_SEM)


filename     = paste0(filecont, "_x0_y0_NCSEM_single_col", FAM0)
colour_fam   = values[floor((FAM0-1)/10)]
ppsol_NCSEM  = fpp_path_traj(traj_cont_1, filename, "SEM", 
              xlab = x_sem, ylab = y_sem,
              xlab.tex = "$x^{sem}$", ylab.tex = "$y^{sem}$",
              colour.traj = colour_fam,
              lib.point.em = LIB_POINT_EM, 
              lib.point.sem = LIB_POINT_SEM)


#-------------------------------------------------------------------------------
# In NCEM coordinates
#-------------------------------------------------------------------------------
filename    = paste0(filecont, "_x0_y0_NCEM_single", FAM0)
ppsol_NCEM  = fpp_path_traj(traj_cont_1, filename, "EM", 
                            xlab = x_em, ylab = y_em,
                            xlab.tex = "$x^{em}$", ylab.tex = "$y^{em}$",
                            colour.traj = set1[1],
                            lib.point.em = LIB_POINT_EM, 
                            lib.point.sem = LIB_POINT_SEM)

filename    = paste0(filecont, "_x0_y0_NCEM_single_col", FAM0)
colour_fam  = values[floor((FAM0-1)/10)]
ppsol_NCEM  = fpp_path_traj(traj_cont_1, filename, "EM", 
                            xlab = x_em, ylab = y_em,
                            xlab.tex = "$x^{em}$", ylab.tex = "$y^{em}$",
                            colour.traj = colour_fam,
                            lib.point.em = LIB_POINT_EM, 
                            lib.point.sem = LIB_POINT_SEM)
  
#===============================================================================
# Plot : tiles (sf_CM_SEM) in the s1_CMU_EM/s3_CMU_EM space
#===============================================================================
#Filename
filename = paste0(filepre, "_s1_s3_rEM")

#-------------------------------------------------------------------------------
#Plot
#-------------------------------------------------------------------------------
prEM = plotdf_tile_1(proj_map_tem, "s1_CMU_EM", "s3_CMU_EM", s1_exp, s3_exp, "sf_CM_SEM", expression(group("||", q[f], "||")), FALSE)


#-------------------------------------------------------------------------------
# Aes
#-------------------------------------------------------------------------------
#prEM = prEM + ggtitle_t0
prEM = prEM + custom_theme
prEM = prEM + scale_x_continuous(breaks = seq(-42,42,6))
prEM = prEM + scale_y_continuous(breaks = seq(-42,42,6)) 

#-------------------------------------------------------------------------------
# Add EMLi
#-------------------------------------------------------------------------------
prEM = prEM + geom_point(data = dfemli, aes(x= x_NC, y = y_NC), size = size_lib)  

#-------------------------------------------------------------------------------
# Colors
#-------------------------------------------------------------------------------
prEM = prEM + scale_colour_gradient(expression(group("||", bold(q)[f], "||")), space="Lab", high = "white", low = muted("green"))
prEM = prEM + scale_fill_gradient("sf_CM_SEM", space="Lab", high = "white", low = muted("green"), guide = FALSE)

#-------------------------------------------------------------------------------
# Display
#-------------------------------------------------------------------------------
prEM

#-------------------------------------------------------------------------------
# Save in png
#-------------------------------------------------------------------------------
prEM = prEM + labs(x = s1_exp, y = s3_exp)
ggsave(prEM, width = xSize, height = ySize,  file = paste0(filename, ".png")) #Save png

#-------------------------------------------------------------------------------
# Save in pdf
#-------------------------------------------------------------------------------
prEM = prEM + labs(x = "$s_1$", y = "$s_3$")
ggsave(prEM, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".pdf")) #Save in pdf
