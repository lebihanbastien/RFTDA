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
values = rev(brewer.pal(NFAM,"Dark2"))

#===============================================================================
# Sizes
#===============================================================================
size_prim = 3;
size_lib  = 2;

#===============================================================================
# Latex 
#===============================================================================
dxncem  = "$\\| \\xb_0^\\mathsctiny{e}\\|$ (km)"
dxncse  = "$\\| \\xb_f^\\mathsctiny{s}\\|$ (km)"

dsncem  = "$\\| \\sb_0 \\|$"
dsncse  = "$\\| \\qb_f \\|$"

dhem  = "$ \\delta H_0^\\mathsctiny{E} $"
dhse  = "$ \\delta H_f^\\mathsctiny{S} $"

#===============================================================================
# Postprocess
#===============================================================================
# TOF
proj_cont_fam$r0_CMU_EMT  = proj_cont_fam$t0_CMU_EM/SEMperiod("EM")
proj_cont_fam$re_CMU_EMT  = proj_cont_fam$tf_CMU_EM/SEMperiod("EM")
proj_cont_fam$dr_CMU_EMT  = proj_cont_fam$re_CMU_EMT - proj_cont_fam$r0_CMU_EMT 
proj_cont_fam$dr_CMU_DAYS = T2days(proj_cont_fam$dr_CMU_EMT, "EM")

# Energy
proj_cont_fam$dH0_EM  = proj_cont_fam$H0_EM  - proj_cont_fam$H0_emli_EM
proj_cont_fam$dHf_SEM = proj_cont_fam$Hf_SEM - proj_cont_fam$Hf_semli_SEM
proj_cont_fam$dHf_EM = proj_cont_fam$Hf_EM   - proj_cont_fam$Hf_semli_EM


#===============================================================================
# Plot: dHEM vs DT
#===============================================================================
# Plot
pp_HEM_DT = ggplot() + geom_path(data = proj_cont_fam, 
                                 aes(x = dH0_EM, y = dr_CMU_EMT, color = factor(family), 
                                     group = factor(family)), size = 3)
pp_HEM_DT = pp_HEM_DT + scale_color_manual(values = values, guide = F)
pp_HEM_DT = pp_HEM_DT + custom_theme
pp_HEM_DT

# Latex
pp_HEM_DT_l = pp_HEM_DT + xlab(dhem) + ylab("TOF ($\\times T$)")

#Save
filename = paste0(filepre, "_HEM_DT")
ggplot2tikz_phd(pp_HEM_DT_l,  xSize, ySize, file = paste0(filename, ".tex"))


#===============================================================================
# Plot: dHEM vs dHSEM
#===============================================================================
# Plot
pp_HEM_HSEM = ggplot() + geom_path(data = proj_cont_fam, aes(x = dH0_EM, y = dHf_SEM, color = factor(family), group = factor(family)), size = 3)
pp_HEM_HSEM = pp_HEM_HSEM + scale_color_manual(values = values, guide = F)
pp_HEM_HSEM = pp_HEM_HSEM + custom_theme
pp_HEM_HSEM

# Latex
pp_HEM_HSEM_l = pp_HEM_HSEM + xlab(dhem) + ylab(dhse)

#Save
filename = paste0(filepre, "_HEM_HSEM")
ggplot2tikz_phd(pp_HEM_HSEM_l,  xSize, ySize, file = paste0(filename, ".tex"))



#===============================================================================
# Plot: dEM vs dSEM
#===============================================================================
# Plot
pp_xEM_xSEM = ggplot() + geom_path(data = proj_cont_fam, aes(x = d0_CMU_NCEM_KM, y = d0_CMS_NCSEM_KM, color = factor(family), group = factor(family)), size = 3)
pp_xEM_xSEM = pp_xEM_xSEM + scale_color_manual(values = values, guide = F)
pp_xEM_xSEM = pp_xEM_xSEM + custom_theme
pp_xEM_xSEM = pp_xEM_xSEM + scale_x_continuous(labels = scientific_format())

# Latex
pp_xEM_xSEM_l = pp_xEM_xSEM + xlab(dxncem) + ylab(dxncse)

#Save
filename = paste0(filepre, "_xEM_xSEM")
ggplot2tikz_phd(pp_xEM_xSEM_l,  xSize, ySize, file = paste0(filename, ".tex"))


#===============================================================================
# Plot: dsEM vs dsSEM
#===============================================================================
# Plot
pp_sEM_sSEM = ggplot() + geom_path(data = proj_cont_fam, aes(x = ds_CMU_EM, y = ds_CMS_SEM, color = factor(family), group = factor(family)), size = 3)
pp_sEM_sSEM = pp_sEM_sSEM + scale_color_manual(values = values, guide = F)
pp_sEM_sSEM = pp_sEM_sSEM + custom_theme
pp_sEM_sSEM

# Latex
pp_sEM_sSEM_l = pp_sEM_sSEM + xlab(dsncem) + ylab(dsncse)

#Save
filename = paste0(filepre, "_sEM_sSEM")
ggplot2tikz_phd(pp_sEM_sSEM_l,  xSize, ySize, file = paste0(filename, ".tex"))

#stop()

#===============================================================================
# Plot : points (pmin_dist_SEM) in the x0_CMU_EM/y0_CMU_EM space
#===============================================================================
#-------------------------------------------------------------------------------
#Plot
#-------------------------------------------------------------------------------
pp_x0y0_pEM_EM = plotdf_point(proj_map_tem, "x0_CMU_EM", "y0_CMU_EM", x_em, y_em, 
                           "pmin_dist_SEM", "pmin_dist_SEM", 0, pointSize = 1)

#-------------------------------------------------------------------------------
#Title & theme
#-------------------------------------------------------------------------------
pp_x0y0_pEM_EM = pp_x0y0_pEM_EM + scg_pem
pp_x0y0_pEM_EM = pp_x0y0_pEM_EM + custom_theme


#-------------------------------------------------------------------------------
# Add objects
#-------------------------------------------------------------------------------
#Add EMLi
pp_x0y0_pEM_EM = pp_x0y0_pEM_EM + geom_point(data = dfemli, aes(x= x_SYS, y = y_SYS), size = size_lib) 

#Add Moon
#pp_x0y0_pEM_EM = pp_x0y0_pEM_EM + geom_point(data = dfmoon_eml, aes(x= x_SYS, y = y_SYS), size = size_prim)

#-------------------------------------------------------------------------------
#Ratio
#-------------------------------------------------------------------------------
#pp_x0y0_pEM_EM = pp_x0y0_pEM_EM + coord_fixed(ratio=1)

#-------------------------------------------------------------------------------
# Labels
#-------------------------------------------------------------------------------
pp_x0y0_pEM_EM = pp_x0y0_pEM_EM + labs(x = X_E, y = Y_E)
pp_x0y0_pEM_EM

#-------------------------------------------------------------------------------
# Fonts
#-------------------------------------------------------------------------------
pp_x0y0_pEM_EM = set_font_cm_leg(pp_x0y0_pEM_EM)

pp_x0y0_pEM_EM


#-------------------------------------------------------------------------------
#Adding some specific points
#-------------------------------------------------------------------------------
some_points = ddply(proj_cont_fam, .(family), summarize, seed = seed[1], 
                    x0_vec = x0_CMU_NCEM[seed[1]], 
                    y0_vec = y0_CMU_NCEM[seed[1]])

# Change for fifth family...
some_points$x0_vec[5] = -0.112560549310534
some_points$y0_vec[5] = -0.550123064539671

# Change of coordinates
some_points$x0_vec_EM = -CST_GAMMA_LIB_EM*(some_points$x0_vec - CST_C1_LIB_EM)
some_points$y0_vec_EM = -CST_GAMMA_LIB_EM*(some_points$y0_vec)

# Create new plot and add points
pp_x0y0_pEM_EM_points = pp_x0y0_pEM_EM + scg_pem_guide_false 

pp_x0y0_pEM_EM_points = pp_x0y0_pEM_EM_points + geom_point(data = some_points, aes(x0_vec_EM, y0_vec_EM), color = "white", size = 6)
# In black
pp_x0y0_pEM_EM_points = pp_x0y0_pEM_EM_points + geom_point(data = some_points, aes(x0_vec_EM, y0_vec_EM), color = "black", size = 4)
# or specific colors
for(i in seq(1, NFAM, 1))
{
  pp_x0y0_pEM_EM_points = pp_x0y0_pEM_EM_points + geom_point(data = some_points[i,], aes(x0_vec_EM, y0_vec_EM), color = values[i], size = 4)
}

# Margins (x-axis is cut off)
pp_x0y0_pEM_EM_points = pp_x0y0_pEM_EM_points + scale_x_continuous(limits = c(NaN, -1.117))

#-------------------------------------------------------------------------------
#Save
#-------------------------------------------------------------------------------
filename = paste0(filepre, "_x0_y0_pEM_EM")
ggsave(pp_x0y0_pEM_EM, width = xSize, height = ySize,  file = paste0(filename, ".png"))


filename = paste0(filepre, "_x0_y0_pEM_EM_points")
ggsave(pp_x0y0_pEM_EM_points, width = xSize, height = ySize,  file = paste0(filename, ".png")) 


#stop()

#===============================================================================
# Plot : points (pmin_dist_SEM) in the x0_CMU_NCEM/y0_CMU_NCEM space
# with continutation families
#===============================================================================
pp_x0y0_pEM_EM_fam = pp_x0y0_pEM_EM

for(IND in seq(1, NFAM))
{
  FAM = FAMVEC[IND]
  #---------------------------------------------------------------------------
  # One family
  #---------------------------------------------------------------------------
  condition = proj_cont_fam$family == FAM
  one_family  = proj_cont_fam[which(condition),]
  
  #---------------------------------------------------------------------------
  # Plots
  #---------------------------------------------------------------------------
  pp_x0y0_pEM_EM_fam = pp_x0y0_pEM_EM_fam + geom_point(data = one_family, aes(x0_CMU_EM, y0_CMU_EM), color = "white", size = 2.2)
  pp_x0y0_pEM_EM_fam = pp_x0y0_pEM_EM_fam + geom_point(data = one_family, aes(x0_CMU_EM, y0_CMU_EM), color = values[IND], size = 1.2)
}

#-------------------------------------------------------------------------------
#Adding some specific points (for ISSFD)
#-------------------------------------------------------------------------------
pp_x0y0_pEM_EM_fam = pp_x0y0_pEM_EM_fam + geom_point(data = some_points, aes(x0_vec_EM, y0_vec_EM), color = "white", size = 6)
# In black
pp_x0y0_pEM_EM_fam = pp_x0y0_pEM_EM_fam + geom_point(data = some_points, aes(x0_vec_EM, y0_vec_EM), color = "black", size = 4)
# or specific colors
for(i in seq(1, NFAM, 1))
{
  pp_x0y0_pEM_EM_fam = pp_x0y0_pEM_EM_fam + geom_point(data = some_points[i,], aes(x0_vec_EM, y0_vec_EM), color = values[i], size = 4)
}

#-------------------------------------------------------------------------------
#Aes
#-------------------------------------------------------------------------------
pp_x0y0_pEM_EM_fam = pp_x0y0_pEM_EM_fam + scg_pem_guide_false

#-------------------------------------------------------------------------------
#Save
#-------------------------------------------------------------------------------
filename = paste0(filepre, "_x0_y0_pEM_EM_cont_fam")
ggsave(pp_x0y0_pEM_EM_fam, width = xSize, height = ySize,  file = paste0(filename, ".png")) #Save png

#stop()


#===============================================================================
# Plot : points (pmin_dist_SEM) in the x0_CMU_NCEM/y0_CMU_NCEM space
#===============================================================================
#-------------------------------------------------------------------------------
#Plot
#-------------------------------------------------------------------------------
pp_x0y0_pEM = plotdf_point(proj_map_tem, "x0_CMU_NCEM", "y0_CMU_NCEM", x_em, y_em, 
                           "pmin_dist_SEM", "pmin_dist_SEM", 0, pointSize = 1)

#-------------------------------------------------------------------------------
#Title & theme
#-------------------------------------------------------------------------------
pp_x0y0_pEM = pp_x0y0_pEM + scg_pem
pp_x0y0_pEM = pp_x0y0_pEM + custom_theme


#-------------------------------------------------------------------------------
# Add objects
#-------------------------------------------------------------------------------
#Add EMLi
pp_x0y0_pEM = pp_x0y0_pEM + geom_point(data = dfemli, aes(x= x_NC, y = y_NC), size = size_lib) 

#Add Moon
#pp_x0y0_pEM = pp_x0y0_pEM + geom_point(data = dfmoon_eml, aes(x= x_NC, y = y_NC), size = size_prim)

#-------------------------------------------------------------------------------
#Ratio
#-------------------------------------------------------------------------------
#pp_x0y0_pEM = pp_x0y0_pEM + coord_fixed(ratio=1)

#-------------------------------------------------------------------------------
# Labels
#-------------------------------------------------------------------------------
pp_x0y0_pEM = pp_x0y0_pEM + labs(x = x_em, y = y_em)

#-------------------------------------------------------------------------------
# Fonts
#-------------------------------------------------------------------------------
pp_x0y0_pEM = set_font_cm_ex(pp_x0y0_pEM)


#-------------------------------------------------------------------------------
#Adding some specific points
#-------------------------------------------------------------------------------
some_points = ddply(proj_cont_fam, .(family), summarize, seed = seed[1], 
                    x0_vec = x0_CMU_NCEM[seed[1]], 
                    y0_vec = y0_CMU_NCEM[seed[1]])

# Change for fifth family...
some_points$x0_vec[5] = -0.112560549310534
some_points$y0_vec[5] = -0.550123064539671

# x0_vec = c(-0.0503021637013195,  0.0570779010089832, -0.0598335194863945, -0.112560549310534, -0.0663892631216)
# y0_vec = c(0.173076575031959, -0.0204216068823488, -0.246786030643921, -0.550123064539671, -0.242757702271792)
#some_points = data.frame(x0_vec = x0_vec, y0_vec = y0_vec);

# Create new plot and add points
pp_x0y0_pEM_points = pp_x0y0_pEM + scg_pem_guide_false 

pp_x0y0_pEM_points = pp_x0y0_pEM_points + geom_point(data = some_points, aes(x0_vec, y0_vec), color = "white", size = 6)
# In black
pp_x0y0_pEM_points = pp_x0y0_pEM_points + geom_point(data = some_points, aes(x0_vec, y0_vec), color = "black", size = 4)
# or specific colors
for(i in seq(1, NFAM, 1))
{
  pp_x0y0_pEM_points = pp_x0y0_pEM_points + geom_point(data = some_points[i,], aes(x0_vec, y0_vec), color = values[i], size = 4)
}


#-------------------------------------------------------------------------------
#Save
#-------------------------------------------------------------------------------
filename = paste0(filepre, "_x0_y0_pEM")
ggsave(pp_x0y0_pEM, width = xSize, height = ySize,  file = paste0(filename, ".png")) 

filename = paste0(filepre, "_x0_y0_pEM_points")
ggsave(pp_x0y0_pEM_points, width = xSize, height = ySize,  file = paste0(filename, ".png")) 

#===============================================================================
# Plot : points (pmin_dist_SEM) in the x0_CMU_NCEM/y0_CMU_NCEM space
# with continutation families
#===============================================================================
pp_x0y0_pEM_fam = pp_x0y0_pEM

for(IND in seq(1, NFAM))
{
  FAM = FAMVEC[IND]
  #---------------------------------------------------------------------------
  # One family
  #---------------------------------------------------------------------------
  condition = proj_cont_fam$family == FAM
  one_family  = proj_cont_fam[which(condition),]
  
  #---------------------------------------------------------------------------
  # Plots
  #---------------------------------------------------------------------------
  pp_x0y0_pEM_fam = pp_x0y0_pEM_fam + geom_point(data = one_family, aes(x0_CMU_NCEM, y0_CMU_NCEM), color = "white", size = 2.2)
  pp_x0y0_pEM_fam = pp_x0y0_pEM_fam + geom_point(data = one_family, aes(x0_CMU_NCEM, y0_CMU_NCEM), color = values[IND], size = 1.2)
}

#-------------------------------------------------------------------------------
#Adding some specific points (for ISSFD)
#-------------------------------------------------------------------------------
pp_x0y0_pEM_fam = pp_x0y0_pEM_fam + geom_point(data = some_points, aes(x0_vec, y0_vec), color = "white", size = 6)
# In black
pp_x0y0_pEM_fam = pp_x0y0_pEM_fam + geom_point(data = some_points, aes(x0_vec, y0_vec), color = "black", size = 4)
# or specific colors
for(i in seq(1, NFAM, 1))
{
  pp_x0y0_pEM_fam = pp_x0y0_pEM_fam + geom_point(data = some_points[i,], aes(x0_vec, y0_vec), color = values[i], size = 4)
}

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

#stop()


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
ppEM

#-------------------------------------------------------------------------------
# Save
#-------------------------------------------------------------------------------
#Filename
filename = paste0(filepre, "_s1_s3_pEM")
#Save png
ggsave(ppEM, width = xSize, height = ySize,  file = paste0(filename, ".png")) 

stop()
