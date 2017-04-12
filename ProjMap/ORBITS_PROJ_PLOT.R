################################################################################
#
# Projection from single orbits. Requires MAIN.R, ORBITS_PROJ_POSTPROCESS.R, 
# and ORBITS_PROJ_TRAJ_LOAD.R (to get refined trajectories)
#
# BLB 2017
#
################################################################################

#===== User inputs =============================================================

# Cuts: segment of time ratio that will be used to segregate the results on the plots.
cutv = seq(0, 1, 0.05)
iscut = TRUE

#===== Routine =================================================================
# Conditional cutting: is iscut == true, we actually cut x. Otherwise,
# we just give factor(x) back
# BLB 2017
conditional_cut <- function(x, cuts, iscut)
{
  if(iscut)
  {
    output = cut(x, cuts)
  }else{
    output = factor(x)
  }
  
  return(output)
}


#===== Plot: epM in x0/y0 ======================================================

# 1. All orbits for which a precise solutions has been found 
ppt_x0_y0 = ggplot() +  geom_path(data = proj_map_prec_all, aes(x0_CMU_NCEM, y0_CMU_NCEM, group = label), color = "black", size = 0.2)

# 2. Theme
ppt_x0_y0 = ppt_x0_y0 + custom_theme
ppt_x0_y0 = ppt_x0_y0 + coord_fixed(ratio=1)

# 3. Plot: all points in white (quite slow)
#ppt_x0_y0 = ppt_x0_y0 + geom_point(data = proj_map_orbit, aes(x0_CMU_NCEM, y0_CMU_NCEM, group = label), color = "white", size = 4)

# 4. All departure positions that are precise enough
ppt_x0_y0 = ppt_x0_y0 + geom_point(data = proj_map_prec, aes(x0_CMU_NCEM, y0_CMU_NCEM),  color= 'black', size = 4.5)
ppt_x0_y0 = ppt_x0_y0 + geom_point(data = proj_map_prec, aes(x0_CMU_NCEM, y0_CMU_NCEM),  color= 'white', size = 4)
#ppt_x0_y0 = ppt_x0_y0 + geom_point(data = proj_map_prec, aes(x0_CMU_NCEM, y0_CMU_NCEM,  color= r0_CMU_EMT), size = 2)


ppt_x0_y0 = ppt_x0_y0 + geom_point(data = proj_map_prec, aes(x0_CMU_NCEM, y0_CMU_NCEM,  color = cut(r0_CMU_EMT, cutv)), size = 2)
ppt_x0_y0 = ppt_x0_y0 + scale_color_brewer("%T", type="seq", palette = "Set1") #we need a more sequential brewer
                                   
# 5. All starting points for which a precise solutions has been found 
ppt_x0_y0 = ppt_x0_y0 + geom_point(data = proj_map_prec_first, aes(x0_CMU_NCEM, y0_CMU_NCEM),  color= 'black', size = 2.5)

# 6. Display
ppt_x0_y0

#===== Adding continuation as points ===========================================
# Adding plots
ppt_x0_y0_cont_points = ppt_x0_y0  + geom_point(data = proj_cont, aes(x0_CMU_NCEM, y0_CMU_NCEM), size = 2)

#Display
ppt_x0_y0_cont_points


#===== Adding continuation =====================================================
# Adding plots
ppt_x0_y0_cont = ppt_x0_y0 + geom_path(data = traj_cont, aes(x = x_CMS_NCEM, y = y_CMS_NCEM,  group = label, color = conditional_cut(r0_CMU_EMT, cutv, iscut)), size = 0.4)

#Zoom
ppt_x0_y0_cont = ppt_x0_y0_cont + scale_x_continuous(limits = c(-0.5, 0.5))
ppt_x0_y0_cont = ppt_x0_y0_cont + scale_y_continuous(limits = c(-0.5, 0.5))

#Display
ppt_x0_y0_cont

#===== Plot: trajectories in NCSEM ========== ==================================
# 1. Adding plots
ppt_x0SEM_y0SEM = ggplot() + geom_path(data = traj_cont, 
                                           aes(x = x_CMS_NCSEM, y = y_CMS_NCSEM, 
                                               group = interaction(label, r0_CMU_EMT), color = conditional_cut(r0_CMU_EMT, cutv, iscut)),
                                           size = 0.4)

# 2. Theme and titles
ppt_x0SEM_y0SEM = ppt_x0SEM_y0SEM + custom_theme
ppt_x0SEM_y0SEM = ppt_x0SEM_y0SEM + coord_fixed(ratio=1)
ppt_x0SEM_y0SEM = ppt_x0SEM_y0SEM + scale_color_discrete(name  = "%T")

# 3. Add SEMLi
ppt_x0SEM_y0SEM = ppt_x0SEM_y0SEM + geom_point(data = dfsemli, aes(x= x_NC, y = y_NC), size = 4) 
ppt_x0SEM_y0SEM = ppt_x0SEM_y0SEM + annotate("text", x = 0, y = -0.08,  label = "SEML[2]", size = 5, parse = TRUE)

# 4. Add Earth
ppt_x0SEM_y0SEM = ppt_x0SEM_y0SEM + geom_point(data = dfearth_seml, aes(x= x_NC, y = y_NC), size = 4) 
ppt_x0SEM_y0SEM = ppt_x0SEM_y0SEM + annotate("text", x = -1, y = -0.08, label = "Earth", size = 5)

# 5. Labels
ppt_x0SEM_y0SEM = ppt_x0SEM_y0SEM + labs(x = "x", y = "y")

# 6. Display
ppt_x0SEM_y0SEM


#===== Plot: epM in s1/s3 ======================================================

# 1. All orbits for which a precise solutions has been found 
ppt_s1EM_s3EM_eP = ggplot() + geom_path(data = proj_map_prec_all, aes(s1_CMU_EM, s3_CMU_EM, group = label),  color= 'black', size = 0.2)

# 2. Theme and titles
ppt_s1EM_s3EM_eP = ppt_s1EM_s3EM_eP + custom_theme
ppt_x0_y0 = ppt_x0_y0 + coord_fixed(ratio=1)

# 3. All points in white (quite slow)
#ppt_s1EM_s3EM_eP = ppt_s1EM_s3EM_eP + geom_point(data = proj_map_orbit, aes(s1_CMU_EM, s3_CMU_EM, group = label), color = "white", size = 4)

# 4. All departure positions that are precise enough
ppt_s1EM_s3EM_eP = ppt_s1EM_s3EM_eP + geom_point(data = proj_map_prec, aes(s1_CMU_EM, s3_CMU_EM),  color= 'black', size = 4.5)
ppt_s1EM_s3EM_eP = ppt_s1EM_s3EM_eP + geom_point(data = proj_map_prec, aes(s1_CMU_EM, s3_CMU_EM),  color= 'white', size = 4)
ppt_s1EM_s3EM_eP = ppt_s1EM_s3EM_eP + geom_point(data = proj_map_prec, aes(s1_CMU_EM, s3_CMU_EM, color = conditional_cut(r0_CMU_EMT, cutv, iscut)), size = 2)
ppt_s1EM_s3EM_eP = ppt_s1EM_s3EM_eP + scale_color_brewer("%T", type="seq", palette = "Set1") 

# 5. All starting points for which a precise solutions has been found 
#ppt_s1EM_s3EM_eP = ppt_s1EM_s3EM_eP + geom_point(data = proj_map_prec_first, aes(s1_CMU_EM, s3_CMU_EM, shape = factor(t0_CMU_EM_seed_T)), size = 2)
#ppt_s1EM_s3EM_eP = ppt_s1EM_s3EM_eP + scale_shape_manual(name  = "tseed %T", values=1:nlevels(proj_map_prec_first$t0_CMU_EM_seed_T))

# 6. Display
ppt_s1EM_s3EM_eP

#===== Adding continuation as points ===========================================


# Adding plots
ppt_s1EM_s3EM_eP_cont = ppt_s1EM_s3EM_eP + geom_point(data = proj_cont, aes(s1_CMU_EM, s3_CMU_EM), size = 2)

#Display
ppt_s1EM_s3EM_eP_cont


#===== Plot: epM in x0/y0 ======================================================
yannot = -4e-2*CST_DIST_PRIM_EM*CST_GAMMA_LIB_EM

# 1. All orbits for which a precise solutions has been found 
ppt_x0_y0_NCSI = ggplot() + geom_path(data = proj_map_prec_all, aes(x0_CMU_SI_NCEM, y0_CMU_SI_NCEM, group = label), color = "black", size = 0.2)
# 2. Theme
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + custom_theme
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + coord_fixed(ratio=1)

# 3. Plot: all points in white (quite slow)
#ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + geom_point(data = proj_map_orbit, aes(x0_CMU_SI_NCEM, y0_CMU_SI_NCEM, group = label), color = "white", size = 4)

# 4. All departure positions that are precise enough
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + geom_point(data = proj_map_prec, aes(x0_CMU_SI_NCEM, y0_CMU_SI_NCEM),  color= 'black', size = 4.5)
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + geom_point(data = proj_map_prec, aes(x0_CMU_SI_NCEM, y0_CMU_SI_NCEM),  color= 'white', size = 4)
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + geom_point(data = proj_map_prec, aes(x0_CMU_SI_NCEM, y0_CMU_SI_NCEM, color = cut(r0_CMU_EMT, cutv)), size = 2)
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + scale_color_discrete(name  = "%T")

# 5. All starting points for which a precise solutions has been found 
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + geom_point(data = proj_map_prec_first, aes(x0_CMU_SI_NCEM, y0_CMU_SI_NCEM),  color= 'black', size = 2.5)

# 6. Add EMLi
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + geom_point(data = dfemli, aes(x= x_NCPH, y = y_NCPH), size = 4) 
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + annotate("text", x = dfemli$x_NCPH, y = yannot,  label = "EML[2]", size = 5, parse = TRUE)

# 7. Add Moon
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + geom_point(data = dfmoon_eml, aes(x= x_NCPH, y = y_NCPH), size = 4) 
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + annotate("text", x = dfmoon_eml$x_NCPH, y = yannot, label = "Moon", size = 5)

# 8. Labels
ppt_x0_y0_NCSI = ppt_x0_y0_NCSI + labs(x = "x (km)", y = "y (km)")

# 9. Display
ppt_x0_y0_NCSI


#===== Adding continuation =====================================================
# Adding plots
ppt_x0_y0_NCSI_cont = ppt_x0_y0_NCSI + geom_path(data = traj_cont, aes(x = x_CMS_SI_NCEM, y = y_CMS_SI_NCEM,  group = label, color = conditional_cut(r0_CMU_EMT, cutv, iscut)), size = 0.4)

#Zoom
ppt_x0_y0_NCSI_cont = ppt_x0_y0_NCSI_cont + scale_x_continuous(limits = c(-10e4, 5e4))
ppt_x0_y0_NCSI_cont = ppt_x0_y0_NCSI_cont + scale_y_continuous(limits = c(-5e4, 5e4))

#Display
ppt_x0_y0_NCSI_cont

#===== Get rid of specific points ==============================================
# We get rid of some points at the end of each trajectories, to have a cleaner 
# end. The result is in traj_cont_clean.
traj_cont_seq <- ddply(traj_cont, .(label, r0_CMU_EMT), mutate, sign = y_CMS_SI_NCSEM > -5e5)
traj_cont_seq <- ddply(traj_cont_seq, .(label, r0_CMU_EMT, sign), mutate, maxtsign = max(t_CMU_SEM))
traj_cont_seq <- ddply(traj_cont_seq, .(label, r0_CMU_EMT), mutate, smaxtsign = min(maxtsign))
traj_cont_seq <- ddply(traj_cont_seq, .(label, r0_CMU_EMT), mutate, bool = t_CMU_SEM < smaxtsign)
traj_cont_clean = traj_cont_seq[which(traj_cont_seq$bool),]
traj_cont_clean = traj_cont_clean[order(traj_cont_clean$t_CMU_SEM),]

#===== POINTS : psome_SEM + pp_x0SEM_y0SEM_eP ==================================
yannot = -8e-2*CST_DIST_PRIM_SEM*CST_GAMMA_LIB_SEM

# Adding plots
ppt_x0SEM_y0SEM_NCSI = ggplot() + geom_path(data = traj_cont_clean, 
                                       aes(x = x_CMS_SI_NCSEM, y = y_CMS_SI_NCSEM, 
                                           group = interaction(label, r0_CMU_EMT), color = conditional_cut(r0_CMU_EMT, cutv, iscut)),
                                       size = 0.4)

#Theme and titles
ppt_x0SEM_y0SEM_NCSI = ppt_x0SEM_y0SEM_NCSI + custom_theme
ppt_x0SEM_y0SEM_NCSI = ppt_x0SEM_y0SEM_NCSI + coord_fixed(ratio=1)
ppt_x0SEM_y0SEM_NCSI = ppt_x0SEM_y0SEM_NCSI + scale_color_discrete(name  = "%T")

#Add SEMLi
ppt_x0SEM_y0SEM_NCSI = ppt_x0SEM_y0SEM_NCSI + geom_point(data = dfsemli, aes(x= x_NCPH, y = y_NCPH), size = 4) 
ppt_x0SEM_y0SEM_NCSI = ppt_x0SEM_y0SEM_NCSI + annotate("text", x = dfsemli$x_NCPH, y = yannot,  label = "SEML[2]", size = 5, parse = TRUE)

#Add Earth
ppt_x0SEM_y0SEM_NCSI = ppt_x0SEM_y0SEM_NCSI + geom_point(data = dfearth_seml, aes(x= x_NCPH, y = y_NCPH), size = 4) 
ppt_x0SEM_y0SEM_NCSI = ppt_x0SEM_y0SEM_NCSI + annotate("text", x = dfearth_seml$x_NCPH, y = yannot, label = "Earth", size = 5)

#Labels
ppt_x0SEM_y0SEM_NCSI = ppt_x0SEM_y0SEM_NCSI + labs(x = "x (km)", y = "y (km)")

#Display
ppt_x0SEM_y0SEM_NCSI

# Save
if(ISSAVED)
{
  # Save in pdf
  filename = paste0(filepre, "_x0SEM_y0SEM_some")
  ggsave(ppt_x0SEM_y0SEM_NCSI, width = xSize, height = ySize,  bg = "transparent",  file = paste0(filename, ".pdf")) #Save in pdf
}

