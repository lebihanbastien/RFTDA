################################################################################
# R script used in EML2_TO_SEML.R for the plotting 
# of a projection map (connections between from SEML1,2 to EML2)
#
# WARNING: MAIN.R must be loaded once before.
#
################################################################################

#-------------------------------------------------------------------------------
# Plot : tiles (pmin_dist_SEM) in the s1_CMU_SEM/s3_CMU_SEM space
#-------------------------------------------------------------------------------
pt_s1EM_s3EM_eP = plotdf_tile_1(proj_map_tem, "s1_CMU_SEM", "s3_CMU_SEM", 
                                s1_exp, s3_exp, "pmin_dist_SEM", "pmin_dist_SEM",
                                FALSE)

pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + scale_x_continuous(breaks = seq(-1,1,0.5))
pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + scale_y_continuous(breaks = seq(-1,1,0.5)) 

pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + ggtitle_t0
pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + scg_pem
pt_s1EM_s3EM_eP

# Get the collisions with the Moon
pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + geom_point(data = proj_map_tem[which(proj_map_tem$collision == 301),], aes(s1_CMU_SEM, s3_CMU_SEM), color = "gray", size = 5)
pt_s1EM_s3EM_eP

# Get the collisions with the Earth
pt_s1EM_s3EM_eP = pt_s1EM_s3EM_eP + geom_point(data = proj_map_tem[which(proj_map_tem$collision == 399),], aes(s1_CMU_SEM, s3_CMU_SEM), color = "red", size = 5)
pt_s1EM_s3EM_eP
