################################################################################
# R script used in EML2_TO_SEML.R for the postprocess 
# of a projection map (connections from EML2 to SEML1,2)
#
# WARNING: MAIN.R or PROJMAP.R must be loaded once before.

# This scripts basically uses the dataframe proj_map_source created in 
# PROJMAP.R and updates the following dataframes:
#
# - proj_map: solutions of a multi projection procedure, with the projection 
#   distance under a certain value (projection_lim_max)
# - proj_map_tem: subset of solutions only with a given initial time
# - proj_map_s1: subset of solutions only with a given initial s1
# - proj_map_s3: subset of solutions only with a given initial s3
#
################################################################################

#===============================================================================
# Define limit of projection
#===============================================================================
#hard
if(LIB_POINT_EM == "L2")
{
  projection_lim_max = 1e-2; #1e-1
  projection_lim_mid = 5e-4;
  projection_color_lim = c(0, 1e-2);
}else
{
  projection_lim_max = 1e-2; #1e-1
  projection_lim_mid = 5e-4;
  projection_color_lim = c(0, 1e-2);
}

# Projection in SI
projection_lim_max_SI = projection_lim_max*CST_DIST_PRIM_SEM
projection_lim_mid_SI = projection_lim_mid*CST_DIST_PRIM_SEM 
projection_lim_mid_SI



#===============================================================================
# Indexes/values for cuts in data
#===============================================================================
s1_index = 50;
s3_index = 50;
s1_value = 0;
s3_value = 0;

#===============================================================================
#===============================================================================
# Postprocess on proj_map_source (new variables, etc)
#===============================================================================
#===============================================================================

#===============================================================================
#Errors and norms
#===============================================================================
#Get the error in position in km
proj_map_source$pmin_dist_SI = proj_map_source$pmin_dist_SEM*CST_DIST_PRIM_SEM
#Norm of yv
proj_map_source$nf_CM_NCSEM = sqrt((-1/CST_GAMMA_LIB_SEM*proj_map_source$xf_CM_SEM + CST_C1_LIB_SEM)^2 + (1/CST_GAMMA_LIB_SEM*proj_map_source$yf_CM_SEM)^2)

#Get the distance wrt to the center of s1_CM_SEM/s3_CM_SEM
proj_map_source$ns_CM_SEM = sqrt(proj_map_source$s1_CM_SEM^2 + proj_map_source$s3_CM_SEM^2)
#Get the distance wrt to the center of s1EM/s3EM
proj_map_source$ns_CMU_EM = abs(proj_map_source$s1_CMU_EM)



#===============================================================================
#DV
#===============================================================================
#Get the dv_at_projection_SEM in position in km/s
proj_map_source$dv_at_projection_SI = proj_map_source$dv_at_projection_SEM*CST_DIST_PRIM_SEM*2*pi/Tcrtbp("SEM")

#===============================================================================
#Minimum in position
#===============================================================================
proj_map_source_min = proj_map_source[which(proj_map_source$pmin_dist_SEM == min(proj_map_source$pmin_dist_SEM)),] 
#Associated position gap in km
proj_map_source_min$pmin_dist_SI
#Associated dv_at_projection_SEM in m/s
1e3*proj_map_source_min$dv_at_projection_SI

#===============================================================================
#Radius
#===============================================================================
proj_map_source$rf_CM_SEM = sqrt(proj_map_source$xf_CM_SEM^2 + proj_map_source$yf_CM_SEM^2 + proj_map_source$zf_CM_SEM^2)
rf_CM_SEM_mid = mean(proj_map_source$rf_CM_SEM)

proj_map_source$sf_CM_SEM = sqrt(proj_map_source$s1_CM_SEM^2 + proj_map_source$s3_CM_SEM^2)
sf_CM_SEM_mid = mean(proj_map_source$sf_CM_SEM)

#===============================================================================
#Time
#===============================================================================
#Get the initial time as a modulo of T
proj_map_source$r0_CMU_EMT     = proj_map_source$t0_CMU_EM/CST_SEM_PERIOD_EM
proj_map_source$r0_CMU_EMT     = round(proj_map_source$r0_CMU_EMT %% 1, digits= 4)
proj_map_source$r0_CMU_EMT_mod = r0_modulo(proj_map_source$r0_CMU_EMT) #same between 0.5 and 1.5

#Get the initial time as a percentage of T
proj_map_source$t0_CMU_EMT = proj_map_source$t0_CMU_EM/CST_SEM_PERIOD_EM
#Get the final time as a percentage of T
proj_map_source$tf_man_SEMT = proj_map_source$tf_man_SEM/CST_SEM_PERIOD_SEM
#tof_EM (in T)
proj_map_source$tof_EM = (proj_map_source$tf_man_SEMT-proj_map_source$t0_CMU_EMT)
#tof_EM (days)
proj_map_source$tof_SI = (proj_map_source$tf_man_SEMT-proj_map_source$t0_CMU_EMT)*SEMperiod("EM")*Tcrtbp("EM")/(2*pi*3600*24)

#===============================================================================
#Energy
#===============================================================================
if("H0_EM" %in% colnames(proj_map_source))
{
  proj_map_source$dH0_EM   = proj_map_source$H0_EM - proj_map_source$H0_emli_EM
  proj_map_source$dHf_EM   = proj_map_source$Hf_EM - proj_map_source$Hf_semli_EM
  
  proj_map_source$dH0_SEM  = proj_map_source$H0_SEM - proj_map_source$H0_emli_SEM
  proj_map_source$dHf_SEM  = proj_map_source$Hf_SEM - proj_map_source$Hf_semli_SEM
}

#===============================================================================
# SEM to NCSEM
#===============================================================================
proj_map_source$xf_CM_NCSEM <- -proj_map_source$xf_CM_SEM/CST_GAMMA_LIB_SEM  + CST_C1_LIB_SEM
proj_map_source$yf_CM_NCSEM <- -proj_map_source$yf_CM_SEM/CST_GAMMA_LIB_SEM  
proj_map_source$zf_CM_NCSEM <- +proj_map_source$zf_CM_SEM/CST_GAMMA_LIB_SEM  

#===============================================================================
# SEM to SISEM
#===============================================================================
proj_map_source$xf_CM_SI_SEM <- proj_map_source$xf_CM_SEM * CST_DIST_PRIM_SEM
proj_map_source$yf_CM_SI_SEM <- proj_map_source$yf_CM_SEM * CST_DIST_PRIM_SEM
proj_map_source$zf_CM_SI_SEM <- proj_map_source$zf_CM_SEM * CST_DIST_PRIM_SEM

#===============================================================================
# NCSEM to SINCSEM
#===============================================================================
proj_map_source$xf_CM_SI_NCSEM <- proj_map_source$xf_CM_NCSEM * CST_DIST_PRIM_SEM * CST_GAMMA_LIB_SEM
proj_map_source$yf_CM_SI_NCSEM <- proj_map_source$yf_CM_NCSEM * CST_DIST_PRIM_SEM * CST_GAMMA_LIB_SEM 
proj_map_source$zf_CM_SI_NCSEM <- proj_map_source$zf_CM_NCSEM * CST_DIST_PRIM_SEM * CST_GAMMA_LIB_SEM

#===============================================================================
# NCEM to EM
#===============================================================================
proj_map_source$x0_CMU_EM <- -(proj_map_source$x0_CMU_NCEM - CST_C1_LIB_EM)*CST_GAMMA_LIB_EM  
proj_map_source$y0_CMU_EM <- - proj_map_source$y0_CMU_NCEM                 *CST_GAMMA_LIB_EM
proj_map_source$z0_CMU_EM <- + proj_map_source$z0_CMU_NCEM                 *CST_GAMMA_LIB_EM 

#===============================================================================
# EM to SIEM
#===============================================================================
proj_map_source$x0_CMU_SI_EM <- proj_map_source$x0_CMU_EM * CST_DIST_PRIM_EM
proj_map_source$y0_CMU_SI_EM <- proj_map_source$y0_CMU_EM * CST_DIST_PRIM_EM
proj_map_source$z0_CMU_SI_EM <- proj_map_source$z0_CMU_EM * CST_DIST_PRIM_EM

#===============================================================================
# NCEM to SINCEM
#===============================================================================
proj_map_source$x0_CMU_SI_NCEM <- proj_map_source$x0_CMU_NCEM * CST_DIST_PRIM_EM * CST_GAMMA_LIB_EM
proj_map_source$y0_CMU_SI_NCEM <- proj_map_source$y0_CMU_NCEM * CST_DIST_PRIM_EM * CST_GAMMA_LIB_EM 
proj_map_source$z0_CMU_SI_NCEM <- proj_map_source$z0_CMU_NCEM * CST_DIST_PRIM_EM * CST_GAMMA_LIB_EM


#===============================================================================
#===============================================================================
# Select a subgroup (only certain distance of projection, time, etc)
#===============================================================================
#===============================================================================

#===============================================================================
#Keep only values with a projection distance below projection_lim_max.
#===============================================================================
proj_map = proj_map_source[which(proj_map_source$pmin_dist_SEM <= projection_lim_max),] 


#===============================================================================
# Unique
#===============================================================================
s1_CMU_EM_vec = unique(proj_map$s1_CMU_EM)
s2_CMU_EM_vec = unique(proj_map$s2_CMU_EM)
s3_CMU_EM_vec = unique(proj_map$s3_CMU_EM)
s4_CMU_EM_vec = unique(proj_map$s4_CMU_EM)

t0_CMU_EM_vec = unique(proj_map$t0_CMU_EM)


#===============================================================================
# Select a given value of time
#===============================================================================
# If time_desired > 0, we use it to define time_index
if(exists('time_desired') && time_desired >= 0)
{
  time_index =  which.min(abs(t0_CMU_EM_vec-time_desired))
}else
{
  time_index = -1
}


if(time_index > 0)
{
  proj_map_tem = proj_map[which(proj_map$t0_CMU_EM == t0_CMU_EM_vec[time_index]),] 
}else{
  proj_map_tem = proj_map
}

# Moreover, if the type is 3D, we select ONE value of s2 and ONE value of s4
if(TYPE == "_3d")
{
  condition         = (proj_map_tem$s1_CMU_EM == s1_value) & (proj_map_tem$s3_CMU_EM == s3_value)
  proj_map_tem_s2s4 = proj_map_tem[which(condition),] 
  
  
  condition  = (proj_map_tem$s2_CMU_EM == s2_value) & (proj_map_tem$s4_CMU_EM == s4_value)
  proj_map_tem = proj_map_tem[which(condition),] 
}else
{
  proj_map_tem_s2s4 = data.frame()
}

#===============================================================================
# Just the collisions within tem
#===============================================================================
proj_map_tem_coll = proj_map_tem[which(proj_map_tem$dv_at_projection_SEM %in% c(301, 399)),]


#===============================================================================
# Select a given value of s1
#===============================================================================
if(s1_index > 0)
{
  #proj_map_s1 = proj_map[which(proj_map$s1_CMU_EM == s1_CMU_EM_vec[s1_index]),] 
  proj_map_s1 = proj_map[which(proj_map$s1_CMU_EM == 0.0),] 
}else{
  proj_map_s1 = proj_map
}

#===============================================================================
# Select a given value of s3
#===============================================================================
if(s3_index > 0)
{
  #proj_map_s3 = proj_map[which(proj_map$s3_CMU_EM == s3_CMU_EM_vec[s3_index]),] 
  proj_map_s3 = proj_map[which(proj_map$s3_CMU_EM == 0.0),] 
}else{
  proj_map_s3 = proj_map
}

#===============================================================================
# Select a given value of s3
#===============================================================================
if(s3_index > 0)
{
  #proj_map_s3 = proj_map[which(proj_map$s3_CMU_EM == s3_CMU_EM_vec[s3_index]),] 
  proj_map_s3 = proj_map[which(proj_map$s3_CMU_EM == 0.0),] 
}else{
  proj_map_s3 = proj_map
}



