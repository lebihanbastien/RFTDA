################################################################################
# R script to handle a projection map (connections between EML2 and SEML1,2)
# continuation case.
# 
# In this script, each continuation files is loaded, and we look for a certain
# phase (phase_pk) at the poincaré section x = cst. 
#
# WARNING: MAIN.R must be loaded once before.
#
################################################################################

# Subroutines
source("ProjMap/util/SUBROUTINES.R")

#===============================================================================
# Which type of data?
#===============================================================================
# Prefix
FILE_PREFIX_CONT      = paste0(ftincppdafolder, "plot/QBCP/EM/L2/Serv/", "cont_atf_order_", ORDER, "_dest_", LIB_POINT_SEM)
FILE_PREFIX_CONT_TRAJ = paste0(ftincppdafolder, "plot/QBCP/EM/L2/Serv/", "cont_atf_traj_order_", ORDER, "_dest_", LIB_POINT_SEM)

#Select phase angle
phase_pk = 6.0#pi/2;
dH0      = 0.006
dH0_eps  = 1e-3

# Family
FAMILY = "_fam1"

#===============================================================================
# Function for spline interpolation, column wise
#===============================================================================
fun <- function(dfc, theta_v, theta_c)
{
  sx = spline(theta_v, dfc, xout = theta_c)
  return(sx$y)
}


#===============================================================================
# Loop on specified time
#===============================================================================
proj_map_multi_cont = data.frame()
pk_map_multi_cont   = data.frame()
pk_map_multi_r      = data.frame()
pk_map_multi_dH     = data.frame()

for(ratio_desired in seq(0.85, 0.995, 0.005))
{
  #===========================================================================
  # Desired time
  #=========================================================================== 
  time_desired  = ratio_desired*CST_SEM_PERIOD_EM;
  
  #=========================================================================== 
  # Suffix from time_desired
  #=========================================================================== 
  FILE_SUFFIX_CONT  = paste0("_t0_", ratio_desired);
  
  #=========================================================================== 
  # Get data from file
  #=========================================================================== 
  proj_cont = get_proj_cont(FILE_PREFIX_CONT, FILE_SUFFIX_CONT, FAMILY = FAMILY)
  
  if(nrow(proj_cont) > 0)
  {
    #=========================================================================== 
    # Post process: additional columns
    #===========================================================================
    proj_cont$r0_CMU_EM = proj_cont$t0_CMU_EM / CST_SEM_PERIOD_EM
    proj_cont$phase     = proj_cont$thetae_NCSEM %% (2*pi)
    proj_cont$phase_pi  = proj_cont$phase/(pi)
    proj_cont$dH0_EM    = proj_cont$H0_EM - proj_cont$H0_emli_EM
    
    #=========================================================================== 
    # Post process: select the point closest to a certain phase angle at Pk map
    #===========================================================================
    #phase_index =  which.min(abs(proj_cont$phase-phase_pk))
    #pk_map_cont = proj_cont[which(proj_cont$phase == proj_cont$phase[phase_index]),]
    phase_eps         =  0.5;
    phase_indices     = which(abs(proj_cont$phase - phase_pk) < phase_eps)
    pk_map_cont       = proj_cont[phase_indices,]
    pk_map_multi_cont = rbind_cc(pk_map_multi_cont, pk_map_cont) 
    
    #===========================================================================
    # Parameters for the interpolation
    #===========================================================================
    rnmin = 1;
    rnmax = as.numeric(length(row.names(proj_cont)));
    rnl = 5;
    
    #===========================================================================
    # Loop on the possible thetae_NCSEM, such that thetae_NCSEM %% (2*pi) close to phase_pk
    #===========================================================================
    for (ic in seq(-15, 15, 1))
    {
      #=========================================================================     
      # Current theta_c, such that theta_c %% (2*pi) == phase_pk
      #=========================================================================
      theta_c  = phase_pk + 2*pi*ic;
      
      #=========================================================================
      # If there exist a minimum close to the good phase, we go on
      #=========================================================================
      argmin    = which(abs(proj_cont$thetae_NCSEM - theta_c) == min(abs(proj_cont$thetae_NCSEM - theta_c)))
      proj_map_min = proj_cont[argmin,]
      
      if(abs(proj_map_min$phase[1]- phase_pk) < phase_eps)
      {
        #=======================================================================
        # Loop on all the solutions found
        #=======================================================================
        for(id in seq(1, length(rownames(proj_cont))-1))
        {
          #=====================================================================
          # If we find an interval that contains phase_pk, we interpolate around 
          # this value to find the exact thetae_NCSEM %% (2*pi) == phase_pk
          #=====================================================================
          condition = (proj_cont$thetae_NCSEM[id] - theta_c)*(proj_cont$thetae_NCSEM[id+1] - theta_c) < 0
          condition = condition & abs(proj_cont$phase[id]- phase_pk) < phase_eps
          if(condition)
          {
            #We create a temp df
            pk_map_c = proj_cont[c(id, id+1),] 
            
            #We take the values around the current position
            rn = as.numeric(rownames(pk_map_c));
            low  = max(rnmin, rn - rnl)
            high = min(rnmax, rn + rnl)
            rnv = seq(low, high, 1)
            
            #Interpolation
            pk_map_r = proj_cont[rnv,]
            pk_map_line = colwise(fun)(pk_map_r, pk_map_r$thetae_NCSEM, theta_c)
            
            #Add to the data pk_map_multi_r
            pk_map_multi_r = rbind_cc(pk_map_multi_r, pk_map_line)
          }
        }
      }
    }
    
    #===========================================================================
    # Looking for a specific energy
    #===========================================================================
    
    #===========================================================================
    # If there exist a minimum close to the good phase, we go on
    #===========================================================================
    argmin    = which(abs(proj_cont$dH0_EM - dH0) == min(abs(proj_cont$dH0_EM - dH0)))
    proj_map_min = proj_cont[argmin,]
    
    if(abs(proj_map_min$dH0_EM[1]- dH0) < dH0_eps)
    {
      #=========================================================================
      # Loop on all the solutions found
      #=========================================================================
      for(id in seq(1, length(rownames(proj_cont))-1))
      {
        #=======================================================================
        # If we find an interval that contains dH0, we interpolate around 
        # this value to find the exact dH0_EM  == dH0
        #=======================================================================
        condition = (proj_cont$dH0_EM[id] - dH0)*(proj_cont$dH0_EM[id+1] - dH0) < 0
        condition = condition & abs(proj_cont$dH0_EM[id]- dH0) < dH0_eps
        if(condition)
        {
          #We create a temp df
          pk_map_c = proj_cont[c(id, id+1),] 
          
          #We take the values around the current position
          rn = as.numeric(rownames(pk_map_c));
          low  = max(rnmin, rn - rnl)
          high = min(rnmax, rn + rnl)
          rnv = seq(low, high, 1)
          
          #Interpolation
          pk_map_r = proj_cont[rnv,]
          pk_map_line = colwise(fun)(pk_map_r, pk_map_r$dH0_EM, dH0)
          
          #Add to the data pk_map_multi_dH
          pk_map_multi_dH = rbind_cc(pk_map_multi_dH, pk_map_line)
        }
      }
    }
    
    
    #=========================================================================== 
    # Add it to the gigantic data
    #=========================================================================== 
    proj_map_multi_cont = rbind_cc(proj_map_multi_cont, proj_cont)
  }
}

#=============================================================================
# HEURISTIC POST-PROCESS: 
# 1. we get rid of the solutions that did not reach the Poincaré section
# 2. we get rid (for now) of the solution for which ye > 0, which should 
# correspond to solution for which the crossing has been computed at the second 
# flyby... Not cool!
#=============================================================================
# If te = 0, there is no crossing
proj_map_multi_cont = proj_map_multi_cont[which(proj_map_multi_cont$te_NCSEM != 0), ] 

# Only y < 0
#proj_map_multi_cont = proj_map_multi_cont[which(proj_map_multi_cont$ye_CMS_NCSEM < 0), ] 

#===============================================================================
# PLOTS, for saving
#===============================================================================

#----- In the x0/y0 plane in NCEM coordinates ----------------------------------

legend_title = expression(gamma[0])
pp_Pk_x0_y0 = plotdf_point(proj_map_multi_cont, "x0_CMU_NCEM", "y0_CMU_NCEM", 
                           x_em, y_em, "r0_CMU_EM", legend_title, 
                           isColorFac = "false")

# In black the point for which the phase at x = xe is equal to phase_pk
# Usually, xe = -0.7 or -0.6.
pp_Pk_x0_y0 = geom_point_pretty(pp_Pk_x0_y0, pk_map_multi_r, aes(x0_CMU_NCEM, y0_CMU_NCEM))
#pp_Pk_x0_y0 = pp_Pk_x0_y0 + geom_point(data = pk_map_multi_r,  aes(x0_CMU_NCEM, y0_CMU_NCEM), color = "black", size = 3)

# In red: the points for which dH(t) = dH0 
pp_Pk_x0_y0 = geom_point_pretty(pp_Pk_x0_y0, pk_map_multi_dH, aes(x0_CMU_NCEM, y0_CMU_NCEM), color = "red")
#pp_Pk_x0_y0 = pp_Pk_x0_y0 + geom_point(data = pk_map_multi_dH, aes(x0_CMU_NCEM, y0_CMU_NCEM), color = "red", size = 3)
pp_Pk_x0_y0

# Saving
filename = paste0(FILE_PREFIX_CONT_TRAJ, "_primary_with_annotations")
ggsave(pp_Pk_x0_y0, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf")) #Save in pdf

#----- Same with a given orbit of energy dH = dH0 ------------------------------

pp_Pk_x0_y0_orb = pp_Pk_x0_y0 + geom_path(data = orbits_eml2, aes(x_CM_NCEM,  y_CM_NCEM), color = muted("black"), size = 0.6)
filename = paste0(FILE_PREFIX_CONT_TRAJ, "_primary_with_orbit")
ggsave(pp_Pk_x0_y0_orb, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf")) #Save in pdf


#----- Same with a given strob orbit of energy dH = dH0 ------------------------

pp_Pk_x0_y0_strob = pp_Pk_x0_y0 + geom_point(data = orbits_eml2_strob, aes(x_CM_NCEM,  y_CM_NCEM), color = muted("green"), size = 0.1)
filename = paste0(FILE_PREFIX_CONT_TRAJ, "_primary_with_strob_orbit")
ggsave(pp_Pk_x0_y0_strob, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf")) #Save in pdf



stop()

#===============================================================================
# PLOTS, rough
#===============================================================================
pp_Pk_s1_s3 = plotdf_point(proj_map_multi_cont, "s1_CMU_EM", "s3_CMU_EM", "s1_CMU_EM", "s3_CMU_EM", "r0_CMU_EM", "r0", isColorFac = "false")
pp_Pk_s1_s3 = pp_Pk_s1_s3 + geom_point(data = pk_map_multi_r, aes(s1_CMU_EM, s3_CMU_EM), color = "black", size = 3)
pp_Pk_s1_s3 = pp_Pk_s1_s3 + geom_point(data = pk_map_multi_dH, aes(s1_CMU_EM, s3_CMU_EM), color = "red", size = 3)
pp_Pk_s1_s3

# With strob map
#pp_Pk_s1_s3 + geom_point(data = orbits_eml2_strob, aes(s1,  s3, type = factor(label)), color = muted("green"), size = 2)

pp_Pk_x0_y0 = plotdf_point(proj_map_multi_cont, "x0_CMU_NCEM", "y0_CMU_NCEM", "x0_CMU_NCEM", "y0_CMU_NCEM", "r0_CMU_EM", expression(gamma[0]), isColorFac = "false")
pp_Pk_x0_y0 = pp_Pk_x0_y0 + geom_point(data = pk_map_multi_r, aes(x0_CMU_NCEM, y0_CMU_NCEM), color = "black", size = 3)
pp_Pk_x0_y0 = pp_Pk_x0_y0 + geom_point(data = pk_map_multi_dH, aes(x0_CMU_NCEM, y0_CMU_NCEM), color = "red", size = 3)
pp_Pk_x0_y0

# With strob map
pp_Pk_x0_y0 + geom_point(data = orbits_eml2_strob, aes(x_CM_NCEM,  y_CM_NCEM, type = factor(label)), color = muted("green"), size = 2)
# With orbits
pp_Pk_x0_y0 + geom_path(data = orbits_eml2, aes(x_CM_NCEM,  y_CM_NCEM, linetype = factor(label)), color = muted("black"), size = 0.2)

pp_Pk_x0_t = plotdf_point(proj_map_multi_cont, "phase", "x0_CMU_NCEM", "phase", "x0_CMU_NCEM", "r0_CMU_EM", "r0", isColorFac = "false",  pointSize = 2)
#pp_Pk_x0_t = pp_Pk_x0_t + geom_point(data = pk_map_multi_cont, aes(phase, x0_CMU_NCEM), color = "grey", size = 3)
pp_Pk_x0_t = pp_Pk_x0_t + geom_point(data = pk_map_multi_r, aes(phase, x0_CMU_NCEM), color = "black", size = 3)
pp_Pk_x0_t


pp_Pk_ye_pye = plotdf_point(proj_map_multi_cont, "ye_CMS_NCSEM", "pye_CMS_NCSEM", "ye_CMS_NCSEM", "pye_CMS_NCSEM", "r0_CMU_EM", "r0", isColorFac = "false",  pointSize = 2)
#pp_Pk_ye_pye = pp_Pk_ye_pye + geom_point(data = pk_map_multi_cont, aes(ye_CMS_NCSEM, pye_CMS_NCSEM), color = "grey", size = 3)
pp_Pk_ye_pye = pp_Pk_ye_pye + geom_point(data = pk_map_multi_r,    aes(ye_CMS_NCSEM, pye_CMS_NCSEM), color = "black", size = 3)
pp_Pk_ye_pye

xx = "x0_CMU_NCEM"
yy = "y0_CMU_NCEM"
pp_pts_phase_x0y0 = plotdf_point(pk_map_multi_r, xx, yy, xx, yy, "r0_CMU_EM", "r0", isColorFac = "false", pointSize = 3)
pp_pts_phase_x0y0

xx = "s1_CMU_EM"
yy = "s3_CMU_EM"
pp_pts_phase_s1s3 = plotdf_point(pk_map_multi_r, xx, yy, xx, yy, "r0_CMU_EM", "r0", isColorFac = "false", pointSize = 3)
pp_pts_phase_s1s3

xx = "ye_CMS_NCSEM"
yy = "pye_CMS_NCSEM"
pp_pts_phase_yepye = plotdf_point(pk_map_multi_r, xx, yy, xx, yy, "r0_CMU_EM", "r0", isColorFac = "false", pointSize = 3)
pp_pts_phase_yepye


pp_Pk_dH0_EM_x0 = plotdf_point(proj_map_multi_cont, "x0_CMU_NCEM", "dH0_EM", "x0_CMU_NCEM", "dH0_EM", "r0_CMU_EM", "r0", isColorFac = "false",  pointSize = 2)
pp_Pk_dH0_EM_x0 = pp_Pk_dH0_EM_x0 + geom_point(data = pk_map_multi_dH,  aes(x0_CMU_NCEM, dH0_EM), color = "black", size = 3)
pp_Pk_dH0_EM_x0
