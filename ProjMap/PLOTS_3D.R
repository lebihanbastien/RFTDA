######################################################################################
# R script used in EML2_TO_SEML.R for the plotting 
# of a projection map (connections between EML2 and SEML1,2)
######################################################################################

#===============================================================================
# Plot : tiles (pmin_dist_SEM) in the s1_CMU_EM/s3_CMU_EM space
#===============================================================================
x = "s1_CMU_EM"
y = "s3_CMU_EM"
ppt_sEM_eP = plotdf_tile_n(proj_map_tem, x, y, x, y, "pmin_dist_SEM", "Projection \ndistance", projection_lim_mid, TRUE)
ppt_sEM_eP

#===============================================================================
# Filename
#===============================================================================
filename1 <- function(t0, s2, s4)
{
  str = paste0(getwd(), "/ProjMap/3D/", "pp_tiles_", 
               "from_EM", LIB_POINT_EM, "_to_SEM", LIB_POINT_SEM, 
               "_s1EM_s3EM_eP_t0_0", toString(1000*t0/CST_SEM_PERIOD_EM), "_s2_", s2, "_s4_", s4, '.pdf')
  return(str)
}

#===============================================================================
# Different plots as a function of the initial time
#===============================================================================
ppl     = list();
ttm_l   = list();
index = 1;
for(i in 1:length(s2_CMU_EM_vec))
{
  for(j in 1:length(s4_CMU_EM_vec))
  {
    #---------------------------------------------------------------------------
    # Select the data
    #---------------------------------------------------------------------------
    #Condition: s2 = s2[i] and s4 = s4[j]
    bool = proj_map$s2_CMU_EM == s2_CMU_EM_vec[i] & proj_map$s4_CMU_EM == s4_CMU_EM_vec[j]
    
    #Select the data wrt bool
    ttm_l[[index]] = proj_map[which(bool),] 
    
    #---------------------------------------------------------------------------
    # Plot : tiles in the s1_CMU_EM/s3_CMU_EM space
    #---------------------------------------------------------------------------
    ppl[[index]]    = plotdf_tile_1(ttm_l[[index]], 
                                    "s1_CMU_EM", "s3_CMU_EM", 
                                    s1_exp, s3_exp, 
                                    "pmin_dist_SEM", "pmin_dist_SEM", 
                                    FALSE)
    
    #---------------------------------------------------------------------------
    # Plot : title
    #---------------------------------------------------------------------------
    title = paste0("t = ", toString(t0_CMU_EM_vec[1]/CST_SEM_PERIOD_EM), "T");
    
    s2_new = s2_CMU_EM_vec[i]#s4_CMU_EM_vec[j] - s2_CMU_EM_vec[i]
    s4_new = s4_CMU_EM_vec[j]#s4_CMU_EM_vec[j] + s2_CMU_EM_vec[i]
    
    title = paste0(title, ", s2 = ", toString(s2_new));
    title = paste0(title, ", s4 = ", toString(s4_new));
    ppl[[index]]    = ppl[[index]]  + ggtitle(title)
    
    #---------------------------------------------------------------------------
    # Save
    #---------------------------------------------------------------------------
    filename = filename1(t0_CMU_EM_vec[1], s2_new, s4_new)
    ggsave(ppl[[index]], width = xSize, height = ySize, file = filename)
    
    #---------------------------------------------------------------------------
    #Index
    #---------------------------------------------------------------------------
    index = index+1
  }
}

#-------------------------------------------------------------------------------
#Actual plot in multiplot format
#-------------------------------------------------------------------------------
#multiplot(plotlist = ppl, cols = 6)
stop()
