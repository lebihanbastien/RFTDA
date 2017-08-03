################################################################################
#
# Projection from single orbits. Subroutines of the file ORBITS_PROJ_PLOT.R
#
# BLB 2017
#
################################################################################

#------------------------------------------------------------------------------#
# Interaction with strings
#------------------------------------------------------------------------------#
cs_interact <- function(var1, var2)
{
  return(
    paste0("interaction(", paste0(c(var1, var2), collapse =  ", "), ")")
  )
}

#------------------------------------------------------------------------------#
# Factor with strings
#------------------------------------------------------------------------------#
cs_factor <- function(var1)
{
  return(
    sprintf("factor(%s)", var1)
  )
}


#------------------------------------------------------------------------------#
# Conditional cutting: is is.cut == true, we actually cut x. Otherwise,
# we just give factor(x) back
#------------------------------------------------------------------------------#
conditional_cut <- function(x, cuts, is.cut)
{
  if(is.cut)
  {
    output = cut(x, cuts)
  }else{
    output = x
  }
  return(output)
}


#------------------------------------------------------------------------------#
# Routine for the plot of EMLi-SEMLi connections in NCEM coordinates
#------------------------------------------------------------------------------#
plot.orbit.con <- function(proj_map_prec,       #dataframe that contains all the departure point that meets the connection requirements
                           proj_map_prec_first, #dataframe that contains the initial point on the orbit that have at least one point in proj_map_prec
                           proj_map_prec_all,   #dataframe that contains all the orbits that have at least one point in proj_map_prec
                           col.x,               #the column to use for the x-axis (either "x0_CMU_NCEM", "y0_CMU_NCEM", or "z0_CMU_NCEM")
                           col.y,               #the column to use for the y-axis (either "x0_CMU_NCEM", "y0_CMU_NCEM", or "z0_CMU_NCEM")
                           xlabel,              #xlabel
                           ylabel,              #ylabel
                           scale.color.brewer,  #color brewer 
                           is.cut,              #is true, a cut is perform in the values of r0_CMU_EMT, for color plotting
                           re.cutv,             #a vector of cuts to perform in the value of r0_CMU_EMT
                           is.moon.plotted,     #is the Moon plotted?
                           is.orbits.plotted,   #are all the orbits plotted?
                           proj_map_orbit,      #dataframe that contains all the orbits
                           is.coord.one         #for ratio=1 in the plot
)
{
  # Changing temporarily the names of the desired columns (x & y)
  #-----------------------------------------------------------------------------
  i1 = which(colnames(proj_map_prec) == col.x)
  i2 = which(colnames(proj_map_prec) == col.y)
  colnames(proj_map_prec)[i1] = "temp1"
  colnames(proj_map_prec)[i2] = "temp2"
  
  i1 = which(colnames(proj_map_prec_first) == col.x)
  i2 = which(colnames(proj_map_prec_first) == col.y)
  colnames(proj_map_prec_first)[i1] = "temp1"
  colnames(proj_map_prec_first)[i2] = "temp2"
  
  i1 = which(colnames(proj_map_prec_all) == col.x)
  i2 = which(colnames(proj_map_prec_all) == col.y)
  colnames(proj_map_prec_all)[i1] = "temp1"
  colnames(proj_map_prec_all)[i2] = "temp2"
  
  # If the orbits are plotted, we start with that. Otherwise, we go straight to
  # the plot of the orbits that meet the connection requirements.
  #-----------------------------------------------------------------------------
  if(is.orbits.plotted)
  {
    i1 = which(colnames(proj_map_orbit) == col.x)
    i2 = which(colnames(proj_map_orbit) == col.y)
    colnames(proj_map_orbit)[i1] = "temp1"
    colnames(proj_map_orbit)[i2] = "temp2"
    
    # All points in white (quite slow)
    ppt = ggplot() +  geom_point(data = proj_map_orbit, aes(temp1, temp2, group = label.conn), color = "white", size = 4)
    # All orbits for which a precise solutions has been found 
    ppt = ppt + geom_path(data = proj_map_prec_all, aes(temp1, temp2, group = label.conn), color = "black", size = 0.2)
    
  }else{
    # All orbits for which a precise solutions has been found 
    ppt = ggplot() +  geom_path(data = proj_map_prec_all, aes(temp1, temp2, group = label.conn), color = "black", size = 0.2)
  }
  
  # All departure positions that are precise enough
  #-----------------------------------------------------------------------------
  ppt = geom_point_pretty(ppt, proj_map_prec, aes(temp1, temp2,  
                                                  group = interaction(label.conn, r0_CMU_EMT), 
                                                  color = conditional_cut(r0_CMU_EMT, re.cutv, is.cut)))
  
  # All starting points for which a precise solutions has been found 
  #-----------------------------------------------------------------------------
  #ppt = ppt + geom_point(data = proj_map_prec_first, aes(temp1, temp2),  color= 'black', size = 2.5)
  
  # Add EMLi
  #-----------------------------------------------------------------------------
  yannot = -0.05
  ppt = ppt + geom_point(data = dfemli, aes(x= x_NC, y = y_NC), size = 4) 
  ppt = ppt + annotate("text", x = dfemli$x_NC, y = yannot,  label = "EML[2]", size = 5, parse = TRUE)
  
  # Add Moon
  #-----------------------------------------------------------------------------
  if(is.moon.plotted)
  {
    ppt = ppt + geom_point(data = dfmoon_eml, aes(x= x_NC, y = y_NC), size = 4) 
    ppt = ppt + annotate("text", x = dfmoon_eml$x_NC, y = yannot, label = "Moon", size = 5)
  }
  
  # Theme
  #-----------------------------------------------------------------------------
  ppt = ppt + custom_theme
  ppt = ppt + labs(x = xlabel, y = ylabel)
  ppt = ppt + scale.color.brewer
  if(is.coord.one){ppt = ppt + coord_fixed(ratio=1)}
  
  # Return
  #-----------------------------------------------------------------------------
  return(ppt)
  
}


#------------------------------------------------------------------------------#
# Routine for the plot of EMLi-SEMLi connections in NCEM coordinates
# Difference to plot.orbit.con: the phase @the Pk section re_CMU_EMT 
# is used, rather than at the departure point (r0_CMU_EMT).
#------------------------------------------------------------------------------#
plot.orbit.pks <- function(proj_map_prec,       #dataframe that contains all the departure point that meets the connection requirements
                           proj_map_prec_first, #dataframe that contains the initial point on the orbit that have at least one point in proj_map_prec
                           proj_map_prec_all,   #dataframe that contains all the orbits that have at least one point in proj_map_prec
                           col.x,               #the column to use for the x-axis 
                           col.y,               #the column to use for the y-axis
                           col.phase,           #the column to use for the phase
                           xlabel,              #xlabel
                           ylabel,              #ylabel
                           is.cut= FALSE,       #is the phase cut?
                           cut.vector.name=NaN, #vector for the cutting to factorized phase (can be NaN)
                           scale.color,         #color scale 
                           is.orbits.plotted,   #are all the orbits plotted?
                           proj_map_orbit,      #dataframe that contains all the orbits
                           is.coord.one         #for ratio=1 in the plot
)
{
  # If the orbits are plotted, we start with that. Otherwise, we go straight to
  # the plot of the orbits that meet the connection requirements.
  #-----------------------------------------------------------------------------
  if(is.orbits.plotted)
  {
    # All points in white (quite slow)
    ppt = ggplot() +  geom_point(data = proj_map_orbit, aes_string(x = col.x, y = col.y, group= "label"), color = "white", size = 4)
    # All orbits for which a precise solutions has been found 
    ppt = ppt + geom_path(data = proj_map_prec_all, aes_string(x = col.x, y = col.y, group= "label"), color = "black", size = 0.2)
    
  }else{
    # All orbits for which a precise solutions has been found 
    ppt = ggplot() +  geom_path(data = proj_map_prec_all, aes_string(x = col.x, y = col.y, group= "label"), color = "black", size = 0.2)
  }
  
  # All departure positions that are precise enough
  #-----------------------------------------------------------------------------
  if(is.cut)
  {
    if(is.nan(cut.vector.name))
    {
      scolor = cs_factor(col.phase)
    }else{
      scolor = sprintf("conditional_cut(%s, %s, TRUE)", col.phase, cut.vector.name)
    }
    aes.pk = aes_string(x = col.x, y = col.y,  
                        #group = cs_interact("label.conn", col.phase), 
                        color = scolor)
    
  }else
  {
    aes.pk = aes_string(x = col.x, y = col.y,  
                        #group = cs_interact("label.conn", col.phase),
                        color = col.phase)
  }
  
  
  ppt = geom_point_pretty(ppt, proj_map_prec, aes.pk)
  
  # All starting points for which a precise solutions has been found 
  #-----------------------------------------------------------------------------
  #ppt = ppt + geom_point(data = proj_map_prec_first, aes(col.x, col.y),  color= 'black', size = 2.5)
  
  # Add EMLi
  #-----------------------------------------------------------------------------
  # yannot = -0.05
  # ppt = ppt + geom_point(data = dfemli, aes(x= x_NC, y = y_NC), size = 4) 
  # ppt = ppt + annotate("text", x = dfemli$x_NC, y = yannot,  label = "EML[2]", size = 5, parse = TRUE)
  
  # Add Moon
  #-----------------------------------------------------------------------------
  # if(is.moon.plotted)
  # {
  #   ppt = ppt + geom_point(data = dfmoon_eml, aes(x= x_NC, y = y_NC), size = 4) 
  #   ppt = ppt + annotate("text", x = dfmoon_eml$x_NC, y = yannot, label = "Moon", size = 5)
  # }
  
  # Theme
  #-----------------------------------------------------------------------------
  ppt = ppt + custom_theme
  ppt = ppt + labs(x = xlabel, y = ylabel)
  ppt = ppt + scale.color
  
  
  if(is.coord.one){ppt = ppt + coord_fixed(ratio=1)}
  
  # Return
  #-----------------------------------------------------------------------------
  return(ppt)
  
}