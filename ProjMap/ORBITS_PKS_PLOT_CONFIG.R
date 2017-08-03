################################################################################
#
# Projection from single orbits. Requires MAIN.R, ORBITS_PROJ_POSTPROCESS.R, 
# and ORBITS_PROJ_TRAJ_LOAD.R (to get refined trajectories)
#
# Difference with ORBITS_PROJ_PLOT.R : the phase is taken on a given pk section 
# rather than at the departure point.
#
# BLB 2017
#
################################################################################

################################################################################
# Reminder:saving with right fonts
#
## Loading extrafonts:
#  library(extrafont)
## Looking @ the possible fonts which have "cm" in their names
#  h = fonttable()
#  h$FamilyName[which(grepl("cm", h$FamilyName, fixed = T))]
#
## Do NOT forget device=cairo_pdf in ggsave!
#
################################################################################

# Subroutines
source("ProjMap/util/SUBROUTINES.R")

#===== Comparison between proj & cont ==========================================

# length(proj_map_prec$label.conn)
# length(proj_cont$label)
# length(unique(traj_from_c$label))
# length(unique(traj_from_jpl$label))

# Note: we temporarily kill values smaller than 0.6 for plotting purposes !!!!!
#proj_map_prec = proj_map_prec[which(proj_map_prec$re_CMU_EMT_mod >= 0.6),]

#===== Selection of the phase ==================================================

re.string = "re_CMU_EMT_mod"
re.seed   = proj_map_prec$re_CMU_EMT_mod
re.limits = c(0.6, 1.05)

# Change these values if we wich to fix the limits to a certain value (-1 for default)
re.cut.min = re.limits[1] #-1
re.cut.max = re.limits[2] #-1

# Change type of scaling
is.color.manual = F
#Cutting
is.cut = TRUE

if(!exists("re.cutf"))
{
  re.cutf = 0.05 #default 0.05
}


# Size
size.traj  = 0.4
color.back = 'grey80' 

# Moon
primaryR =  1737.10
L        = Ldist("EM");
gamma_li = gamma(LIB_POINT_EM, "EM");

#===== User inputs =============================================================

if(!is.cut)
{
  re.string = sprintf("as.numeric(%s)", re.string)
}

# Plotting
is.moon.pretty    = FALSE
is.moon.plotted   = TRUE
is.orbits.plotted = FALSE
palette.brewer    = "PuBu" #Pubu, YlOrRd, YlOrBr

# Saving
is.saved = FALSE

#Plot
dpi = 200

#===== Cuts ====================================================================
# segment of time ratio that will be used to segregate the results on the plots.


if(re.cut.min == -1){re.cut.min = max(re.cutf*floor(min(re.seed)/re.cutf) - re.cutf, 0)}
if(re.cut.max == -1){re.cut.max = re.cutf*floor(max(re.seed)/re.cutf) + re.cutf}
re.cutv       = seq(re.cut.min, re.cut.max, re.cutf)


#===== Colors =================================================================
scale.color.title  = expression(gamma[italic(e)])

if(is.color.manual)
{
  scale.color.palette           = brewer.pal(length(re.cutv)-1, palette.brewer)
  scale.color.brewer            = scale_color_manual(scale.color.title, values = scale.color.palette)
  scale.color.brewer.noguide    = scale_color_manual(scale.color.title, values = scale.color.palette,  guide = FALSE)
  
  scale.color.distiller         = scale_color_distiller(scale.color.title, type="seq", 
                                                        limits = re.limits, palette = palette.brewer, direction = 1)  
  scale.color.distiller.noguide = scale_color_distiller(scale.color.title, type="seq", 
                                                        limits = re.limits, palette = palette.brewer, direction = 1,  guide = FALSE)
}else
  {
  scale.color.brewer            = scale_color_brewer(scale.color.title, type="seq",
                                                     palette = palette.brewer, direction = 1)  
  scale.color.brewer.noguide    = scale_color_brewer(scale.color.title, type="seq", 
                                                     palette = palette.brewer, direction = 1, guide = FALSE) 
  
  scale.color.distiller         = scale_color_distiller(scale.color.title, type="seq", palette = palette.brewer, direction = 1)  
  scale.color.distiller.noguide = scale_color_distiller(scale.color.title, type="seq", palette = palette.brewer, direction = 1,  guide = FALSE) 
}


if(is.cut)
{
  scale.color         = scale.color.brewer
  scale.color.noguide = scale.color.brewer.noguide  
}else
{
  scale.color         = scale.color.distiller
  scale.color.noguide = scale.color.distiller.noguide
}

# Color and aesthetics to match re.string
scolor = sprintf("conditional_cut(%s, %s, is.cut)", re.string, "re.cutv")
sgroup = cs_interact("label.conn", re.string)

# JPL
dark2 = rev(brewer.pal(3,"Dark2"))
dark2 = dark2[c(1,2)]
scale.color.jpl = scale_colour_manual(values=dark2, name = "Model", breaks = c(0, 13), labels = c("QBCP", "JPL"))
