################################################################################
#
# Projection from single orbits. Requires MAIN.R, ORBITS_PROJ_POSTPROCESS.R, 
# and ORBITS_PROJ_TRAJ_LOAD.R (to get refined trajectories)
#
# Difference with ORBITS_PROJ_PLOT.R : the phase is taken on a given pk section 
# rather than at the departure point.
#
# This file focuses on the JPL transitioning results and on average estimates
# computed from these results
#
# BLB 2017
#
################################################################################

#-------------------------------------------------------------------------------
# Aes
#-------------------------------------------------------------------------------
scale.color.title.local = expression("Initial seed " * s[1])
scolor.local = "factor(label)"
scale.color.jpl.local           = scale_color_brewer(scale.color.title.local, type="seq",palette = "Dark2", direction = 1)
is.saved = T

#-------------------------------------------------------------------------------
# Label
#-------------------------------------------------------------------------------
# For R
dHf.SE              = expression(delta*italic(H)[italic(f)])
mean.n.NCEM         = expression(bar(italic(a))[italic(e)])
mean.n.NCSE         = expression(bar(italic(a))[italic(s)])
mean.dn.NCEM        = expression(delta*bar(italic(a))[italic(e)])
mean.dn.NCSE        = expression(delta*bar(italic(a))[italic(s)])
mean.dn.NCEM.over.n =  expression(delta*bar(italic(a))[italic(e)]/bar(italic(a))[italic(e)])

mean.n.EM         = expression(bar(italic(A))[italic(E)])
mean.n.SE         = expression(bar(italic(A))[italic(S)])
mean.dn.EM        = expression(delta*bar(italic(A))[italic(E)])
mean.dn.SE        = expression(delta*bar(italic(A))[italic(S)])

mean.gamma       = expression(gamma[italic(e)])

#-------------------------------------------------------------------------------
# Load from file  & rbind. Used to save the file "traj_from_jpl_mean_phd.Rda"
#-------------------------------------------------------------------------------
# load(file = "Rda/traj_from_jpl_mean_10.Rda")
# load(file = "Rda/traj_from_jpl_mean_20.Rda")
# load(file = "Rda/traj_from_jpl_mean_30.Rda")
# load(file = "Rda/traj_from_jpl_mean_40.Rda")
# 
# traj_from_jpl_mean_10$seed = 10
# traj_from_jpl_mean_20$seed = 20
# traj_from_jpl_mean_30$seed = 30
# traj_from_jpl_mean_40$seed = 40
# 
# traj_from_jpl_mean = rbind(traj_from_jpl_mean_10, traj_from_jpl_mean_20)
# traj_from_jpl_mean = rbind(traj_from_jpl_mean, traj_from_jpl_mean_30)
# traj_from_jpl_mean = rbind(traj_from_jpl_mean, traj_from_jpl_mean_40)
# 
# save(traj_from_jpl_mean, file = "Rda/traj_from_jpl_mean_phd.Rda")


#-------------------------------------------------------------------------------
# Load from file
#-------------------------------------------------------------------------------
load(file = "Rda/traj_from_jpl_mean_phd.Rda")

# OR, just 20
# load(file = "Rda/traj_from_jpl_mean_20.Rda")
# traj_from_jpl_mean = traj_from_jpl_mean_20
  
#-------------------------------------------------------------------------------
# Cleaning, if desired: we get rid of the "too large" orbits at semlt 
# that might have led to wrong results BEFORE the JPL refinement
# Computed from the results of pp_mean_dHf_n_NCEM
#-------------------------------------------------------------------------------
traj_from_jpl_mean = traj_from_jpl_mean[which(traj_from_jpl_mean$dHf_SEM < 1.3e-4),]


#-------------------------------------------------------------------------------
# Plot of the mean distance between QBCP and JPL implementation @EML2
# as a function of the final energy @SEML2
#-------------------------------------------------------------------------------
# Init
pp_mean_dHf_n_NCEM = ggplot()

# Plot
aes.pk = aes_string("dHf_SEM", "mean_n_NCEM",  group = cs_interact("label", re.string), color = scolor)
pp_mean_dHf_n_NCEM = geom_point_pretty(pp_mean_dHf_n_NCEM, traj_from_jpl_mean, aes.pk)

# Theme and titles
pp_mean_dHf_n_NCEM = pp_mean_dHf_n_NCEM + theme_pgrey(color.back)
pp_mean_dHf_n_NCEM = pp_mean_dHf_n_NCEM + scale.color.noguide
pp_mean_dHf_n_NCEM = pp_mean_dHf_n_NCEM + scale_x_continuous(labels = scientific_format())

#Labels;
pp_mean_dHf_n_NCEM = pp_mean_dHf_n_NCEM + xlab(dHf.SE) + ylab(mean.n.NCEM)

# Right font
pp_mean_dHf_n_NCEM = set_font_cm(pp_mean_dHf_n_NCEM)

# Extend the x-axis a little bit, for SEM_20
pp_mean_dHf_n_NCEM  = pp_mean_dHf_n_NCEM+ expand_limits(x =1.605e-4)
pp_mean_dHf_n_NCEM  = pp_mean_dHf_n_NCEM+ scale_y_continuous(limits = c(0.21, 0.235))

pp_mean_dHf_n_NCEM

# Saving
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_pp_mean_dHf_n_NCEM_", palette.brewer)
  ggsave(pp_mean_dHf_n_NCEM, width = xSize, height = ySize,  bg = "transparent", device = cairo_pdf,   file = paste0(filename, ".pdf")) #Save in pdf
}


#-------------------------------------------------------------------------------
# Plot of mean_n_NCEM vs mean_n_NCSEM
#-------------------------------------------------------------------------------
aes.pk = aes_string("mean_n_NCEM", "mean_n_NCSEM",  
                    group = cs_interact("label", re.string), 
                    color = "factor(seed)")
pp_mean_nNCEM_nNCSE = ggplot() + geom_point(data = traj_from_jpl_mean, aes.pk, size = 3) 

#Color and shape
pp_mean_nNCEM_nNCSE = pp_mean_nNCEM_nNCSE + scale.color.jpl.local

#labels
pp_mean_nNCEM_nNCSE = pp_mean_nNCEM_nNCSE + xlab(mean.n.NCEM) + ylab(mean.n.NCSE)

# Theme
pp_mean_nNCEM_nNCSE = pp_mean_nNCEM_nNCSE + custom_theme

# Right font
pp_mean_nNCEM_nNCSE = set_font_cm(pp_mean_nNCEM_nNCSE)

pp_mean_nNCEM_nNCSE

# Saving
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, "_pp_mean_nNCEM_nNCSE")
  ggsave(pp_mean_nNCEM_nNCSE, width = xSize, height = ySize,  bg = "transparent", device = cairo_pdf,   file = paste0(filename, ".pdf")) #Save in pdf
}


#-------------------------------------------------------------------------------
# Plot of the mean distance between QBCP and JPL implementation @EML2
# as a function of mean_n_NCEM
# DO NOT SHOW! WHY? because we are not supposed to have difference amplitude...
# aïe aïe aïe! Comes from the pre-refinement in QBCP.
#-------------------------------------------------------------------------------
aes.pk = aes_string("mean_n_NCEM", "mean_dn_NCEM",  
                    group = cs_interact("label", re.string), 
                    color = "factor(seed)")
pp_mean_nNCEM_dnNCEM = ggplot() + geom_point(data = traj_from_jpl_mean, aes.pk, size = 3) 

# Labels on plot
#pp_mean_nNCEM_dnNCEM = pp_mean_nNCEM_dnNCEM + geom_text(data = traj_from_jpl_mean, aes(x = mean_n_NCEM, y = mean_dn_NCEM, label=label),hjust=0, vjust=0)

#Color and shape
pp_mean_nNCEM_dnNCEM = pp_mean_nNCEM_dnNCEM + scale.color.jpl.local

#labels
pp_mean_nNCEM_dnNCEM = pp_mean_nNCEM_dnNCEM + xlab(mean.n.NCEM) + ylab(mean.dn.NCEM)

# Theme
pp_mean_nNCEM_dnNCEM = pp_mean_nNCEM_dnNCEM + custom_theme

# Right font
pp_mean_nNCEM_dnNCEM = set_font_cm(pp_mean_nNCEM_dnNCEM)

pp_mean_nNCEM_dnNCEM



# Saving
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, "_pp_mean_nNCEM_dnNCEM")
  ggsave(pp_mean_nNCEM_dnNCEM, width = xSize, height = ySize,  bg = "transparent", device = cairo_pdf,   file = paste0(filename, ".pdf")) #Save in pdf
}


#-------------------------------------------------------------------------------
# Plot of the mean distance between QBCP and JPL implementation @SEML2
# as a function of mean_n_NCSEM
#-------------------------------------------------------------------------------
aes.pk = aes_string("mean_n_NCSEM", "mean_dn_NCSEM",  
                    group = cs_interact("label", re.string), 
                    color = "factor(seed)")
pp_mean_nNCSE_dnNCSE = ggplot() + geom_point(data = traj_from_jpl_mean, aes.pk, size = 3) 

# Labels on plot
#pp_mean_nNCSE_dnNCSE = pp_mean_nNCSE_dnNCSE + geom_text(data = traj_from_jpl_mean, aes(x = mean_n_NCSEM, y = mean_dn_NCSEM, label=label),hjust=0, vjust=0)

#Color and shape
pp_mean_nNCSE_dnNCSE = pp_mean_nNCSE_dnNCSE + scale.color.jpl.local

#labels
pp_mean_nNCSE_dnNCSE = pp_mean_nNCSE_dnNCSE + xlab(mean.n.NCSE) + ylab(mean.dn.NCSE)

# Theme
pp_mean_nNCSE_dnNCSE = pp_mean_nNCSE_dnNCSE + custom_theme

# Right font
pp_mean_nNCSE_dnNCSE = set_font_cm(pp_mean_nNCSE_dnNCSE)

pp_mean_nNCSE_dnNCSE

# Saving
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, "_pp_mean_nNCSE_dnNCSE")
  ggsave(pp_mean_nNCSE_dnNCSE, width = xSize, height = ySize,  bg = "transparent", device = cairo_pdf,   file = paste0(filename, ".pdf")) #Save in pdf
}

#-------------------------------------------------------------------------------
# Looking at mean_dn_NCEM vs mean_n_NCSEM
#-------------------------------------------------------------------------------
aes.pk = aes_string("mean_n_NCSEM", "mean_dn_NCEM",  
                    group = cs_interact("label", re.string), 
                    color = "factor(seed)")
pp_mean_nNCSE_dnNCEM = ggplot() + geom_point(data = traj_from_jpl_mean, aes.pk, size = 3) 

# Labels on plot
#pp_mean_nNCSE_dnNCEM = pp_mean_nNCSE_dnNCEM + geom_text(data = traj_from_jpl_mean, aes(x = dHf_SEM, y = mean_dn_NCEM, label=label),hjust=0, vjust=0)

#Color and shape
pp_mean_nNCSE_dnNCEM = pp_mean_nNCSE_dnNCEM + scale.color.jpl.local

#labels
pp_mean_nNCSE_dnNCEM = pp_mean_nNCSE_dnNCEM + xlab(mean.n.NCSE) + ylab(mean.dn.NCEM)

# Limits
#pp_mean_nNCSE_dnNCEM = pp_mean_nNCSE_dnNCEM + scale_y_continuous(limits = c(NaN, 0.5))

# Theme
pp_mean_nNCSE_dnNCEM = pp_mean_nNCSE_dnNCEM + custom_theme

# Right font
pp_mean_nNCSE_dnNCEM = set_font_cm(pp_mean_nNCSE_dnNCEM)


pp_mean_nNCSE_dnNCEM

# Saving
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, "_pp_mean_nNCSE_dnNCEM")
  ggsave(pp_mean_nNCSE_dnNCEM, width = xSize, height = ySize,  bg = "transparent", device = cairo_pdf,   file = paste0(filename, ".pdf")) #Save in pdf
}


#-------------------------------------------------------------------------------
# Looking at mean_dn_NCSEM vs mean_n_NCEM
#-------------------------------------------------------------------------------
aes.pk = aes_string("mean_n_NCEM", "mean_dn_NCSEM",  
                    group = cs_interact("label", re.string), 
                    color = "factor(seed)")
pp_mean_nNCEM_dnNCSE = ggplot() + geom_point(data = traj_from_jpl_mean, aes.pk, size = 3) 

# Labels on plot
#pp_mean_nNCEM_dnNCSE = pp_mean_nNCEM_dnNCSE + geom_text(data = traj_from_jpl_mean, aes(x = dHf_SEM, y = mean_dn_NCEM, label=label),hjust=0, vjust=0)

#Color and shape
pp_mean_nNCEM_dnNCSE = pp_mean_nNCEM_dnNCSE + scale.color.jpl.local

#labels
pp_mean_nNCEM_dnNCSE = pp_mean_nNCEM_dnNCSE + xlab(mean.n.NCEM) + ylab(mean.dn.NCSE)

# Theme
pp_mean_nNCEM_dnNCSE = pp_mean_nNCEM_dnNCSE + custom_theme

# Right font
pp_mean_nNCEM_dnNCSE = set_font_cm(pp_mean_nNCEM_dnNCSE)


pp_mean_nNCEM_dnNCSE

# Saving
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, "_pp_mean_nNCEM_dnNCSE")
  ggsave(pp_mean_nNCEM_dnNCSE, width = xSize, height = ySize,  bg = "transparent", device = cairo_pdf,   file = paste0(filename, ".pdf")) #Save in pdf
}


#-------------------------------------------------------------------------------
# Plot of the mean distance between QBCP and JPL implementation @SEML2
# as a function of re_CMU_EMT_mod
#-------------------------------------------------------------------------------
aes.pk = aes_string("re_CMU_EMT_mod", "mean_dn_NCSEM",  
                    group = cs_interact("label", re.string), 
                    color = "factor(seed)")
pp_mean_re_dn_NCSE = ggplot() + geom_point(data = traj_from_jpl_mean, aes.pk, size = 3) 

#Color and shape
pp_mean_re_dn_NCSE = pp_mean_re_dn_NCSE + scale.color.jpl.local

# Theme
pp_mean_re_dn_NCSE = pp_mean_re_dn_NCSE + custom_theme

# Right font
pp_mean_re_dn_NCSE = set_font_cm(pp_mean_re_dn_NCSE)

pp_mean_re_dn_NCSE



#-------------------------------------------------------------------------------
# Plot of the mean distance between QBCP and JPL implementation @EML2
# as a function of re_CMU_EMT_mod
#-------------------------------------------------------------------------------
aes.pk = aes_string("re_CMU_EMT_mod", "mean_dn_NCEM",  
                    group = cs_interact("label", re.string), 
                    color = "factor(seed)")
pp_mean_re_dn_NCEM = ggplot() + geom_point(data = traj_from_jpl_mean, aes.pk, size = 3) 

#Color and shape
pp_mean_re_dn_NCEM = pp_mean_re_dn_NCEM + scale.color.jpl.local

# Theme
pp_mean_re_dn_NCEM = pp_mean_re_dn_NCEM + custom_theme

# Right font
pp_mean_re_dn_NCEM = set_font_cm(pp_mean_re_dn_NCEM)


pp_mean_re_dn_NCEM

#-------------------------------------------------------------------------------
# Plot of the mean distance between QBCP and JPL implementation @SEML2
# as a function of the mean distance between QBCP and JPL implementation @EML2
#-------------------------------------------------------------------------------
aes.pk = aes_string("mean_dn_NCEM", "mean_dn_NCSEM",  
                    group = cs_interact("label", re.string), 
                    color = "factor(seed)")
pp_mean_dn_NCEM_dn_NCSE = ggplot() + geom_point(data = traj_from_jpl_mean, aes.pk, size = 3) 

#Color and shape
pp_mean_dn_NCEM_dn_NCSE = pp_mean_dn_NCEM_dn_NCSE + scale.color.jpl.local

# Theme
pp_mean_dn_NCEM_dn_NCSE = pp_mean_dn_NCEM_dn_NCSE + custom_theme

# Right font
pp_mean_dn_NCEM_dn_NCSE = set_font_cm(pp_mean_dn_NCEM_dn_NCSE)


pp_mean_dn_NCEM_dn_NCSE


stop("3D plots after this stop")

#-------------------------------------------------------------------------------
# 3D plot
#-------------------------------------------------------------------------------
rgl_init(bg = "#23373b")


points3d(x =  traj_from_jpl_mean$mean_n_NCEM, 
         z =  traj_from_jpl_mean$mean_n_NCSEM, 
         y =  traj_from_jpl_mean$mean_dn_NCEM,
        lwd = 2,
        fog = TRUE,
        color = traj_from_jpl_mean$seed)
# Box
axes3d(color = c("black", "white"))

# Title
title3d(xlab = 'mean_n_NCEM', zlab = 'mean_n_NCSEM', ylab = 'mean_dn_NCEM', color = "white")