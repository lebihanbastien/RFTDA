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

#=====  Config  ================================================================
source("ProjMap/ORBITS_PKS_PLOT_CONFIG.R")       #configuration file
is.saved = T

#-------------------------------------------------------------------------------
# Aes
#-------------------------------------------------------------------------------
scale.color.title.local = "Initial orbit"
scolor.local = "factor(label)"
scale.color.jpl.local  = scale_color_brewer(scale.color.title.local, 
                                            type="seq",palette = "Dark2", 
                                            direction = 1)

#-------------------------------------------------------------------------------
# Label
#-------------------------------------------------------------------------------
# For R
dHf.SE             = expression(delta*italic(H)[italic(f)])
mean.n.EM         = expression(bar(italic(a))[italic(e)])
mean.n.SE         = expression(bar(italic(a))[italic(s)])
mean.dn.EM        = expression(delta*bar(italic(a))[italic(e)])
mean.dn.SE        = expression(delta*bar(italic(a))[italic(s)])
mean.dn.EM.over.n =  expression(delta*bar(italic(a))[italic(e)]/bar(italic(a))[italic(e)])

mean.n.EM         = expression(bar(italic(A))[italic(E)])
mean.n.SE         = expression(bar(italic(A))[italic(S)])
mean.dn.EM        = expression(delta*bar(italic(A))[italic(E)])
mean.dn.SE        = expression(delta*bar(italic(A))[italic(S)])

mean.gamma       = expression(gamma[italic(e)])

#-------------------------------------------------------------------------------
# Load from file  & rbind. Used to save the file "traj_from_jpl_mean_phd.Rda"
#-------------------------------------------------------------------------------
load(file = "Rda/traj_from_jpl_mean_QHalo_small.Rda")
load(file = "Rda/traj_from_jpl_mean_QHalo_big.Rda")

traj_from_jpl_mean_QHalo_small$size = "Small"
traj_from_jpl_mean_QHalo_big$size = "Big"

traj_from_jpl_mean = rbind(traj_from_jpl_mean_QHalo_small, traj_from_jpl_mean_QHalo_big)

#-------------------------------------------------------------------------------
# Cleaning, if desired: we get rid of the "too large" orbits at semlt 
# that might have led to wrong results BEFORE the JPL refinement
# Computed from the results of pp_mean_dHf_n_NCEM
#-------------------------------------------------------------------------------
traj_from_jpl_mean   = traj_from_jpl_mean[which(traj_from_jpl_mean$dHf_SEM < 1.35e-4),]

#-------------------------------------------------------------------------------
# Plot of the mean distance between QBCP and JPL implementation @EML2
# as a function of the final energy @SEML2
#-------------------------------------------------------------------------------
# Init
pp_mean_dHf_n_EM = ggplot()

# Plot
aes.pk = aes_string("dHf_SEM", "mean_n_NCEM",  group = cs_interact("label", re.string), color = scolor)
pp_mean_dHf_n_EM = geom_point_pretty(pp_mean_dHf_n_EM, traj_from_jpl_mean, aes.pk)

# Theme and titles
pp_mean_dHf_n_EM = pp_mean_dHf_n_EM + theme_pgrey(color.back)
pp_mean_dHf_n_EM = pp_mean_dHf_n_EM + scale.color
pp_mean_dHf_n_EM = pp_mean_dHf_n_EM + scale_x_continuous(labels = scientific_format())

#Labels;
pp_mean_dHf_n_EM = pp_mean_dHf_n_EM + xlab(dHf.SE) + ylab(mean.n.EM)

# Extend the x-axis a little bit, for SEM_20
# pp_mean_dHf_n_EM  = pp_mean_dHf_n_EM+ expand_limits(x =1.605e-4)
# pp_mean_dHf_n_EM  = pp_mean_dHf_n_EM+ scale_y_continuous(limits = c(0.035, 0.0395))

# Right font
pp_mean_dHf_n_EM = set_font_cm_x(pp_mean_dHf_n_EM)

pp_mean_dHf_n_EM

# Saving
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_pp_mean_dHf_n_EM_", palette.brewer)
  ggsave(pp_mean_dHf_n_EM, width = xSize, height = ySize,  bg = "transparent", device = cairo_pdf,   file = paste0(filename, ".pdf")) #Save in pdf
}


#-------------------------------------------------------------------------------
# Plot of mean_n_EM vs mean_n_SEM
#-------------------------------------------------------------------------------
aes.pk = aes_string("CST_GAMMA_LIB_EM*mean_n_NCEM", "CST_GAMMA_LIB_SEM*mean_n_NCSEM",  
                    group = cs_interact("label", re.string), color = "factor(size)")
pp_mean_nEM_nSEM = ggplot() + geom_point(data = traj_from_jpl_mean, aes.pk, size = 3) 

#Color and shape
pp_mean_nEM_nSEM = pp_mean_nEM_nSEM + scale.color.jpl.local

#labels
pp_mean_nEM_nSEM = pp_mean_nEM_nSEM + xlab(mean.n.EM) + ylab(mean.n.SE)

# Theme
pp_mean_nEM_nSEM = pp_mean_nEM_nSEM + custom_theme

pp_mean_nEM_nSEM

# Saving
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_pp_mean_nEM_nSEM")
  ggsave(pp_mean_nEM_nSEM, width = xSize, height = ySize,  bg = "transparent", device = cairo_pdf,   file = paste0(filename, ".pdf")) #Save in pdf
}

#-------------------------------------------------------------------------------
# Plot of the mean distance between QBCP and JPL implementation @EML2
# as a function of mean_n_EM
# DO NOT SHOW! WHY? because we are not supposed to have difference amplitude...
# aïe aïe aïe! Comes from the pre-refinement in QBCP.
#-------------------------------------------------------------------------------
aes.pk = aes_string("CST_GAMMA_LIB_EM*mean_n_NCEM", "CST_GAMMA_LIB_EM*mean_dn_NCEM",  
                    group = cs_interact("label", re.string), color = "factor(size)")
pp_mean_nEM_dnEM = ggplot() + geom_point(data = traj_from_jpl_mean, aes.pk, size = 3) 

# Labels on plot
#pp_mean_nEM_dnEM = pp_mean_nEM_dnEM + geom_text(data = traj_from_jpl_mean, aes(x = mean_n_NCEM, y = mean_dn_NCEM, label=label),hjust=0, vjust=0)

#Color and shape
pp_mean_nEM_dnEM = pp_mean_nEM_dnEM + scale.color.jpl.local

#labels
pp_mean_nEM_dnEM = pp_mean_nEM_dnEM + xlab(mean.n.EM) + ylab(mean.dn.EM)

# Theme
pp_mean_nEM_dnEM = pp_mean_nEM_dnEM + custom_theme

pp_mean_nEM_dnEM

# Saving
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_pp_mean_nEM_dnEM")
  ggsave(pp_mean_nEM_dnEM, width = xSize, height = ySize,  bg = "transparent", device = cairo_pdf,   file = paste0(filename, ".pdf")) #Save in pdf
}


#-------------------------------------------------------------------------------
# Plot of the mean distance between QBCP and JPL implementation @SEML2
# as a function of mean_n_NCSEM
#-------------------------------------------------------------------------------
aes.pk = aes_string("CST_GAMMA_LIB_SEM*mean_n_NCSEM", "CST_GAMMA_LIB_SEM*mean_dn_NCSEM",  
                    group = cs_interact("label", re.string), color = "factor(size)")
pp_mean_nSEM_dnSEM = ggplot() + geom_point(data = traj_from_jpl_mean, aes.pk, size = 3) 

# Labels on plot
#pp_mean_nSEM_dnSEM = pp_mean_nSEM_dnSEM + geom_text(data = traj_from_jpl_mean, aes(x = mean_n_NCSEM, y = mean_dn_NCSEM, label=label),hjust=0, vjust=0)

#Color and shape
pp_mean_nSEM_dnSEM = pp_mean_nSEM_dnSEM + scale.color.jpl.local

#labels
pp_mean_nSEM_dnSEM = pp_mean_nSEM_dnSEM + xlab(mean.n.SE) + ylab(mean.dn.SE)

# Theme
pp_mean_nSEM_dnSEM = pp_mean_nSEM_dnSEM + custom_theme

pp_mean_nSEM_dnSEM

# Saving
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_pp_mean_nSEM_dnSEM")
  ggsave(pp_mean_nSEM_dnSEM, width = xSize, height = ySize,  bg = "transparent", device = cairo_pdf,   file = paste0(filename, ".pdf")) #Save in pdf
}

#-------------------------------------------------------------------------------
# Looking at mean_dn_NCEM vs mean_n_NCSEM
#-------------------------------------------------------------------------------
aes.pk = aes_string("CST_GAMMA_LIB_SEM*mean_n_NCSEM", "CST_GAMMA_LIB_EM*mean_dn_NCEM",  
                    group = cs_interact("label", re.string), color = "factor(size)")
pp_mean_nSEM_dnEM = ggplot() + geom_point(data = traj_from_jpl_mean, aes.pk, size = 3) 

# Labels on plot
#pp_mean_nSEM_dnEM = pp_mean_nSEM_dnEM + geom_text(data = traj_from_jpl_mean, aes(x = dHf_SEM, y = mean_dn_NCEM, label=label),hjust=0, vjust=0)

#Color and shape
pp_mean_nSEM_dnEM = pp_mean_nSEM_dnEM + scale.color.jpl.local

#labels
pp_mean_nSEM_dnEM = pp_mean_nSEM_dnEM + xlab(mean.n.SE) + ylab(mean.dn.EM)

# Limits
#pp_mean_nSEM_dnEM = pp_mean_nSEM_dnEM + scale_y_continuous(limits = c(NaN, 0.5))

# Theme
pp_mean_nSEM_dnEM = pp_mean_nSEM_dnEM + custom_theme

pp_mean_nSEM_dnEM

# Saving
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_pp_mean_nSEM_dnEM")
  ggsave(pp_mean_nSEM_dnEM, width = xSize, height = ySize,  bg = "transparent", device = cairo_pdf,   file = paste0(filename, ".pdf")) #Save in pdf
}


#-------------------------------------------------------------------------------
# Looking at mean_dn_NCSEM vs mean_n_NCEM
#-------------------------------------------------------------------------------
aes.pk = aes_string("CST_GAMMA_LIB_EM*mean_n_NCEM", "CST_GAMMA_LIB_SEM*mean_dn_NCSEM",  
                    group = cs_interact("label", re.string), color = "factor(size)")
pp_mean_nEM_dnSEM = ggplot() + geom_point(data = traj_from_jpl_mean, aes.pk, size = 3) 

# Labels on plot
#pp_mean_nEM_dnSEM = pp_mean_nEM_dnSEM + geom_text(data = traj_from_jpl_mean, aes(x = dHf_SEM, y = mean_dn_NCEM, label=label),hjust=0, vjust=0)

#Color and shape
pp_mean_nEM_dnSEM = pp_mean_nEM_dnSEM + scale.color.jpl.local

#labels
pp_mean_nEM_dnSEM = pp_mean_nEM_dnSEM + xlab(mean.n.EM) + ylab(mean.dn.SE)

# Theme
pp_mean_nEM_dnSEM = pp_mean_nEM_dnSEM + custom_theme

pp_mean_nEM_dnSEM

# Saving
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_pp_mean_nEM_dnSEM")
  ggsave(pp_mean_nEM_dnSEM, width = xSize, height = ySize,  bg = "transparent", device = cairo_pdf,   file = paste0(filename, ".pdf")) #Save in pdf
}


#-------------------------------------------------------------------------------
# Plot re_CMU_EMT_mod vs mean_dn_EM
#-------------------------------------------------------------------------------
aes.pk = aes_string("re_CMU_EMT_mod", "CST_GAMMA_LIB_EM*mean_dn_NCEM",  
                    group = cs_interact("label", re.string), color = "factor(size)")
pp_mean_re_nEM = ggplot() + geom_point(data = traj_from_jpl_mean, aes.pk, size = 3) 

#Color and shape
pp_mean_re_nEM = pp_mean_re_nEM + scale.color.jpl.local

#Labels
pp_mean_re_nEM = pp_mean_re_nEM + xlab(mean.gamma) + ylab(mean.dn.EM)


# Theme
pp_mean_re_nEM = pp_mean_re_nEM + custom_theme
pp_mean_re_nEM

# Saving
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_pp_mean_re_dnEM")
  ggsave(pp_mean_re_nEM, width = xSize, height = ySize,  bg = "transparent", device = cairo_pdf,   file = paste0(filename, ".pdf")) #Save in pdf
}

#-------------------------------------------------------------------------------
# Plot re_CMU_EMT_mod vs mean_dn_SEM
#-------------------------------------------------------------------------------
aes.pk = aes_string("re_CMU_EMT_mod", "CST_GAMMA_LIB_SEM*mean_dn_NCSEM",  
                    group = cs_interact("label", re.string), color = "factor(size)")
pp_mean_re_nSEM = ggplot() + geom_point(data = traj_from_jpl_mean, aes.pk, size = 3) 

#Color and shape
pp_mean_re_nSEM = pp_mean_re_nSEM + scale.color.jpl.local

#Labels
pp_mean_re_nSEM = pp_mean_re_nSEM + xlab(mean.gamma) + ylab(mean.dn.EM)


# Theme
pp_mean_re_nSEM = pp_mean_re_nSEM + custom_theme
pp_mean_re_nSEM

# Saving
if(is.saved)
{
  filename = paste0(FILE_PREFIX_FROM_JPL, seeds, "_pp_mean_re_dnSEM")
  ggsave(pp_mean_re_nSEM, width = xSize, height = ySize,  bg = "transparent", device = cairo_pdf,   file = paste0(filename, ".pdf")) #Save in pdf
}

################################################################################
stop("After this point: sandbox")
################################################################################

#-------------------------------------------------------------------------------
# Plot of the mean distance between QBCP and JPL implementation @SEML2
# as a function of re_CMU_EMT_mod
#-------------------------------------------------------------------------------
pp_mean_re_dn_SEM = ggplot() + geom_point(data = traj_from_jpl_mean, aes(x = r0_CMU_EMT_mod, y = mean_dn_NCSEM)) 
# Labels on plot
pp_mean_re_dn_SEM = pp_mean_re_dn_SEM + geom_text(data = traj_from_jpl_mean, aes(x = r0_CMU_EMT_mod, y = mean_dn_NCSEM, label=label),hjust=0, vjust=0)
# Theme
pp_mean_re_dn_SEM = pp_mean_re_dn_SEM + custom_theme
pp_mean_re_dn_SEM

#-------------------------------------------------------------------------------
# Plot of the mean distance between QBCP and JPL implementation @EML2
# as a function of re_CMU_EMT_mod
#-------------------------------------------------------------------------------
pp_mean_re_dn = ggplot() + geom_point(data = traj_from_jpl_mean, aes(x = r0_CMU_EMT_mod, y = mean_dn_NCEM)) 
# Labels on plot
pp_mean_re_dn = pp_mean_re_dn + geom_text(data = traj_from_jpl_mean, aes(x = r0_CMU_EMT_mod, y = mean_dn_NCEM, label=label),hjust=0, vjust=0)
# Theme
pp_mean_re_dn = pp_mean_re_dn + custom_theme
pp_mean_re_dn

#-------------------------------------------------------------------------------
# Plot of the mean distance between QBCP and JPL implementation @EML2
# as a function of mean_n_NCEM
# DO NOT SHOW! WHY? because we are not supposed to have difference amplitude...
# aïe aïe aïe! Comes from the pre-refinement in QBCP.
#-------------------------------------------------------------------------------
pp_mean_n_dn = ggplot() + geom_point(data = traj_from_jpl_mean, aes(x = mean_n_NCEM, y = mean_dn_NCEM)) 
pp_mean_n_dn = pp_mean_n_dn + geom_point(data = traj_from_jpl_mean, aes(x = mean_n_NCEM, y = mean_dnxy_NCEM), color = "red") 
# Labels on plot
pp_mean_n_dn = pp_mean_n_dn + geom_text(data = traj_from_jpl_mean, aes(x = mean_n_NCEM, y = mean_dn_NCEM, label=label),hjust=0, vjust=0)
pp_mean_n_dn = pp_mean_n_dn + geom_text(data = traj_from_jpl_mean, aes(x = mean_n_NCEM, y = mean_dnxy_NCEM, label=label),hjust=0, vjust=0, color = "red")
# Theme
pp_mean_n_dn = pp_mean_n_dn + custom_theme
pp_mean_n_dn




#-------------------------------------------------------------------------------
# Looking at mean_dn_NCEM
#-------------------------------------------------------------------------------
aes.pk = aes_string("dHf_SEM", "mean_dn_NCEM",  group = cs_interact("label", re.string), color = scolor)

pp_mean_dHf_dn = ggplot()
pp_mean_dHf_dn = geom_point_pretty(pp_mean_dHf_dn, traj_from_jpl_mean, aes.pk)
#
# OR
#
#pp_mean_dHf_dn = ggplot() + geom_point(data = traj_from_jpl_mean, aes(x = dHf_SEM, y = mean_dn_NCEM)) 

# Labels on plot
pp_mean_dHf_dn = pp_mean_dHf_dn + geom_text(data = traj_from_jpl_mean, aes_string(x = "dHf_SEM", y = "mean_dn_NCEM", label="label", color = scolor), hjust=0, vjust=0)
# Theme
pp_mean_dHf_dn = pp_mean_dHf_dn + custom_theme
pp_mean_dHf_dn



