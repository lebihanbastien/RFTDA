############################################################
# R script to handle a precision map of the QBCP around EML1
############################################################

#------------------------------------------------
# Select Models & libration point
#------------------------------------------------
Li        = "L1"
Lit       = "$L_1$"
MODEL     = "QBCP"
FWRK      = "EM"
METHOD    = "_SINGLE_INT"
Type      = "s1s2s3s4"#"s1eqms3" #s1, s1eqs3, s1eqms3, s3, s1eq2s2eqs3eq2s4
order     = "20"
ofs_order = "30"
Energy    = "0"#-10 #0.01 #0.01 #0.0025, 0.005, 0.0075
vorders   = c(seq(6,26,4),22)

#------------------------------------------------
# Plotting parameters
#------------------------------------------------
pE_limits=FALSE
pE_limits_x=c(0.0, 0.02)
pE_limits_y=c(-8, -2)

pX_limits  =FALSE
pX_breaks_x=FALSE
pX_breaks_y=TRUE

pX_limits_x        = c(NaN, 0.91)
pX_limits_y        = c(-9, -1)
pX_breaks_x_values = seq(-1.19,-1.10,0.01)
pX_breaks_y_values = seq(-10,0,2)

pY_limits=FALSE
pY_breaks_x =FALSE
pY_breaks_y =FALSE
pY_limits_x=c(-0.07, 0.07)
pY_limits_y=c(-11, -2)
pY_breaks_x_values = seq(-0.10,0.10,0.01)
pY_breaks_y_values = seq(-10,0,2)

pS1_limits=FALSE

pG_limits_y = c(-12, -1);
pG_breaks_y = seq(-16,0,2);

pE_scale_x = scale_x_continuous()
pE_scale_y = scale_y_log10(breaks = 10^(-8:0*2), limits=c(1e-12,1e-1))

#------------------------------------------------
# Process data
#------------------------------------------------
# source("Emap/Eplot.R")
# source("Emap/Eplot_TEX.R")
source("Emap/Eplot_TEX_PhD.R")