################################################################################
# R script to handle a projection map (connections between EML2 and SEML1,2)
################################################################################

#===============================================================================
# Initialization
#===============================================================================
source("source/init.R")

#-------------------------------------------------------------------------------
# Select parameters
#-------------------------------------------------------------------------------
LIB_POINT_EM  = "L2"
LIB_POINT_SEM = "L2"
DYN_MODEL     = "QBCP"
FWRK          = "EM"
DATA_SOURCE   = "FROM_SERVER" #FROM_SERVER or LOCAL
ORDER         =  switch(DATA_SOURCE, "LOCAL" = "16", "FROM_SERVER" = "20");
LOCAL_ORDER   = "20";
TYPE          = "" # "_3d" or ""
ISSAVED       = FALSE

#-------------------------------------------------------------------------------
# Primaries & Lib points for plotting
#-------------------------------------------------------------------------------
dfearth_eml  = dfprimary("Earth", LIB_POINT_EM, "EM")
dfearth_seml = dfprimary("Earth", LIB_POINT_SEM, "SEM")
dfmoon_eml   = dfprimary("Moon",  LIB_POINT_EM, "EM")
dfmoon_seml  = dfprimary("Moon",  LIB_POINT_SEM, "SEM")
dfsemli      = dflibpoint(LIB_POINT_SEM, "SEM")
dfemli       = dflibpoint(LIB_POINT_EM, "EM")


#-------------------------------------------------------------------------------
# Constraints on the center/center-unstable manifolds
#-------------------------------------------------------------------------------
SNMAX    = 0.6;
YNMAX    = 0.6;
GSIZEMAX = 35;


#-------------------------------------------------------------------------------
# Subfolder
#-------------------------------------------------------------------------------
FILE_SUBFOLDER = switch(DATA_SOURCE, 
                        "LOCAL" = paste0("plot/QBCP/EM/", LIB_POINT_EM, "/"), 
                        "FROM_SERVER" = paste0("plot/QBCP/EM/", LIB_POINT_EM, "/Serv/"))
#-------------------------------------------------------------------------------
# Prefix
#-------------------------------------------------------------------------------
FILE_PREFIX_SOL = paste0(ftincppdafolder, FILE_SUBFOLDER, "sortprojintcu_order_", ORDER, "_dest_", LIB_POINT_SEM)
FILE_PREFIX     = paste0(ftincppdafolder, FILE_SUBFOLDER, "projcu", TYPE , "_order_", ORDER, "_dest_", LIB_POINT_SEM)

if(LIB_POINT_EM == "L2")
{
  if(LIB_POINT_SEM == "L2")
  {
    #---------------------------------------------------------------------------
    # Main data for all times in [0, T], old implementation
    #---------------------------------------------------------------------------
    #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_tspan_0T_025T_FINAL" )
    #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_tspan_025T_05T_FINAL" )
    #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_tspan_05T_075T_FINAL" )
    #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_tspan_075T_T_FINAL" )
    
    #---------------------------------------------------------------------------
    # Main data for all times in [0.99, T], old implementation
    #---------------------------------------------------------------------------
    #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_tspan_099T_T" )
    
    #---------------------------------------------------------------------------
    # Main data for specific times, old implementation
    #---------------------------------------------------------------------------
    #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_t0_099T" )
    #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_t0_0995T" )
    #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_t0_0065T" )
    
    #---------------------------------------------------------------------------
    # Main data for all times in [0, T], new implementation (crossings)
    #---------------------------------------------------------------------------
    #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_tspan_0T_T_crossings" )

    #---------------------------------------------------------------------------
    # Main data specific orbits and all times in [0, T]:
    #
    #  - s1.seed in c(10, 20, 30, 40)
    #  - s3.seed = -s1.seed
    #---------------------------------------------------------------------------
    FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_Orbit_10_40" )
    
    #---------------------------------------------------------------------------
    # Main data for specific times, old implementation, 3D case
    #---------------------------------------------------------------------------
    if(TYPE == "_3d")
    {
      FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_t0_0995T" )
    }
    
    
  }else
  {
    #---------------------------------------------------------------------------
    # Main data for all times in [0, T], old implementation
    #---------------------------------------------------------------------------
    #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "" )
    #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_tspan_075T_T_FINAL" )
    #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_tspan_05T_075T_FINAL" )
    #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_tspan_025T_05T_FINAL" )
    FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_tspan_0T_025T_FINAL" )
  }
}else{
  
  #---------------------------------------------------------------------------
  # Main data for t0 = 0, old implementation
  #---------------------------------------------------------------------------
  FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_t0_0" )
  
  #---------------------------------------------------------------------------
  # Main data for some times between [0, T], old implemenation
  #---------------------------------------------------------------------------
  #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "" )
}


#-------------------------------------------------------------------------------
# Constants
#-------------------------------------------------------------------------------
# EM framework
CST_MASS_RATIO_EM  = muR("EM");
CST_GAMMA_LIB_EM   = gamma(LIB_POINT_EM, "EM");
CST_C1_LIB_EM      = c1(LIB_POINT_EM, "EM");
CST_DIST_PRIM_EM   = Ldist("EM");
CST_SEM_PERIOD_EM  = SEMperiod("EM");

# SEM framework
CST_DIST_PRIM_SEM  = Ldist("SEM");
CST_GAMMA_LIB_SEM  = gamma(LIB_POINT_SEM, "SEM");
CST_C1_LIB_SEM     = c1(LIB_POINT_SEM, "SEM");
CST_SEM_PERIOD_SEM = SEMperiod("SEM");
CST_NC_TO_SI_FACTOR_SEM = CST_DIST_PRIM_SEM*CST_GAMMA_LIB_SEM;

#===============================================================================
# Common variables
#===============================================================================
s1_exp  = expression(italic(s)[1])
s2_exp  = expression(italic(s)[2])
s3_exp  = expression(italic(s)[3])
s4_exp  = expression(italic(s)[4])
tof_exp = expression(group(delta, "t", "[", "T", "]"))

x_em = expression(italic(x)^italic(em))
y_em = expression(italic(y)^italic(em))
z_em = expression(italic(z)^italic(em))

x_sem = expression(italic(x)^italic(sem))
y_sem = expression(italic(y)^italic(sem))
z_sem = expression(italic(z)^italic(sem))

s1_tex = "$s_1$"
s3_tex = "$s_3$"

pmin_exp = expression(italic(d)[italic(p)]^italic(m))
pmin_tex = "$d_p^m$"

# Color gradients
scg_pem_tex         = scale_colour_gradient(pmin_tex, space="Lab", low = muted("blue"), high = "white")
scg_pem             = scale_colour_gradient(pmin_exp, space="Lab", low = muted("blue"), high = "white")
scg_pem_guide_false = scale_colour_gradient(pmin_exp, space="Lab", low = muted("blue"), high = "white", guide = FALSE)
sfg_pem_guide_false = scale_fill_gradient(  pmin_exp, space="Lab", low = muted("blue"), high = "white", guide = FALSE)

#===============================================================================
# Get data from file
#===============================================================================
source("ProjMap/PROJMAP.R")

#===============================================================================
# Postprocess
#===============================================================================
source("ProjMap/POSTPROCESS.R")

#===============================================================================
# Continuation
#===============================================================================
#source("ProjMap/CONTINUATION.R")
#source("ProjMap/CONTINUATION_MULTI.R")


#===============================================================================
# PLOTS
#===============================================================================
#source("ProjMap/PLOTS.R")
#source("ProjMap/PLOTS_LATEX.R")

#===============================================================================
# MULTIPLOTS
#===============================================================================
#source("ProjMap/MULTIPLOTS.R")
