################################################################################
# R script to handle a projection map (connections between EML2 and SEML1,2)
#
# Particular case of SEML1,2 to EML2: since there is no continuation procedure
# available for this direction, do NOT use CONTINUATION.R if FWRK == "SEM"
#
################################################################################

#===============================================================================
# Initialization
#===============================================================================
source("source/init.R")

#-------------------------------------------------------------------------------
# Select parameters
#-------------------------------------------------------------------------------
LIB_POINT_EM   = "L2"    # "L1" or "L2"
LIB_POINT_SEM  = "L2"    # "L1" or "L2"
FWRK           = "EM"    # "EM" (mainly) or "SEM"
DYN_MODEL      = "QBCP"  # "QBCP" for now
DATA_SOURCE    = "FROM_SERVER" # "FROM_SERVER" or "LOCAL"
TYPE           = ""   # "_3d" or "", or "_orbit" in some cases.
ISSAVED        = FALSE   # TRUE or FALSE

# Order of the series
ORDER          =  switch(DATA_SOURCE, "LOCAL" = "16", "FROM_SERVER" = "20");

# Origin and target
LIB_POINT_OR   =  switch(FWRK, "EM" = LIB_POINT_EM, "SEM" = LIB_POINT_SEM);
LIB_POINT_DEST =  switch(FWRK, "EM" = LIB_POINT_SEM, "SEM" = LIB_POINT_EM);

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
                        "LOCAL" = paste0("plot/QBCP/", FWRK, "/", LIB_POINT_OR, "/"), 
                        "FROM_SERVER" = paste0("plot/QBCP/", FWRK, "/", LIB_POINT_OR, "/Serv/"))
#-------------------------------------------------------------------------------
# Prefix
#-------------------------------------------------------------------------------
FILE_PREFIX_SOL = paste0(ftincppdafolder, FILE_SUBFOLDER, "sortprojintcu_order_", ORDER, "_dest_", LIB_POINT_DEST)
FILE_PREFIX     = paste0(ftincppdafolder, FILE_SUBFOLDER, "projcu", TYPE , "_order_", ORDER, "_dest_", LIB_POINT_DEST)


if(FWRK == "EM")
{
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
      #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_tspan_0T_T_crossings_eps_1e-5" )
      
      
      #---------------------------------------------------------------------------
      # Main data specific orbits and all times in [0, T]:
      #
      #  - s1.seed in c(10, 20, 30, 40)
      #  - s3.seed = -s1.seed
      #---------------------------------------------------------------------------
      #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_Orbit_10_40" )
      #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_Orbit_10_40_eps_1e-5" )
      #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_Orbit_10_eps_1e-5" )
      #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_Orbit_10_All" )
      
      # For plotting initial seeds (no corresponding refined trajectories, 
      # the plots are obtained via ORBITS_PKS_PLOT_NCEM.R/ORBITS_PKS_PLOT_EM.R)
      #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_Orbit_10_local" ) 
      
      #---------------------------------------------------------------------------
      # Main data for ONE specific orbit and all times in [0, T] (QHalo)
      # For Qhalo (big and small): 
      #
      # to load: 1. load(file = "Rda/proj_map_source_Qhalo.Rda")
      #          2. proj_map_source = proj_map_source_Qhalo
      # (do that after MAIN.R, before ORBITS_PROJ_POSTPROCESS.R)
      # 
      #---------------------------------------------------------------------------
      #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "_Orbit", "FROM_SERVER" = "_QHalo" )
      #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_Orbit_QHalo_eps_1e-5" )
      #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_Orbit_QHalo_small" )
      #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_Orbit_QHalo_medium" )
      #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_Orbit_QHalo_big" )
      #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_Orbit_QHalo_big_3T" )
      
      #---------------------------------------------------------------------------
      # Main data for ONE specific orbit and all times in [0, T] (Lissajous)
      #---------------------------------------------------------------------------
      #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_Orbit_Liss_s1_10_s2_5" )
      #FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_Orbit_Liss_s1_10_s2_5_3T" )
      FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "_Orbit_Liss_s1_20_s2_5" )
      
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
}else
{
  #---------------------------------------------------------------------------
  # Main data from SEM to EM
  #---------------------------------------------------------------------------
  FILE_SUFFIX = switch(DATA_SOURCE, "LOCAL" = "", "FROM_SERVER" = "" )
  
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

X_syn  = expression(italic(X))
Y_syn  = expression(italic(Y))
Z_syn  = expression(italic(Z))

X_S  = expression(italic(X)^italic(S))
Y_S  = expression(italic(Y)^italic(S))
Z_S  = expression(italic(Z)^italic(S))

X_E  = expression(italic(X)^italic(E))
Y_E  = expression(italic(Y)^italic(E))
Z_E  = expression(italic(Z)^italic(E))
PX_E = expression(italic(P)[italic(X)]^italic(E))

x_em  = expression(italic(x)^italic(e))
y_em  = expression(italic(y)^italic(e))
z_em  = expression(italic(z)^italic(e))
px_em = expression(italic(p)[italic(x)]^italic(e))
B_ems = "italic(B)[italic(em)]"

X_S_tex = "$\\mathsc{X}^\\mathsctiny{S}$"
Y_S_tex = "$\\mathsc{Y}^\\mathsctiny{S}$"
Z_S_tex = "$\\mathsc{Z}^\\mathsctiny{S}$"

X_E_tex = "$\\mathsc{X}^\\mathsctiny{E}$"
Y_E_tex = "$\\mathsc{Y}^\\mathsctiny{E}$"
Z_E_tex = "$\\mathsc{Z}^\\mathsctiny{E}$"

x_em_tex = "$x^{e}$"
y_em_tex = "$y^{e}$"
z_em_tex = "$z^{e}$"

x_sem = expression(italic(x)^italic(s))
y_sem = expression(italic(y)^italic(s))
z_sem = expression(italic(z)^italic(s))

x_sem_tex = "$x^{s}$"
y_sem_tex = "$y^{s}$"
z_sem_tex = "$z^{s}$"

s1_tex = "$s_1$"
s2_tex = "$s_2$"
s3_tex = "$s_3$"
s4_tex = "$s_4$"

pmin_exp = expression(italic(d)[italic(p)]^italic(m))
pmin_tex = "$d_p^m$"

tj_T      = expression(t[j]~"(x T)")
tj_T_tex  = "$t_j~(\\times T)$"

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
if(FWRK == "EM")
{
  source("ProjMap/POSTPROCESS.R")
}else
{
  source("ProjMap/POSTPROCESS_SEML.R")
}

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
