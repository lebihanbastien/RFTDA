################################################################################
# Misc routines and dataframes on CRTBP/QBCP/BCP
################################################################################
#-------------------------------------------------------------------------------
# Potential
#-------------------------------------------------------------------------------
cr3bpot <- function(x, y, mu)
{
  r1  = sqrt((x-mu)^2 + y^2);
  r2  = sqrt((x-mu+1)^2 + y^2);
  pot = -0.5*(x^2 + y^2) - (1-mu)/r1 - mu/r2 - 1/2*(1-mu)*mu;
  return (pot)
}

cr3bpot_EM <- function(z)
{
  mu = 0.0121505816234336;
  r1  = sqrt((z[1]-mu)^2 + z[2]^2);
  r2  = sqrt((z[1]-mu+1)^2 + z[2]^2);
  pot = -0.5*(z[1]^2 + z[2]^2) - (1-mu)/r1 - mu/r2 - 1/2*(1-mu)*mu;
  return (pot)
}

#-------------------------------------------------------------------------------
# Return the mass ratio
#-------------------------------------------------------------------------------
muR <- function(FWRK)
{
  if (FWRK == "EM")
  {
    mu = +1.215058162343360e-02;
  }
  
  if (FWRK == "SEM")
  {
    mu = +3.040277404912506e-06;
  }
  
  return(mu)
}

#-------------------------------------------------------------------------------
# Return the value gamma = distance(Li-smallest primary) 
# for L1,2, distance(Li-biggest primary for L3)
#-------------------------------------------------------------------------------
gamma <- function(Li, FWRK)
{
  if (FWRK == "EM")
  {
    if (Li == "L1")
      gamma = 0.150934272990064
    if (Li == "L2")
      gamma = 0.16783273173707
    if (Li == "L3")
      gamma = +9.929120655179929e-01
  }
  
  if (FWRK == "SEM")
  {
    if (Li == "L1")
      gamma = 0.0100108175327472
    if (Li == "L2")
      gamma = 0.0100780785917628
    if (Li == "L3")
      gamma = 0.999998226504847
  }
  
  return(gamma)
}

#-------------------------------------------------------------------------------
# Return the value c1 = f(gamma) for L1,2,3 (see Summary)
#-------------------------------------------------------------------------------
c1 <- function(Li, FWRK)
{
  if (FWRK == "EM")
  {
    if (Li == "L1")
      c1 = -5.5448979798087
    if (Li == "L2")
      c1 = -6.8859163415402
    if (Li == "L3")
      c1 = +1.012237318938304e+00
  }
  
  if (FWRK == "SEM")
  {
    if (Li == "L1")
      c1 = -98.8916378512805
    if (Li == "L2")
      c1 = -100.224961432011
  }
  
  return(c1)
}

#-------------------------------------------------------------------------------
# Return the value L of the distance between the two primaries
#-------------------------------------------------------------------------------
Ldist <- function(FWRK)
{
  if (FWRK == "SEM")
  {
    L = 149.60e6; #Sun-Earth distance in [km]
  }else{
    L = 384400;   #Earth-Moon distance in [km]
  }
  return(L)
}

#-------------------------------------------------------------------------------
# Return the value T of the period associated with the two primaries
#-------------------------------------------------------------------------------
Tcrtbp <- function(FWRK)
{
  if (FWRK == "SEM")
  {
    T = 3.155814950400000e+07;
  }else{
    T = 2.360584684800000e+06;
  }
  return(T)
}

#-------------------------------------------------------------------------------
# Transition from RATIO*T to DAYS
#-------------------------------------------------------------------------------
T2days <- function(RATIO, FWRK)
{
  return(RATIO*SEMperiod(FWRK)*Tcrtbp(FWRK)/(2*pi*86400))
}

#-------------------------------------------------------------------------------
# Return the value of the SEM period in the FWRK, in normalized units
#-------------------------------------------------------------------------------
SEMperiod <- function(FWRK)
{
  if (FWRK == "SEM")
  {
    T = 5.080085647283307e-01;
  }else{
    T = 6.791193871907917e+00;
  }
  return(T)
}

#-------------------------------------------------------------------------------
# From NC coordinates to C coordinates (Earth-Moon coordinates, 
# but still centered at Li)
#-------------------------------------------------------------------------------
NCtoC <- function(df, gamma)
{
  df$xC <- -gamma * (df$x);#+6.8859163415402);
  df$yC <- -gamma * (df$y);
  df$zC <- +gamma * (df$z);
  return(df)
}

#-------------------------------------------------------------------------------
# From NC coordinates to SYS coordinates
#-------------------------------------------------------------------------------
NCtoSYS <- function(df, gamma, c1)
{
  df$xEM <- -gamma * (df$x-c1);
  df$yEM <- -gamma * (df$y);
  df$zEM <- +gamma * (df$z);
  
  return(df)
}

#-------------------------------------------------------------------------------
# From EM (or C) to physical units
#-------------------------------------------------------------------------------
CtoPH <- function(df, L)
{
  df$xCPH = L * df$xC
  df$yCPH = L * df$yC
  df$zCPH = L * df$zC
  return(df)
}

#-------------------------------------------------------------------------------
# From EM to physical units
#-------------------------------------------------------------------------------
EMtoPH <- function(df, L)
{
  df$xPH = L * df$xEM
  df$yPH = L * df$yEM
  df$zPH = L * df$zEM
  return(df)
}

#-------------------------------------------------------------------------------
# From SYS to physical units
#-------------------------------------------------------------------------------
SYStoPH <- function(df, L)
{
  df$xPH = L * df$xEM
  df$yPH = L * df$yEM
  df$zPH = L * df$zEM
  return(df)
}


#-------------------------------------------------------------------------------
# Libration point as dataframes
#-------------------------------------------------------------------------------
# Function
dflibpoint <- function(LIB_POINT, FWRK)
{
  # Constants
  CST_GAMMA_LIB  = gamma(LIB_POINT, FWRK);
  CST_C1_LIB     = c1(LIB_POINT, FWRK);
  CST_DIST_PRIM  = Ldist(FWRK);
  
  # Dataframe with NC coordinats (0, 0, 0)
  li = data.frame(x_NC = 0.0, y_NC = 0.0, z_NC = 0.0)
  
  # Constants (complementary coordinates)
  if(LIB_POINT == "L1"){LIB_POINT_COMP = "L2"}else{LIB_POINT_COMP = "L1"}
  CST_GAMMA_LIB_COMP  = gamma(LIB_POINT_COMP, FWRK);
  CST_C1_LIB_COMP     = c1(LIB_POINT_COMP, FWRK);
  
  # System coordinates (either SEM or EM)
  li$x_SYS = -CST_GAMMA_LIB * (li$x_NC-CST_C1_LIB);
  li$y_SYS = -CST_GAMMA_LIB * (li$y_NC);
  li$z_SYS = +CST_GAMMA_LIB * (li$z_NC);
  
  # Physical coordinates
  li$x_PH = CST_DIST_PRIM * li$x_SYS
  li$y_PH = CST_DIST_PRIM * li$y_SYS
  li$z_PH = CST_DIST_PRIM * li$z_SYS
  
  # Physical coordinates centered on the libration point
  li$x_NCPH = CST_GAMMA_LIB * CST_DIST_PRIM * li$x_NC
  li$y_NCPH = CST_GAMMA_LIB * CST_DIST_PRIM * li$y_NC
  li$z_NCPH = CST_GAMMA_LIB * CST_DIST_PRIM * li$z_NC
  
  # Complementary coordinates
  li$x_NCC = -li$x_SYS/CST_GAMMA_LIB_COMP + CST_C1_LIB_COMP;
  li$y_NCC = -li$y_SYS/CST_GAMMA_LIB_COMP;
  li$z_NCC = -li$z_SYS/CST_GAMMA_LIB_COMP;
  
  return(li)
}

# All four points
dfeml1 = dflibpoint("L1", "EM")
dfeml2 = dflibpoint("L2", "EM")

dfseml1 = dflibpoint("L1", "SEM")
dfseml2 = dflibpoint("L2", "SEM")

#-------------------------------------------------------------------------------
# Primaries as dataframes
#-------------------------------------------------------------------------------
# Function (only Earth and Moon cases)
dfprimary <- function(NAME, LIB_POINT, FWRK)
{
  # Constants
  CST_GAMMA_LIB  = gamma(LIB_POINT, FWRK);
  CST_C1_LIB     = c1(LIB_POINT, FWRK);
  CST_DIST_PRIM  = Ldist(FWRK);
  
  #Earth case
  if(NAME == "Earth")
  {
    # System coordinates (either SEM or EM)
    if(FWRK == "SEM") dfprim = data.frame(x_SYS = -1 + muR("SEM"), y_SYS = 0.0, z_SYS = 0.0)
    if(FWRK == "EM")  dfprim = data.frame(x_SYS =    + muR("EM"),  y_SYS = 0.0, z_SYS = 0.0)
    
    # Natural constants
    R_NC =  6378.10  #radius in km
    O_NC = 150e6  #orbit radius in km
  }
  
  # Moon case
  if(NAME == "Moon")
  {
    # System coordinates (EM)
    dfprim = data.frame(x_SYS = -1 + muR("EM"), y_SYS = 0.0, z_SYS = 0.0)
    
    # Natural constants
    R_NC =  1737.10  #radius in km
    
    # Orbit radius in km
    # Mean radius of the orbit of the moon, in NCSEM coordinates: 2.518747349676265e-01
    O_NC = 2.518747349676265e-01*gamma("L2", "SEM")*Ldist("SEM")
  }
  

  # NC coordinates
  dfprim$x_NC = -dfprim$x_SYS/CST_GAMMA_LIB + CST_C1_LIB;
  dfprim$y_NC = -dfprim$y_SYS/CST_GAMMA_LIB;
  dfprim$z_NC = -dfprim$z_SYS/CST_GAMMA_LIB;
  dfprim$r_NC = R_NC/(CST_DIST_PRIM*CST_GAMMA_LIB)
  dfprim$o_NC = O_NC/(CST_DIST_PRIM*CST_GAMMA_LIB)
  
  
  # Physical coordinates
  dfprim$x_PH = CST_DIST_PRIM * dfprim$x_SYS
  dfprim$y_PH = CST_DIST_PRIM * dfprim$y_SYS
  dfprim$z_PH = CST_DIST_PRIM * dfprim$z_SYS
  dfprim$r_PH = R_NC
  dfprim$o_PH = O_NC
  
  
  # Physical coordinates centered on the libration point
  dfprim$x_NCPH = CST_GAMMA_LIB * CST_DIST_PRIM * dfprim$x_NC
  dfprim$y_NCPH = CST_GAMMA_LIB * CST_DIST_PRIM * dfprim$y_NC
  dfprim$z_NCPH = CST_GAMMA_LIB * CST_DIST_PRIM * dfprim$z_NC
  dfprim$r_NCPH = R_NC
  dfprim$o_NCPH = O_NC
  
  # SYS coordinates
  dfprim$o_SYS = O_NC/(CST_DIST_PRIM)
  dfprim$r_SYS = R_NC/(CST_DIST_PRIM)
  
  
  return(dfprim)
  
}

# All eight cases
dfearth_eml1 = dfprimary("Earth", "L1", "EM")
dfearth_eml2 = dfprimary("Earth", "L2", "EM")
dfearth_seml1 = dfprimary("Earth", "L1", "SEM")
dfearth_seml2 = dfprimary("Earth", "L2", "SEM")

dfmoon_eml1 = dfprimary("Moon", "L1", "EM")
dfmoon_eml2 = dfprimary("Moon", "L2", "EM")
dfmoon_seml1 = dfprimary("Moon", "L1", "SEM")
dfmoon_seml2 = dfprimary("Moon", "L2", "SEM")