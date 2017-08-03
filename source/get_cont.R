# Routines to get the results of continuation procedures.
# BLB 2016, 2017

#------------------------------------------------------------------------------#
# To get r0_CMU_EMT between [0.5, 1.5]
#------------------------------------------------------------------------------#
r0_modulo <- function(r0_CMU_EMT){
  
  r0_CMU_EMT_m = r0_CMU_EMT
  for(i in seq(1, length(r0_CMU_EMT_m)))
  {
    if(r0_CMU_EMT_m[i] < 0.5)
    {
      r0_CMU_EMT_m[i] = r0_CMU_EMT_m[i] + 1.0
    }
  }
  
  return(r0_CMU_EMT_m)
}


#-------------------------------------------------------------------------------
# Routine to actually get proj_cont from file
#-------------------------------------------------------------------------------
get_proj_cont <- function(FILE_PREFIX_CONT, FILE_SUFFIX_CONT, FAMILY="")
{
  #-----------------------------------------------------------------------------
  #Name of the data file
  #-----------------------------------------------------------------------------
  filename_cont = paste0(FILE_PREFIX_CONT, FILE_SUFFIX_CONT, FAMILY, ".txt");
  
  #-----------------------------------------------------------------------------
  # 4 possibilities:
  #
  #  - The datafile exists, e.g.:
  #         ".../Serv/cont_atf_order_20_dest_L2_t0_0.03.txt"
  #         ".../Serv/cont_atf_order_20_dest_L2_t0_0.03_fam2.txt"
  #
  #  - The datafile is here but split in two files, of the form:
  #         ".../Serv/cont_atf_order_20_dest_L2_t0_0.03_leg1.txt"
  #         ".../Serv/cont_atf_order_20_dest_L2_t0_0.03_leg2.txt"
  #
  #  - The datafile is here but split in several files
  #
  #  - The datafile does not exist.
  #
  #-----------------------------------------------------------------------------
  if (file.exists(filename_cont))
  {
    #Read table
    proj_cont = read.table(file = filename_cont, header = TRUE)
    
    #Label of the seed
    if(("label" %in% colnames(proj_cont)))
    {
      proj_cont$seed = min(proj_cont$label)
    }
    
  }else #we try the split leg1/leg2 option or several splits option
  {
    #---------------------------------------------------------------------------
    # Flag to check if data was found
    #---------------------------------------------------------------------------
    istheredata = FALSE
    
    #---------------------------------------------------------------------------
    # If there is only leg1 & leg2
    #---------------------------------------------------------------------------
    filename_cont_leg2 = paste0(FILE_PREFIX_CONT, FILE_SUFFIX_CONT, FAMILY, "_leg2.txt");   
    filename_cont_leg1 = paste0(FILE_PREFIX_CONT, FILE_SUFFIX_CONT, FAMILY, "_leg1.txt");
    filename_cont_leg3 = paste0(FILE_PREFIX_CONT, FILE_SUFFIX_CONT, FAMILY, "_leg3.txt");
    
    if(file.exists(filename_cont_leg2) && file.exists(filename_cont_leg1) && !file.exists(filename_cont_leg3))
    {
      #-------------------------------------------------------------------------
      # There is data
      #-------------------------------------------------------------------------
      istheredata = TRUE
      
      #-------------------------------------------------------------------------
      # leg2 first
      #-------------------------------------------------------------------------
      #Read the leg2
      proj_cont = read.table(file = filename_cont_leg2, header = TRUE)
      #Reverse table
      proj_cont = proj_cont[rev(rownames(proj_cont)),]
      #Change the labels
      if(("label" %in% colnames(proj_cont)))
      {
          proj_cont$label = max(proj_cont$label) - proj_cont$label
      }
      
      #-------------------------------------------------------------------------
      # Then leg1
      #-------------------------------------------------------------------------
      proj_map_cont_leg1 = read.table(file = filename_cont_leg1, header = TRUE)
      #Get rid of first line, which is redundant with leg1
      proj_map_cont_leg1 = tail(proj_map_cont_leg1, nrow(proj_map_cont_leg1)-1)
      
      #Label of the seed
      if(("label" %in% colnames(proj_cont)))
      {
        proj_cont$seed          = max(proj_cont$label)
        proj_map_cont_leg1$seed = max(proj_cont$label)
      }

      #Change the labels
      if(("label" %in% colnames(proj_cont)))
      {
        proj_map_cont_leg1$label = max(proj_cont$label) + proj_map_cont_leg1$label
      }
      
      #Rbind with leg1
      proj_cont = rbind(proj_cont, proj_map_cont_leg1) 
      
      
      
    }else
    {
      #-------------------------------------------------------------------------
      # Else, there is more than two legs
      #-------------------------------------------------------------------------
      #Flag to detect the first datafile
      flag = 0
      
      #Loop on several legs (up to leg10)
      for(n in seq(1, 10, 1))
      {
        #Try the filename associated to legn
        filename_cont = paste0(FILE_PREFIX_CONT, 
                               FILE_SUFFIX_CONT, FAMILY, "_leg", n, ".txt");
        
        #Check if the filename exists
        if (file.exists(filename_cont))
        {
          #---------------------------------------------------------------------
          # There is data
          #---------------------------------------------------------------------
          istheredata = TRUE
          
          #---------------------------------------------------------------------
          # TWe can read it
          #---------------------------------------------------------------------
          if(flag == 0)
          {
            #Reset the flag
            flag = 1
            
            #Read table
            proj_cont = read.table(file = filename_cont, header = TRUE)
            
            #             #Reverse table if s1 is increasing
            #             if(proj_cont$s1_CMU_EM[1] < proj_cont$s1_CMU_EM[2])
            #             {
            #               proj_cont = proj_cont[rev(rownames(proj_cont)),]
            #             }
            
          }else
          {
            #Read table
            proj_map_cont_legn = read.table(file = filename_cont, header = TRUE)
            
            #             #Reverse table if s1 is increasing
            #             if(proj_map_cont_legn$s1_CMU_EM[1] < proj_map_cont_legn$s1_CMU_EM[2])
            #             {
            #               proj_map_cont_legn = proj_map_cont_legn[rev(rownames(proj_map_cont_legn)),]
            #             }
            
            #Rbind with proj_cont
            proj_cont = rbind(proj_cont, proj_map_cont_legn)
            
          }
        }
      }
    }
    
    #---------------------------------------------------------------------------
    #Check if there is actually some data. If not, empty dataframe!
    #---------------------------------------------------------------------------
    if(!istheredata)
    {
      proj_cont = data.frame()
    }
    
  }
  
  #-----------------------------------------------------------------------------
  #Rename of the rownames
  #-----------------------------------------------------------------------------
  if(nrow(proj_cont) > 0)
  {
    row.names(proj_cont) = seq(1, length(row.names(proj_cont)))
  }
  
  
  #-----------------------------------------------------------------------------
  #Post process
  #-----------------------------------------------------------------------------
  if(!empty(proj_cont))
  {
    #Small postprocess (should be changed in the C++ code...)
    proj_cont$thetae_NCSEM = proj_cont$te_NCSEM
    
    # Initial phase
    if(("t0_CMU_EM" %in% colnames(proj_cont)))
    {
      proj_cont$r0_CMU_EMT     = proj_cont$t0_CMU_EM/SEMperiod("EM")
      proj_cont$r0_CMU_EMT     = round(proj_cont$r0_CMU_EMT %% 1, digits= 4)
      proj_cont$r0_CMU_EMT_mod = r0_modulo(proj_cont$r0_CMU_EMT)
    }
    
    # If the Pk section results are present
    if(("te_NCEM" %in% colnames(proj_cont)))
    {
      # Time at the Pk section, given as x T
      proj_cont$te_CMU_EMT  = proj_cont$te_NCEM/SEMperiod("EM")
      # Time at the Pk section, given as x T, in [0, 1]
      proj_cont$re_CMU_EMT  = round(proj_cont$te_CMU_EMT  %% 1, digits= 4)
    }
    
    # If the Pk section results are present
    if(("te_NCSEM" %in% colnames(proj_cont)))
    {
      # Time at the Pk section, given as x T
      proj_cont$te_CMU_SEMT = proj_cont$te_NCSEM/SEMperiod("SEM")
      # Time at the Pk section, given as x T, in [0, 1]
      proj_cont$re_CMU_SEMT = round(proj_cont$te_CMU_SEMT %% 1, digits= 4)
    }
    
    # Label
    if(("label" %in% colnames(proj_cont)))
    {
      # Time at the Pk section, given as x T
      proj_cont$label.conn = proj_cont$label
    }
    
  }
  
  
  #-----------------------------------------------------------------------------
  #Return the dataframe
  #-----------------------------------------------------------------------------
  return(proj_cont)
}


#-------------------------------------------------------------------------------
# Routine to actually get traj_cont from file
#-------------------------------------------------------------------------------
get_traj_cont <- function(FILE_PREFIX_CONT_TRAJ, FILE_SUFFIX_CONT_TRAJ, FAMILY="", PRINT=TRUE)
{
  #-----------------------------------------------------------------------------
  # Column names
  #-----------------------------------------------------------------------------
  names_8  = c("label",  "t_CMU_SEM", "x_CMS_NCSEM", "y_CMS_NCSEM", "z_CMS_NCSEM", 
               "px_CMS_NCSEM", "py_CMS_NCSEM", "pz_CMS_NCSEM")
  
  names_16 = c("label",  "type", "t_CMU_SEM", 
               "x_CMS_NCSEM", "y_CMS_NCSEM", "z_CMS_NCSEM", 
               "px_CMS_NCSEM", "py_CMS_NCSEM", "pz_CMS_NCSEM",
               "x_CMS_NCEM", "y_CMS_NCEM", "z_CMS_NCEM", 
               "px_CMS_NCEM", "py_CMS_NCEM", "pz_CMS_NCEM","H_NCSEM")
  
  names_17 = c(names_16, "r0_CMU_EMT")
  
  names_37 = c(names_17, 
               "te_SEM",  "xe_NCSEM", "ye_NCSEM", "ze_NCSEM", "pxe_NCSEM", "pye_NCSEM", "pze_NCSEM",
               "vxe_NCSEM", "vye_NCSEM", "vze_NCSEM",
               "te_EM",  "xe_NCEM", "ye_NCEM", "ze_NCEM", "pxe_NCEM", "pye_NCEM", "pze_NCEM",
               "vxe_NCEM", "vye_NCEM", "vze_NCEM")
  
  VNAMES = list(names_37, names_17, names_16, names_8)
  VNCOL  = c(37, 17, 16, 8)
  
  #-----------------------------------------------------------------------------
  # Build the filename 
  #-----------------------------------------------------------------------------
  filepre_cont_traj  = paste0(FILE_PREFIX_CONT_TRAJ, FILE_SUFFIX_CONT_TRAJ, FAMILY);
  filename_cont_traj = paste0(filepre_cont_traj, ".bin");
  
  #-----------------------------------------------------------------------------
  # 4 possibilities:
  #
  #  - The datafile exists, e.g.:
  #         ".../Serv/cont_atf_order_20_dest_L2_t0_0.03.txt"
  #         ".../Serv/cont_atf_order_20_dest_L2_t0_0.03_fam2.txt"
  #
  #  - The datafile is here but split in two files, of the form:
  #         ".../Serv/cont_atf_order_20_dest_L2_t0_0.03_leg1.txt"
  #         ".../Serv/cont_atf_order_20_dest_L2_t0_0.03_leg2.txt"
  #
  #  - The datafile is here but split in several files
  #
  #  - The datafile does not exist.
  #
  #-----------------------------------------------------------------------------
  if (file.exists(filename_cont_traj))
  {
    #Read table
    traj_cont = dffbinaryv(filename_cont_traj, VNCOL, VNAMES, PRINT)
    
  }else #we try the split leg1/leg2 option or several splits option
  {
    #---------------------------------------------------------------------------
    # Flag to check if data was found
    #---------------------------------------------------------------------------
    istheredata = FALSE
    
    #---------------------------------------------------------------------------
    # If there is only leg1 & leg2
    #---------------------------------------------------------------------------
    filename_cont_leg1 = paste0(filepre_cont_traj, "_leg1.bin");
    filename_cont_leg2 = paste0(filepre_cont_traj, "_leg2.bin");   
    filename_cont_leg3 = paste0(filepre_cont_traj, "_leg3.bin");
    
    if(file.exists(filename_cont_leg2) && file.exists(filename_cont_leg1) && !file.exists(filename_cont_leg3))
    {
      #-------------------------------------------------------------------------
      # There is data
      #-------------------------------------------------------------------------
      istheredata = TRUE
      
      #-------------------------------------------------------------------------
      # leg2 first
      #-------------------------------------------------------------------------
      #Read table
      traj_cont = dffbinaryv(filename_cont_leg2, VNCOL, VNAMES, PRINT)
      #Reverse table
      traj_cont = traj_cont[rev(rownames(traj_cont)),]
      #Change the labels
      traj_cont$label = max(traj_cont$label) - traj_cont$label
      
      #print(colnames(traj_cont))
      
      #-------------------------------------------------------------------------
      # Then leg1
      #-------------------------------------------------------------------------
      #Read table
      proj_map_cont_leg1 = dffbinaryv(filename_cont_leg1, VNCOL, VNAMES, PRINT)
      #Get rid of first line, which is redundant with leg1
      proj_map_cont_leg1 = tail(proj_map_cont_leg1, nrow(proj_map_cont_leg1)-1)
      #Change the labels
      proj_map_cont_leg1$label = max(traj_cont$label) + proj_map_cont_leg1$label
      
      #print(colnames(proj_map_cont_leg1))
      
      #Rbind with leg1
      traj_cont = rbind(traj_cont, proj_map_cont_leg1)
      
    }else
    {
      #-------------------------------------------------------------------------
      # Else, there is more than two legs
      #-------------------------------------------------------------------------
      #Flag to detect the first datafile
      flag = 0
      
      #Loop on several legs (up to leg10)
      for(n in seq(1, 10, 1))
      {
        #Try the filename associated to legn
        filename_cont = paste0(filepre_cont_traj, "_leg", n, ".bin");
        
        #Check if the filename exists
        if (file.exists(filename_cont))
        {
          #---------------------------------------------------------------------
          # There is data
          #---------------------------------------------------------------------
          istheredata = TRUE
          
          #---------------------------------------------------------------------
          # We can read it
          #---------------------------------------------------------------------
          if(flag == 0)
          {
            #Reset the flag
            flag = 1
            #Read table
            traj_cont = dffbinaryv(filename_cont, VNCOL, VNAMES, PRINT)
            # Labels
            traj_cont$label = traj_cont$label + 1000*n
            
          }else
          {
            #Read table
            proj_map_cont_legn = dffbinaryv(filename_cont, VNCOL, VNAMES, PRINT)
            # Labels
            proj_map_cont_legn$label = proj_map_cont_legn$label + 1000*n
            #Rbind with proj_cont
            traj_cont = rbind(traj_cont, proj_map_cont_legn)
          }
        }
      }
    }
    
    #---------------------------------------------------------------------------
    #Check if there is actually some data. If not, empty dataframe!
    #---------------------------------------------------------------------------
    if(!istheredata)
    {
      traj_cont = data.frame()
    }
  }
  
  #-----------------------------------------------------------------------------
  #Post process
  #-----------------------------------------------------------------------------
  if(!empty(traj_cont))
  {
    #===========================================================================
    # NCSEM to SEM
    #===========================================================================
    traj_cont$x_CMS_SEM <- -(traj_cont$x_CMS_NCSEM - CST_C1_LIB_SEM)*CST_GAMMA_LIB_SEM  
    traj_cont$y_CMS_SEM <- - traj_cont$y_CMS_NCSEM                  *CST_GAMMA_LIB_SEM
    traj_cont$z_CMS_SEM <- + traj_cont$z_CMS_NCSEM                  *CST_GAMMA_LIB_SEM 
    
    #===========================================================================
    # SEM to SISEM
    #===========================================================================
    traj_cont$x_CMS_SI_SEM <- traj_cont$x_CMS_SEM * CST_DIST_PRIM_SEM
    traj_cont$y_CMS_SI_SEM <- traj_cont$y_CMS_SEM * CST_DIST_PRIM_SEM
    traj_cont$z_CMS_SI_SEM <- traj_cont$z_CMS_SEM * CST_DIST_PRIM_SEM
    
    #===========================================================================
    # NCSEM to SINCSEM
    #===========================================================================
    traj_cont$x_CMS_SI_NCSEM <- traj_cont$x_CMS_NCSEM * CST_DIST_PRIM_SEM * CST_GAMMA_LIB_SEM
    traj_cont$y_CMS_SI_NCSEM <- traj_cont$y_CMS_NCSEM * CST_DIST_PRIM_SEM * CST_GAMMA_LIB_SEM 
    traj_cont$z_CMS_SI_NCSEM <- traj_cont$z_CMS_NCSEM * CST_DIST_PRIM_SEM * CST_GAMMA_LIB_SEM
    
    #===========================================================================
    # NCEM to EM
    #===========================================================================
    traj_cont$x_CMS_EM <- -(traj_cont$x_CMS_NCEM - CST_C1_LIB_EM)*CST_GAMMA_LIB_EM  
    traj_cont$y_CMS_EM <- - traj_cont$y_CMS_NCEM                 *CST_GAMMA_LIB_EM
    traj_cont$z_CMS_EM <- + traj_cont$z_CMS_NCEM                 *CST_GAMMA_LIB_EM 
    
    #===========================================================================
    # EM to SIEM
    #===========================================================================
    traj_cont$x_CMS_SI_EM <- traj_cont$x_CMS_EM * CST_DIST_PRIM_EM
    traj_cont$y_CMS_SI_EM <- traj_cont$y_CMS_EM * CST_DIST_PRIM_EM
    traj_cont$z_CMS_SI_EM <- traj_cont$z_CMS_EM * CST_DIST_PRIM_EM
    
    #===========================================================================
    # NCEM to SINCEM
    #===========================================================================
    traj_cont$x_CMS_SI_NCEM <- traj_cont$x_CMS_NCEM * CST_DIST_PRIM_EM * CST_GAMMA_LIB_EM
    traj_cont$y_CMS_SI_NCEM <- traj_cont$y_CMS_NCEM * CST_DIST_PRIM_EM * CST_GAMMA_LIB_EM 
    traj_cont$z_CMS_SI_NCEM <- traj_cont$z_CMS_NCEM * CST_DIST_PRIM_EM * CST_GAMMA_LIB_EM
    
    #===========================================================================
    # Just to be sure
    #===========================================================================
    if("r0_CMU_EMT" %in% colnames(traj_cont))
    {
      traj_cont$r0_CMU_EMT     = round(traj_cont$r0_CMU_EMT %% 1, digits= 4)
      traj_cont$r0_CMU_EMT_mod = r0_modulo(traj_cont$r0_CMU_EMT)
    }
    
    # If the Pk section results are present
    if(("te_EM" %in% colnames(traj_cont)))
    {
      # Time at the Pk section, given as x T
      traj_cont$te_CMU_EMT  = traj_cont$te_EM/SEMperiod("EM")
      # Time at the Pk section, given as x T, in [0, 1]
      traj_cont$re_CMU_EMT  = round(traj_cont$te_CMU_EMT  %% 1, digits= 4)
      # Same but between 0.5 and 1.5
      traj_cont$re_CMU_EMT_mod = r0_modulo(traj_cont$re_CMU_EMT)
    }
    
    # Label
    if(("label" %in% colnames(traj_cont)))
    {
      # Time at the Pk section, given as x T
      traj_cont$label.conn = traj_cont$label
    }
    
  }
  return(traj_cont)
}


#-------------------------------------------------------------------------------
# Routine to actually get traj_comp from file (complete trajectory, either in QBCP or JPL)
#-------------------------------------------------------------------------------
get_traj_comp <- function(FILE_PREFIX_T, FILE_SUFFIX_T, FAMILY="", PRINT=TRUE)
{
  #-----------------------------------------------------------------------------
  # Column names
  #-----------------------------------------------------------------------------
  #-----------------------------------------------------------------------------
  # Column names
  #-----------------------------------------------------------------------------
  names_17 = c(  "label",  "coord", "t_eph", 
                 "t_SEM",  "x_NCSEM", "y_NCSEM", "z_NCSEM", "px_NCSEM", "py_NCSEM", "pz_NCSEM",
                 "t_EM", "x_NCEM", "y_NCEM", "z_NCEM", "px_NCEM", "py_NCEM", "pz_NCEM")
  names_37 = c(names_17, 
               "te_SEM",  "xe_NCSEM", "ye_NCSEM", "ze_NCSEM", "pxe_NCSEM", "pye_NCSEM", "pze_NCSEM",
               "vxe_NCSEM", "vye_NCSEM", "vze_NCSEM",
               "te_EM",  "xe_NCEM", "ye_NCEM", "ze_NCEM", "pxe_NCEM", "pye_NCEM", "pze_NCEM",
               "vxe_NCEM", "vye_NCEM", "vze_NCEM")
  
  VNAMES = list(names_37, names_17)
  VNCOL  = c(37, 17)
  
  #-----------------------------------------------------------------------------
  # Build the filename 
  #-----------------------------------------------------------------------------
  filepre_cont_traj  = paste0(FILE_PREFIX_T, FILE_SUFFIX_T, FAMILY);
  filename_cont_traj = paste0(filepre_cont_traj, ".bin");
  
  #-----------------------------------------------------------------------------
  # Read data
  #-----------------------------------------------------------------------------
  if (file.exists(filename_cont_traj))
  {
    #Read table
    traj_cont = dffbinaryv(filename_cont_traj, VNCOL, VNAMES, PRINT) #dffbinary(filename_cont_traj, 17, names)
    
  }
  
  #-----------------------------------------------------------------------------
  #Post process
  #-----------------------------------------------------------------------------
  if(!empty(traj_cont))
  {
    # If the Pk section results are present
    if(("te_EM" %in% colnames(traj_cont)))
    {
      # Time at the Pk section, given as x T
      traj_cont$te_CMU_EMT  = traj_cont$te_EM/SEMperiod("EM")
      # Time at the Pk section, given as x T, in [0, 1]
      traj_cont$re_CMU_EMT  = round(traj_cont$te_CMU_EMT  %% 1, digits= 4)
      # Same but between 0.5 and 1.5
      traj_cont$re_CMU_EMT_mod = r0_modulo(traj_cont$re_CMU_EMT)
    }
    
    # Label
    if(("label" %in% colnames(traj_cont)))
    {
      # Time at the Pk section, given as x T
      traj_cont$label.conn = traj_cont$label
    }
    
    # # If the Pk section results are present
    # if(("te_SEM" %in% colnames(traj_cont)))
    # {
    #   # Time at the Pk section, given as x T
    #   traj_cont$te_CMU_SEMT  = traj_cont$te_SEM/SEMperiod("SEM")
    #   # Time at the Pk section, given as x T, in [0, 1]
    #   traj_cont$re_CMU_SEMT  = round(traj_cont$te_CMU_SEMT  %% 1, digits= 4)
    # }
    

  }
  
  return(traj_cont)
}
