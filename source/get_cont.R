# Routines to get the results of continuation procedures.
# BLB 2016, 2017

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
      
      #-------------------------------------------------------------------------
      # Then leg1
      #-------------------------------------------------------------------------
      proj_map_cont_leg1 = read.table(file = filename_cont_leg1, header = TRUE)
      #Get rid of first line, which is redundant with leg1
      proj_map_cont_leg1 = tail(proj_map_cont_leg1, nrow(proj_map_cont_leg1)-1)
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
  #Small postprocess (should be changed in the C++ code...)
  #-----------------------------------------------------------------------------
  proj_cont$thetae_NCSEM = proj_cont$te_NCSEM
  
  #-----------------------------------------------------------------------------
  #Rename of the rownames
  #-----------------------------------------------------------------------------
  if(nrow(proj_cont) > 0)
  {
    row.names(proj_cont) = seq(1, length(row.names(proj_cont)))
  }
  
  #-----------------------------------------------------------------------------
  #Return the dataframe
  #-----------------------------------------------------------------------------
  
  return(proj_cont)
}


#-------------------------------------------------------------------------------
# Routine to actually get traj_cont from file
#-------------------------------------------------------------------------------
get_traj_cont <- function(FILE_PREFIX_CONT_TRAJ, FILE_SUFFIX_CONT_TRAJ, FAMILY="", PRINT)
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
  
  VNAMES = list(names_17, names_16, names_8)
  VNCOL  = c(17, 16, 8)
  
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
      
      
      #-------------------------------------------------------------------------
      # Then leg1
      #-------------------------------------------------------------------------
      #Read table
      proj_map_cont_leg1 = dffbinaryv(filename_cont_leg1, VNCOL, VNAMES, PRINT)
      #Get rid of first line, which is redundant with leg1
      proj_map_cont_leg1 = tail(proj_map_cont_leg1, nrow(proj_map_cont_leg1)-1)
      #Change the labels
      proj_map_cont_leg1$label = max(traj_cont$label) + proj_map_cont_leg1$label
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
  
  #---------------------------------------------------------------------------
  #Post process
  #---------------------------------------------------------------------------
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
  }
  return(traj_cont)
}
