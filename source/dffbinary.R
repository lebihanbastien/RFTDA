#
# Routine to get data from a binary file named FILENAME
# The data must be stored in NCOL columns, whose names will be NAMES
# 
# BLB 2016
dffbinary<- function(FILENAME, NCOL, NAMES)
{
  #-----------------------------------------------------------------------------
  #Estimate the size of the file
  #-----------------------------------------------------------------------------
  newdata = file(FILENAME, "rb")
  size = 1;
  while (length(a <- readBin(newdata, double(), n = 1000)) > 0) {
    size = size +1;
  }
  size = 1000*(size+1)/NCOL;
  close(newdata)
  
  #-----------------------------------------------------------------------------
  #Read the data in binary form
  #-----------------------------------------------------------------------------
  newdata = file(FILENAME, "rb")
  datavals = readBin(newdata, double(), n = NCOL * size)
  close(newdata)
  
  
  #-----------------------------------------------------------------------------
  #Columns names
  #-----------------------------------------------------------------------------
  dimnames <- list(
    row = seq(1,length(datavals)/NCOL),
    name = NAMES
  )
  
  #-----------------------------------------------------------------------------
  #Create matrix from array
  #-----------------------------------------------------------------------------
  matavals = matrix(datavals, ncol = NCOL, byrow = TRUE, dimnames = dimnames)
  #Create data.frame from matrix
  imap = data.frame(matavals)
  #Return data frame
  return(imap)
}


#
# Routine to get data from a binary file named FILENAME
#
# We make the following hypotheses on the data:
# 1. There exists i in seq(1, length(VNCOL)) such that the data are stored 
#    in VNCOL[i] columns, whose names will be LNAMES[[i]]
# 2. The first two values in the first column are identical, and are used 
#    as a test during the search of the "good" value of i.
#
# VNCOL and LNAMES must have the same lengths
#
# VNCOL is a vector, LNAMES is a list.
# 
# BLB 2016
dffbinaryv<- function(FILENAME, VNCOL, LNAMES, PRINT = TRUE)
{
  #-----------------------------------------------------------------------------
  #Estimate the size of the file
  #-----------------------------------------------------------------------------
  newdata = file(FILENAME, "rb")
  size = 1;
  while (length(a <- readBin(newdata, double(), n = 1000)) > 0) {
    size = size +1;
  }
  size = 1000*(size+1)/VNCOL;
  close(newdata)
  
  #-----------------------------------------------------------------------------
  # We check if any value of VNCOL matches. If the remainder is zero, 
  # then the number of columns in VNCOL[n] matches the size of the data.
  #-----------------------------------------------------------------------------
  NNCOL = length(VNCOL)
  
  for(n in seq(1, NNCOL, 1))
  {
    #---------------------------------------------------------------------------
    # We read the first data in binary form
    #---------------------------------------------------------------------------
    newdata = file(FILENAME, "rb")
    datavals = readBin(newdata, double(), n = VNCOL * 2)
    close(newdata)
    
    #-------------------------------------------------------------------------
    #We check that the first column has two consecutive equal values
    #-------------------------------------------------------------------------
    d1 = datavals[1]
    d2 = datavals[1 + VNCOL[n]]
    
    if(d1 == d2)
    {
      #-------------------------------------------------------------------------
      #Read again, but the full set
      #-------------------------------------------------------------------------
      newdata = file(FILENAME, "rb")
      datavals = readBin(newdata, double(), n = VNCOL * size)
      close(newdata)
      
      #-------------------------------------------------------------------------
      # Go on if the remainder is null
      #-------------------------------------------------------------------------
      remainder = length(datavals) %% VNCOL[n];
      if(remainder == 0)
      {
        #-----------------------------------------------------------------------
        # Print results if necessary
        #-----------------------------------------------------------------------
        if(PRINT)
        {
          print(paste0("dffbinaryv. Data was coherend with ", VNCOL[n], " columns."))
        }
          
        #-----------------------------------------------------------------------
        #Columns names
        #-----------------------------------------------------------------------
        dimnames <- list(
          row = seq(1,length(datavals)/VNCOL[n]),
          name = LNAMES[[n]]
        )
        
        #-----------------------------------------------------------------------
        #Create matrix from array if the two value are equal
        #-----------------------------------------------------------------------
        matavals = matrix(datavals, ncol = VNCOL[n], byrow = TRUE, dimnames = dimnames)
        
        #-----------------------------------------------------------------------
        #Create & Return data.frame from matrix
        #-----------------------------------------------------------------------
        imap = data.frame(matavals)
        return(imap)
      }
    }
    
  }
  
  #-----------------------------------------------------------------------------
  # If no values in VNCOL was good, we return an empty data.frame
  #-----------------------------------------------------------------------------
  #Create data.frame 
  imap = data.frame()
  #Return data frame
  return(imap)
  
}