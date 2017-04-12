# Rbind with common columns
#
#
# BLB 2017, from Richie Cotton

rbind_cc <- function(dfr1, dfr2)
{
  if(empty(dfr1) || empty(dfr2))
  {
    dfr3 = rbind(dfr1, dfr2)
  }else
  {
    common_cols <- intersect(colnames(dfr1), colnames(dfr2))
    dfr3 = rbind(
      subset(dfr1, select = common_cols), 
      subset(dfr2, select = common_cols)
    )
  }
    
  return(dfr3)
}  