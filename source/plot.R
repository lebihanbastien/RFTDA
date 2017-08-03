#------------------------------------------------
# Load libraries
#------------------------------------------------
library(plyr)
library(ggplot2)
library(reshape2)
library(scales)
library(grid)

#------------------------------------------------
# Load Source files
#------------------------------------------------
source("source/folder.R")

#------------------------------------------------------------------------------#
# color-blind-friendly palettes
#------------------------------------------------------------------------------#
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
#scale_fill_manual(values=cbPalette)

# To use for line and point colors, add
#scale_colour_manual(values=cbPalette)

#------------------------------------------------------------------------------#
# Default size
#------------------------------------------------------------------------------#
xSize = 9.01
ySize = 7.66

#------------------------------------------------------------------------------#
#Constants
#------------------------------------------------------------------------------#
#Fontsize
fontsize = list(title = 20,    #fontsize for title
                label = 20,
                legend = 18,
                big   = 30) 

#Linesize
linesize = list(line = 2, point = 2)

#------------------------------------------------------------------------------#
#Themes
#------------------------------------------------------------------------------#
#Base custom theme
custom_theme = theme(
  #Background
  plot.background = element_rect(fill = "transparent", colour = NA),
  #Axes
  axis.text.x  = element_text(colour="grey20", size=fontsize[["label"]], angle=0, hjust = 0.5, vjust=  0.5),#, margin = margin(10,1,1,1)),
  axis.text.y  = element_text(colour="grey20", size=fontsize[["label"]], angle=0, hjust = 1,   vjust=  0.5),#, margin = margin(1,5,1,1)),
  axis.title.x = element_text(colour="grey20", size=fontsize[["label"]], vjust =  0.5, margin = margin(20,1,1,1)),
  axis.title.y = element_text(colour="grey20", size=fontsize[["label"]], vjust =  0.5, margin = margin(1,15,1,1)),
  #Legend
  legend.text  = element_text(colour="grey20", size=fontsize[["legend"]], hjust = 0.0),
  legend.title = element_text(colour="grey20", size=fontsize[["legend"]]),
  legend.title.align   = 0.5,
  legend.key.size      = unit(1, 'cm'),
  legend.key.width     = unit(2, 'cm'),
  legend.background = element_rect(fill = "transparent", colour = NA)
)

#BW custom theme
custom_bw_theme = theme_bw() + theme(
  #Background
  plot.background = element_rect(fill = "transparent", colour = NA),
  #Axes
  axis.text.x  = element_text(colour="grey20", size=fontsize[["label"]], angle=0, hjust = 0.5, vjust=  0.5),#, margin = margin(10,1,1,1)),
  axis.text.y  = element_text(colour="grey20", size=fontsize[["label"]], angle=0, hjust = 1,   vjust=  0.5),#, margin = margin(1,5,1,1)),
  axis.title.x = element_text(colour="grey20", size=fontsize[["label"]], vjust =  0.5, margin = margin(20,1,1,1)),
  axis.title.y = element_text(colour="grey20", size=fontsize[["label"]], vjust =  0.5, margin = margin(1,15,1,1)),
  #Legend
  legend.text  = element_text(colour="grey20", size=fontsize[["legend"]], hjust = 0.0),
  legend.title = element_text(colour="grey20", size=fontsize[["legend"]]),
  legend.title.align   = 0.5,
  legend.key.size      = unit(1, 'cm'),
  legend.key.width     = unit(2, 'cm'),
  legend.background = element_rect(fill = "transparent", colour = NA)
)

#ISSFD theme
issfd_theme = theme(
  #All in Times New Roman
  text=element_text(family="Times New Roman"),
  #Axes in italic
  axis.title.x  = element_text(family="Times New Roman", face="italic"),
  axis.title.y  = element_text(family="Times New Roman", face="italic"),
  legend.title  = element_text(family="Times New Roman", face="italic")
)

#Big fong
big_font_theme = theme(
  #Background
  plot.background = element_rect(fill = "transparent", colour = NA),
  #Axes
  axis.text.x  = element_text(colour="grey20", size=fontsize[["big"]] , angle=0, hjust = 0.5, vjust=  0.5, margin = margin(10,1,1,1)),
  axis.text.y  = element_text(colour="grey20", size=fontsize[["big"]] , angle=0, hjust = 1,   vjust=  0.5, margin = margin(1,5,1,1)),
  axis.title.x = element_text(colour="grey20", size=fontsize[["big"]] , vjust = 0.5, margin = margin(20,1,1,1)),
  axis.title.y = element_text(colour="grey20", size=fontsize[["big"]] , vjust = 0.5, margin = margin(1,20,1,1)),
  #Legend
  legend.text  = element_text(colour="grey20", size=fontsize[["big"]], hjust = 0.0),
  legend.title = element_text(colour="grey20", size=fontsize[["big"]]),
  legend.title.align   = 0.5,
  legend.key.size      = unit(1, 'cm'),
  legend.key.width     = unit(2, 'cm')
)

#With legend inside the plot
legend_inside_theme = custom_theme + theme(
  legend.justification = c(1,0),          #legend inside plot
  legend.position      = c(1,0),          #legend inside plot
  legend.background = element_rect(fill = "white", colour = "white")
)

#With legend inside the plot
legend_left_theme = custom_theme + theme(
  legend.justification = c(0,0),         #legend inside plot
  legend.position      = c(0,0)          #legend inside plot
)


#Blank theme (for insets, for ex)
blank_theme = theme(
  axis.text.x  = element_blank(),
  axis.text.y  = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  legend.text  = element_blank(),
  legend.title = element_blank(),
  plot.title = element_text(colour="grey20", size=fontsize[["title"]]),
  plot.background=element_blank(),
  axis.ticks=element_blank()
)

# Position of the legen
legend_pos <- function(vec)
{
  legend = theme(
    legend.justification = vec,          #legend inside plot
    legend.position      = vec,          #legend inside plot
    legend.background = element_rect(fill = "white", colour = "white"))
  return(legend)
}

# Darker background
theme_pgrey <- function(color)
{
  th = custom_theme + theme(panel.background = element_rect(fill = color), legend.key = element_rect(fill = color))
  return(th)
}


# Set CM fonts
set_font_cm <- function(ppt)
{
  ppt = ppt + theme(
                    # Titles
                    axis.title.x  = element_text(family="jsMath-cmmi10", size=fontsize[["label"]]+5),
                    axis.title.y  = element_text(family="jsMath-cmmi10", size=fontsize[["label"]]+5)
                    # # Figures
                    # axis.text.y = element_text(family="jsMath-cmr10"),
                    # axis.text.x = element_text(family="jsMath-cmr10")
                    )
  return(ppt)
}

# Set CM fonts (x)
set_font_cm_x <- function(ppt)
{
  ppt = ppt + theme(
    # Titles
    axis.title.x  = element_text(family="jsMath-cmmi10", size=fontsize[["label"]]+5)
  )
  return(ppt)
}

# Set CM fonts (y)
set_font_cm_y <- function(ppt)
{
  ppt = ppt + theme(
    # Titles
    axis.title.y  = element_text(family="jsMath-cmmi10", size=fontsize[["label"]]+5)
  )
  return(ppt)
}

# Set CM fonts (extended)
set_font_cm_ex <- function(ppt)
{
  ppt = ppt + theme(
    # Titles
    axis.title.x  = element_text(family="jsMath-cmmi10", size=fontsize[["label"]]+5),
    axis.title.y  = element_text(family="jsMath-cmmi10", size=fontsize[["label"]]+5),
    legend.title  = element_text(family="jsMath-cmmi10", size=fontsize[["legend"]]+5)
    # # Figures
    # axis.text.y = element_text(family="jsMath-cmr10"),
    # axis.text.x = element_text(family="jsMath-cmr10")
  )
  return(ppt)
}


# Set CM fonts (just the legend)
set_font_cm_leg <- function(ppt)
{
  ppt = ppt + theme(
    # Titles
    legend.title  = element_text(family="jsMath-cmmi10", size=fontsize[["legend"]]+5)
  )
  return(ppt)
}


# Annotations in black and white
annotate_bold <- function(ppt, x = 0, y = 0, label ="Default", size = 5, parse = FALSE)
{
  if(parse)
  {
    ppt = ppt + annotate("text", x = x, y = y,  label = paste0("bold(", label, ")"), colour = "white", size = size, parse = TRUE)
    ppt = ppt + annotate("text", x = x, y = y,  label = label, colour = "black", size = size, parse = TRUE)
  }else
  {
    ppt = ppt + annotate("text", x = x, y = y, label = label, colour = "white", size = size+0.1)
    ppt = ppt + annotate("text", x = x, y = y, label = label, size = size)
  }
  
  return(ppt)
}

#------------------------------------------------------------------------------#
# Plot function for line plots
#------------------------------------------------------------------------------#
plotdf_line<- function(ttm,              #dataframe
                       colx,             #x
                       coly,             #y
                       xlabel,           #xlabel
                       ylabel,           #ylabel
                       colorCol,         #column for color scaling
                       colorLabel,       #associated label
                       isColorFac,       #is color factorized
                       lineTypeCol,      #column for line type
                       lineTypeLabel,    #associated label
                       lineSize,         #size of the lines
                       lineType)         #type of the lines
{
  #Ggplot init
  #-----------------------------------------------------------------------------
  px = ggplot()
  
  #Changing temporarily the names of the desired columns (x & y)
  #-----------------------------------------------------------------------------
  i1 = which(colnames(ttm) == colx)
  i2 = which(colnames(ttm) == coly)
  colnames(ttm)[i1] = "temp1"
  colnames(ttm)[i2] = "temp2"
  
  #LineType & LineSize
  #-----------------------------------------------------------------------------
  if(!missing(lineSize)) 
  {
    lSize = lineSize
  }else{
    lSize = linesize[["line"]]
  }
  
  #LineType & LineSize
  #-----------------------------------------------------------------------------
  if(!missing(lineType)) 
  {
    lType = lineType
  }else{
    lType = "solid"
  }
  
  
  #Plot
  #-----------------------------------------------------------------------------
  if(!missing(colorCol)) #if a color scheme is provided
  {
    #Changing temporarily the names of the desired columns (colorCol)
    i1 = which(colnames(ttm) == colorCol)
    colnames(ttm)[i1] = "colorCol"
    
    if(!missing(lineTypeCol)) #if a linetype scheme is provided
    {
      #If there is no troubleshooting between lineTypeCol & colorCol
      if(colorCol != lineTypeCol)
      {
        #Changing temporarily the names of the desired columns (lineTypeCol)
        i2 = which(colnames(ttm) == lineTypeCol)
        colnames(ttm)[i2] = "lineTypeCol"
        
        if(!missing(isColorFac) & isColorFac == 0)   #if the boolean isColorFac is provided and is false, continuous color scale
          px = px + geom_line(data = ttm, 
                              aes(x = temp1, y = temp2, color = colorCol, linetype = factor(lineTypeCol)),
                              size = lSize)
        else  #discrete color scale otherwise
          px = px + geom_line(data = ttm, 
                              aes(x = temp1, y = temp2, color = factor(colorCol), linetype = factor(lineTypeCol)), 
                              size = lSize)
      }else #otherwise, use colorCol for both!
      {
        if(!missing(isColorFac) & isColorFac == 0)   #if the boolean isColorFac is provided and is false, continuous color scale
          px = px + geom_line(data = ttm, 
                              aes(x = temp1, y = temp2, color = colorCol, linetype = factor(colorCol)), 
                              size = lSize)
        else  #discrete color scale otherwise
          px = px + geom_line(data = ttm, 
                              aes(x = temp1, y = temp2, color = factor(colorCol), linetype = factor(colorCol)), 
                              size = lSize)
      }
      
      #Linetype label if necessary
      if(!missing(lineTypeLabel)) 
      {
        px = px + scale_linetype_discrete(name=lineTypeLabel)
      } else
      { 
        px = px + scale_linetype_discrete(name="Francis", guide = F)
      }
      
    }
    else #no linetype scheme
    {
      if(!missing(isColorFac) & isColorFac == 0)   #if the boolean isColorFac is provided and is false, continuous color scale
        px = px + geom_line(data = ttm, 
                            aes(x = temp1, y = temp2, color = colorCol), 
                            linetype = lType,
                            size = lSize)
      else  #discrete color scale otherwise
        px = px + geom_line(data = ttm, 
                            aes(x = temp1, y = temp2, color = factor(colorCol)), 
                            linetype = lType,
                            size = lSize)
    }
    
    #Colour label if necessary
    if(!missing(colorLabel))
    {
      if(!missing(isColorFac) & isColorFac == 0)
        px = px + scale_color_continuous(name=colorLabel)
      else 
      {
        px = px + scale_color_discrete(name=colorLabel)
      }
    }
    
  }
  else #no colour scheme
  {
    px = px + geom_line(data = ttm, 
                        aes(x = temp1, y = temp2), 
                        size = lSize)
  }
  
  
  #Labels
  #-----------------------------------------------------------------------------
  if(!missing(xlabel)) xlabeli = xlabel
  else xlabeli = colx
  
  if(!missing(ylabel)) ylabeli = ylabel
  else ylabeli = coly
  
  px = px + labs(x = xlabeli, y = ylabeli)
  
  
  #Theme
  #-----------------------------------------------------------------------------
  px= px + custom_theme 
  
  
  #Return the plot handle
  #-----------------------------------------------------------------------------
  return(px)
}

#------------------------------------------------------------------------------#
# Plot function for smooth plots
#------------------------------------------------------------------------------#
plotdf_smooth<- function(ttm,              #dataframe
                       colx,             #x
                       coly,             #y
                       xlabel,           #xlabel
                       ylabel,           #ylabel
                       colorCol,         #column for color scaling
                       colorLabel,       #associated label
                       isColorFac,       #is color factorized
                       lineTypeCol,      #column for line type
                       lineTypeLabel,    #associated label
                       lineSize,         #size of the lines
                       lineType)         #type of the lines
{
  
  
  #Ggplot init
  #--------------------------------------
  px = ggplot()
  
  #Changing temporarily the names of the desired columns (x & y)
  #--------------------------------------
  i1 = which(colnames(ttm) == colx)
  i2 = which(colnames(ttm) == coly)
  colnames(ttm)[i1] = "temp1"
  colnames(ttm)[i2] = "temp2"
  
  #LineType & LineSize
  #--------------------------------------
  if(!missing(lineSize)) 
  {
    lSize = lineSize
  }else{
    lSize = linesize[["line"]]
  }
  
  #LineType & LineSize
  #--------------------------------------
  if(!missing(lineType)) 
  {
    lType = lineType
  }else{
    lType = "solid"
  }
  
  
  #Plot
  #--------------------------------------
  if(!missing(colorCol)) #if a color scheme is provided
  {
    #Changing temporarily the names of the desired columns (colorCol)
    i1 = which(colnames(ttm) == colorCol)
    colnames(ttm)[i1] = "colorCol"
    
    if(!missing(lineTypeCol)) #if a linetype scheme is provided
    {
      #If there is no troubleshooting between lineTypeCol & colorCol
      if(colorCol != lineTypeCol)
      {
        #Changing temporarily the names of the desired columns (lineTypeCol)
        i2 = which(colnames(ttm) == lineTypeCol)
        colnames(ttm)[i2] = "lineTypeCol"
        
        if(!missing(isColorFac) & isColorFac == 0)   #if the boolean isColorFac is provided and is false, continuous color scale
          px = px + geom_smooth(data = ttm, 
                              aes(x = temp1, y = temp2, color = colorCol, linetype = factor(lineTypeCol)),
                              size = lSize, se = FALSE)
        else  #discrete color scale otherwise
          px = px + geom_smooth(data = ttm, 
                              aes(x = temp1, y = temp2, color = factor(colorCol), linetype = factor(lineTypeCol)), 
                              size = lSize, se = FALSE)
      }else #otherwise, use colorCol for both!
      {
        if(!missing(isColorFac) & isColorFac == 0)   #if the boolean isColorFac is provided and is false, continuous color scale
          px = px + geom_smooth(data = ttm, 
                              aes(x = temp1, y = temp2, color = colorCol, linetype = factor(colorCol)), 
                              size = lSize, se = FALSE)
        else  #discrete color scale otherwise
          px = px + geom_smooth(data = ttm, 
                              aes(x = temp1, y = temp2, color = factor(colorCol), linetype = factor(colorCol)), 
                              size = lSize, se = FALSE)
      }
      
      #Linetype label if necessary
      if(!missing(lineTypeLabel)) 
      {
        px = px + scale_linetype_discrete(name=lineTypeLabel)
      } else
      { 
        px = px + scale_linetype_discrete(name="Francis", guide = F)
      }
      
    }
    else #no linetype scheme
    {
      if(!missing(isColorFac) & isColorFac == 0)   #if the boolean isColorFac is provided and is false, continuous color scale
        px = px + geom_smooth(data = ttm, 
                            aes(x = temp1, y = temp2, color = colorCol), 
                            linetype = lType,
                            size = lSize, se = FALSE)
      else  #discrete color scale otherwise
        px = px + geom_smooth(data = ttm, 
                            aes(x = temp1, y = temp2, color = factor(colorCol)), 
                            linetype = lType,
                            size = lSize, se = FALSE)
    }
    
    #Colour label if necessary
    if(!missing(colorLabel))
    {
      if(!missing(isColorFac) & isColorFac == 0)
        px = px + scale_color_continuous(name=colorLabel)
      else 
      {
        px = px + scale_color_discrete(name=colorLabel)
      }
    }
    
  }
  else #no colour scheme
  {
    px = px + geom_smooth(data = ttm, 
                        aes(x = temp1, y = temp2), 
                        size = lSize, se = FALSE)
  }
  
  
  #Labels
  #--------------------------------------
  if(!missing(xlabel)) xlabeli = xlabel
  else xlabeli = colx
  
  if(!missing(ylabel)) ylabeli = ylabel
  else ylabeli = coly
  
  px = px + labs(x = xlabeli, y = ylabeli)
  
  
  #Theme
  #--------------------------------------
  px= px + custom_theme 
  
  
  #Return the plot handle
  #--------------------------------------
  return(px)
}


#------------------------------------------------------------------------------#
# Plot function for line plots
#------------------------------------------------------------------------------#
plotdf_point<- function(ttm,               #dataframe
                        colx,              #x
                        coly,              #y
                        xlabel,            #xlabel
                        ylabel,            #ylabel
                        colorCol,          #column for color scaling
                        colorLabel,        #associated label
                        isColorFac = FALSE,#is color factorized
                        lineTypeCol,       #column for line type
                        lineTypeLabel,     #associated label
                        pointSize,         #size of the points
                        pointType)         #type of the points
{
  #Ggplot init
  #--------------------------------------
  px = ggplot()
  
  #Changing temporarily the names of the desired columns (x & y)
  #--------------------------------------
  i1 = which(colnames(ttm) == colx)
  i2 = which(colnames(ttm) == coly)
  colnames(ttm)[i1] = "temp1"
  colnames(ttm)[i2] = "temp2"
  
  #LineType & LineSize
  #--------------------------------------
  if(!missing(pointSize)) 
  {
    lSize = pointSize
  }else{
    lSize = linesize[["point"]]
  }
  
  #LineType & LineSize
  #--------------------------------------
  if(!missing(pointType)) 
  {
    lType = pointType
  }else{
    lType = 16
  }
  
  
  #Plot
  #--------------------------------------
  if(!missing(colorCol)) #if a color scheme is provided
  {
    #Changing temporarily the names of the desired columns (colorCol)
    i1 = which(colnames(ttm) == colorCol)
    colnames(ttm)[i1] = "colorCol"
    
    if(!missing(lineTypeCol)) #if a linetype scheme is provided
    {
      #If there is no troubleshooting between lineTypeCol & colorCol
      if(colorCol != lineTypeCol)
      {
        #Changing temporarily the names of the desired columns (lineTypeCol)
        i2 = which(colnames(ttm) == lineTypeCol)
        colnames(ttm)[i2] = "lineTypeCol"
        
        if(!missing(isColorFac) & isColorFac == 0)   #if the boolean isColorFac is provided and is false, continuous color scale
          px = px + geom_point(data = ttm, 
                               aes(x = temp1, y = temp2, color = colorCol, linetype = factor(lineTypeCol)), 
                               size = lSize)
        else  #discrete color scale otherwise
          px = px + geom_point(data = ttm, 
                               aes(x = temp1, y = temp2, color = factor(colorCol), linetype = factor(lineTypeCol)), 
                               size = lSize)
      }else #otherwise, use colorCol for both!
      {
        if(!missing(isColorFac) & isColorFac == 0)   #if the boolean isColorFac is provided and is false, continuous color scale
          px = px + geom_point(data = ttm, 
                               aes(x = temp1, y = temp2, color = colorCol, linetype = factor(colorCol)), 
                               size = lSize)
        else  #discrete color scale otherwise
          px = px + geom_point(data = ttm, 
                               aes(x = temp1, y = temp2, color = factor(colorCol), linetype = factor(colorCol)), 
                               size = lSize)
      }
      
      #Linetype label if necessary
      if(!missing(lineTypeLabel)) 
      {
        px = px + scale_linetype_discrete(name=lineTypeLabel)
      } else
      { 
        px = px + scale_linetype_discrete(name="Francis", guide = F)
      }
      
    }
    else #no linetype scheme
    {
      if(!missing(isColorFac) & isColorFac == 0)   #if the boolean isColorFac is provided and is false, continuous color scale
        px = px + geom_point(data = ttm, 
                             aes(x = temp1, y = temp2, color = colorCol), 
                             shape = lType,
                             size = lSize)
      else  #discrete color scale otherwise
        px = px + geom_point(data = ttm, 
                             aes(x = temp1, y = temp2, color = factor(colorCol)),
                             shape = lType,
                             size = lSize)
      
    }
    
    #Colour label if necessary
    if(!missing(colorLabel))
    {
      if(!missing(isColorFac) & isColorFac == 0)
        px = px + scale_color_continuous(name=colorLabel)
      else 
      {
        px = px + scale_color_discrete(name=colorLabel)
      }
    }
    
  }
  else #no colour scheme
  {
    px = px + geom_point(data = ttm, 
                         aes(x = temp1, y = temp2), 
                         size = pointSize)
  }
  
  
  #Labels
  #--------------------------------------
  if(!missing(xlabel)) xlabeli = xlabel
  else xlabeli = colx
  
  if(!missing(ylabel)) ylabeli = ylabel
  else ylabeli = coly
  
  px = px + labs(x = xlabeli, y = ylabeli)
  
  
  #Theme
  #--------------------------------------
  px= px + custom_theme 
  
  
  #Return the plot handle
  #--------------------------------------
  return(px)
}


#------------------------------------------------------------------------------#
# Plot function for path plots
#------------------------------------------------------------------------------#
plotdf_path<- function(ttm,              #dataframe
                       colx,             #x
                       coly,             #y
                       xlabel,           #xlabel
                       ylabel,           #ylabel
                       colorCol,         #column for color scaling
                       colorLabel,       #associated label
                       isColorFac,       #is color factorized
                       lineTypeCol,      #column for line type
                       lineTypeLabel,    #associated label
                       lineSize,         #size of the lines
                       lineType)         #type of the lines
{
  #Ggplot init
  #--------------------------------------
  px = ggplot()
  
  #Changing temporarily the names of the desired columns (x & y)
  #--------------------------------------
  i1 = which(colnames(ttm) == colx)
  i2 = which(colnames(ttm) == coly)
  colnames(ttm)[i1] = "temp1"
  colnames(ttm)[i2] = "temp2"
  
  #LineType & LineSize
  #--------------------------------------
  if(!missing(lineSize)) 
  {
    lSize = lineSize
  }else{
    lSize = linesize[["line"]]
  }
  
  #LineType & LineSize
  #--------------------------------------
  if(!missing(lineType)) 
  {
    lType = lineType
  }else{
    lType = "solid"
  }
  
  
  #Plot
  #--------------------------------------
  if(!missing(colorCol)) #if a color scheme is provided
  {
    #Changing temporarily the names of the desired columns (colorCol)
    i1 = which(colnames(ttm) == colorCol)
    colnames(ttm)[i1] = "colorCol"
    
    if(!missing(lineTypeCol)) #if a linetype scheme is provided
    {
      #If there is no troubleshooting between lineTypeCol & colorCol
      if(colorCol != lineTypeCol)
      {
        #Changing temporarily the names of the desired columns (lineTypeCol)
        i2 = which(colnames(ttm) == lineTypeCol)
        colnames(ttm)[i2] = "lineTypeCol"
        
        if(!missing(isColorFac) & isColorFac == 0) 
        {
          #if the boolean isColorFac is provided and is false, continuous color scale
          px = px + geom_path(data = ttm, 
                              aes(x = temp1, y = temp2, color = colorCol, linetype = factor(lineTypeCol)), 
                              size = lSize)
        }else{  #discrete color scale otherwise
          px = px + geom_path(data = ttm, 
                              aes(x = temp1, y = temp2, color = factor(colorCol), linetype = factor(lineTypeCol)), 
                              size = lSize)
        }
        
      }else #otherwise, use colorCol for both!
      {
        if(!missing(isColorFac) & isColorFac == FALSE)   #if the boolean isColorFac is provided and is false, continuous color scale
          px = px + geom_path(data = ttm, 
                              aes(x = temp1, y = temp2, color = colorCol, linetype = colorCol), 
                              size = lSize)
        else  #discrete color scale otherwise
          px = px + geom_path(data = ttm, 
                              aes(x = temp1, y = temp2, color = factor(colorCol), linetype = factor(colorCol)), 
                              size = lSize)
      }
      
      #Linetype label if necessary
      if(!missing(lineTypeLabel)) 
      {
        px = px + scale_linetype_discrete(name=lineTypeLabel)
      } else
      { 
        px = px + scale_linetype_discrete(name="Francis", guide = F)
      }
      
    }
    else #no linetype scheme
    {
      if(!missing(isColorFac) & isColorFac == 0)   #if the boolean isColorFac is provided and is false, continuous color scale
      {
        px = px + geom_path(data = ttm, 
                            aes(x = temp1, y = temp2, color = colorCol), 
                            linetype = lType,
                            size = lSize)
      }else{  #discrete color scale otherwise
        px = px + geom_path(data = ttm, 
                            aes(x = temp1, y = temp2, color = factor(colorCol)),
                            linetype = lType,
                            size = lSize)
      }
    }
    
    #Colour label if necessary
    if(!missing(colorLabel))
    {
      if(!missing(isColorFac) & isColorFac == 0)
        px = px + scale_color_continuous(name=colorLabel)
      else 
      {
        px = px + scale_color_discrete(name=colorLabel)
      }
    }
    
  }
  else #no colour scheme
  {
    px = px + geom_path(data = ttm, 
                        aes(x = temp1, y = temp2), 
                        size = lSize)
  }
  
  
  #Labels
  #--------------------------------------
  if(!missing(xlabel)) xlabeli = xlabel
  else xlabeli = colx
  
  if(!missing(ylabel)) ylabeli = ylabel
  else ylabeli = coly
  
  px = px + labs(x = xlabeli, y = ylabeli)
  
  
  #Theme
  #--------------------------------------
  px= px + custom_theme 
  
  
  #Return the plot handle
  #--------------------------------------
  return(px)
}

#------------------------------------------------------------------------------#
# Tile plot
# €€TODO: adapt this routine to look like the rest of the plot routines
#------------------------------------------------------------------------------#
plotdf_tile_n<- function(ttm,              #dataframe
                         colx,             #x
                         coly,             #y
                         xlabel,           #xlabel
                         ylabel,           #ylabel
                         colorCol,         #column for color scaling
                         colorLabel,       #associated label
                         colorMidPoint,
                         isLegendOn,
                         legendPos,
                         lowColor,
                         midColor,
                         highColor,
                         colorLimits)
{
  #Ggplot init
  #--------------------------------------
  px = ggplot()
  
  #Changing temporarily the names of the desired columns (x & y)
  #--------------------------------------
  i1 = which(colnames(ttm) == colx)
  i2 = which(colnames(ttm) == coly)
  colnames(ttm)[i1] = "temp1"
  colnames(ttm)[i2] = "temp2"
  
  #Colour label if necessary
  #--------------------------------------
  if(!missing(colorLabel))
  {
    clb = colorLabel #new title
  }
  else{
    clb = expression(log[10]~bgroup("(",e[O]~bgroup("(",frac(T,2),")"),")") ~~~~ "") #orbital error by default
  }
  
  #Colour mid point if necessary
  #--------------------------------------
  if(!missing(colorLabel))
  {
    cmp = colorMidPoint #new mid point
  }
  else{
    cmp = -6
  }
  
  #Plot
  #--------------------------------------
  if(!missing(colorCol)) #if a color scheme is provided
  {
    #Changing temporarily the names of the desired columns (colorCol)
    i1 = which(colnames(ttm) == colorCol)
    colnames(ttm)[i1] = "colorCol"
    
    # Plot tiles
    px = px + geom_tile(data = ttm, aes(temp1, temp2, fill = colorCol, colour = colorCol))
  }
  else #no colour scheme, automatically on log10(eOm) if it exists
  {
    if(is.null(ttm$log10eOm))
    {
      stop("Error in plotdf_tile_n: no colour scheme provided and ttm$log10eOm does not exist")
    }
    else
    {
      px = px + geom_tile(data = ttm, aes(temp1, temp2, fill = log10eOm, colour = log10eOm))
    }
  }
  
  #Colors
  #--------------------------------------
  if(!missing(lowColor)) #if a lowColor was provided
  {
    lowColor_temp = lowColor
  }else
  {
    lowColor_temp = muted("red")
  }

  if(!missing(midColor)) #if a midColor was provided
  {
    midColor_temp = midColor
  }else
  {
    midColor_temp = "white"
  }
  
  if(!missing(highColor)) #if a highColor was provided
  {
    highColor_temp = highColor
  }else
  {
    highColor_temp = muted("blue")
  }

  #Fill color gradient
  #--------------------------------------
  if(isLegendOn==1)
  {
    cg = guide_legend(title.hjust = 0.0)
    
    if(!missing(colorLimits))
    {
      px = px + scale_fill_gradient2(clb, space="Lab", midpoint = cmp, mid = midColor_temp, high = highColor_temp, limits = colorLimits)
    }else{
      px = px + scale_fill_gradient2(clb, space="Lab", midpoint = cmp, mid = midColor_temp, high = highColor_temp)
    }
  }else{
    if(!missing(colorLimits))
    {
      px = px + scale_fill_gradient2(clb, space="Lab", midpoint = cmp, mid = midColor_temp, high = highColor_temp, limits = colorLimits,  guide = FALSE)
    }else{
      px = px + scale_fill_gradient2(clb, space="Lab", midpoint = cmp, mid = midColor_temp, high = highColor_temp, guide = FALSE)
    }
  }
  
  #Color gradient
  #--------------------------------------
  if(!missing(colorLimits))
  {
    px = px + scale_colour_gradient2(clb, space="Lab", midpoint = cmp, mid = midColor_temp, high = highColor_temp, guide = FALSE, limits = colorLimits)
  }else{
    px = px + scale_colour_gradient2(clb, space="Lab", midpoint = cmp, mid = midColor_temp, high = highColor_temp, guide = FALSE)
  }
  
  #Labels
  #--------------------------------------
  if(!missing(xlabel)) xlabeli = xlabel
  else xlabeli = colx
  
  if(!missing(ylabel)) ylabeli = ylabel
  else ylabeli = coly
  
  px = px + labs(x = xlabeli, y = ylabeli)
  
  
  #Theme
  #--------------------------------------
  px= px + custom_theme +theme(legend.title.align= 0.5)
  
  
  #Legend position
  #--------------------------------------
  if(!missing(legendPos))
  {
    px = px+theme(legend.position=legendPos, legend.title.align= 0.5)
  }
  
  #Return the plot handle
  #--------------------------------------
  return(px)
}

#------------------------------------------------------------------------------#
# Tile plot with low/high color
#------------------------------------------------------------------------------#
plotdf_tile_1<- function(ttm,              #dataframe
                         colx,             #x
                         coly,             #y
                         xlabel,           #xlabel
                         ylabel,           #ylabel
                         colorCol,         #column for color scaling
                         colorLabel,       #associated label
                         isLegendOn,
                         legendPos,
                         lowColor,
                         highColor,
                         colorLimits,
                         na.value)
{
  #Ggplot init
  #--------------------------------------
  px = ggplot()
  
  #Changing temporarily the names of the desired columns (x & y)
  #--------------------------------------
  i1 = which(colnames(ttm) == colx)
  i2 = which(colnames(ttm) == coly)
  colnames(ttm)[i1] = "temp1"
  colnames(ttm)[i2] = "temp2"
  
  #Colour label if necessary
  #--------------------------------------
  if(!missing(colorLabel))
  {
    clb = colorLabel #new title
  }
  else{
    clb = expression(log[10]~bgroup("(",e[O]~bgroup("(",frac(T,2),")"),")") ~~~~ "") #orbital error by default
  }
  
  #Plot
  #--------------------------------------
  if(!missing(colorCol)) #if a color scheme is provided
  {
    #Changing temporarily the names of the desired columns (colorCol)
    i1 = which(colnames(ttm) == colorCol)
    colnames(ttm)[i1] = "colorCol"
    
    # Plot tiles
    px = px + geom_tile(data = ttm, aes(temp1, temp2, fill = colorCol, colour = colorCol))
  }
  else #no colour scheme, automatically on log10(eOm) if it exists
  {
    if(is.null(ttm$log10eOm))
    {
      stop("Error in plotdf_tile_n: no colour scheme provided and ttm$log10eOm does not exist")
    }
    else
    {
      px = px + geom_tile(data = ttm, aes(temp1, temp2, fill = log10eOm, colour = log10eOm))
    }
  }
  
  #Colors
  #--------------------------------------
  if(!missing(lowColor)) #if a lowColor was provided
  {
    lowColor_temp = lowColor
  }else
  {
    lowColor_temp = muted("blue")
  }
  
  if(!missing(highColor)) #if a highColor was provided
  {
    highColor_temp = highColor
  }else
  {
    highColor_temp = "white" 
  }
  
  if(!missing(na.value)) #if a highColor was provided
  {
    na.value_temp = na.value
  }else
  {
    na.value_temp = "grey50" 
  }
  
  
  
  #Fill color gradient
  #--------------------------------------
  if(isLegendOn==1)
  {
    cg = guide_legend(title.hjust = 0.0)
    
    if(!missing(colorLimits))
    {
      px = px + scale_fill_gradient(clb, space="Lab", low = lowColor_temp, high = highColor_temp,  na.value = na.value_temp, limits = colorLimits)
    }else{
      px = px + scale_fill_gradient2(clb, space="Lab", low = lowColor_temp, high = highColor_temp, na.value = na.value_temp)
    }
  }else{
    if(!missing(colorLimits))
    {
      px = px + scale_fill_gradient(clb, space="Lab", low = lowColor_temp, high = highColor_temp, na.value = na.value_temp, limits = colorLimits,  guide = FALSE)
    }else{
      px = px + scale_fill_gradient(clb, space="Lab", low = lowColor_temp, high = highColor_temp, na.value = na.value_temp, guide = FALSE)
    }
  }
  
  #Color gradient
  #--------------------------------------
  if(!missing(colorLimits))
  {
    px = px + scale_colour_gradient(clb, space="Lab",  low = lowColor_temp, high = highColor_temp, na.value = na.value_temp, guide = FALSE, limits = colorLimits)
  }else{
    px = px + scale_colour_gradient(clb, space="Lab",  low = lowColor_temp, high = highColor_temp, na.value = na.value_temp, guide = FALSE)
  }
  
  #Labels
  #--------------------------------------
  if(!missing(xlabel)) xlabeli = xlabel
  else xlabeli = colx
  
  if(!missing(ylabel)) ylabeli = ylabel
  else ylabeli = coly
  
  px = px + labs(x = xlabeli, y = ylabeli)
  
  
  #Theme
  #--------------------------------------
  px= px + custom_theme +theme(legend.title.align= 0.5)
  
  
  #Legend position
  #--------------------------------------
  if(!missing(legendPos))
  {
    px = px+theme(legend.position=legendPos, legend.title.align= 0.5)
  }
  
  #Return the plot handle
  #--------------------------------------
  return(px)
}


#------------------------------------------------------------------------------#
# Tile plot, old edition
# €€TODO: adapt this routine to look like the rest of the plot routines
#------------------------------------------------------------------------------#
plotdf_tile<- function(ttm_c, st1, st2, isLegendOn, scale_midpoint, title, legendOnTop) 
{
  
  #Build the -log10(eOm) if it does not exist
  if(is.null(ttm_c$log10eOm))
  {
    ttm_c$log10eOm = -log10(ttm_c$eOm)
  }
  #Ggplot init
  px = ggplot()
  # Plot tiles
  px = px + geom_tile(data = ttm_c, aes_string(st1, st2, fill = "log10eOm", colour = "log10eOm"))
  #Get the proper color gradient title for fill scheme
  if(!missing(title)) 
  {
    legTile = title #new title
  }
  else{
    legTitle = expression(log[10]~bgroup("(",e[O]~bgroup("(",frac(T,2),")"),")") ~~~~ "") #orbital error by default
  }

  if(isLegendOn==1)
  {
    cg = guide_legend(title.hjust = 0.0)
    px = px + scale_fill_gradient2(legTitle, space="Lab", midpoint = scale_midpoint, mid = "white", high = muted("blue"))#, guide = cg)
  }else{
    px = px + scale_fill_gradient2(legTitle, space="Lab", midpoint = scale_midpoint, mid = "white", high = muted("blue"),   guide = FALSE)
  }
  #Get the proper color gradient for color scheme
  px = px + scale_colour_gradient2(legTitle, space="Lab", midpoint = scale_midpoint, mid = "white", high = muted("blue"), guide = FALSE)
  #Labels
  px = px + labs(x = st1, y = st2)
  #Theme
  px = px + custom_theme+theme(legend.title.align= 0.5)
  #Place legend on top if necessary
  if(!missing(legendOnTop))
  {
    if(legendOnTop == 1)
    {
      px = px+theme(legend.position="top", legend.title.align= 0.5)
    }
  }

  
  return(px)
}

#------------------------------------------------------------------------------#
#Extract Legend 
#------------------------------------------------------------------------------#
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

#------------------------------------------------------------------------------#
# Define the circle; add a point at the center if the 'pie slice' if the shape is to be filled
#------------------------------------------------------------------------------#
circleFun <- function(center=c(0,0), diameter=1, npoints=100, start=0, end=2, filled=TRUE)
{
  tt <- seq(start*pi, end*pi, length.out=npoints)
  df <- data.frame(
    x = center[1] + diameter / 2 * cos(tt),
    y = center[2] + diameter / 2 * sin(tt)
  )
  if(filled==TRUE) { #add a point at the center so the whole 'pie slice' is filled
    df <- rbind(df, center)
  }
  return(df)
}

#------------------------------------------------------------------------------#
# Add the Moon to a given dataframe
#------------------------------------------------------------------------------#
addMoon <- function(porb, x, y, primR, surfSize, cratSize)
{
  moon  = circleFun(center = c(x, y), diameter = primR, npoints = 100, start = 0, end = 2, filled = TRUE)
  moonT = circleFun(center = c(x, y), diameter = primR, npoints = 100, start = 0, end = 2, filled = FALSE)
  
  fac = primR/1737.10
  fac
  crat  = circleFun(center = c(x-100*fac, y-300*fac), diameter = 1e-1*primR,   npoints = 100, start = 0, end = 2, filled = FALSE)
  crat2 = circleFun(center = c(x+400*fac, y+300*fac), diameter = 2.5e-1*primR, npoints = 100, start = 0, end = 2, filled = FALSE)
  crat3 = circleFun(center = c(x-500*fac, y+400*fac), diameter = 1.7e-1*primR, npoints = 100, start = 0, end = 2, filled = FALSE)
  crat4 = circleFun(center = c(x+500*fac, y-700*fac), diameter = 3e-1*primR,   npoints = 100, start = 0.2, end = 1.2, filled = FALSE)
  
  porb = porb + geom_polygon(data=moon, aes(x,y), color="grey40", fill="grey40")+geom_path(data=moonT, aes(x, y), color="black", size = surfSize)
  porb = porb + geom_path(data=crat, aes(x, y), color="black",  size = cratSize)
  porb = porb + geom_path(data=crat2, aes(x, y), color="black", size = cratSize)
  porb = porb + geom_path(data=crat3, aes(x, y), color="black", size = cratSize)
  porb = porb + geom_path(data=crat4, aes(x, y), color="black", size = cratSize)
  
  return(porb)
}

#------------------------------------------------------------------------------#
# Add the Moon to a given dataframe
#------------------------------------------------------------------------------#
addPrimary <- function(porb, x, y, primR, surfSize, primColor)
{
  moon  = circleFun(center = c(x, y), diameter = primR, npoints = 100, start = 0, end = 2, filled = TRUE)
  moonT = circleFun(center = c(x, y), diameter = primR, npoints = 100, start = 0, end = 2, filled = FALSE)
  
  fac = primR/1737.10
  fac
  crat  = circleFun(center = c(x-100*fac, y-300*fac), diameter = 1e-1*primR,   npoints = 100, start = 0, end = 2, filled = FALSE)
  crat2 = circleFun(center = c(x+400*fac, y+300*fac), diameter = 2.5e-1*primR, npoints = 100, start = 0, end = 2, filled = FALSE)
  crat3 = circleFun(center = c(x-500*fac, y+400*fac), diameter = 1.7e-1*primR, npoints = 100, start = 0, end = 2, filled = FALSE)
  crat4 = circleFun(center = c(x+500*fac, y-700*fac), diameter = 3e-1*primR,   npoints = 100, start = 0.2, end = 1.2, filled = FALSE)
  
  porb = porb + geom_polygon(data=moon, aes(x,y), color=primColor, fill=primColor)
  porb = porb + geom_path(data=moonT, aes(x, y), color="black", size = surfSize)
  return(porb)
}

#------------------------------------------------------------------------------#
# Pretty number of ticks
#------------------------------------------------------------------------------#
number_ticks <- function(n) {function(limits) pretty(limits, n)}


#------------------------------------------------------------------------------#
# [DEPRECATED] Plot function for dashed line plots
#------------------------------------------------------------------------------#
plotdf_line_dashed<- function(ttm,       #dataframe
                              colx,             #x
                              coly,             #y
                              xlabel,           #xlabel
                              ylabel,           #ylabel
                              colorCol,         #column for color scaling
                              colorLabel,       #associated label
                              isColorFac)       #is color factorized
{
  
  
  #Ggplot init
  #--------------------------------------
  px = ggplot()
  
  #Changing temporarily the names of the desired columns (x & y)
  #--------------------------------------
  i1 = which(colnames(ttm) == colx)
  i2 = which(colnames(ttm) == coly)
  colnames(ttm)[i1] = "temp1"
  colnames(ttm)[i2] = "temp2"
  
  
  #Plot
  #--------------------------------------
  if(!missing(colorCol)) #if a color scheme is provided
  {
    #Changing temporarily the names of the desired columns (colorCol)
    i1 = which(colnames(ttm) == colorCol)
    colnames(ttm)[i1] = "colorCol"
    
    
    if(!missing(isColorFac) & isColorFac == 0)   #if the boolean isColorFac is provided and is false, continuous color scale
      px = px + geom_line(data = ttm, 
                          aes(x = temp1, y = temp2, color = colorCol), 
                          linetype = "dashed",
                          size = linesize[["line"]])
    else  #discrete color scale otherwise
      px = px + geom_line(data = ttm, 
                          aes(x = temp1, y = temp2, color = factor(colorCol)), 
                          linetype = "dashed",
                          size = linesize[["line"]])
    
    
    
    #Colour label if necessary
    if(!missing(colorLabel))
    {
      print(1)
      if(!missing(isColorFac) & isColorFac == 0)
        px = px + scale_color_continuous(name=colorLabel)
      else 
      {
        px = px + scale_color_discrete(name=colorLabel)
      }
    }
    
  }
  else #no colour scheme
  {
    px = px + geom_line(data = ttm, 
                        aes(x = temp1, y = temp2),
                        linetype = "dashed",
                        size = linesize[["line"]])
  }
  
  
  #Labels
  #--------------------------------------
  if(!missing(xlabel)) xlabeli = xlabel
  else xlabeli = colx
  
  if(!missing(ylabel)) ylabeli = ylabel
  else ylabeli = coly
  
  px = px + labs(x = xlabeli, y = ylabeli)
  
  
  #Theme
  #--------------------------------------
  px= px + custom_theme
  
  
  #Return the plot handle
  #--------------------------------------
  return(px)
}


#------------------------------------------------------------------------------#
# Pretty geom_point
#------------------------------------------------------------------------------#
geom_point_pretty <- function(figure, dataf, aes, color=NaN, size = 2)
{
  figure = figure + geom_point(data = dataf, aes, color= "black", size = size+2.5)
  figure = figure + geom_point(data = dataf, aes, color= 'white', size = size+2)
  
  if(is.nan(color))
  {
    figure = figure + geom_point(data = dataf, aes, size = size)
  }else
  {
    figure = figure + geom_point(data = dataf, aes, color = color, size = size)
  }
  
  return(figure)
}

#------------------------------------------------------------------------------#
# Dataframe of a circle at center "center", of radius r, using npoints points.
#------------------------------------------------------------------------------#
circleOrbit <- function(center = c(0,0), r = 1, npoints = 100){
  tt <- seq(0, 2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}