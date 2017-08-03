#-------------------------------------------------------------------------------
# fpp_path_traj_phd function to plot trajectories from refinement/continuation 
# procedures
#
# Note: complementary.objects = TRUE works only if lib.point.em = "L2"
#-------------------------------------------------------------------------------
fpp_path_traj_phd <- function(df, filename, fwrk, limits.x = c(NaN, NaN), limits.y = c(NaN, NaN),
                              xlab = "x", ylab = "y", xlab.tex = "$x$", ylab.tex = "$y$",
                              size.traj = 0.8, size.txt = 7, size.lib = 2, size.prim = 3,
                              y.position.labels = -0.09, y.position.earth = -0.09, y.position.emlt = +0.75,
                              colour.traj = NaN, complementary.objects = FALSE,
                              lib.point.em = "L2", lib.point.sem = "L2", isLaTeX = TRUE,
                              isPNG = TRUE, isPDF = TRUE, isPrimary = TRUE, isSingle = FALSE)
{
  #-----------------------------------------------------------------------------
  # Local coordinates
  #-----------------------------------------------------------------------------
  if(lib.point.em == "L2")
  {
    num.em = 2
    moon.x = -1 
  }else{
    num.em = 1
    moon.x = +1 
  }
  
  if(lib.point.sem == "L2")
  {
    num.sem = 2
    earth.x = -1 
  }else{
    num.sem = 1
    earth.x = +1 
  }
  
  
  #-----------------------------------------------------------------------------
  # SE case
  #-----------------------------------------------------------------------------
  if(fwrk == "SE")
  {
    #---------------------------------------------------------------------------
    #Plot
    #---------------------------------------------------------------------------
    if("type" %in% colnames(df))
    {
      fplot = ggplot() + geom_path(data = df, 
                                   aes(x = x_CMS_SEM, y = y_CMS_SEM, 
                                       group = interaction(label, type), 
                                       colour = factor(type)), 
                                   size = size.traj)
    }else{
      fplot = ggplot() + geom_path(data = df, aes(x = x_CMS_SEM, 
                                                  y = y_CMS_SEM, 
                                                  group = label), 
                                   size = size.traj)
    }
    
    #---------------------------------------------------------------------------
    # First & last solutions
    #---------------------------------------------------------------------------
    if(!isSingle)
    {
      fplot = fplot + geom_path(data = df[which(df$label == min(df$label)),], 
                                aes(x = x_CMS_SEM, y = y_CMS_SEM, 
                                    group = interaction(label, type)), 
                                colour = muted("red"),
                                size = 2*size.traj)
      
      fplot = fplot + geom_path(data = df[which(df$label == max(df$label)),], 
                                aes(x = x_CMS_SEM, y = y_CMS_SEM, 
                                    group = interaction(label, type)), 
                                colour = muted("blue"),
                                size = 2*size.traj)
    }
    
    
    #---------------------------------------------------------------------------
    # Additional objects
    #---------------------------------------------------------------------------
    # Add SEMLi
    fplot = fplot + geom_point(data = dfseml2, aes(x= x_SYS, y = y_SYS), size = size.lib)
    
    # Add comp SEMLi
    if(complementary.objects)
    {
      fplot = fplot + geom_point(data = dfseml1, aes(x= x_SYS, y = x_SYS), size = size.lib)
    }
    
    #Add Earth
    if(isPrimary)
    {
      fplot = fplot + geom_point(data = dfearth_seml, aes(x= x_SYS, y = y_SYS), size = size.prim) 
    }
    
    
    #---------------------------------------------------------------------------
    # Limits
    #---------------------------------------------------------------------------
    # Limits on x-axis
    if(is.na(limits.x[1]) && is.na(limits.x[2]) )
    {
    }else
    {
      fplot = fplot + scale_x_continuous(limits = limits.x)
    }
    
    # Limits on y-axis
    if(is.na(limits.y[1]) && is.na(limits.y[2]))
    {
    }else
    {
      fplot = fplot + scale_y_continuous(limits = limits.y)
    }
    
    #---------------------------------------------------------------------------
    # Annotations & labels
    #---------------------------------------------------------------------------
    # SEMLi
    fplot.pdf = fplot + annotate("text", x = dfseml2$x_SYS, y = y.position.labels,  
                                 label = paste0("bold(SEL[", num.sem, "])"), colour = "white",
                                 size = 5, parse = TRUE)
    fplot.pdf = fplot.pdf + annotate("text", x = dfseml2$x_SYS, y = y.position.labels,  label = paste0("SEL[", num.sem, "]"), size = 5, parse = TRUE)
    # Earth
    if(isPrimary)
    {
      fplot.pdf = fplot.pdf + annotate("text", x = dfearth_seml$x_SYS, y = y.position.earth, 
                                       label = "bold(Earth)",  colour = "white",
                                       size = 5, parse = TRUE)
      
      fplot.pdf = fplot.pdf + annotate("text", x = dfearth_seml$x_SYS, y = y.position.earth, label = "Earth", size = 5)
    }
    
    #---------------------------------------------------------------------------
    # Annotations & labels in tex form
    #---------------------------------------------------------------------------
    # SEMLi
    fplot.tex = fplot + annotate("text", x = dfseml2$x_SYS, y = y.position.labels,  label = paste0("\\textsc{sel}$_", num.sem, "$"), size = size.txt)
    if(complementary.objects)
    {
      fplot.tex = fplot.tex + annotate("text", x = dfseml1$x_SYS, y = y.position.labels,  label = "\\textsc{sel}$_1$", size = size.txt)
    }
    # Earth
    if(isPrimary)
    {
      fplot.tex = fplot.tex + annotate("text", x = dfearth_seml$x_SYS, y = y.position.labels, label = "Earth", colour = "white", size = size.txt+0.1)
      fplot.tex = fplot.tex + annotate("text", x = dfearth_seml$x_SYS, y = y.position.labels, label = "Earth", size = size.txt)
    }
  }
  else if(fwrk == "NCSE")
  {
    #---------------------------------------------------------------------------
    #Plot
    #---------------------------------------------------------------------------
    if("type" %in% colnames(df))
    {
      fplot = ggplot() + geom_path(data = df, 
                                   aes(x = x_CMS_NCSEM, y = y_CMS_NCSEM, 
                                       group = interaction(label, type), 
                                       colour = factor(type)), 
                                   size = size.traj)
    }else{
      fplot = ggplot() + geom_path(data = df, aes(x = x_CMS_NCSEM, 
                                                  y = y_CMS_NCSEM, 
                                                  group = label), 
                                   size = size.traj)
    }
    
    #---------------------------------------------------------------------------
    # First & last solutions
    #---------------------------------------------------------------------------
    if(!isSingle)
    {
    fplot = fplot + geom_path(data = df[which(df$label == min(df$label)),], 
                              aes(x = x_CMS_NCSEM, y = y_CMS_NCSEM, 
                                  group = interaction(label, type)), 
                              colour = muted("red"),
                              size = 2*size.traj)
    
    fplot = fplot + geom_path(data = df[which(df$label == max(df$label)),], 
                              aes(x = x_CMS_NCSEM, y = y_CMS_NCSEM, 
                                  group = interaction(label, type)), 
                              colour = muted("blue"),
                              size = 2*size.traj)
    }
    #---------------------------------------------------------------------------
    # Additional objects
    #---------------------------------------------------------------------------
    # Add SEMLi
    fplot = fplot + geom_point(data = dfseml2, aes(x= x_NC, y = y_NC), size = size.lib)
    
    # Add comp SEMLi
    if(complementary.objects)
    {
      fplot = fplot + geom_point(data = dfseml1, aes(x= x_NCC, y = y_NCC), size = size.lib)
    }
    
    #Add Earth
    if(isPrimary)
    {
      fplot = fplot + geom_point(data = dfearth_seml, aes(x= x_NC, y = y_NC), size = size.prim) 
    }
    
    
    #---------------------------------------------------------------------------
    # Limits
    #---------------------------------------------------------------------------
    # Limits on x-axis
    if(is.na(limits.x[1]) && is.na(limits.x[2]) )
    {
      if(complementary.objects)
      {
        fplot = fplot + scale_x_continuous(limits = c(-2.2, 0.2))
      }else{
        
        fplot = fplot + scale_x_continuous(limits = c(-1.5, 0.3))
      }
    }else
    {
      fplot = fplot + scale_x_continuous(limits = limits.x)
    }
    
    # Limits on y-axis
    if(is.na(limits.y[1]) && is.na(limits.y[2]))
    {
      if(complementary.objects)
      {
        fplot = fplot + scale_y_continuous(limits = c(-1.0, 1.0))
      }else{
        fplot = fplot + scale_y_continuous(limits = c(-1.1, 1.1))
      }
    }else
    {
      fplot = fplot + scale_y_continuous(limits = limits.y)
    }
    
    #---------------------------------------------------------------------------
    # Annotations & labels
    #---------------------------------------------------------------------------
    # SEMLi
    fplot.pdf = fplot + annotate("text", x = 0, y = y.position.labels,  
                                 label = paste0("bold(SEL[", num.sem, "])"), colour = "white",
                                 size = 5, parse = TRUE)
    fplot.pdf = fplot.pdf + annotate("text", x = 0, y = y.position.labels,  label = paste0("SEL[", num.sem, "]"), size = 5, parse = TRUE)
    # Earth
    if(isPrimary)
    {
      fplot.pdf = fplot.pdf + annotate("text", x = earth.x, y = y.position.earth, 
                                       label = "bold(Earth)",  colour = "white",
                                       size = 5, parse = TRUE)
      
      fplot.pdf = fplot.pdf + annotate("text", x = earth.x, y = y.position.earth, label = "Earth", size = 5)
    }
    
    #---------------------------------------------------------------------------
    # Annotations & labels in tex form
    #---------------------------------------------------------------------------
    # SEMLi
    fplot.tex = fplot + annotate("text", x = 0, y = y.position.labels,  label = paste0("\\textsc{sel}$_", num.sem, "$"), size = size.txt)
    if(complementary.objects)
    {
      fplot.tex = fplot.tex + annotate("text", x = -1.993, y = y.position.labels,  label = "\\textsc{sel}$_1$", size = size.txt)
    }
    # Earth
    if(isPrimary)
    {
      fplot.tex = fplot.tex + annotate("text", x = earth.x, y = y.position.labels, label = "Earth", colour = "white", size = size.txt+0.1)
      fplot.tex = fplot.tex + annotate("text", x = earth.x, y = y.position.labels, label = "Earth", size = size.txt)
    }
  }
  else if(fwrk == "EM")
  {
    #---------------------------------------------------------------------------
    #Plot
    #---------------------------------------------------------------------------
    if("type" %in% colnames(df))
    {
      fplot = ggplot() + geom_path(data = df, 
                                   aes(x = x_CMS_EM, y = y_CMS_EM, 
                                       group = interaction(label, type), 
                                       colour = factor(type)), 
                                   size = size.traj)
    }else{
      fplot = ggplot() + geom_path(data = df, aes(x = x_CMS_EM, 
                                                  y = y_CMS_EM, 
                                                  group = label), 
                                   size = size.traj)
    }
    
    #---------------------------------------------------------------------------
    # First & last solutions
    #---------------------------------------------------------------------------
    if(!isSingle)
    {
    fplot = fplot + geom_path(data = df[which(df$label == min(df$label)),], 
                              aes(x = x_CMS_EM, y = y_CMS_EM, 
                                  group = interaction(label, type)), 
                              colour = muted("red"),
                              size = 2*size.traj)
    
    fplot = fplot + geom_path(data = df[which(df$label == max(df$label)),], 
                              aes(x = x_CMS_EM, y = y_CMS_EM, 
                                  group = interaction(label, type)), 
                              colour = muted("blue"),
                              size = 2*size.traj)
    }
    
    
    #---------------------------------------------------------------------------
    # Additional objects
    #---------------------------------------------------------------------------
    #Add EMLi
    fplot = fplot + geom_point(data = dfemli, aes(x= x_SYS, y = y_SYS), size = size.lib) 
    
    #Add Moon
    if(isPrimary)
    {
      fplot = fplot + geom_point(data = dfmoon_eml, aes(x= x_SYS, y = y_SYS), size = size.prim)
    }
    
    #---------------------------------------------------------------------------
    # Zoom
    #---------------------------------------------------------------------------
    # Limits on x-axis
    if(is.na(limits.x[1]) && is.na(limits.x[2]) )
    {
    }else
    {
      fplot = fplot + scale_x_continuous(limits = limits.x)
    }
    
    # Limits on y-axis
    if(is.na(limits.y[1]) && is.na(limits.y[2]))
    {
    }else
    {
      fplot = fplot + scale_y_continuous(limits = limits.y)
    }
    
    #---------------------------------------------------------------------------
    # Annotations & labels
    #---------------------------------------------------------------------------
    # EMLi
    fplot.pdf = fplot + annotate("text", x = dfemli$x_SYS, y = y.position.emlt,  
                                 label = paste0("bold(EML[", num.em, "])"), colour = "white",
                                 size = 5, parse = TRUE)
    fplot.pdf = fplot.pdf + annotate("text", x = dfemli$x_SYS, y = y.position.emlt,  label = paste0("EML[", num.em, "]"), size = 5, parse = TRUE)
    
    # Moon
    if(isPrimary)
    {
      fplot.pdf = fplot.pdf + annotate("text", x = moon.x, y = -0.013, 
                                       label = "bold(Moon)",  colour = "white",
                                       size = 5, parse = TRUE)
      fplot.pdf = fplot.pdf + annotate("text", x = dfmoon_eml$x_SYS, y = -0.013, label = "Moon", size = 5)
    }
    
    #---------------------------------------------------------------------------
    # Annotations & labels in tex form
    #---------------------------------------------------------------------------
    fplot.tex = fplot
    if(lib.point.em == "L2")
    {
      # EMLi
      fplot.tex = fplot.tex + annotate("text", x = dfemli$x_SYS, y = -0.013,  label = "\\textsc{eml}$_2$", size = 5)
    }else{
      # EMLi
      fplot.tex = fplot.tex + annotate("text", x = dfemli$x_SYS, y = -0.013,  label = "\\textsc{eml}$_1$", size = 5)
    }
    
    # Moon
    if(isPrimary)
    {
      fplot.tex = fplot.tex + annotate("text", x = dfmoon_eml$x_SYS, y = -0.013, label = "Moon", size = 5)
    }
  } else if(fwrk == "NCEM")
  {
    #---------------------------------------------------------------------------
    #Plot
    #---------------------------------------------------------------------------
    if("type" %in% colnames(df))
    {
      fplot = ggplot() + geom_path(data = df, 
                                   aes(x = x_CMS_NCEM, y = y_CMS_NCEM, 
                                       group = interaction(label, type), 
                                       colour = factor(type)), 
                                   size = size.traj)
    }else{
      fplot = ggplot() + geom_path(data = df, aes(x = x_CMS_NCEM, 
                                                  y = y_CMS_NCEM, 
                                                  group = label), 
                                   size = size.traj)
    }
    
    #---------------------------------------------------------------------------
    # First & last solutions
    #---------------------------------------------------------------------------
    if(!isSingle)
    {
      fplot = fplot + geom_path(data = df[which(df$label == min(df$label)),], 
                                aes(x = x_CMS_NCEM, y = y_CMS_NCEM, 
                                    group = interaction(label, type)), 
                                colour = muted("red"),
                                size = 2*size.traj)
      
      fplot = fplot + geom_path(data = df[which(df$label == max(df$label)),], 
                                aes(x = x_CMS_NCEM, y = y_CMS_NCEM, 
                                    group = interaction(label, type)), 
                                colour = muted("blue"),
                                size = 2*size.traj)
    }
    
    
    #---------------------------------------------------------------------------
    # Additional objects
    #---------------------------------------------------------------------------
    #Add EMLi
    fplot = fplot + geom_point(data = dfemli, aes(x= x_NC, y = y_NC), size = size.lib) 
    
    #Add Moon
    if(isPrimary)
    {
      fplot = fplot + geom_point(data = dfmoon_eml, aes(x= x_NC, y = y_NC), size = size.prim)
    }
    
    #---------------------------------------------------------------------------
    # Zoom
    #---------------------------------------------------------------------------
    # Limits on x-axis
    if(is.na(limits.x[1]) && is.na(limits.x[2]) )
    {
      if(lib.point.em == "L2")
      {
        fplot = fplot + scale_x_continuous(limits = c(-1.25, 0.25))
      }else
      {
        fplot = fplot + scale_x_continuous(limits = c(-1, 2))
      }
    }else
    {
      fplot = fplot + scale_x_continuous(limits = limits.x)
    }
    
    # Limits on y-axis
    if(is.na(limits.y[1]) && is.na(limits.y[2]))
    {
      if(lib.point.em == "L2")
      {
        fplot = fplot + scale_y_continuous(limits = c(-0.6, 0.6))
      }else
      {
        fplot = fplot + scale_y_continuous(limits = c(-1, 1))
      }
    }else
    {
      fplot = fplot + scale_y_continuous(limits = limits.y)
    }
    
    #---------------------------------------------------------------------------
    # Annotations & labels
    #---------------------------------------------------------------------------
    # EMLi
    fplot.pdf = fplot + annotate("text", x = 0, y = y.position.emlt,  
                                 label = paste0("bold(EML[", num.em, "])"), colour = "white",
                                 size = 5, parse = TRUE)
    fplot.pdf = fplot.pdf + annotate("text", x = 0, y = y.position.emlt,  label = paste0("EML[", num.em, "]"), size = 5, parse = TRUE)
    
    # Moon
    if(isPrimary)
    {
      fplot.pdf = fplot.pdf + annotate("text", x = moon.x, y = -0.08, 
                                       label = "bold(Moon)",  colour = "white",
                                       size = 5, parse = TRUE)
      fplot.pdf = fplot.pdf + annotate("text", x = moon.x, y = -0.08, label = "Moon", size = 5)
    }
    
    #---------------------------------------------------------------------------
    # Annotations & labels in tex form
    #---------------------------------------------------------------------------
    fplot.tex = fplot
    if(lib.point.em == "L2")
    {
      # EMLi
      fplot.tex = fplot.tex + annotate("text", x = 0, y = -0.08,  label = "\\textsc{eml}$_2$", size = 5)
    }else{
      # EMLi
      fplot.tex = fplot.tex + annotate("text", x = 0, y = -0.08,  label = "\\textsc{eml}$_1$", size = 5)
    }
    
    # Moon
    if(isPrimary)
    {
      fplot.tex = fplot.tex + annotate("text", x = moon.x, y = -0.08, label = "Moon", size = 5)
    }
  } else
  {
    warning('Unknown framework: should be SEM or EM.')
  }
  
  #---------------------------------------------------------------------------
  # Annotations & labels
  #---------------------------------------------------------------------------
  fplot.pdf = fplot.pdf + labs(x = xlab, y = ylab)
  fplot.tex = fplot.tex + labs(x = xlab.tex, y = ylab.tex)
  
  
  #-----------------------------------------------------------------------------
  # Guide: if there is a colour.traj argument, the transfer leg is painted in 
  # this color.
  #-----------------------------------------------------------------------------
  #scd = scale_colour_discrete(guide = FALSE);
  if(is.na(colour.traj))
  {
    scd = scale_colour_discrete(guide = FALSE)
    
  }else
  {
    if("type" %in% colnames(df))
    {
      if(length(unique(df$type)) == 3)
      {
        set1 = rev(brewer.pal(3,"Set1"))
        scd = scale_colour_manual(values = c(set1[3], colour.traj, set1[2]), guide = FALSE)
      }else if(length(unique(df$type)) == 1)
      {
        scd = scale_colour_manual(values = c(colour.traj), guide = FALSE)
      }else{
        scd = scale_colour_discrete(guide = FALSE)
      }
      
    }else
    {
      scd = scale_colour_manual(values = c(colour.traj), guide = FALSE)
    }
  }
  
  fplot.pdf = fplot.pdf + scd
  fplot.tex = fplot.tex + scd
  
  #---------------------------------------------------------------------------
  #Theme
  #---------------------------------------------------------------------------
  fplot.pdf = fplot.pdf + custom_theme
  fplot.pdf = fplot.pdf + coord_fixed(ratio=1)
  
  fplot.tex = fplot.tex + custom_theme
  fplot.tex = fplot.tex + coord_fixed(ratio=1)
  
  #-----------------------------------------------------------------------------
  # Save in pdf, with annotations
  #-----------------------------------------------------------------------------
  #fplot.pdf = fplot.pdf + issfd_theme
  if(isPDF){ggsave(fplot.pdf, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".pdf"))} #Save in pdf
  if(isPNG){ggsave(fplot.pdf, width = xSize, height = ySize,  bg = "transparent",  device=cairo_pdf,  file = paste0(filename, ".png"))} #Save in png
  
  #-----------------------------------------------------------------------------
  #Save in Latex, with annotations
  #-----------------------------------------------------------------------------
  if(isLaTeX)
  {
    fplot.tex = fplot.tex + labs(x = xlab.tex, y = ylab.tex)
    ggplot2tikz_phd(fplot.tex, width = xSize, height = ySize, file = paste0(filename, "_tex.tex"))
  }
  
  
  return(fplot.pdf)
}

