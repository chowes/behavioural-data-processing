# clean light dark data
ldClean <- function(data) {
  
  # find negative consumption
  badMatrix <- (data$Light_LATENCY_W == 0) # create a matrix of impossible negative values following computation of consumption
  badMatrix[is.na(badMatrix)] <- F # handles missing data, which cannot be subject to boolean operators.  this sets missing data to F, so it is ignored
  if(any(badMatrix == T)){data[badMatrix,]$Light_LATENCY_W <- NA} # sets all scores that are below 0 to 0, use if statement to avoid returning an error if all values are false

  data$Gonad <- factor(data$Gonad, levels = c("PRE", "SHAM", "GDX", "REP"))
  data$Sex <- as.factor(data$Sex)
  data$Time <- as.factor(data$Time)
  data$Treatment <- as.factor(data$Treatment)
  
  
  return(data)
}

lightDarkPanel <- function(data, yVar, variableName, graphTitle, yMax, fileName) {
  
  maleGraph <- lightDarkGraph(subset(data, Sex == "M"), yVar, variableName, "Male", yMax)
  femaleGraph <- lightDarkGraph(subset(data, Sex == "F"), yVar, variableName, "Female", yMax)
  graphLegend <- getLegend(femaleGraph);
  
  maleGraph <- maleGraph + theme(legend.position = "none") 
  femaleGraph <- femaleGraph + theme(legend.position = "none") 

  pdf(paste("output/", fileName), 8, 9)
  grid.arrange(maleGraph, graphLegend, femaleGraph, ncol = 2, nrow = 2, widths = c(7, 2))
  dev.off()
  
}

lightDarkGraph <- function(data, yVar, variableName, graphTitle, yMax, fileName) {
  ldBar <- ggplot(data, aes_string(x = "Gonad", y = yVar, fill = "Treatment")) +
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.9), width = 0.2, size  = 1) +
    coord_cartesian(ylim = c(0, yMax)) +
    scale_y_continuous(expand = c(0, 0)) +
    
    labs (title = graphTitle, x = NULL, y = variableName) +
    scale_x_discrete(breaks=c("SHAM", "GDX", "REP"), labels=c("Intact", "Gonadectomy + \nVehicle", "Gonadectomy + \nReplacement")) +
    scale_fill_manual("Treatment", values = c("light gray", "black"), labels = c("Control", "Testosterone")) +
    
    theme_bw() +
    theme(plot.title = element_text(colour="black", size=14, face="bold"),
          axis.title.y = element_text(size=14, margin = margin(0, 30, 0, 0), face="bold"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.text.y = element_text(size=12, color = "black"), 
          axis.text.x = element_text(size=12, color = "black", face="bold"), 
          axis.ticks = element_line(size=1, color = "black"),
          legend.text = element_text(size = 12, colour = "black"),
          legend.title = element_text(size = 14, colour = "black", face="bold"),
          legend.key = element_blank(),
          legend.key.size = unit(1, "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(colour = 'black', size = 1),
          axis.line.y = element_line(colour = 'black', size = 1))
  
  ldBar
  
}


# take a data set and a title and create graphs
lightDarkGraphPre <- function(data, yVar, variableName, graphTitle, yMax, save, fileName) {
  lightDarkBar <- ggplot(data, aes_string(x = "Sex", y = yVar, fill = "Treatment")) +
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.9), width = 0.2, size  = 1) +
    coord_cartesian(ylim = c(0, yMax)) +
    scale_y_continuous(expand = c(0, 0)) +
    
    labs (title = graphTitle, x = NULL, y = variableName) +
    scale_x_discrete(breaks=c("F", "M"), labels=c("Female", "Male")) +
    scale_fill_manual("Treatment", values = c("light gray", "black"), labels = c("Control", "Testosterone")) +
    
    theme_bw() +
    theme(plot.title = element_text(colour="black", size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.text.y = element_text(size = 12, color = "black", face = "bold"), 
          axis.text.x = element_text(size=14, color = "black", face="bold"), 
          axis.ticks = element_line(size=1, color = "black"),
          legend.text = element_text(size = 12, colour = "black", face = "bold"),
          legend.title = element_text(size = 14, colour = "black", face = "bold"),
          legend.key = element_blank(),
          legend.key.size = unit(1.5,"cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(colour = 'black', size = 1),
          axis.line.y = element_line(colour = 'black', size = 1))
  
  if (save == TRUE) {
    fileName = paste("output/", fileName)
    ggsave(file=fileName)
  } else {
    lightDarkBar
  }
}


# take a data set and a title and create graphs
lightDarkGraphPost <- function(data, yVar, variableName, graphTitle, yMax, save, fileName) {
  
  data$group <- paste(data$Sex, data$Treatment)
  
  lightDarkBar <- ggplot(data, aes_string(x = "group", y = yVar, fill = "Gonad")) +
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.9), width = 0.3, size  = 1.2) +
    coord_cartesian(ylim = c(0, yMax)) +
    scale_y_continuous(expand = c(0, 0)) +
     
    labs (title = graphTitle, x = NULL, y = variableName) +
    scale_x_discrete(breaks=c("F C", "F D", "M C", "M D"), labels=c("Female - Control", "Female - Treated", "Male - Control", "Male - Treated")) +
    #scale_fill_manual("Gonad", values = c("red", "blue", "black"), labels = c("GDX", "Replacement","Sham")) +
    
    theme_bw() +
    theme(plot.title = element_text(colour="black", size=30, margin = margin(0, 0, 40, 0), face="bold"),
          axis.title.y = element_text(size=26, margin = margin(0, 30, 0, 0), face="bold"),
          axis.title.x = element_text(size=26, face="bold"),
          axis.text.y = element_text(size=20, color = "black"), 
          axis.text.x = element_text(size=26, color = "black", face="bold"), 
          axis.ticks = element_line(size=1.6, color = "black"),
          legend.text = element_text(size = 16, colour = "black"),
          legend.title = element_text(size = 18, colour = "black"),
          legend.key = element_blank(),
          legend.key.size = unit(1.5, "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(colour = 'black', size = 1.6),
          axis.line.y = element_line(colour = 'black', size = 1.6))
  
  if (save == TRUE) {
    fileName = paste("output/", fileName)
    ggsave(file=fileName)
  } else {
    lightDarkBar
  }
}

getLegend <- function(myPlot){
  tmp <- ggplot_gtable(ggplot_build(myPlot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}



# take a data set and a title and create graphs
lightDarkGraphPrePoster <- function(data, yVar, variableName, graphTitle, yMax, save, fileName) {
  lightDarkBar <- ggplot(data, aes_string(x = "Sex", y = yVar, fill = "Treatment")) +
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.9), width = 0.3, size  = 1.6) +
    coord_cartesian(ylim = c(0, yMax)) +
    scale_y_continuous(expand = c(0, 0)) +
    
    labs (title = graphTitle, x = NULL, y = variableName) +
    scale_x_discrete(breaks=c("F", "M"), labels=c("Female", "Male")) +
    scale_fill_manual("Treatment", values = c("firebrick1", "dodgerblue"), labels = c("Control", "Testosterone")) +
    
    theme_bw() +
    theme(plot.title = element_text(colour="black", size=36, margin = margin(0, 0, 40, 0), face="bold"),
          axis.title.y = element_text(size=36, margin = margin(0, 30, 0, 0), face="bold"),
          axis.title.x = element_text(size=36, face="bold"),
          axis.text.y = element_text(size=26, color = "black", face = "bold"), 
          axis.text.x = element_text(size=30, color = "black", face="bold"), 
          axis.ticks = element_line(size=2, color = "black"),
          legend.text = element_text(size = 20, colour = "black", face = "bold"),
          legend.title = element_text(size = 26, colour = "black", face = "bold"),
          legend.key = element_blank(),
          legend.key.size = unit(2,"cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(colour = 'black', size = 2),
          axis.line.y = element_line(colour = 'black', size = 2))
  
  if (save == TRUE) {
    fileName = paste("output/", fileName)
    ggsave(file=fileName)
  } else {
    lightDarkBar
  }
}


