createFactors <- function(data) {

  data$Gonad <- factor(data$Gonad, levels = c("SHAM", "GDX", "REP"))
  data$Sex <- as.factor(data$Sex)
  data$Treatment <- as.factor(data$Treatment)
  
  
  return(data)
}


weightGraph <- function(dataSet, saveFile = F) {
  weightLine <- ggplot(dataSet, aes(x=Time, y=Weight, group=Treatment, colour = Treatment, shape = Treatment)) +
    
    stat_summary(fun.y = mean, geom = "line", size = 2) +
    stat_summary(fun.y = mean, geom = "point", size = 5, stroke = 1.5, show.legend = FALSE, fill = "white") +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, size = 2) + 
    stat_summary(fun.y = mean, geom = "point", size = 5, show.legend = FALSE, fill = "white") +
    
    labs(title = "", x = "Time", y = "Litter Weight (grams)", colour = "Prenatal Treatment") +
    
    coord_cartesian(ylim = c(0, 150)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_discrete(breaks=c("PD4", "PD8", "PD12", "PD16", "PD20"), labels=c("Day 4", "Day 8", "Day 12", "Day 16", "Day 20")) + 
    scale_shape_manual(values = c(21, 22)) +
    scale_color_manual("Prenatal Treatment", limits = c("C", "T"), breaks = c("C", "T"), values = c("black", "dark gray"), labels = c("Sesame Oil", "Testosterone")) +
    
    
    theme_bw() +
    theme(plot.title = element_text(colour="black", size=30, margin = margin(60, 0, 0, 0), face="bold"),
          axis.title.y = element_text(size=26, margin = margin(0, 30, 0, 0), face="bold"),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=20, color = "black"), 
          axis.text.x = element_text(size=26, color = "black", face="bold"), 
          axis.ticks = element_line(size=1.6, color = "black"),
          legend.text = element_text(size = 18, colour = "black", face = "bold"),
          legend.title = element_text(size = 24, colour = "black", face = "bold"),
          legend.key = element_blank(),
          legend.key.size = unit(1.5, "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(colour = 'black', size = 1.6),
          axis.line.y = element_line(colour = 'black', size = 1.6))

    if (saveFile == T) {
      ggsave(filename = "output/developmental-weights.pdf", plot = weightLine, width = 16, height = 9)
    } else {
      weightLine
    }
}

barPanel <- function(data, yVar, variableName, graphTitle, yMax, fileName) {
  
  maleGraph <- barGraph(subset(data, Sex == "M"), yVar, variableName, "Male", yMax)
  femaleGraph <- barGraph(subset(data, Sex == "F"), yVar, variableName, "Female", yMax)
  graphLegend <- getLegend(femaleGraph);
  
  maleGraph <- maleGraph + theme(legend.position = "none") 
  femaleGraph <- femaleGraph + theme(legend.position = "none") 
  
  pdf(paste("output/", fileName, sep = ""), 14, 5)
  grid.arrange(maleGraph, femaleGraph, ncol = 2, nrow = 1, widths = c(7, 7), top = textGrob(graphTitle, gp=gpar(fontsize=18, fontface = "bold")))
  dev.off()
  
}

barGraph <- function(data, yVar, variableName, graphTitle, yMax, fileName) {
  bar <- ggplot(data, aes_string(x = "Gonad", y = yVar, fill = "Treatment")) +
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.9), width = 0.2, size  = 1) +
    coord_cartesian(ylim = c(0, yMax)) +
    scale_y_continuous(expand = c(0, 0)) +
    
    labs (title = graphTitle, x = NULL, y = variableName) +
    scale_x_discrete(breaks=c("SHAM", "GDX", "REP"), labels=c("Intact", "Gonadectomy + \nVehicle", "Gonadectomy + \nReplacement")) +
    scale_fill_manual("Prenatal Treatment", values = c("light gray", "black"), labels = c("Sesame Oil", "Testosterone")) +
    
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
  
  bar
  
}

simpleBarGraph <- function(data, yVar, variableName, graphTitle, yMax, fileName) {
  bar <- ggplot(data, aes_string(x = "Treatment", y = yVar, fill = "Treatment")) +
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.9), width = 0.2, size  = 1) +
    coord_cartesian(ylim = c(0, yMax)) +
    scale_y_continuous(expand = c(0, 0)) +
    
    labs (title = graphTitle, x = NULL, y = variableName) +
    scale_x_discrete(breaks=c("C", "T"), labels=c("Sesame Oil", "Testosterone")) +
    scale_fill_manual("Prenatal Treatment", values = c("light gray", "black"), labels = c("Sesame Oil", "Testosterone")) +
    
    theme_bw() +
    theme(plot.title = element_text(colour="black", size=20, face="bold"),
          axis.title.y = element_text(size=18, margin = margin(0, 30, 0, 0), face="bold"),
          axis.title.x = element_text(size=18, face="bold"),
          axis.text.y = element_text(size=16, color = "black"), 
          axis.text.x = element_text(size=16, color = "black", face="bold"), 
          axis.ticks = element_line(size=1, color = "black"),
          legend.text = element_text(size = 16, colour = "black"),
          legend.title = element_text(size = 18, colour = "black", face="bold"),
          legend.key = element_blank(),
          legend.key.size = unit(2, "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(colour = 'black', size = 1),
          axis.line.y = element_line(colour = 'black', size = 1))
  
  bar
  
}

maleFemaleBarGraph <- function(data, yVar, variableName, graphTitle, yMax, fileName) {
  bar <- ggplot(data, aes_string(x = "Sex", y = yVar, fill = "Treatment")) +
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.9), width = 0.2, size  = 1) +
    coord_cartesian(ylim = c(0, yMax)) +
    scale_y_continuous(expand = c(0, 0)) +
    
    labs (title = graphTitle, x = NULL, y = variableName) +
    scale_x_discrete(limits = c("M", "F"), breaks=c("M", "F"), labels=c("Male", "Female")) +
    scale_fill_manual("Prenatal Treatment", values = c("light gray", "black"), labels = c("Sesame Oil", "Testosterone")) +
    
    theme_bw() +
    theme(plot.title = element_text(colour="black", size=20, face="bold"),
          axis.title.y = element_text(size=18, margin = margin(0, 30, 0, 0), face="bold"),
          axis.title.x = element_text(size=18, face="bold"),
          axis.text.y = element_text(size=16, color = "black"), 
          axis.text.x = element_text(size=16, color = "black", face="bold"), 
          axis.ticks = element_line(size=1, color = "black"),
          legend.text = element_text(size = 16, colour = "black"),
          legend.title = element_text(size = 18, colour = "black", face="bold"),
          legend.key = element_blank(),
          legend.key.size = unit(1.5, "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(colour = 'black', size = 1),
          axis.line.y = element_line(colour = 'black', size = 1))
  
  bar
  
}


# melt data for graphing
weightMelt <- function(data) {
  
  id <- 1:nrow(data)
  data <- cbind(id = as.factor(id), data)
  
  meltData <- melt(data, id.vars = c("id", "Litter", "Treatment"), 
                   measure.vars = c("PD4", "PD8", 
                                    "PD12", "PD16", "PD20"), 
                   value.name = "Weight", variable.name = "Time")
  
  return(meltData)
  
}

getLegend <- function(myPlot){
  tmp <- ggplot_gtable(ggplot_build(myPlot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

