createComposites <- function(data) {
  
  data$TotalActivityOccur <- data$SniffAnoEv + data$SniffHeadEv + data$DigEv + data$ActiveHEv + 
    data$AvoidEv + data$SubEv + data$DefenseEv + data$OpenAggEv + 
    data$RitAggEv + data$ActiveVEv + data$ApproachEv + data$StereoEv + 
    data$DomEv + data$SniffBodyEv + data$FollowEv + data$StretchEv + 
    data$AttackDelEv + data$AttackRecEv
  
  data$TotalActivityDuration <- data$SniffAnoDur + data$SniffHeadDur + data$DigDur + data$ActiveHDur + 
    data$AvoidDur + data$SubDur + data$DefenseDur + data$OpenAggDur + 
    data$RitAggDur + data$ActiveVDur + data$ApproachDur + data$StereoDur + 
    data$DomDur + data$SniffBodyDur + data$FollowDur
  
  data$TotalSocialOccur <- data$SniffAnoEv + data$SniffHeadEv + data$AvoidEv + data$SubEv + 
    data$DefenseEv + data$OpenAggEv + data$RitAggEv + data$ApproachEv + data$InactiveTogetherEv +
    data$DomEv + data$SniffBodyEv + data$FollowEv + data$StretchEv + 
    data$AttackDelEv + data$AttackRecEv
  
  data$TotalSocialDuration <- data$SniffAnoDur + data$SniffHeadDur + data$AvoidDur + data$SubDur + 
    data$DefenseDur + data$OpenAggDur + data$RitAggDur + data$InactiveTogetherDur +
    data$ApproachDur + data$DomDur + data$SniffBodyDur + data$FollowDur
  
  data$AgonisticDeliveredOccur <- data$FollowEv + data$DomEv + data$AttackDelEv
  data$AgonisticDeliveredDuration <- data$FollowDur + data$DomDur
  
  data$AgonisticReceivedOccur <- data$AvoidEv + data$SubEv + data$AttackRecEv + data$DefenseEv
  data$AgonisticReceivedDuration <- data$AvoidDur + data$SubDur + data$DefenseDur
  
  data$AgonisticTotalOccur <- data$AgonisticDeliveredOccur + data$AgonisticReceivedOccur
  data$AgonisticTotalDuration <- data$AgonisticDeliveredDuration + data$AgonisticReceivedDuration
  
  data$DominanceScoreOccur <- data$AgonisticDeliveredOccur - data$AgonisticReceivedOccur
  data$DominanceScoreDuration <- data$AgonisticDeliveredDuration - data$AgonisticReceivedDuration
  
  data$SocialInvestigationOccur <- data$SniffHeadEv + data$SniffBodyEv + data$SniffAnoEv + data$StretchEv + data$ApproachEv
  data$SocialInvestigationDuration <- data$SniffHeadDur + data$SniffBodyDur + data$SniffAnoDur + data$ApproachDur
  
  data$NonSocialOccur <- data$ActiveHEv + data$ActiveVEv + data$DigEv + data$StereoEv + data$InactiveEv + 
    data$GroomEv
  data$NonSocialDuration <- data$ActiveHDur + data$ActiveVDur + data$DigDur + data$StereoDur + 
    data$InactiveDur + data$GroomDur
  
  data$NonSocialLocomotorOccur <- data$ActiveHEv + data$ActiveVEv + data$DigEv
  data$NonSocialLocomotorDuration <- data$ActiveHDur + data$ActiveVDur + data$DigDur
  
  data$InactiveNonSocialOccur <- data$InactiveEv + data$GroomEv
  data$InactiveNonSocialDuration <- data$InactiveDur + data$GroomDur
  
  return(data)
  
}

createFactors <- function(data) {
  data$Gonad <- factor(data$Gonad, levels = c("SHAM", "GDX", "REP"))
  data$Sex <- as.factor(data$Sex)
  data$Treatment <- as.factor(data$Treatment)
  
  return(data)
}

lnTransform <- function(data) {
  
  data <- log(data + 1)
  
  return(data)
  
}

sqrtTransform <- function(data) {
  
  data <- sqrt(data + 1)
  
  return(data)
  
}


intruderGraph <- function(data, yVar, variableName, graphTitle, yMax, fileName) {
  intruderBar <- ggplot(data, aes_string(x = "Gonad", y = yVar, fill = "Treatment")) +
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.9), width = 0.2, size  = 1) +
    coord_cartesian(ylim = c(0, yMax)) +
    scale_y_continuous(expand = c(0, 0)) +
    
    labs (title = graphTitle, x = NULL, y = variableName) +
    scale_x_discrete(breaks=c("SHAM", "GDX", "REP"), limits=c("SHAM", "GDX", "REP"), labels=c("Intact", "Gonadectomy + \nVehicle", "Gonadectomy + \nReplacement")) +
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
  
  intruderBar
  
}



intruderPanel <- function(data, yVar, variableName, graphTitle, yMax, fileName, saveGraph = F) {
  
  maleGraph <- intruderGraph(subset(data, Sex == "M"), yVar, variableName, "Male", yMax)
  femaleGraph <- intruderGraph(subset(data, Sex == "F"), yVar, variableName, "Female", yMax)
  graphLegend <- getLegend(femaleGraph);
  
  maleGraph <- maleGraph + theme(legend.position = "none") 
  femaleGraph <- femaleGraph + theme(legend.position = "none") 
  
  if (saveGraph == T) {
    pdf(paste("output/PostPuberty/", fileName, sep = ""), 8, 9)
    grid.arrange(maleGraph, graphLegend, femaleGraph, ncol = 2, nrow = 2, widths = c(7, 2))
    dev.off()
  } else {
    grid.arrange(maleGraph, graphLegend, femaleGraph, ncol = 2, nrow = 2, widths = c(7, 2))
  }
  
}

dominancePanel <- function(data, yVar, variableName, graphTitle, yMin, yMax, fileName, saveGraph = F) {

  maleGraph <- dominanceGraph(subset(data, Sex == "M"), yVar, variableName, "Male", yMin, yMax)
  femaleGraph <- dominanceGraph(subset(data, Sex == "F"), yVar, variableName, "Female", yMin, yMax)
  graphLegend <- getLegend(femaleGraph);
  
  maleGraph <- maleGraph + theme(legend.position = "none") 
  femaleGraph <- femaleGraph + theme(legend.position = "none") 
  
  if (saveGraph == T) {
    pdf(paste("output/PostPuberty/", fileName, sep = ""), 8, 9)
    grid.arrange(maleGraph, graphLegend, femaleGraph, ncol = 2, nrow = 2, widths = c(7, 2))
    dev.off()
  } else {
    grid.arrange(maleGraph, graphLegend, femaleGraph, ncol = 2, nrow = 2, widths = c(7, 2))
  }
  
}

dominanceGraph <- function(data, yVar, variableName, graphTitle, yMin, yMax, fileName) {
  dominanceBar <- ggplot(data, aes_string(x = "Gonad", y = yVar, fill = "Treatment")) +
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.9), width = 0.2, size  = 1) +
    coord_cartesian(ylim = c(yMin, yMax)) +
    scale_y_continuous(expand = c(0, 0)) +
    
    labs (title = graphTitle, x = NULL, y = variableName) +
    scale_x_discrete(breaks=c("SHAM", "GDX", "REP"), limits=c("SHAM", "GDX", "REP"), labels=c("Intact", "Gonadectomy + \nVehicle", "Gonadectomy + \nReplacement")) +
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
  
  dominanceBar
}

intruderGraphPre <- function(data, yVar, variableName, graphTitle, yMax, fileName) {
  intruderBar <- ggplot(data, aes_string(x = "Sex", y = yVar, fill = "Treatment")) +
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.9), width = 0.2, size  = 1) +
    coord_cartesian(ylim = c(0, yMax)) +
    scale_y_continuous(expand = c(0, 0)) +
    
    labs (title = graphTitle, x = NULL, y = variableName) +
    scale_x_discrete(breaks=c("F", "M"), labels=c("Female", "Male")) +
    scale_fill_manual("Prenatal Treatment", values = c("light gray", "black"), labels = c("Sesame Oil", "Testosterone")) +
    
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
  
  intruderBar
  
}


dominanceGraphPre <- function(data, yVar, variableName, graphTitle, yMin, yMax, fileName) {
  dominanceBar <- ggplot(data, aes_string(x = "Sex", y = yVar, fill = "Treatment")) +
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.9), width = 0.2, size  = 1) +
    coord_cartesian(ylim = c(0, yMax)) +
    scale_y_continuous(expand = c(0, 0)) +
    
    labs (title = graphTitle, x = NULL, y = variableName) +
    scale_x_discrete(breaks=c("F", "M"), labels=c("Female", "Male")) +
    scale_fill_manual("Prenatal Treatment", values = c("light gray", "black"), labels = c("Sesame Oil", "Testosterone")) +
    
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
  
  dominanceBar
}

getLegend <- function(myPlot){
  tmp <- ggplot_gtable(ggplot_build(myPlot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}