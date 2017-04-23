


# unblind treatment group
unblindTreatment <- function(stfpData) {
  for (i in 1:nrow(stfpData)){
    if((stfpData[i,]$Group == "A") || (stfpData[i,]$Group == "D")){
      stfpData[i,]$Group <- "T"
    }
    else if((stfpData[i,]$Group == "B") || (stfpData[i,]$Group == "C")){
      stfpData[i,]$Group <- "C"
    }
  }
  
  return(stfpData)
}



# generate flavour consumption data based on which feeder was left/right
# TUM == FLA == CIN
# THY == FLB == COC
genFlavourConsumption <- function(stfpData){
  
  stfpData$FLA2H <- NA
  stfpData$FLB2H <- NA
  stfpData$FLA4H <- NA
  stfpData$FLB4H <- NA
  stfpData$FLA6H <- NA
  stfpData$FLB6H <- NA
  stfpData$FLA8H <- NA
  stfpData$FLB8H <- NA
  
  # compute tum and thy consumption at each time point based on whether the left feeder was tum or thy
  for (i in 1:nrow(stfpData)){
    if(stfpData[i,]$LeftFeed == "TUM" || stfpData[i,]$LeftFeed == "CIN"){
      stfpData[i,]$FLA2H <- stfpData[i,]$Left0H - stfpData[i,]$Left2H
      stfpData[i,]$FLB2H <- stfpData[i,]$Right0H - stfpData[i,]$Right2H
      stfpData[i,]$FLA4H <- stfpData[i,]$Left2H - stfpData[i,]$Left4H
      stfpData[i,]$FLB4H <- stfpData[i,]$Right2H - stfpData[i,]$Right4H
      stfpData[i,]$FLA6H <- stfpData[i,]$Left4H - stfpData[i,]$Left6H
      stfpData[i,]$FLB6H <- stfpData[i,]$Right4H - stfpData[i,]$Right6H
      stfpData[i,]$FLA8H <- stfpData[i,]$Left6H - stfpData[i,]$Left8H
      stfpData[i,]$FLB8H <- stfpData[i,]$Right6H - stfpData[i,]$Right8H
    }
    else if(stfpData[i,]$LeftFeed == "THY" || stfpData[i,]$LeftFeed == "COC"){
      stfpData[i,]$FLA2H <- stfpData[i,]$Right0H - stfpData[i,]$Right2H
      stfpData[i,]$FLB2H <- stfpData[i,]$Left0H - stfpData[i,]$Left2H
      stfpData[i,]$FLA4H <- stfpData[i,]$Right2H - stfpData[i,]$Right4H
      stfpData[i,]$FLB4H <- stfpData[i,]$Left2H - stfpData[i,]$Left4H
      stfpData[i,]$FLA6H <- stfpData[i,]$Right4H - stfpData[i,]$Right6H
      stfpData[i,]$FLB6H <- stfpData[i,]$Left4H - stfpData[i,]$Left6H
      stfpData[i,]$FLA8H <- stfpData[i,]$Right6H - stfpData[i,]$Right8H
      stfpData[i,]$FLB8H <- stfpData[i,]$Left6H - stfpData[i,]$Left8H
    }
  }
  
  return(stfpData)
}



# generate total consumption data
genTotalConsumption <- function(stfpData) {
  
  stfpData$CON2H <- stfpData$FLA2H + stfpData$FLB2H
  stfpData$CON4H <- stfpData$FLA4H + stfpData$FLB4H
  stfpData$CON6H <- stfpData$FLA6H + stfpData$FLB6H
  stfpData$CON8H <- stfpData$FLA8H + stfpData$FLB8H
  
  return(stfpData)
}



# generate preference data
genPreferenceData <- function(stfpData){
  
  # calculate cin preference ratio for all animals
  stfpData$flavAPref2H <- stfpData$FLA2H / (stfpData$FLA2H + stfpData$FLB2H)
  stfpData$flavAPref4H <- stfpData$FLA4H / (stfpData$FLA4H + stfpData$FLB4H)
  stfpData$flavAPref6H <- stfpData$FLA6H / (stfpData$FLA6H + stfpData$FLB6H)
  stfpData$flavAPref8H <- stfpData$FLA8H / (stfpData$FLA8H + stfpData$FLB8H)
  
  # calculate dem preference ratio for all animals
  stfpData$demPref2H <- NA
  stfpData$demPref4H <- NA
  stfpData$demPref6H <- NA
  stfpData$demPref8H <- NA
  
  for (i in 1:nrow(stfpData)){
    if(stfpData[i,]$DemFood == "TUM" || stfpData[i,]$DemFood == "CIN"){
      stfpData[i,]$demPref2H <- stfpData[i,]$FLA2H / (stfpData[i,]$FLA2H + stfpData[i,]$FLB2H)
      stfpData[i,]$demPref4H <- stfpData[i,]$FLA4H / (stfpData[i,]$FLA4H + stfpData[i,]$FLB4H)
      stfpData[i,]$demPref6H <- stfpData[i,]$FLA6H / (stfpData[i,]$FLA6H + stfpData[i,]$FLB6H)
      stfpData[i,]$demPref8H <- stfpData[i,]$FLA8H / (stfpData[i,]$FLA8H + stfpData[i,]$FLB8H)
    }
    else if(stfpData[i,]$DemFood == "THY" || stfpData[i,]$DemFood == "COC"){
      stfpData[i,]$demPref2H <- stfpData[i,]$FLB2H / (stfpData[i,]$FLA2H + stfpData[i,]$FLB2H)
      stfpData[i,]$demPref4H <- stfpData[i,]$FLB4H / (stfpData[i,]$FLA4H + stfpData[i,]$FLB4H)
      stfpData[i,]$demPref6H <- stfpData[i,]$FLB6H / (stfpData[i,]$FLA6H + stfpData[i,]$FLB6H)
      stfpData[i,]$demPref8H <- stfpData[i,]$FLB8H / (stfpData[i,]$FLA8H + stfpData[i,]$FLB8H)
    }
  }
  
  return(stfpData)
}



# generate graphs

prepubLearningPanel <- function(data, yVar, variableName, graphTitle, yMax, fileName) {
  
  controlGraph <- summaryGraph(subset(data, Group == "C"), yVar, variableName, "Sesame Oil", yMax)
  testGraph <- summaryGraph(subset(data, Group == "T"), yVar, variableName, "Testosterone", yMax)
  graphLegend <- getLegend(controlGraph);
  
  controlGraph <- controlGraph + theme(legend.position = "none") 
  testGraph <- testGraph + theme(legend.position = "none") 
  
  pdf(paste("output/", fileName), 16, 7)
  grid.arrange(controlGraph, testGraph, graphLegend, ncol = 3, nrow = 1, widths = c(7, 7, 2))
  dev.off()
  
}

stfpGraphPre <- function(dataSet, graphTitle, flavourA, flavourB, showLegend = T) {
  stfpLine <- ggplot(dataSet, aes(x=Time, y=FlavAPref, group=DemFood, colour = DemFood, shape = DemFood)) +
    
    stat_summary(fun.y = mean, geom = "line", size = 2) +
    stat_summary(fun.y = mean, geom = "point", size = 5, stroke = 1.5, show.legend = FALSE, fill = "white") +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, size = 2) + 
    stat_summary(fun.y = mean, geom = "point", size = 5, show.legend = FALSE, fill = "white") +
    
    labs(title = graphTitle, x = "Time", y = paste(flavourA, " Preference Ratio"), colour = "DEM Food") +
    
    coord_cartesian(ylim = c(0, 1)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_discrete(breaks=c("flavAPref2H", "flavAPref4H", "flavAPref6H", "flavAPref8H"), labels=c("2 Hours", "4 Hours", "6 Hours", "8 Hours")) + 
    scale_shape_manual(values = c(21, 22)) +
    scale_color_manual("Demonstrated Food", limits = c("CIN", "COC"), breaks = c("CIN", "COC"), values = c("black", "dark gray"), labels = c(flavourA, flavourB)) +
    
    
    theme_bw() +
    theme(plot.title = element_text(colour="black", size=30, margin = margin(60, 0, 0, 0), face="bold"),
          axis.title.y = element_text(size=26, margin = margin(0, 30, 0, 0), face="bold"),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=20, color = "black"), 
          axis.text.x = element_text(size=26, color = "black", face="bold"), 
          axis.ticks = element_line(size=1.6, color = "black"),
          legend.text = element_text(size = 26, colour = "black", face = "bold"),
          legend.title = element_text(size = 28, colour = "black", face = "bold"),
          legend.key = element_blank(),
          legend.key.size = unit(3.5, "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(colour = 'black', size = 1.6),
          axis.line.y = element_line(colour = 'black', size = 1.6))
  
  if (showLegend == F) {
    stfpLine <- stfpLine + theme(legend.position = "none")
  }
  
  stfpLine
}

stfpGraph <- function(dataSet, graphTitle, flavourA, flavourB, showLegend = T) {
  stfpLine <- ggplot(dataSet, aes(x=Time, y=FlavAPref, group=DemFood, colour = DemFood, shape = DemFood)) +
    
    stat_summary(fun.y = mean, geom = "line", size = 2) +
    stat_summary(fun.y = mean, geom = "point", size = 5, stroke = 1.5, show.legend = FALSE, fill = "white") +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, size = 2) + 
    stat_summary(fun.y = mean, geom = "point", size = 5, show.legend = FALSE, fill = "white") +
    
    labs(title = graphTitle, x = "Time", y = paste(flavourA, " Preference Ratio"), colour = "DEM Food") +
    
    coord_cartesian(ylim = c(0, 1)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_discrete(breaks=c("flavAPref2H", "flavAPref4H", "flavAPref6H", "flavAPref8H"), labels=c("2 Hours", "4 Hours", "6 Hours", "8 Hours")) + 
    scale_shape_manual(values = c(21, 22)) +
    scale_color_manual("Demonstrated Food", limits = c("TUM", "THY"), breaks = c("TUM", "THY"), values = c("black", "dark gray"), labels = c(flavourA, flavourB)) +

    
    theme_bw() +
    theme(plot.title = element_text(colour="black", size=30, margin = margin(60, 0, 0, 0), face="bold"),
          axis.title.y = element_text(size=26, margin = margin(0, 30, 0, 0), face="bold"),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=20, color = "black"), 
          axis.text.x = element_text(size=26, color = "black", face="bold"), 
          axis.ticks = element_line(size=1.6, color = "black"),
          legend.text = element_text(size = 26, colour = "black", face = "bold"),
          legend.title = element_text(size = 28, colour = "black", face = "bold"),
          legend.key = element_blank(),
          legend.key.size = unit(3.5, "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(colour = 'black', size = 1.6),
          axis.line.y = element_line(colour = 'black', size = 1.6))
  
  if (showLegend == F) {
    stfpLine <- stfpLine + theme(legend.position = "none")
  }
  
  stfpLine
}

consumptionGraphPre <- function(dataSet, graphTitle, yMax, showLegend = T) {
  
  consumptionLine <- ggplot(dataSet, aes(x=Time, y=Consumption, group = Group, colour = Group, shape = Group)) + 
    stat_summary(fun.y = mean, geom = "line", size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, size = 2) + 
    stat_summary(fun.y = mean, geom = "point", size = 7, stroke = 2, fill = "white", show.legend = F) +
    
    labs(title = graphTitle, y = "Consumption (grams)", colour = "Prenatal Treatment") +
    
    coord_cartesian(ylim = c(0, yMax)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_discrete(breaks=c("CON2H", "CON4H", "CON6H", "CON8H"), labels=c("2 Hours", "4 Hours", "6 Hours", "8 Hours")) + 
    scale_color_manual(limits = c("C", "T"), breaks = c("C", "T"), values = c("gray", "black"), labels = c("Sesame Oil", "Testosterone")) +
    
    
    theme_bw() +
    theme(plot.title = element_text(colour="black", size=30, margin = margin(60, 0, 0, 0), face="bold"),
          axis.title.y = element_text(size=26, margin = margin(0, 30, 0, 0), face="bold"),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=20, color = "black"), 
          axis.text.x = element_text(size=26, color = "black", face="bold"), 
          axis.ticks = element_line(size=1.6, color = "black"),
          legend.text = element_text(size = 26, colour = "black", face = "bold"),
          legend.title = element_text(size = 28, colour = "black", face = "bold"),
          legend.key = element_blank(),
          legend.key.size = unit(2.5, "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(colour = 'black', size = 1.6),
          axis.line.y = element_line(colour = 'black', size = 1.6))
  
  if (showLegend == F) {
    consumptionLine <- consumptionLine + theme(legend.position = "none")
  }
  
  consumptionLine
  
}

consumptionGraph <- function(dataSet, graphTitle, yMax, showLegend = T) {
  
  consumptionLine <- ggplot(dataSet, aes(x=Time, y=Consumption, group = Gonad, colour = Gonad, shape = Gonad)) + 
    stat_summary(fun.y = mean, geom = "line", size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, size = 2) + 
    stat_summary(fun.y = mean, geom = "point", size = 7, stroke = 2, fill = "white", show.legend = F) +
    
    labs(title = graphTitle, y = "Consumption (grams)", colour = "Gonadal Condition") +
    
    coord_cartesian(ylim = c(0, yMax)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_discrete(breaks=c("CON2H", "CON4H", "CON6H", "CON8H"), labels=c("2 Hours", "4 Hours", "6 Hours", "8 Hours")) + 
    scale_color_manual(limits = c("SHAM", "GDX", "REP"), breaks = c("SHAM", "GDX", "REP"), values = c("black", "dark gray", "dim gray"), labels = c("Intact", "GDX+VEH", "GDX+REP")) +
    
    
    theme_bw() +
    theme(plot.title = element_text(colour="black", size=30, margin = margin(60, 0, 0, 0), face="bold"),
          axis.title.y = element_text(size=26, margin = margin(0, 30, 0, 0), face="bold"),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=20, color = "black"), 
          axis.text.x = element_text(size=26, color = "black", face="bold"), 
          axis.ticks = element_line(size=1.6, color = "black"),
          legend.text = element_text(size = 26, colour = "black", face = "bold"),
          legend.title = element_text(size = 28, colour = "black", face = "bold"),
          legend.key = element_blank(),
          legend.key.size = unit(2.5, "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(colour = 'black', size = 1.6),
          axis.line.y = element_line(colour = 'black', size = 1.6))
  
  if (showLegend == F) {
    consumptionLine <- consumptionLine + theme(legend.position = "none")
  }
  
  consumptionLine
  
}

demPrefGraphPre <- function(dataSet, graphTitle, yMax, showLegend = T) {
  
  demPrefLine <- ggplot(dataSet, aes(x=Time, y=DemPref, group = Group, colour = Group, shape = Group)) +
    stat_summary(fun.y = mean, geom = "line", size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, size = 2) + 
    stat_summary(fun.y = mean, geom = "point", size = 7, stroke = 2, fill = "white", show.legend = F) +
    
    labs(title = graphTitle, y = "Dem. Food Pref. Ratio", colour = "Prenatal Treatment") +
    
    coord_cartesian(ylim = c(0, yMax)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_discrete(breaks=c("demPref2H", "demPref4H", "demPref6H", "demPref8H"), labels=c("2 Hours", "4 Hours", "6 Hours", "8 Hours")) + 
    scale_color_manual(limits = c("C", "T"), breaks = c("C", "T"), values = c("gray", "black"), labels = c("Sesame Oil", "Testosterone")) +
    
    
    theme_bw() +
    theme(plot.title = element_text(colour="black", size=30, margin = margin(60, 0, 0, 0), face="bold"),
          axis.title.y = element_text(size=26, margin = margin(0, 30, 0, 0), face="bold"),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=20, color = "black"), 
          axis.text.x = element_text(size=26, color = "black", face="bold"), 
          axis.ticks = element_line(size=1.6, color = "black"),
          legend.text = element_text(size = 26, colour = "black", face = "bold"),
          legend.title = element_text(size = 28, colour = "black", face = "bold"),
          legend.key = element_blank(),
          legend.key.size = unit(2.5, "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(colour = 'black', size = 1.6),
          axis.line.y = element_line(colour = 'black', size = 1.6))
  
  if (showLegend == F) {
    demPrefLine <- demPrefLine + theme(legend.position = "none")
  }
  
  demPrefLine
  
}

demPrefGraph <- function(dataSet, graphTitle, yMax, showLegend = T) {
  
  demPrefLine <- ggplot(dataSet, aes(x=Time, y=DemPref, group = Gonad, colour = Gonad, shape = Gonad)) +
    stat_summary(fun.y = mean, geom = "line", size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, size = 2) + 
    stat_summary(fun.y = mean, geom = "point", size = 7, stroke = 2, fill = "white", show.legend = F) +
    
    labs(title = graphTitle, y = "Dem. Food Pref. Ratio", colour = "Gonadal Condition") +
    
    coord_cartesian(ylim = c(0, yMax)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_discrete(breaks=c("demPref2H", "demPref4H", "demPref6H", "demPref8H"), labels=c("2 Hours", "4 Hours", "6 Hours", "8 Hours")) + 
    scale_color_manual(limits = c("SHAM", "GDX", "REP"), breaks = c("SHAM", "GDX", "REP"), values = c("black", "dark gray", "dim gray"), labels = c("Intact", "GDX+VEH", "GDX+REP")) +
    
    
    theme_bw() +
    theme(plot.title = element_text(colour="black", size=30, margin = margin(0, 0, 40, 0), face="bold"),
          axis.title.y = element_text(size=26, margin = margin(0, 30, 0, 0), face="bold"),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=20, color = "black"), 
          axis.text.x = element_text(size=26, color = "black", face="bold"), 
          axis.ticks = element_line(size=1.6, color = "black"),
          legend.text = element_text(size = 26, colour = "black", face = "bold"),
          legend.title = element_text(size = 28, colour = "black", face = "bold"),
          legend.key = element_blank(),
          legend.key.size = unit(2.5, "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(colour = 'black', size = 1.6),
          axis.line.y = element_line(colour = 'black', size = 1.6))
  
  if (showLegend == F) {
    demPrefLine <- demPrefLine + theme(legend.position = "none")
  }
  
  demPrefLine
  
}

flavRecGraph <- function(data, yVar, graphTitle, fileName) {
  flavourRecBar <- ggplot(data, aes_string(x = "Group", y = yVar, fill = "Phase")) +
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.9), width = 0.2, size  = 1) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_y_continuous(expand = c(0, 0)) +
    
    labs (title = graphTitle, x = NULL, y = "Investigation Ratio") +
    scale_x_discrete(limits=c("M-GDX", "M-REP", "F-GDX"), breaks=c("M-GDX", "M-REP", "F-GDX"), labels=c("Male - GDX", "Male - GDX+T", "Female - GDX")) +
    scale_fill_manual("Phase", values = c("light gray", "black"), labels = c("Habituation", "Test")) +
    
    theme_bw() +
    theme(plot.title = element_text(colour="black", size=18, face="bold"),
          axis.title.y = element_text(size=18, margin = margin(0, 30, 0, 0), face="bold"),
          axis.title.x = element_text(size=18, face="bold"),
          axis.text.y = element_text(size=14, color = "black"), 
          axis.text.x = element_text(size=14, color = "black", face="bold"), 
          axis.ticks = element_line(size=1, color = "black"),
          legend.text = element_text(size = 14, colour = "black"),
          legend.title = element_text(size = 18, colour = "black", face="bold"),
          legend.key = element_blank(),
          legend.key.size = unit(1, "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(colour = 'black', size = 1),
          axis.line.y = element_line(colour = 'black', size = 1))
  
  flavourRecBar
}


pilotGraph <- function(data) {
  flavourRecBar <- ggplot(data, aes_string(x = "Flavour", y = "Consumption", fill = "Flavour")) +
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.9), width = 0.2, size  = 1) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_y_continuous(expand = c(0, 0)) +
    
    labs (title = "Flavour Preference", x = NULL, y = "Consumption (grams)") +
    scale_x_discrete(limits=c("TUM", "THY"), breaks=c("TUM", "THY"), labels=c("Turmeric", "Thyme")) +
    scale_fill_manual("Flavour", values = c("light gray", "black"), labels = c("Turmeric", "Thyme")) +
    
    theme_bw() +
    theme(plot.title = element_text(colour="black", size=18, face="bold"),
          axis.title.y = element_text(size=18, margin = margin(0, 30, 0, 0), face="bold"),
          axis.title.x = element_text(size=18, face="bold"),
          axis.text.y = element_text(size=14, color = "black"), 
          axis.text.x = element_text(size=14, color = "black", face="bold"), 
          axis.ticks = element_line(size=1, color = "black"),
          legend.text = element_text(size = 14, colour = "black"),
          legend.title = element_text(size = 18, colour = "black", face="bold"),
          legend.key = element_blank(),
          legend.key.size = unit(1, "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(colour = 'black', size = 1),
          axis.line.y = element_line(colour = 'black', size = 1))
  
  flavourRecBar
}

summaryPanel <- function(data, yVar, variableName, graphTitle, yMax, fileName) {
  
  controlGraph <- summaryGraph(subset(data, Group == "C"), yVar, variableName, "Sesame Oil", yMax)
  testGraph <- summaryGraph(subset(data, Group == "T"), yVar, variableName, "Testosterone", yMax)
  graphLegend <- getLegend(controlGraph);
  
  controlGraph <- controlGraph + theme(legend.position = "none") 
  testGraph <- testGraph + theme(legend.position = "none") 
  
  pdf(paste("output/", fileName), 16, 7)
  grid.arrange(controlGraph, testGraph, graphLegend, ncol = 3, nrow = 1, widths = c(7, 7, 2))
  dev.off()
  
}

summaryGraph <- function(data, yVar, variableName, graphTitle, yMax, fileName) {
  
  gonadConditions <- c("SHAM", "GDX", "REP")
  
  ldBar <- ggplot(data, aes_string(x = "Gonad", y = yVar, fill = "DemFood")) +
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 0.9), width = 0.2, size  = 1) +
    coord_cartesian(ylim = c(0, yMax)) +
    scale_y_continuous(expand = c(0, 0)) +
    
    
    
    labs (title = graphTitle, x = NULL, y = variableName) +
    scale_x_discrete(limits = gonadConditions, breaks=c("SHAM", "GDX", "REP"), labels=c("Intact", "Gonadectomy + \nVehicle", "Gonadectomy + \nReplacement")) +
    scale_fill_manual("Demonstrated Food", limits = c("TUM", "THY"), breaks = c("TUM", "THY"), values = c("black", "gray"), labels = c("Turmeric", "Thyme")) +
    
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

getLegend <- function(myPlot){
  tmp <- ggplot_gtable(ggplot_build(myPlot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}