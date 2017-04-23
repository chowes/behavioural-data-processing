# clean data, remove negative values, remove cases with too little consumption

createFactors <- function(stfpData) {
  stfpData$Subject <- as.factor(stfpData$Subject)
  stfpData$Sex <- as.factor(stfpData$Sex)
  stfpData$Group <- as.factor(stfpData$Group)
  stfpData$Gonad <- as.factor(stfpData$Gonad, levels = c("PRE", "SHAM", "GDX", "REP"))
  stfpData$Age <- as.factor(stfpData$Age)
  stfpData$DemFood <- as.factor(stfpData$DemFood)
  
  return(stfpData)
}

frFactors <- function(data) {
  data$Subject <- as.factor(data$Subject)
  data$Sex <- as.factor(data$Sex)
  data$Treatment <- as.factor(data$Treatment)
  data$Gonad <- as.factor(data$Gonad)
  
  return(data)
}

frGraphFactors <- function(data) {
  data$Subject <- as.factor(data$Subject)
  data$Group <- as.factor(data$Group, levels = c("M-GDX", "M-REP", "F-GDX"))
  data$Phase <- as.factor(data$Phase)
  
  return(data)
}

removeNegative <- function(stfpData){
  
  # find negative consumption
  badMatrix <- (stfpData$FLA2H < 0) # create a matrix of impossible negative values following computation of consumption
  badMatrix[is.na(badMatrix)] <- F # handles missing data, which cannot be subject to boolean operators.  this sets missing data to F, so it is ignored
  if(any(badMatrix == T)){stfpData[badMatrix,]$FLA2H <- 0} # sets all scores that are below 0 to 0, use if statement to avoid returning an error if all values are false
  badMatrix <- (stfpData$FLA4H < 0)
  badMatrix[is.na(badMatrix)] <- F
  if(any(badMatrix == T)){stfpData[badMatrix,]$FLA4H <- 0}
  badMatrix <- (stfpData$FLA6H < 0)
  badMatrix[is.na(badMatrix)] <- F
  if(any(badMatrix == T)){stfpData[badMatrix,]$FLA6H <- 0}
  badMatrix <- (stfpData$FLA8H < 0)
  badMatrix[is.na(badMatrix)] <- F
  if(any(badMatrix == T)){stfpData[badMatrix,]$FLA8H <- 0}
  
  badMatrix <- (stfpData$FLB2H < 0)
  badMatrix[is.na(badMatrix)] <- F
  if(any(badMatrix == T)){stfpData[badMatrix,]$FLB2H <- 0}
  badMatrix <- (stfpData$FLB4H < 0)
  badMatrix[is.na(badMatrix)] <- F
  if(any(badMatrix == T)){stfpData[badMatrix,]$FLB4H <- 0}
  badMatrix <- (stfpData$FLB6H < 0)
  badMatrix[is.na(badMatrix)] <- F
  if(any(badMatrix == T)){stfpData[badMatrix,]$FLB6H <- 0}
  badMatrix <- (stfpData$FLB8H < 0)
  badMatrix[is.na(badMatrix)] <- F
  if(any(badMatrix == T)){stfpData[badMatrix,]$FLB8H <- 0}
  
  return(stfpData)
  
}


removeLowFeeding <- function(stfpData) {
  
  # calculate demonstrator consumption
  stfpData$DemCons <- stfpData$DemInitial - stfpData$DemFinal
  
  # sets all instances where demonstrator consumption is <.1g to NA
  badMatrix <- (stfpData$DemCons < 0.1)
  badMatrix[is.na(badMatrix)] <- T
  if(any(badMatrix == T)){
    stfpData[badMatrix,]$FLA2H <- NA
    stfpData[badMatrix,]$FLB2H <- NA
    stfpData[badMatrix,]$FLA4H <- NA
    stfpData[badMatrix,]$FLB4H <- NA
    stfpData[badMatrix,]$FLA6H <- NA
    stfpData[badMatrix,]$FLB6H <- NA
    stfpData[badMatrix,]$FLA8H <- NA
    stfpData[badMatrix,]$FLB8H <- NA
  }
  
  # sets all instances where 2h consumption is <.1g to NA
  badMatrix <- (stfpData$CON2H < 0.1)
  if(any(badMatrix == T)){
    stfpData[badMatrix,]$FLA2H <- NA
    stfpData[badMatrix,]$FLB2H <- NA
  }
  
  #sets all instances where 4h consumption is <.1g to NA
  badMatrix <- (stfpData$CON4H < 0.1)
  if(any(badMatrix == T)){
    stfpData[badMatrix,]$FLA4H <- NA
    stfpData[badMatrix,]$FLB4H <- NA
  }
  
  #sets all instances where 6h consumption is <.1g to NA
  badMatrix <- (stfpData$CON6H < 0.1)
  if(any(badMatrix == T)){
    stfpData[badMatrix,]$FLA6H <- NA
    stfpData[badMatrix,]$FLB6H <- NA
  }
  
  #sets all instances where 8h consumption is <.1g to NA
  badMatrix <- (stfpData$CON8H < 0.1)
  if(any(badMatrix == T)){
    stfpData[badMatrix,]$FLA8H <- NA
    stfpData[badMatrix,]$FLB8H <- NA
  }
  
  return(stfpData)
  
}



# melt data for graphing
stfpMelt <- function(stfpData) {
  
  id <- 1:nrow(stfpData)
  stfpData <- cbind(id = as.factor(id), stfpData)
  
  meltData <- melt(stfpData, id.vars = c("id", "Subject", "Sex", "Group", "Gonad", "DemFood"), 
                   measure.vars = c("flavAPref2H", "flavAPref4H", 
                                    "flavAPref6H", "flavAPref8H"), 
                   value.name = "FlavAPref", variable.name = "Time")
  
  return(meltData)
  
}


consumptionMelt <- function(stfpData) {
  
  meltData <- melt(stfpData, id.vars = c("Subject", "Sex", "Group", "Gonad"), 
                   measure.vars = c("CON2H", "CON4H", 
                                    "CON6H", "CON8H"), 
                   value.name = "Consumption", variable.name = "Time")
  
  return(meltData)
}

demPrefMelt <- function(stfpData) {
  
  meltData <- melt(stfpData, id.vars = c("Subject", "Sex", "Group", "Gonad"), 
                   measure.vars = c("demPref2H", "demPref4H", 
                                    "demPref6H", "demPref8H"), 
                   value.name = "DemPref", variable.name = "Time")
  
  return(meltData)
}


# apply arcsine square root transformations
stfpTransform <- function(stfpData){
  stfpData$transFlavAPref2H <- asin(sqrt(stfpData$flavAPref2H))
  stfpData$transFlavAPref4H <- asin(sqrt(stfpData$flavAPref4H))
  stfpData$transFlavAPref6H <- asin(sqrt(stfpData$flavAPref6H))
  stfpData$transFlavAPref8H <- asin(sqrt(stfpData$flavAPref8H))
  
  stfpData$transDemPref2H <- asin(sqrt(stfpData$demPref2H))
  stfpData$transDemPref4H <- asin(sqrt(stfpData$demPref4H))
  stfpData$transDemPref6H <- asin(sqrt(stfpData$demPref6H))
  stfpData$transDemPref8H <- asin(sqrt(stfpData$demPref8H))
  
  return(stfpData)
}

flavTransform <- function(data) {
  data$IRHAB <- asin(sqrt(data$IRHAB))
  data$IRTEST <- asin(sqrt(data$IRTEST))
  
  return(data)
}