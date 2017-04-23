library(phia)
library(reshape2)
library(devtools)
library(ez)
library(ggplot2)

source("src/data.R")
source("src/clean.R")

stfpData <- read.csv(file = "data/stfp-pre.csv", na.strings = c("NA", "-999"), stringsAsFactors=FALSE)
stfpData <- subset(stfpData, Age == "PRE")

# unblind treatment groups
stfpData <- unblindTreatment(stfpData)

# generates flavour consumption data for each time point
stfpData <- genFlavourConsumption(stfpData)

# removes negative values resulting from scale error
stfpData <- removeNegative(stfpData)

# calculate total feeding
stfpData <- genTotalConsumption(stfpData)

# removes animals and time points where feeding was less than 0.1 grams
stfpData <- removeLowFeeding(stfpData)

# calculate preference and demonstrated food preference
stfpData <- genPreferenceData(stfpData)

# perform transformations
stfpData <- stfpTransform(stfpData)

# get treatment, sex, and gonadal condition groups

treatmentC <- subset(stfpData, Group == "C")
treatmentD <- subset(stfpData, Group == "T")

maleC <- subset(treatmentC, Sex =="M")
femaleC <- subset(treatmentC, Sex == "F")
maleD <- subset(treatmentD, Sex =="M")
femaleD <- subset(treatmentD, Sex == "F")



# analysis

# Male C
t.test(transTumPref2H ~ DemFood, data = maleC)
t.test(transTumPref4H ~ DemFood, data = maleC)
t.test(transTumPref6H ~ DemFood, data = maleC)
t.test(transTumPref8H ~ DemFood, data = maleC)

# Male D
t.test(transTumPref2H ~ DemFood, data = maleD)
t.test(transTumPref4H ~ DemFood, data = maleD)
t.test(transTumPref6H ~ DemFood, data = maleD)
t.test(transTumPref8H ~ DemFood, data = maleD)

# Female C
t.test(transTumPref2H ~ DemFood, data = femaleC)
t.test(transTumPref4H ~ DemFood, data = femaleC)
t.test(transTumPref6H ~ DemFood, data = femaleC)
t.test(transTumPref8H ~ DemFood, data = femaleC)

# Female D Sham
t.test(transTumPref2H ~ DemFood, data = femaleShamD)
t.test(transTumPref4H ~ DemFood, data = femaleShamD)
t.test(transTumPref6H ~ DemFood, data = femaleShamD)
t.test(transTumPref8H ~ DemFood, data = femaleShamD)


# melt for graphing

maleShamC <- stfpMelt(maleShamC)
maleGdxC <- stfpMelt(maleGdxC)
maleRepC <- stfpMelt(maleRepC)

maleShamD <- stfpMelt(maleShamD)
maleGdxD <- stfpMelt(maleGdxD)
maleRepD <- stfpMelt(maleRepD)

femaleShamC <- stfpMelt(femaleShamC)
femaleGdxC <- stfpMelt(femaleGdxC)
femaleRepC <- stfpMelt(femaleRepC)

femaleShamD <- stfpMelt(femaleShamD)
femaleGdxD <- stfpMelt(femaleGdxD)
femaleRepD <- stfpMelt(femaleRepD)


maleC <- consumptionMelt(maleC)
femaleC <- consumptionMelt(femaleC)
maleD <- consumptionMelt(maleD)
femaleD <- consumptionMelt(femaleD)


# graphs

stfpGraph(dataSet = maleShamC, "Male Control Sham")
stfpGraph(dataSet = maleGdxC, "Male Control GDX")
stfpGraph(dataSet = maleRepC, "Male Control Replacement ")

stfpGraph(dataSet = maleShamD, "Male Testosterone Sham")
stfpGraph(dataSet = maleGdxD, "Male Testosterone GDX")
stfpGraph(dataSet = maleRepD, "Male Testosterone Replacement")

stfpGraph(dataSet = femaleShamC, "Female Control Sham")
stfpGraph(dataSet = femaleGdxC, "Female Control GDX")
stfpGraph(dataSet = femaleRepC, "Female Control Replacement")

stfpGraph(dataSet = femaleShamD, "Female Testosterone Sham")
stfpGraph(dataSet = femaleGdxD, "Female Testosterone GDX")
stfpGraph(dataSet = femaleRepD, "Female Testosterone Replacement")


consumptionGraph(dataSet = maleC, "Male Control", 1.5)
consumptionGraph(dataSet = femaleC, "Female Control", 1.5)
consumptionGraph(dataSet = maleD, "Male Testosteorne", 1.5)
consumptionGraph(dataSet = femaleD, "Female Testosterone", 1.5)

# Sample size

nrow(subset(subset(femaleShamC, Time == "tumPref2H"), DemFood == "TUM"))
nrow(subset(subset(femaleShamC, Time == "tumPref2H"), DemFood == "THY"))
nrow(subset(subset(femaleGdxC, Time == "tumPref2H"), DemFood == "TUM"))
nrow(subset(subset(femaleGdxC, Time == "tumPref2H"), DemFood == "THY"))
nrow(subset(subset(femaleRepC, Time == "tumPref2H"), DemFood == "TUM"))
nrow(subset(subset(femaleRepC, Time == "tumPref2H"), DemFood == "THY"))

nrow(subset(subset(femaleShamD, Time == "tumPref2H"), DemFood == "TUM"))
nrow(subset(subset(femaleShamD, Time == "tumPref2H"), DemFood == "THY"))
nrow(subset(subset(femaleGdxD, Time == "tumPref2H"), DemFood == "TUM"))
nrow(subset(subset(femaleGdxD, Time == "tumPref2H"), DemFood == "THY"))
nrow(subset(subset(femaleRepD, Time == "tumPref2H"), DemFood == "TUM"))
nrow(subset(subset(femaleRepD, Time == "tumPref2H"), DemFood == "THY"))

nrow(subset(subset(maleShamC, Time == "tumPref2H"), DemFood == "TUM"))
nrow(subset(subset(maleShamC, Time == "tumPref2H"), DemFood == "THY"))
nrow(subset(subset(maleGdxC, Time == "tumPref2H"), DemFood == "TUM"))
nrow(subset(subset(maleGdxC, Time == "tumPref2H"), DemFood == "THY"))
nrow(subset(subset(maleRepC, Time == "tumPref2H"), DemFood == "TUM"))
nrow(subset(subset(maleRepC, Time == "tumPref2H"), DemFood == "THY"))

nrow(subset(subset(maleShamD, Time == "tumPref2H"), DemFood == "TUM"))
nrow(subset(subset(maleShamD, Time == "tumPref2H"), DemFood == "THY"))
nrow(subset(subset(maleGdxD, Time == "tumPref2H"), DemFood == "TUM"))
nrow(subset(subset(maleGdxD, Time == "tumPref2H"), DemFood == "THY"))
nrow(subset(subset(maleRepD, Time == "tumPref2H"), DemFood == "TUM"))
nrow(subset(subset(maleRepD, Time == "tumPref2H"), DemFood == "THY"))

