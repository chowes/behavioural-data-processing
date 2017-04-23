library(ggplot2)
library(reshape2)
library(car)
library(gridExtra)
library(grid)
library(lsr)

source("src/data.R")

developmentalWeights <- read.csv(file = "data/developmentalWeights.csv", na.strings = c("NA", "-999"), stringsAsFactors=T)
perfusionData <- read.csv(file = "data/perfusion.csv", na.strings = c("NA", "-999"), stringsAsFactors=T)
vaginalOpeningData <- read.csv(file = "data/vaginalOpenings.csv")
estrusData <- read.csv(file = "data/firstEstrus.csv")
fecalData <- read.csv(file = "data/fecal.csv")
developmentalData <- read.csv(file = "data/litterRecord.csv")
maternalSerumData <- read.csv(file = "data/maternalSerum.csv")
perfusionData <- createFactors(perfusionData)

#############################
# Fecal
#############################

t.test(Difference ~ Treatment, data = fecalData)
cohensD(Difference ~ Treatment, data = fecalData)

t.test(subset(fecalData, Treatment == "C")$PreTreatment, subset(fecalData, Treatment == "C")$PostTreatment, paired = T)
cohensD(subset(fecalData, Treatment == "C")$PreTreatment, subset(fecalData, Treatment == "C")$PostTreatment)

t.test(PostTreatment ~ Treatment, data = fecalData)
cohensD(PostTreatment ~ Treatment, data = fecalData)

#############################
# Maternal Serum
#############################

t.test(PlasmaT ~ Treatment, data = maternalSerumData)
cohensD(PlasmaT ~ Treatment, data = maternalSerumData)

#############################
# Developmental Weights
#############################

### analysis

t.test(PD4 ~ Treatment, data = developmentalWeights)
t.test(PD8 ~ Treatment, data = developmentalWeights)
t.test(PD12 ~ Treatment, data = developmentalWeights)
t.test(PD16 ~ Treatment, data = developmentalWeights)
t.test(PD20 ~ Treatment, data = developmentalWeights)

### graphs

developmentalWeightsMelt <- weightMelt(developmentalWeights)
developmentalWeightGraph <- weightGraph(developmentalWeightsMelt, T)



#############################
# Developmental Data
#############################

t.test(Ears ~ Treatment, data = developmentalData)
cohensD(Ears ~ Treatment, data = developmentalData)

t.test(Eyes ~ Treatment, data = developmentalData)
cohensD(Eyes ~ Treatment, data = developmentalData)

t.test(Fur ~ Treatment, data = developmentalData)
cohensD(Fur ~ Treatment, data = developmentalData)


#############################
# Adult Weight
#############################

# subsetting necessary for all analyses of perfusion data
femaleControlSham <- subset(perfusionData, Gonad == "SHAM" & Sex == "F" & Treatment == "C")
femaleControlRep <- subset(perfusionData, Gonad == "REP" & Sex == "F" & Treatment == "C")
femaleControlGdx <- subset(perfusionData, Gonad == "GDX" & Sex == "F" & Treatment == "C")

maleControlSham <- subset(perfusionData, Gonad == "SHAM" & Sex == "M" & Treatment == "C")
maleControlRep <- subset(perfusionData, Gonad == "REP" & Sex == "M" & Treatment == "C")
maleControlGdx <- subset(perfusionData, Gonad == "GDX" & Sex == "M" & Treatment == "C")

femaleTestSham <- subset(perfusionData, Gonad == "SHAM" & Sex == "F" & Treatment == "T")
femaleTestRep <- subset(perfusionData, Gonad == "REP" & Sex == "F" & Treatment == "T")
femaleTestGdx <- subset(perfusionData, Gonad == "GDX" & Sex == "F" & Treatment == "T")

maleTestSham <- subset(perfusionData, Gonad == "SHAM" & Sex == "M" & Treatment == "T")
maleTestRep <- subset(perfusionData, Gonad == "REP" & Sex == "M" & Treatment == "T")
maleTestGdx <- subset(perfusionData, Gonad == "GDX" & Sex == "M" & Treatment == "T")

### analysis

adultWeightModel <- lm(AnimalWeight ~ Treatment * Sex * Gonad, data = perfusionData)
Anova(adultWeightModel)

# planned comparisons - hormone condition
t.test(femaleControlSham$AnimalWeight, femaleControlGdx$AnimalWeight) # sham - gdx
cohensD(femaleControlSham$AnimalWeight, femaleControlGdx$AnimalWeight)
t.test(femaleControlGdx$AnimalWeight, femaleControlRep$AnimalWeight) # rep - gdx
cohensD(femaleControlGdx$AnimalWeight, femaleControlRep$AnimalWeight)

t.test(femaleTestSham$AnimalWeight, femaleTestGdx$AnimalWeight)
cohensD(femaleTestSham$AnimalWeight, femaleTestGdx$AnimalWeight)
t.test(femaleTestGdx$AnimalWeight, femaleTestRep$AnimalWeight)
cohensD(femaleTestGdx$AnimalWeight, femaleTestRep$AnimalWeight)

t.test(maleControlSham$AnimalWeight, maleControlGdx$AnimalWeight)
cohensD(maleControlSham$AnimalWeight, maleControlGdx$AnimalWeight)
t.test(maleControlGdx$AnimalWeight, maleControlRep$AnimalWeight)
cohensD(maleControlGdx$AnimalWeight, maleControlRep$AnimalWeight)

t.test(maleTestSham$AnimalWeight, maleTestGdx$AnimalWeight)
cohensD(maleTestSham$AnimalWeight, maleTestGdx$AnimalWeight)
t.test(maleTestGdx$AnimalWeight, maleTestRep$AnimalWeight)
cohensD(maleTestGdx$AnimalWeight, maleTestRep$AnimalWeight)


# planned comparisons - male to female
t.test(femaleControlSham$AnimalWeight, maleControlSham$AnimalWeight)
cohensD(femaleControlSham$AnimalWeight, maleControlSham$AnimalWeight)
t.test(femaleControlRep$AnimalWeight, maleControlRep$AnimalWeight)
cohensD(femaleControlRep$AnimalWeight, maleControlRep$AnimalWeight)
t.test(femaleControlGdx$AnimalWeight, maleControlGdx$AnimalWeight)
cohensD(femaleControlGdx$AnimalWeight, maleControlGdx$AnimalWeight)

t.test(femaleTestSham$AnimalWeight, maleTestSham$AnimalWeight)
cohensD(femaleTestSham$AnimalWeight, maleTestSham$AnimalWeight)
t.test(femaleTestRep$AnimalWeight, maleTestRep$AnimalWeight)
cohensD(femaleTestRep$AnimalWeight, maleTestRep$AnimalWeight)
t.test(femaleTestGdx$AnimalWeight, maleTestGdx$AnimalWeight)
cohensD(femaleTestGdx$AnimalWeight, maleTestGdx$AnimalWeight)


# planned comparisons - test to control
t.test(femaleControlSham$AnimalWeight, femaleTestSham$AnimalWeight)
cohensD(femaleControlSham$AnimalWeight, femaleTestSham$AnimalWeight)
t.test(femaleControlRep$AnimalWeight, femaleTestRep$AnimalWeight)
cohensD(femaleControlRep$AnimalWeight, femaleTestRep$AnimalWeight)
t.test(femaleControlGdx$AnimalWeight, femaleTestGdx$AnimalWeight)
cohensD(femaleControlGdx$AnimalWeight, femaleTestGdx$AnimalWeight)

t.test(maleControlSham$AnimalWeight, maleTestSham$AnimalWeight)
cohensD(maleControlSham$AnimalWeight, maleTestSham$AnimalWeight)
t.test(maleControlRep$AnimalWeight, maleTestRep$AnimalWeight)
cohensD(maleControlRep$AnimalWeight, maleTestRep$AnimalWeight)
t.test(maleControlGdx$AnimalWeight, maleTestGdx$AnimalWeight)
cohensD(maleControlGdx$AnimalWeight, maleTestGdx$AnimalWeight)

### graphs

barPanel(perfusionData, "AnimalWeight", "Body Weight (grams)", "Adult Body Weight", 50, "adult-body-weight.pdf")

#############################
# Vaginal Openings
#############################

t.test(VaginalOpening ~ Treatment, data = vaginalOpeningData)
cohensD(VaginalOpening ~ Treatment, data = vaginalOpeningData)

#############################
# First Estrus
#############################

t.test(FirstEstrus ~ Treatment, data = estrusData)
cohensD(FirstEstrus ~ Treatment, data = estrusData)

#############################
# Gonad and Prostate Weights
#############################

### prostate weights

# analysis
perfusedMale <- subset(perfusionData, Sex == "M")

prostateModel <- lm(ProstateWeight ~ Treatment * Gonad, data = perfusedMale)
Anova(prostateModel)

t.test(maleControlSham$ProstateWeight, maleControlGdx$ProstateWeight)
cohensD(maleControlSham$ProstateWeight, maleControlGdx$ProstateWeight)
t.test(maleControlGdx$ProstateWeight, maleControlRep$ProstateWeight)
cohensD(maleControlGdx$ProstateWeight, maleControlRep$ProstateWeight)
t.test(maleControlRep$ProstateWeight, maleControlSham$ProstateWeight)
cohensD(maleControlRep$ProstateWeight, maleControlSham$ProstateWeight)

t.test(maleTestSham$ProstateWeight, maleTestGdx$ProstateWeight)
cohensD(maleTestSham$ProstateWeight, maleTestGdx$ProstateWeight)
t.test(maleTestGdx$ProstateWeight, maleTestRep$ProstateWeight)
cohensD(maleTestGdx$ProstateWeight, maleTestRep$ProstateWeight)
t.test(maleTestRep$ProstateWeight, maleTestSham$ProstateWeight)
cohensD(maleTestRep$ProstateWeight, maleTestSham$ProstateWeight)


t.test(maleControlSham$ProstateWeight, maleTestSham$ProstateWeight)
cohensD(maleControlSham$ProstateWeight, maleTestSham$ProstateWeight)
t.test(maleControlGdx$ProstateWeight, maleTestGdx$ProstateWeight)
cohensD(maleControlGdx$ProstateWeight, maleTestGdx$ProstateWeight)
t.test(maleControlRep$ProstateWeight, maleTestRep$ProstateWeight)
cohensD(maleControlRep$ProstateWeight, maleTestRep$ProstateWeight)

# graph
pdf("output/prostate-weights.pdf", width = 9, 5)
barGraph(perfusedMale, "ProstateWeight", "Prostate Weight (grams)", "", .252, "prostate-weight.pdf")
dev.off()

### gonad weights

# analysis
t.test(maleControlSham$GonadWeight, maleTestSham$GonadWeight)
cohensD(maleControlSham$GonadWeight, maleTestSham$GonadWeight)
t.test(femaleControlSham$GonadWeight, femaleTestSham$GonadWeight)
cohensD(femaleControlSham$GonadWeight, femaleTestSham$GonadWeight)

# graph

gonadGraph <- maleFemaleBarGraph(perfusionData, "GonadWeight", "Gonad Weight (grams)", "Gonad Weight", 0.4, "gonad-weight.pdf")
pdf("output/gonad-weights.pdf", 8.5, 5)
gonadGraph
dev.off()

maleGonadGraph <- simpleBarGraph(subset(perfusionData, Sex == "M"), "GonadWeight", "Gonad Weight (grams)", "Male", 0.4, "gonad-weight.pdf")
femaleGonadGraph <- simpleBarGraph(subset(perfusionData, Sex == "F"), "GonadWeight", "Gonad Weight (grams)", "Female", 0.4, "gonad-weight.pdf")

gonadLegend <- getLegend(maleGonadGraph)
maleGonadGraph <- maleGonadGraph + theme(legend.position = "none")
femaleGonadGraph <- femaleGonadGraph + theme(legend.position = "none")

pdf("output/gonad-weights.pdf", width = 10, 5)
grid.arrange(maleGonadGraph, femaleGonadGraph, ncol = 2, nrow = 1, widths = c(5, 5))
dev.off()
