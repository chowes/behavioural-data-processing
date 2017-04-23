library(phia)
library(reshape2)
library(ez)
library(ggplot2)
library(lsr)
library(gridExtra)

source("src/data.R")
source("src/clean.R")

stfpData <- read.csv(file = "data/stfp-data-complete.csv", na.strings = c("NA", "-999"), stringsAsFactors=FALSE)
chocChip <- read.csv(file = "data/chocchip.csv", na.strings = c("NA", "-999"), stringsAsFactors = FALSE)
flavRec <- read.csv(file = "data/flavourRec.csv", na.strings = c("NA", "-999"), stringsAsFactors = FALSE)
flavRecGraphing <- read.csv(file = "data/flavourRecGraphing.csv", na.strings = c("NA", "-999"))
stfpData <- read.csv(file = "data/stfpPilot.csv")


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
flavRec <- flavTransform(flavRec)

# get pre and post stfp
preStfp <- subset(stfpData, stfpData$Age == "PRE")
postStfp <- subset(stfpData, stfpData$Age == "POST")

# convert to factors
preStfp <- createFactors(preStfp)
postStfp <- createFactors(postStfp)
flavRec <- frFactors(flavRec)



# export for spss
write.csv(subset(stfpData, stfpData$Age == "POST"), file = "output/postCleanSTFP.csv", na = "-999")
write.csv(subset(stfpData, stfpData$Age == "PRE"), file = "output/preCleanSTFP.csv", na = "-999")

########################
# Analysis - Pre-Puberty
########################
# note - main analysis conducted in SPSS because it's difficult to do mixed models in R

malePreC <- subset(subset(preStfp, Sex == "M"), Group == "C")
femalePreC <- subset(subset(preStfp, Sex == "F"), Group == "C")
malePreT <- subset(subset(preStfp, Sex == "M"), Group == "T")
femalePreT <- subset(subset(preStfp, Sex == "F"), Group == "T")

### Flavour Preference - Planned Comparisons

# Male C
t.test(transFlavAPref2H ~ DemFood, data = malePreC)
cohensD(transFlavAPref2H ~ DemFood, data = malePreC)
t.test(transFlavAPref4H ~ DemFood, data = malePreC)
cohensD(transFlavAPref4H ~ DemFood, data = malePreC)
t.test(transFlavAPref6H ~ DemFood, data = malePreC)
cohensD(transFlavAPref6H ~ DemFood, data = malePreC)
t.test(transFlavAPref8H ~ DemFood, data = malePreC)
cohensD(transFlavAPref8H ~ DemFood, data = malePreC)

# Female C
t.test(transFlavAPref2H ~ DemFood, data = femalePreC)
cohensD(transFlavAPref2H ~ DemFood, data = femalePreC)
t.test(transFlavAPref4H ~ DemFood, data = femalePreC)
cohensD(transFlavAPref4H ~ DemFood, data = femalePreC)
t.test(transFlavAPref6H ~ DemFood, data = femalePreC)
cohensD(transFlavAPref6H ~ DemFood, data = femalePreC)
t.test(transFlavAPref8H ~ DemFood, data = femalePreC)
cohensD(transFlavAPref8H ~ DemFood, data = femalePreC)

# Male T
t.test(transFlavAPref2H ~ DemFood, data = malePreT)
cohensD(transFlavAPref2H ~ DemFood, data = malePreT)
t.test(transFlavAPref4H ~ DemFood, data = malePreT)
cohensD(transFlavAPref4H ~ DemFood, data = malePreT)
t.test(transFlavAPref6H ~ DemFood, data = malePreT)
cohensD(transFlavAPref6H ~ DemFood, data = malePreT)
t.test(transFlavAPref8H ~ DemFood, data = malePreT)
cohensD(transFlavAPref8H ~ DemFood, data = malePreT)

# Female T
t.test(transFlavAPref2H ~ DemFood, data = femalePreT)
cohensD(transFlavAPref2H ~ DemFood, data = femalePreT)
t.test(transFlavAPref4H ~ DemFood, data = femalePreT)
cohensD(transFlavAPref4H ~ DemFood, data = femalePreT)
t.test(transFlavAPref6H ~ DemFood, data = femalePreT)
cohensD(transFlavAPref6H ~ DemFood, data = femalePreT)
t.test(transFlavAPref8H ~ DemFood, data = femalePreT)
cohensD(transFlavAPref8H ~ DemFood, data = femalePreT)

# Dem Preference - Planned Comparisons

## Treatment
# 2h
t.test(malePreC$transDemPref2H, malePreT$transDemPref2H)
cohensD(malePreC$transDemPref2H, malePreT$transDemPref2H)
t.test(femalePreC$transDemPref2H, femalePreT$transDemPref2H)
cohensD(femalePreC$transDemPref2H, femalePreT$transDemPref2H)
# 4h
t.test(malePreC$transDemPref4H, malePreT$transDemPref4H)
cohensD(malePreC$transDemPref4H, malePreT$transDemPref4H)
t.test(femalePreC$transDemPref4H, femalePreT$transDemPref4H)
cohensD(femalePreC$transDemPref4H, femalePreT$transDemPref4H)
# 6h
t.test(malePreC$transDemPref6H, malePreT$transDemPref6H)
cohensD(malePreC$transDemPref6H, malePreT$transDemPref6H)
t.test(femalePreC$transDemPref6H, femalePreT$transDemPref6H)
cohensD(femalePreC$transDemPref6H, femalePreT$transDemPref6H)
# 8h
t.test(malePreC$transDemPref8H, malePreT$transDemPref8H)
cohensD(malePreC$transDemPref8H, malePreT$transDemPref8H)
t.test(femalePreC$transDemPref8H, femalePreT$transDemPref8H)
cohensD(femalePreC$transDemPref8H, femalePreT$transDemPref8H)

## Sex
# 2h
t.test(femalePreC$transDemPref2H, malePreC$transDemPref2H)
cohensD(femalePreC$transDemPref2H, malePreC$transDemPref2H)
t.test(femalePreT$transDemPref2H, malePreT$transDemPref2H)
cohensD(femalePreT$transDemPref2H, malePreT$transDemPref2H)
# 4h
t.test(femalePreC$transDemPref4H, malePreC$transDemPref4H)
cohensD(femalePreC$transDemPref4H, malePreC$transDemPref4H)
t.test(femalePreT$transDemPref4H, malePreT$transDemPref4H)
cohensD(femalePreT$transDemPref4H, malePreT$transDemPref4H)
# 6h
t.test(femalePreC$transDemPref6H, malePreC$transDemPref6H)
cohensD(femalePreC$transDemPref6H, malePreC$transDemPref6H)
t.test(femalePreT$transDemPref6H, malePreT$transDemPref6H)
cohensD(femalePreT$transDemPref6H, malePreT$transDemPref6H)
# 8h
t.test(femalePreC$transDemPref8H, malePreC$transDemPref8H)
cohensD(femalePreC$transDemPref8H, malePreC$transDemPref8H)
t.test(femalePreT$transDemPref8H, malePreT$transDemPref8H)
cohensD(femalePreT$transDemPref8H, malePreT$transDemPref8H)


# Total Consumption - Planned Comparisons

## Treatment
# 2h
t.test(malePreC$CON2H, malePreT$CON2H)
cohensD(malePreC$CON2H, malePreT$CON2H)
t.test(femalePreC$CON2H, femalePreT$CON2H)
cohensD(femalePreC$CON2H, femalePreT$CON2H)
# 4h
t.test(malePreC$CON4H, malePreT$CON4H)
cohensD(malePreC$CON4H, malePreT$CON4H)
t.test(femalePreC$CON4H, femalePreT$CON4H)
cohensD(femalePreC$CON4H, femalePreT$CON4H)
# 6h
t.test(malePreC$CON6H, malePreT$CON6H)
cohensD(malePreC$CON6H, malePreT$CON6H)
t.test(femalePreC$CON6H, femalePreT$CON6H)
cohensD(femalePreC$CON6H, femalePreT$CON6H)
# 8h
t.test(malePreC$CON8H, malePreT$CON8H)
cohensD(malePreC$CON8H, malePreT$CON8H)
t.test(femalePreC$CON8H, femalePreT$CON8H)
cohensD(femalePreC$CON8H, femalePreT$CON8H)

## Sex
# 2h
t.test(femalePreC$CON2H, malePreC$CON2H)
cohensD(femalePreC$CON2H, malePreC$CON2H)
t.test(femalePreT$CON2H, malePreT$CON2H)
cohensD(femalePreT$CON2H, malePreT$CON2H)
# 4h
t.test(femalePreC$CON4H, malePreC$CON4H)
cohensD(femalePreC$CON4H, malePreC$CON4H)
t.test(femalePreT$CON4H, malePreT$CON4H)
cohensD(femalePreT$CON4H, malePreT$CON4H)
# 6h
t.test(femalePreC$CON6H, malePreC$CON6H)
cohensD(femalePreC$CON6H, malePreC$CON6H)
t.test(femalePreT$CON6H, malePreT$CON6H)
cohensD(femalePreT$CON6H, malePreT$CON6H)
# 8h
t.test(femalePreC$CON8H, malePreC$CON8H)
cohensD(femalePreC$CON8H, malePreC$CON8H)
t.test(femalePreT$CON8H, malePreT$CON8H)
cohensD(femalePreT$CON8H, malePreT$CON8H)

#########################
# Analysis - Post-Puberty
#########################

treatmentC <- subset(postStfp, Group == "C")
treatmentD <- subset(postStfp, Group == "T")

maleC <- subset(treatmentC, Sex =="M")
femaleC <- subset(treatmentC, Sex == "F")
maleD <- subset(treatmentD, Sex =="M")
femaleD <- subset(treatmentD, Sex == "F")

maleShamC <- subset(maleC, Gonad == "SHAM")
maleGdxC <- subset(maleC, Gonad == "GDX")
maleRepC <- subset(maleC, Gonad == "REP")
femaleShamC <- subset(femaleC, Gonad == "SHAM")
femaleGdxC <- subset(femaleC, Gonad == "GDX")
femaleRepC <- subset(femaleC, Gonad == "REP")
maleShamT <- subset(maleD, Gonad == "SHAM")
maleGdxT <- subset(maleD, Gonad == "GDX")
maleRepT <- subset(maleD, Gonad == "REP")
femaleShamT <- subset(femaleD, Gonad == "SHAM")
femaleGdxT <- subset(femaleD, Gonad == "GDX")
femaleRepT <- subset(femaleD, Gonad == "REP")

# Flavour Preference - Planned Comparisons

# Male C Sham
t.test(transFlavAPref2H ~ DemFood, data = maleShamC)
cohensD(transFlavAPref2H ~ DemFood, data = maleShamC)
t.test(transFlavAPref4H ~ DemFood, data = maleShamC)
cohensD(transFlavAPref4H ~ DemFood, data = maleShamC)
t.test(transFlavAPref6H ~ DemFood, data = maleShamC)
cohensD(transFlavAPref6H ~ DemFood, data = maleShamC)
t.test(transFlavAPref8H ~ DemFood, data = maleShamC)
cohensD(transFlavAPref8H ~ DemFood, data = maleShamC)

# Male C GDX
t.test(transFlavAPref2H ~ DemFood, data = maleGdxC)
cohensD(transFlavAPref2H ~ DemFood, data = maleGdxC)
t.test(transFlavAPref4H ~ DemFood, data = maleGdxC)
cohensD(transFlavAPref4H ~ DemFood, data = maleGdxC)
t.test(transFlavAPref6H ~ DemFood, data = maleGdxC)
cohensD(transFlavAPref6H ~ DemFood, data = maleGdxC)
t.test(transFlavAPref8H ~ DemFood, data = maleGdxC)
cohensD(transFlavAPref8H ~ DemFood, data = maleGdxC)

# Male C REP
t.test(transFlavAPref2H ~ DemFood, data = maleRepC)
cohensD(transFlavAPref2H ~ DemFood, data = maleRepC)
t.test(transFlavAPref4H ~ DemFood, data = maleRepC)
cohensD(transFlavAPref4H ~ DemFood, data = maleRepC)
t.test(transFlavAPref6H ~ DemFood, data = maleRepC)
cohensD(transFlavAPref6H ~ DemFood, data = maleRepC)
t.test(transFlavAPref8H ~ DemFood, data = maleRepC)
cohensD(transFlavAPref8H ~ DemFood, data = maleRepC)


# Male T Sham
t.test(transFlavAPref2H ~ DemFood, data = maleShamT)
cohensD(transFlavAPref2H ~ DemFood, data = maleShamT)
t.test(transFlavAPref4H ~ DemFood, data = maleShamT)
cohensD(transFlavAPref4H ~ DemFood, data = maleShamT)
t.test(transFlavAPref6H ~ DemFood, data = maleShamT)
cohensD(transFlavAPref6H ~ DemFood, data = maleShamT)
t.test(transFlavAPref8H ~ DemFood, data = maleShamT)
cohensD(transFlavAPref8H ~ DemFood, data = maleShamT)

# Male T GDX
t.test(transFlavAPref2H ~ DemFood, data = maleGdxT)
cohensD(transFlavAPref2H ~ DemFood, data = maleGdxT)
t.test(transFlavAPref4H ~ DemFood, data = maleGdxT)
cohensD(transFlavAPref4H ~ DemFood, data = maleGdxT)
t.test(transFlavAPref6H ~ DemFood, data = maleGdxT)
cohensD(transFlavAPref6H ~ DemFood, data = maleGdxT)
t.test(transFlavAPref8H ~ DemFood, data = maleGdxT)
cohensD(transFlavAPref8H ~ DemFood, data = maleGdxT)

# Male T REP
t.test(transFlavAPref2H ~ DemFood, data = maleRepT)
cohensD(transFlavAPref2H ~ DemFood, data = maleRepT)
t.test(transFlavAPref4H ~ DemFood, data = maleRepT)
cohensD(transFlavAPref4H ~ DemFood, data = maleRepT)
t.test(transFlavAPref6H ~ DemFood, data = maleRepT)
cohensD(transFlavAPref6H ~ DemFood, data = maleRepT)
t.test(transFlavAPref8H ~ DemFood, data = maleRepT)
cohensD(transFlavAPref8H ~ DemFood, data = maleRepT)


# Female C Sham
t.test(transFlavAPref2H ~ DemFood, data = femaleShamC)
cohensD(transFlavAPref2H ~ DemFood, data = femaleShamC)
t.test(transFlavAPref4H ~ DemFood, data = femaleShamC)
cohensD(transFlavAPref4H ~ DemFood, data = femaleShamC)
t.test(transFlavAPref6H ~ DemFood, data = femaleShamC)
cohensD(transFlavAPref6H ~ DemFood, data = femaleShamC)
t.test(transFlavAPref8H ~ DemFood, data = femaleShamC)
cohensD(transFlavAPref8H ~ DemFood, data = femaleShamC)

# Female C GDX
t.test(transFlavAPref2H ~ DemFood, data = femaleGdxC)
cohensD(transFlavAPref2H ~ DemFood, data = femaleGdxC)
t.test(transFlavAPref4H ~ DemFood, data = femaleGdxC)
cohensD(transFlavAPref4H ~ DemFood, data = femaleGdxC)
t.test(transFlavAPref6H ~ DemFood, data = femaleGdxC)
cohensD(transFlavAPref6H ~ DemFood, data = femaleGdxC)
t.test(transFlavAPref8H ~ DemFood, data = femaleGdxC)
cohensD(transFlavAPref8H ~ DemFood, data = femaleGdxC)

# Female C REP
t.test(transFlavAPref2H ~ DemFood, data = femaleRepC)
cohensD(transFlavAPref2H ~ DemFood, data = femaleRepC)
t.test(transFlavAPref4H ~ DemFood, data = femaleRepC)
cohensD(transFlavAPref4H ~ DemFood, data = femaleRepC)
t.test(transFlavAPref6H ~ DemFood, data = femaleRepC)
cohensD(transFlavAPref6H ~ DemFood, data = femaleRepC)
t.test(transFlavAPref8H ~ DemFood, data = femaleRepC)
cohensD(transFlavAPref8H ~ DemFood, data = femaleRepC)


# Female T Sham
t.test(transFlavAPref2H ~ DemFood, data = femaleShamT)
cohensD(transFlavAPref2H ~ DemFood, data = femaleShamT)
t.test(transFlavAPref4H ~ DemFood, data = femaleShamT)
cohensD(transFlavAPref4H ~ DemFood, data = femaleShamT)
t.test(transFlavAPref6H ~ DemFood, data = femaleShamT)
cohensD(transFlavAPref6H ~ DemFood, data = femaleShamT)
t.test(transFlavAPref8H ~ DemFood, data = femaleShamT)
cohensD(transFlavAPref8H ~ DemFood, data = femaleShamT)

# Female T GDX
t.test(transFlavAPref2H ~ DemFood, data = femaleGdxT)
cohensD(transFlavAPref2H ~ DemFood, data = femaleGdxT)
t.test(transFlavAPref4H ~ DemFood, data = femaleGdxT)
cohensD(transFlavAPref4H ~ DemFood, data = femaleGdxT)
t.test(transFlavAPref6H ~ DemFood, data = femaleGdxT)
cohensD(transFlavAPref6H ~ DemFood, data = femaleGdxT)
t.test(transFlavAPref8H ~ DemFood, data = femaleGdxT)
cohensD(transFlavAPref8H ~ DemFood, data = femaleGdxT)

# Female T REP
t.test(transFlavAPref2H ~ DemFood, data = femaleRepT)
cohensD(transFlavAPref2H ~ DemFood, data = femaleRepT)
t.test(transFlavAPref4H ~ DemFood, data = femaleRepT)
cohensD(transFlavAPref4H ~ DemFood, data = femaleRepT)
t.test(transFlavAPref6H ~ DemFood, data = femaleRepT)
cohensD(transFlavAPref6H ~ DemFood, data = femaleRepT)
t.test(transFlavAPref8H ~ DemFood, data = femaleRepT)
cohensD(transFlavAPref8H ~ DemFood, data = femaleRepT)


### Dem Preference - Planned Comparisons

## 2 hour
# planned comparisons - hormone condition

t.test(femaleShamC$transDemPref2H, femaleGdxC$transDemPref2H) # sham - gdx
cohensD(femaleShamC$transDemPref2H, femaleGdxC$transDemPref2H)
t.test(femaleGdxC$transDemPref2H, femaleRepC$transDemPref2H) # rep - gdx
cohensD(femaleGdxC$transDemPref2H, femaleRepC$transDemPref2H)

t.test(femaleShamT$transDemPref2H, femaleGdxT$transDemPref2H)
cohensD(femaleShamT$transDemPref2H, femaleGdxT$transDemPref2H)
t.test(femaleGdxT$transDemPref2H, femaleRepT$transDemPref2H)
cohensD(femaleGdxT$transDemPref2H, femaleRepT$transDemPref2H)

t.test(maleShamC$transDemPref2H, maleGdxC$transDemPref2H)
cohensD(maleShamC$transDemPref2H, maleGdxC$transDemPref2H)
t.test(maleGdxC$transDemPref2H, maleRepC$transDemPref2H)
cohensD(maleGdxC$transDemPref2H, maleRepC$transDemPref2H)

t.test(maleShamT$transDemPref2H, maleGdxT$transDemPref2H)
cohensD(maleShamT$transDemPref2H, maleGdxT$transDemPref2H)
t.test(maleGdxT$transDemPref2H, maleRepT$transDemPref2H)
cohensD(maleGdxT$transDemPref2H, maleRepT$transDemPref2H)

# planned comparisons - male to female
t.test(femaleShamC$transDemPref2H, maleShamC$transDemPref2H)
cohensD(femaleShamC$transDemPref2H, maleShamC$transDemPref2H)
t.test(femaleRepC$transDemPref2H, maleRepC$transDemPref2H)
cohensD(femaleRepC$transDemPref2H, maleRepC$transDemPref2H)
t.test(femaleGdxC$transDemPref2H, maleGdxC$transDemPref2H)
cohensD(femaleGdxC$transDemPref2H, maleGdxC$transDemPref2H)

t.test(femaleShamT$transDemPref2H, maleShamT$transDemPref2H)
cohensD(femaleShamT$transDemPref2H, maleShamT$transDemPref2H)
t.test(femaleRepT$transDemPref2H, maleRepT$transDemPref2H)
cohensD(femaleRepT$transDemPref2H, maleRepT$transDemPref2H)
t.test(femaleGdxT$transDemPref2H, maleGdxT$transDemPref2H)
cohensD(femaleGdxT$transDemPref2H, maleGdxT$transDemPref2H)

# planned comparisons - test to control
t.test(femaleShamC$transDemPref2H, femaleShamT$transDemPref2H)
cohensD(femaleShamC$transDemPref2H, femaleShamT$transDemPref2H)
t.test(femaleRepC$transDemPref2H, femaleRepT$transDemPref2H)
cohensD(femaleRepC$transDemPref2H, femaleRepT$transDemPref2H)
t.test(femaleGdxC$transDemPref2H, femaleGdxT$transDemPref2H)
cohensD(femaleGdxC$transDemPref2H, femaleGdxT$transDemPref2H)

t.test(maleShamC$transDemPref2H, maleShamT$transDemPref2H)
cohensD(maleShamC$transDemPref2H, maleShamT$transDemPref2H)
t.test(maleRepC$transDemPref2H, maleRepT$transDemPref2H)
cohensD(maleRepC$transDemPref2H, maleRepT$transDemPref2H)
t.test(maleGdxC$transDemPref2H, maleGdxT$transDemPref2H)
cohensD(maleGdxC$transDemPref2H, maleGdxT$transDemPref2H)


## 4 hour
# planned comparisons - hormone condition

t.test(femaleShamC$transDemPref4H, femaleGdxC$transDemPref4H) # sham - gdx
cohensD(femaleShamC$transDemPref4H, femaleGdxC$transDemPref4H)
t.test(femaleGdxC$transDemPref4H, femaleRepC$transDemPref4H) # rep - gdx
cohensD(femaleGdxC$transDemPref4H, femaleRepC$transDemPref4H)

t.test(femaleShamT$transDemPref4H, femaleGdxT$transDemPref4H)
cohensD(femaleShamT$transDemPref4H, femaleGdxT$transDemPref4H)
t.test(femaleGdxT$transDemPref4H, femaleRepT$transDemPref4H)
cohensD(femaleGdxT$transDemPref4H, femaleRepT$transDemPref4H)

t.test(maleShamC$transDemPref4H, maleGdxC$transDemPref4H)
cohensD(maleShamC$transDemPref4H, maleGdxC$transDemPref4H)
t.test(maleGdxC$transDemPref4H, maleRepC$transDemPref4H)
cohensD(maleGdxC$transDemPref4H, maleRepC$transDemPref4H)

t.test(maleShamT$transDemPref4H, maleGdxT$transDemPref4H)
cohensD(maleShamT$transDemPref4H, maleGdxT$transDemPref4H)
t.test(maleGdxT$transDemPref4H, maleRepT$transDemPref4H)
cohensD(maleGdxT$transDemPref4H, maleRepT$transDemPref4H)

# planned comparisons - male to female
t.test(femaleShamC$transDemPref4H, maleShamC$transDemPref4H)
cohensD(femaleShamC$transDemPref4H, maleShamC$transDemPref4H)
t.test(femaleRepC$transDemPref4H, maleRepC$transDemPref4H)
cohensD(femaleRepC$transDemPref4H, maleRepC$transDemPref4H)
t.test(femaleGdxC$transDemPref4H, maleGdxC$transDemPref4H)
cohensD(femaleGdxC$transDemPref4H, maleGdxC$transDemPref4H)

t.test(femaleShamT$transDemPref4H, maleShamT$transDemPref4H)
cohensD(femaleShamT$transDemPref4H, maleShamT$transDemPref4H)
t.test(femaleRepT$transDemPref4H, maleRepT$transDemPref4H)
cohensD(femaleRepT$transDemPref4H, maleRepT$transDemPref4H)
t.test(femaleGdxT$transDemPref4H, maleGdxT$transDemPref4H)
cohensD(femaleGdxT$transDemPref4H, maleGdxT$transDemPref4H)

# planned comparisons - test to control
t.test(femaleShamC$transDemPref4H, femaleShamT$transDemPref4H)
cohensD(femaleShamC$transDemPref4H, femaleShamT$transDemPref4H)
t.test(femaleRepC$transDemPref4H, femaleRepT$transDemPref4H)
cohensD(femaleRepC$transDemPref4H, femaleRepT$transDemPref4H)
t.test(femaleGdxC$transDemPref4H, femaleGdxT$transDemPref4H)
cohensD(femaleGdxC$transDemPref4H, femaleGdxT$transDemPref4H)

t.test(maleShamC$transDemPref4H, maleShamT$transDemPref4H)
cohensD(maleShamC$transDemPref4H, maleShamT$transDemPref4H)
t.test(maleRepC$transDemPref4H, maleRepT$transDemPref4H)
cohensD(maleRepC$transDemPref4H, maleRepT$transDemPref4H)
t.test(maleGdxC$transDemPref4H, maleGdxT$transDemPref4H)
cohensD(maleGdxC$transDemPref4H, maleGdxT$transDemPref4H)


## 6 hour
# planned comparisons - hormone condition

t.test(femaleShamC$transDemPref6H, femaleGdxC$transDemPref6H) # sham - gdx
cohensD(femaleShamC$transDemPref6H, femaleGdxC$transDemPref6H)
t.test(femaleGdxC$transDemPref6H, femaleRepC$transDemPref6H) # rep - gdx
cohensD(femaleGdxC$transDemPref6H, femaleRepC$transDemPref6H)

t.test(femaleShamT$transDemPref6H, femaleGdxT$transDemPref6H)
cohensD(femaleShamT$transDemPref6H, femaleGdxT$transDemPref6H)
t.test(femaleGdxT$transDemPref6H, femaleRepT$transDemPref6H)
cohensD(femaleGdxT$transDemPref6H, femaleRepT$transDemPref6H)

t.test(maleShamC$transDemPref6H, maleGdxC$transDemPref6H)
cohensD(maleShamC$transDemPref6H, maleGdxC$transDemPref6H)
t.test(maleGdxC$transDemPref6H, maleRepC$transDemPref6H)
cohensD(maleGdxC$transDemPref6H, maleRepC$transDemPref6H)

t.test(maleShamT$transDemPref6H, maleGdxT$transDemPref6H)
cohensD(maleShamT$transDemPref6H, maleGdxT$transDemPref6H)
t.test(maleGdxT$transDemPref6H, maleRepT$transDemPref6H)
cohensD(maleGdxT$transDemPref6H, maleRepT$transDemPref6H)

# planned comparisons - male to female
t.test(femaleShamC$transDemPref6H, maleShamC$transDemPref6H)
cohensD(femaleShamC$transDemPref6H, maleShamC$transDemPref6H)
t.test(femaleRepC$transDemPref6H, maleRepC$transDemPref6H)
cohensD(femaleRepC$transDemPref6H, maleRepC$transDemPref6H)
t.test(femaleGdxC$transDemPref6H, maleGdxC$transDemPref6H)
cohensD(femaleGdxC$transDemPref6H, maleGdxC$transDemPref6H)

t.test(femaleShamT$transDemPref6H, maleShamT$transDemPref6H)
cohensD(femaleShamT$transDemPref6H, maleShamT$transDemPref6H)
t.test(femaleRepT$transDemPref6H, maleRepT$transDemPref6H)
cohensD(femaleRepT$transDemPref6H, maleRepT$transDemPref6H)
t.test(femaleGdxT$transDemPref6H, maleGdxT$transDemPref6H)
cohensD(femaleGdxT$transDemPref6H, maleGdxT$transDemPref6H)

# planned comparisons - test to control
t.test(femaleShamC$transDemPref6H, femaleShamT$transDemPref6H)
cohensD(femaleShamC$transDemPref6H, femaleShamT$transDemPref6H)
t.test(femaleRepC$transDemPref6H, femaleRepT$transDemPref6H)
cohensD(femaleRepC$transDemPref6H, femaleRepT$transDemPref6H)
t.test(femaleGdxC$transDemPref6H, femaleGdxT$transDemPref6H)
cohensD(femaleGdxC$transDemPref6H, femaleGdxT$transDemPref6H)

t.test(maleShamC$transDemPref6H, maleShamT$transDemPref6H)
cohensD(maleShamC$transDemPref6H, maleShamT$transDemPref6H)
t.test(maleRepC$transDemPref6H, maleRepT$transDemPref6H)
cohensD(maleRepC$transDemPref6H, maleRepT$transDemPref6H)
t.test(maleGdxC$transDemPref6H, maleGdxT$transDemPref6H)
cohensD(maleGdxC$transDemPref6H, maleGdxT$transDemPref6H)


## 8 hour
# planned comparisons - hormone condition

t.test(femaleShamC$transDemPref8H, femaleGdxC$transDemPref8H) # sham - gdx
cohensD(femaleShamC$transDemPref8H, femaleGdxC$transDemPref8H)
t.test(femaleGdxC$transDemPref8H, femaleRepC$transDemPref8H) # rep - gdx
cohensD(femaleGdxC$transDemPref8H, femaleRepC$transDemPref8H)

t.test(femaleShamT$transDemPref8H, femaleGdxT$transDemPref8H)
cohensD(femaleShamT$transDemPref8H, femaleGdxT$transDemPref8H)
t.test(femaleGdxT$transDemPref8H, femaleRepT$transDemPref8H)
cohensD(femaleGdxT$transDemPref8H, femaleRepT$transDemPref8H)

t.test(maleShamC$transDemPref8H, maleGdxC$transDemPref8H)
cohensD(maleShamC$transDemPref8H, maleGdxC$transDemPref8H)
t.test(maleGdxC$transDemPref8H, maleRepC$transDemPref8H)
cohensD(maleGdxC$transDemPref8H, maleRepC$transDemPref8H)

t.test(maleShamT$transDemPref8H, maleGdxT$transDemPref8H)
cohensD(maleShamT$transDemPref8H, maleGdxT$transDemPref8H)
t.test(maleGdxT$transDemPref8H, maleRepT$transDemPref8H)
cohensD(maleGdxT$transDemPref8H, maleRepT$transDemPref8H)

# planned comparisons - male to female
t.test(femaleShamC$transDemPref8H, maleShamC$transDemPref8H)
cohensD(femaleShamC$transDemPref8H, maleShamC$transDemPref8H)
t.test(femaleRepC$transDemPref8H, maleRepC$transDemPref8H)
cohensD(femaleRepC$transDemPref8H, maleRepC$transDemPref8H)
t.test(femaleGdxC$transDemPref8H, maleGdxC$transDemPref8H)
cohensD(femaleGdxC$transDemPref8H, maleGdxC$transDemPref8H)

t.test(femaleShamT$transDemPref8H, maleShamT$transDemPref8H)
cohensD(femaleShamT$transDemPref8H, maleShamT$transDemPref8H)
t.test(femaleRepT$transDemPref8H, maleRepT$transDemPref8H)
cohensD(femaleRepT$transDemPref8H, maleRepT$transDemPref8H)
t.test(femaleGdxT$transDemPref8H, maleGdxT$transDemPref8H)
cohensD(femaleGdxT$transDemPref8H, maleGdxT$transDemPref8H)

# planned comparisons - test to control
t.test(femaleShamC$transDemPref8H, femaleShamT$transDemPref8H)
cohensD(femaleShamC$transDemPref8H, femaleShamT$transDemPref8H)
t.test(femaleRepC$transDemPref8H, femaleRepT$transDemPref8H)
cohensD(femaleRepC$transDemPref8H, femaleRepT$transDemPref8H)
t.test(femaleGdxC$transDemPref8H, femaleGdxT$transDemPref8H)
cohensD(femaleGdxC$transDemPref8H, femaleGdxT$transDemPref8H)

t.test(maleShamC$transDemPref8H, maleShamT$transDemPref8H)
cohensD(maleShamC$transDemPref8H, maleShamT$transDemPref8H)
t.test(maleRepC$transDemPref8H, maleRepT$transDemPref8H)
cohensD(maleRepC$transDemPref8H, maleRepT$transDemPref8H)
t.test(maleGdxC$transDemPref8H, maleGdxT$transDemPref8H)
cohensD(maleGdxC$transDemPref8H, maleGdxT$transDemPref8H)




# Total Consumption - Planned Comparisons

## 2 hour
# planned comparisons - hormone condition

t.test(femaleShamC$CON2H, femaleGdxC$CON2H) # sham - gdx
cohensD(femaleShamC$CON2H, femaleGdxC$CON2H)
t.test(femaleGdxC$CON2H, femaleRepC$CON2H) # rep - gdx
cohensD(femaleGdxC$CON2H, femaleRepC$CON2H)

t.test(femaleShamT$CON2H, femaleGdxT$CON2H)
cohensD(femaleShamT$CON2H, femaleGdxT$CON2H)
t.test(femaleGdxT$CON2H, femaleRepT$CON2H)
cohensD(femaleGdxT$CON2H, femaleRepT$CON2H)

t.test(maleShamC$CON2H, maleGdxC$CON2H)
cohensD(maleShamC$CON2H, maleGdxC$CON2H)
t.test(maleGdxC$CON2H, maleRepC$CON2H)
cohensD(maleGdxC$CON2H, maleRepC$CON2H)

t.test(maleShamT$CON2H, maleGdxT$CON2H)
cohensD(maleShamT$CON2H, maleGdxT$CON2H)
t.test(maleGdxT$CON2H, maleRepT$CON2H)
cohensD(maleGdxT$CON2H, maleRepT$CON2H)

# planned comparisons - male to female
t.test(femaleShamC$CON2H, maleShamC$CON2H)
cohensD(femaleShamC$CON2H, maleShamC$CON2H)
t.test(femaleRepC$CON2H, maleRepC$CON2H)
cohensD(femaleRepC$CON2H, maleRepC$CON2H)
t.test(femaleGdxC$CON2H, maleGdxC$CON2H)
cohensD(femaleGdxC$CON2H, maleGdxC$CON2H)

t.test(femaleShamT$CON2H, maleShamT$CON2H)
cohensD(femaleShamT$CON2H, maleShamT$CON2H)
t.test(femaleRepT$CON2H, maleRepT$CON2H)
cohensD(femaleRepT$CON2H, maleRepT$CON2H)
t.test(femaleGdxT$CON2H, maleGdxT$CON2H)
cohensD(femaleGdxT$CON2H, maleGdxT$CON2H)

# planned comparisons - test to control
t.test(femaleShamC$CON2H, femaleShamT$CON2H)
cohensD(femaleShamC$CON2H, femaleShamT$CON2H)
t.test(femaleRepC$CON2H, femaleRepT$CON2H)
cohensD(femaleRepC$CON2H, femaleRepT$CON2H)
t.test(femaleGdxC$CON2H, femaleGdxT$CON2H)
cohensD(femaleGdxC$CON2H, femaleGdxT$CON2H)

t.test(maleShamC$CON2H, maleShamT$CON2H)
cohensD(maleShamC$CON2H, maleShamT$CON2H)
t.test(maleRepC$CON2H, maleRepT$CON2H)
cohensD(maleRepC$CON2H, maleRepT$CON2H)
t.test(maleGdxC$CON2H, maleGdxT$CON2H)
cohensD(maleGdxC$CON2H, maleGdxT$CON2H)


## 4 hour
# planned comparisons - hormone condition

t.test(femaleShamC$CON4H, femaleGdxC$CON4H) # sham - gdx
cohensD(femaleShamC$CON4H, femaleGdxC$CON4H)
t.test(femaleGdxC$CON4H, femaleRepC$CON4H) # rep - gdx
cohensD(femaleGdxC$CON4H, femaleRepC$CON4H)

t.test(femaleShamT$CON4H, femaleGdxT$CON4H)
cohensD(femaleShamT$CON4H, femaleGdxT$CON4H)
t.test(femaleGdxT$CON4H, femaleRepT$CON4H)
cohensD(femaleGdxT$CON4H, femaleRepT$CON4H)

t.test(maleShamC$CON4H, maleGdxC$CON4H)
cohensD(maleShamC$CON4H, maleGdxC$CON4H)
t.test(maleGdxC$CON4H, maleRepC$CON4H)
cohensD(maleGdxC$CON4H, maleRepC$CON4H)

t.test(maleShamT$CON4H, maleGdxT$CON4H)
cohensD(maleShamT$CON4H, maleGdxT$CON4H)
t.test(maleGdxT$CON4H, maleRepT$CON4H)
cohensD(maleGdxT$CON4H, maleRepT$CON4H)

# planned comparisons - male to female
t.test(femaleShamC$CON4H, maleShamC$CON4H)
cohensD(femaleShamC$CON4H, maleShamC$CON4H)
t.test(femaleRepC$CON4H, maleRepC$CON4H)
cohensD(femaleRepC$CON4H, maleRepC$CON4H)
t.test(femaleGdxC$CON4H, maleGdxC$CON4H)
cohensD(femaleGdxC$CON4H, maleGdxC$CON4H)

t.test(femaleShamT$CON4H, maleShamT$CON4H)
cohensD(femaleShamT$CON4H, maleShamT$CON4H)
t.test(femaleRepT$CON4H, maleRepT$CON4H)
cohensD(femaleRepT$CON4H, maleRepT$CON4H)
t.test(femaleGdxT$CON4H, maleGdxT$CON4H)
cohensD(femaleGdxT$CON4H, maleGdxT$CON4H)

# planned comparisons - test to control
t.test(femaleShamC$CON4H, femaleShamT$CON4H)
cohensD(femaleShamC$CON4H, femaleShamT$CON4H)
t.test(femaleRepC$CON4H, femaleRepT$CON4H)
cohensD(femaleRepC$CON4H, femaleRepT$CON4H)
t.test(femaleGdxC$CON4H, femaleGdxT$CON4H)
cohensD(femaleGdxC$CON4H, femaleGdxT$CON4H)

t.test(maleShamC$CON4H, maleShamT$CON4H)
cohensD(maleShamC$CON4H, maleShamT$CON4H)
t.test(maleRepC$CON4H, maleRepT$CON4H)
cohensD(maleRepC$CON4H, maleRepT$CON4H)
t.test(maleGdxC$CON4H, maleGdxT$CON4H)
cohensD(maleGdxC$CON4H, maleGdxT$CON4H)


## 6 hour
# planned comparisons - hormone condition

t.test(femaleShamC$CON6H, femaleGdxC$CON6H) # sham - gdx
cohensD(femaleShamC$CON6H, femaleGdxC$CON6H)
t.test(femaleGdxC$CON6H, femaleRepC$CON6H) # rep - gdx
cohensD(femaleGdxC$CON6H, femaleRepC$CON6H)

t.test(femaleShamT$CON6H, femaleGdxT$CON6H)
cohensD(femaleShamT$CON6H, femaleGdxT$CON6H)
t.test(femaleGdxT$CON6H, femaleRepT$CON6H)
cohensD(femaleGdxT$CON6H, femaleRepT$CON6H)

t.test(maleShamC$CON6H, maleGdxC$CON6H)
cohensD(maleShamC$CON6H, maleGdxC$CON6H)
t.test(maleGdxC$CON6H, maleRepC$CON6H)
cohensD(maleGdxC$CON6H, maleRepC$CON6H)

t.test(maleShamT$CON6H, maleGdxT$CON6H)
cohensD(maleShamT$CON6H, maleGdxT$CON6H)
t.test(maleGdxT$CON6H, maleRepT$CON6H)
cohensD(maleGdxT$CON6H, maleRepT$CON6H)

# planned comparisons - male to female
t.test(femaleShamC$CON6H, maleShamC$CON6H)
cohensD(femaleShamC$CON6H, maleShamC$CON6H)
t.test(femaleRepC$CON6H, maleRepC$CON6H)
cohensD(femaleRepC$CON6H, maleRepC$CON6H)
t.test(femaleGdxC$CON6H, maleGdxC$CON6H)
cohensD(femaleGdxC$CON6H, maleGdxC$CON6H)

t.test(femaleShamT$CON6H, maleShamT$CON6H)
cohensD(femaleShamT$CON6H, maleShamT$CON6H)
t.test(femaleRepT$CON6H, maleRepT$CON6H)
cohensD(femaleRepT$CON6H, maleRepT$CON6H)
t.test(femaleGdxT$CON6H, maleGdxT$CON6H)
cohensD(femaleGdxT$CON6H, maleGdxT$CON6H)

# planned comparisons - test to control
t.test(femaleShamC$CON6H, femaleShamT$CON6H)
cohensD(femaleShamC$CON6H, femaleShamT$CON6H)
t.test(femaleRepC$CON6H, femaleRepT$CON6H)
cohensD(femaleRepC$CON6H, femaleRepT$CON6H)
t.test(femaleGdxC$CON6H, femaleGdxT$CON6H)
cohensD(femaleGdxC$CON6H, femaleGdxT$CON6H)

t.test(maleShamC$CON6H, maleShamT$CON6H)
cohensD(maleShamC$CON6H, maleShamT$CON6H)
t.test(maleRepC$CON6H, maleRepT$CON6H)
cohensD(maleRepC$CON6H, maleRepT$CON6H)
t.test(maleGdxC$CON6H, maleGdxT$CON6H)
cohensD(maleGdxC$CON6H, maleGdxT$CON6H)


## 8 hour
# planned comparisons - hormone condition

t.test(femaleShamC$CON8H, femaleGdxC$CON8H) # sham - gdx
cohensD(femaleShamC$CON8H, femaleGdxC$CON8H)
t.test(femaleGdxC$CON8H, femaleRepC$CON8H) # rep - gdx
cohensD(femaleGdxC$CON8H, femaleRepC$CON8H)

t.test(femaleShamT$CON8H, femaleGdxT$CON8H)
cohensD(femaleShamT$CON8H, femaleGdxT$CON8H)
t.test(femaleGdxT$CON8H, femaleRepT$CON8H)
cohensD(femaleGdxT$CON8H, femaleRepT$CON8H)

t.test(maleShamC$CON8H, maleGdxC$CON8H)
cohensD(maleShamC$CON8H, maleGdxC$CON8H)
t.test(maleGdxC$CON8H, maleRepC$CON8H)
cohensD(maleGdxC$CON8H, maleRepC$CON8H)

t.test(maleShamT$CON8H, maleGdxT$CON8H)
cohensD(maleShamT$CON8H, maleGdxT$CON8H)
t.test(maleGdxT$CON8H, maleRepT$CON8H)
cohensD(maleGdxT$CON8H, maleRepT$CON8H)

# planned comparisons - male to female
t.test(femaleShamC$CON8H, maleShamC$CON8H)
cohensD(femaleShamC$CON8H, maleShamC$CON8H)
t.test(femaleRepC$CON8H, maleRepC$CON8H)
cohensD(femaleRepC$CON8H, maleRepC$CON8H)
t.test(femaleGdxC$CON8H, maleGdxC$CON8H)
cohensD(femaleGdxC$CON8H, maleGdxC$CON8H)

t.test(femaleShamT$CON8H, maleShamT$CON8H)
cohensD(femaleShamT$CON8H, maleShamT$CON8H)
t.test(femaleRepT$CON8H, maleRepT$CON8H)
cohensD(femaleRepT$CON8H, maleRepT$CON8H)
t.test(femaleGdxT$CON8H, maleGdxT$CON8H)
cohensD(femaleGdxT$CON8H, maleGdxT$CON8H)

# planned comparisons - test to control
t.test(femaleShamC$CON8H, femaleShamT$CON8H)
cohensD(femaleShamC$CON8H, femaleShamT$CON8H)
t.test(femaleRepC$CON8H, femaleRepT$CON8H)
cohensD(femaleRepC$CON8H, femaleRepT$CON8H)
t.test(femaleGdxC$CON8H, femaleGdxT$CON8H)
cohensD(femaleGdxC$CON8H, femaleGdxT$CON8H)

t.test(maleShamC$CON8H, maleShamT$CON8H)
cohensD(maleShamC$CON8H, maleShamT$CON8H)
t.test(maleRepC$CON8H, maleRepT$CON8H)
cohensD(maleRepC$CON8H, maleRepT$CON8H)
t.test(maleGdxC$CON8H, maleGdxT$CON8H)
cohensD(maleGdxC$CON8H, maleGdxT$CON8H)


### Flavour Rec Analysis
flavRecMaleTGdx <- subset(flavRec, Sex == "M" & Gonad == "GDX")
flavRecMaleTRep <- subset(flavRec, Sex == "M" & Gonad == "REP")
flavRecFemaleTGdx <- subset(flavRec, Sex == "F" & Gonad == "GDX")

t.test(flavRecMaleTGdx$IRHAB, flavRecMaleTGdx$IRTEST)
cohensD(flavRecMaleTGdx$IRHAB, flavRecMaleTGdx$IRTEST)
t.test(flavRecMaleTRep$IRHAB, flavRecMaleTRep$IRTEST)
cohensD(flavRecMaleTRep$IRHAB, flavRecMaleTRep$IRTEST)
t.test(flavRecFemaleTGdx$IRHAB, flavRecFemaleTGdx$IRTEST)
cohensD(flavRecFemaleTGdx$IRHAB, flavRecFemaleTGdx$IRTEST)


maleChoc <- subset(chocChip, Sex == "M")
femaleChoc <- subset(chocChip, Sex == "F")

t.test(Time ~ Treatment, data = maleChoc)
t.test(Time ~ Treatment, data = femaleChoc)




# GDX pilot test

stfpData <- read.csv(file = "data/stfpPilot.csv")

stfpData <- genFlavourConsumption(stfpData)
stfpData <- removeNegative(stfpData)
stfpData <- genTotalConsumption(stfpData)
stfpData <- removeLowFeeding(stfpData)
stfpData <- genPreferenceData(stfpData)
stfpData <- stfpTransform(stfpData)

t.test(transFlavAPref2H ~ DemFood, data = stfpData)
cohensD(transFlavAPref2H ~ DemFood, data = stfpData)
t.test(transFlavAPref4H ~ DemFood, data = stfpData)
cohensD(transFlavAPref4H ~ DemFood, data = stfpData)
t.test(transFlavAPref6H ~ DemFood, data = stfpData)
cohensD(transFlavAPref6H ~ DemFood, data = stfpData)
t.test(transFlavAPref8H ~ DemFood, data = stfpData)
cohensD(transFlavAPref8H ~ DemFood, data = stfpData)

pilotMelt <- stfpMelt(stfpData)
gdxGraph <- stfpGraph(dataSet = pilotMelt, "GDX Female", "Turmeric", "Thyme", showLegend = F)

stfpData <- read.csv(file = "data/intactPilot.csv", na.strings = "NA")

stfpData$transFlavAPref2H <- asin(sqrt(stfpData$flavAPref2H))
stfpData$transFlavAPref4H <- asin(sqrt(stfpData$flavAPref4H))
stfpData$transFlavAPref6H <- asin(sqrt(stfpData$flavAPref6H))
stfpData$transFlavAPref8H <- asin(sqrt(stfpData$flavAPref8H))

malePilot <- subset(stfpData, Sex == "M")
femalePilot <- subset(stfpData, Sex == "F")
maleMelt <- stfpMelt(malePilot)
femaleMelt <- stfpMelt(femalePilot)
maleGraph <- stfpGraph(dataSet = maleMelt, "Intact Male", "Turmeric", "Thyme", showLegend = F)
femaleGraph <- stfpGraph(dataSet = femaleMelt, "Intact Female", "Turmeric", "Thyme", showLegend = T)
legend <- getLegend(femaleGraph)
femaleGraph <- femaleGraph + theme(legend.position = "none")

pdf(paste("output/pilot.pdf"), 36, 8)
grid.arrange(gdxGraph,
             maleGraph,
             femaleGraph,
             legend,
             ncol = 4, nrow = 1, widths = c(10, 10, 10, 6), heights = c(8))
dev.off()



t.test(transFlavAPref2H ~ DemFood, data = malePilot)
cohensD(transFlavAPref2H ~ DemFood, data = malePilot)
t.test(transFlavAPref4H ~ DemFood, data = malePilot)
cohensD(transFlavAPref4H ~ DemFood, data = malePilot)
t.test(transFlavAPref6H ~ DemFood, data = malePilot)
cohensD(transFlavAPref6H ~ DemFood, data = malePilot)
t.test(transFlavAPref8H ~ DemFood, data = malePilot)
cohensD(transFlavAPref8H ~ DemFood, data = malePilot)

t.test(transFlavAPref2H ~ DemFood, data = femalePilot)
cohensD(transFlavAPref2H ~ DemFood, data = femalePilot)
t.test(transFlavAPref4H ~ DemFood, data = femalePilot)
cohensD(transFlavAPref4H ~ DemFood, data = femalePilot)
t.test(transFlavAPref6H ~ DemFood, data = femalePilot)
cohensD(transFlavAPref6H ~ DemFood, data = femalePilot)
t.test(transFlavAPref8H ~ DemFood, data = femalePilot)
cohensD(transFlavAPref8H ~ DemFood, data = femalePilot)


pilotPref <- read.csv("data/pilot2.csv", na.strings = "NA")
consPilot <- pilotGraph(data = pilotPref)

t.test(subset(pilotPref, Flavour == "TUM")$Consumption, subset(pilotPref, Flavour == "THY")$Consumption)

########################
# Graphing - Pre-Puberty
########################

malePre <- subset(preStfp, Sex == "M")
femalePre <- subset(preStfp, Sex == "F")

# melt
malePreCMelt <- stfpMelt(malePreC)
femalePreCMelt <- stfpMelt(femalePreC)
malePreTMelt <- stfpMelt(malePreT)
femalePreTMelt <- stfpMelt(femalePreT)

malePreDemMelt <- demPrefMelt(malePre)
femalePreDemMelt <- demPrefMelt(femalePre)

malePreConMelt <- consumptionMelt(malePre)
femalePreConMelt <- consumptionMelt(femalePre)

# Flavour Preference
malePreCPrefGraph <- stfpGraphPre(dataSet = malePreCMelt, "Prepuberty - Male Sesame Oil", "Cinnamon", "Cocoa", showLegend = T)
femalePreCPrefGraph <- stfpGraphPre(dataSet = femalePreCMelt, "Prepuberty - Female Sesame Oil", "Cinnamon", "Cocoa", showLegend = F)
malePreTPrefGraph <- stfpGraphPre(dataSet = malePreTMelt, "Prepuberty - Male Testosterone", "Cinnamon", "Cocoa", showLegend = F)
femalePreTPrefGraph <- stfpGraphPre(dataSet = femalePreTMelt, "Prepuberty - Female Testosterone", "Cinnamon", "Cocoa", showLegend = F)

preLegend <- getLegend(malePreCPrefGraph)
malePreCPrefGraph <- malePreCPrefGraph + theme(legend.position = "none")

pdf(paste("output/PrePuberty/prepubSTFP.pdf"), 18, 18)
grid.arrange(malePreCPrefGraph,
             malePreTPrefGraph,
             femalePreCPrefGraph,
             femalePreTPrefGraph,
             preLegend,
             ncol = 2, nrow = 3, widths = c(12, 12), heights = c(8, 8, 7))
dev.off()


# Dem Preference

malePreDemGraph <- demPrefGraphPre(malePreDemMelt, graphTitle = "Male - Prepuberty", yMax = 1)
femalePreDemGraph <- demPrefGraphPre(femalePreDemMelt, graphTitle = "Female - Prepuberty", yMax = 1, showLegend = F)

demLegendPre <- getLegend(malePreDemGraph)
malePreDemGraph <- malePreDemGraph + theme(legend.position = "none")

pdf(paste("output/PrePuberty/prepubDemPref.pdf"), 16, 10)
grid.arrange(malePreDemGraph,
             femalePreDemGraph,
             demLegendPre,
             ncol = 2, nrow = 2, widths = c(6, 6), heights = c(6, 4))
dev.off()

# Total Consumption

malePreConsGraph <- consumptionGraphPre(malePreConMelt, graphTitle = "Male - Prepuberty", yMax = 1.5)
femalePreConsGraph <- consumptionGraphPre(femalePreConMelt, graphTitle = "Female - Prepuberty", yMax = 1.5, showLegend = F)

conLegendPre <- getLegend(malePreConsGraph)
malePreConsGraph <- malePreConsGraph + theme(legend.position = "none")

pdf(paste("output/PrePuberty/prepubConsumption.pdf"), 16, 10)
grid.arrange(malePreConsGraph,
             femalePreConsGraph,
             conLegendPre,
             ncol = 2, nrow = 2, widths = c(6, 6), heights = c(6, 4))
dev.off()

#########################
# Graphing - Post-Puberty
#########################

# melt
maleShamCMelt <- stfpMelt(maleShamC)
maleGdxCMelt <- stfpMelt(maleGdxC)
maleRepCMelt <- stfpMelt(maleRepC)
femaleShamCMelt <- stfpMelt(femaleShamC)
femaleGdxCMelt <- stfpMelt(femaleGdxC)
femaleRepCMelt <- stfpMelt(femaleRepC)
maleShamTMelt <- stfpMelt(maleShamT)
maleGdxTMelt <- stfpMelt(maleGdxT)
maleRepTMelt <- stfpMelt(maleRepT)
femaleShamTMelt <- stfpMelt(femaleShamT)
femaleGdxTMelt <- stfpMelt(femaleGdxT)
femaleRepTMelt <- stfpMelt(femaleRepT)

maleSham <- subset(subset(postStfp, Sex == "M"), Gonad == "SHAM")
maleGdx <- subset(subset(postStfp, Sex == "M"), Gonad == "GDX")
maleRep <- subset(subset(postStfp, Sex == "M"), Gonad == "REP")
femaleSham <- subset(subset(postStfp, Sex == "F"), Gonad == "SHAM")
femaleGdx <- subset(subset(postStfp, Sex == "F"), Gonad == "GDX")
femaleRep <- subset(subset(postStfp, Sex == "F"), Gonad == "REP")

demMaleShamMelt <- demPrefMelt(maleSham)
demMaleGdxMelt <- demPrefMelt(maleGdx)
demMaleRepMelt <- demPrefMelt(maleRep)
demFemaleShamMelt <- demPrefMelt(femaleSham)
demFemaleGdxMelt <- demPrefMelt(femaleGdx)
demFemaleRepMelt <- demPrefMelt(femaleRep)

conMaleShamMelt <- consumptionMelt(maleSham)
conMaleGdxMelt <- consumptionMelt(maleGdx)
conMaleRepMelt <- consumptionMelt(maleRep)
conFemaleShamMelt <- consumptionMelt(femaleSham)
conFemaleGdxMelt <- consumptionMelt(femaleGdx)
conFemaleRepMelt <- consumptionMelt(femaleRep)


# Flavour Preference

maleShamCPrefGraph <- stfpGraph(dataSet = maleShamCMelt, "Male Sesame Oil Intact", "Turmeric", "Thyme")
maleGdxCPrefGraph <- stfpGraph(dataSet = maleGdxCMelt, "Male Sesame Oil GDX+VEH", "Turmeric", "Thyme", showLegend = F)
maleRepCPrefGraph <- stfpGraph(dataSet = maleRepCMelt, "Male Sesame Oil GDX+T", "Turmeric", "Thyme", showLegend = F)
femaleShamCPrefGraph <- stfpGraph(dataSet = femaleShamCMelt, "Female Sesame Oil Intact", "Turmeric", "Thyme", showLegend = F)
femaleGdxCPrefGraph <- stfpGraph(dataSet = femaleGdxCMelt, "Female Sesame Oil GDX+VEH", "Turmeric", "Thyme", showLegend = F)
femaleRepCPrefGraph <- stfpGraph(dataSet = femaleRepCMelt, "Female Sesame Oil GDX+EB", "Turmeric", "Thyme", showLegend = F)
maleShamTPrefGraph <- stfpGraph(dataSet = maleShamTMelt, "Male Testosterone Intact", "Turmeric", "Thyme", showLegend = F)
maleGdxTPrefGraph <- stfpGraph(dataSet = maleGdxTMelt, "Male Testosterone GDX+VEH", "Turmeric", "Thyme", showLegend = F)
maleRepTPrefGraph <- stfpGraph(dataSet = maleRepTMelt, "Male Testosterone GDX+T", "Turmeric", "Thyme", showLegend = F)
femaleShamTPrefGraph <- stfpGraph(dataSet = femaleShamTMelt, "Female Testosterone Intact", "Turmeric", "Thyme", showLegend = F)
femaleGdxTPrefGraph <- stfpGraph(dataSet = femaleGdxTMelt, "Female Testosterone GDX+VEH", "Turmeric", "Thyme", showLegend = F)
femaleRepTPrefGraph <- stfpGraph(dataSet = femaleRepTMelt, "Female Testosterone GDX+EB", "Turmeric", "Thyme", showLegend = F)

postLegend <- getLegend(maleShamCPrefGraph)
maleShamCPrefGraph <- maleShamCPrefGraph + theme(legend.position = "none")

pdf(paste("output/PostPuberty/postpubSTFP.pdf"), 26, 35)
grid.arrange(maleShamCPrefGraph, 
             maleGdxCPrefGraph, 
             maleRepCPrefGraph,
             maleShamTPrefGraph, 
             maleGdxTPrefGraph, 
             maleRepTPrefGraph, 
             femaleShamCPrefGraph, 
             femaleGdxCPrefGraph, 
             femaleRepCPrefGraph, 
             femaleShamTPrefGraph, 
             femaleGdxTPrefGraph, 
             femaleRepTPrefGraph,
             postLegend,
             ncol = 3, nrow = 5, widths = c(12, 12, 12))
dev.off()

# Dem Preference

maleShamDemGraph <- demPrefGraphPre(dataSet = demMaleShamMelt, graphTitle = "Male - Intact", yMax = 1)
maleGdxDemGraph <- demPrefGraphPre(dataSet = demMaleGdxMelt, graphTitle = "Male - GDX+VEH", yMax = 1, showLegend = F)
maleRepDemGraph <- demPrefGraphPre(dataSet = demMaleRepMelt, graphTitle = "Male - GDX+T", yMax = 1, showLegend = F)
femaleShamDemGraph <- demPrefGraphPre(dataSet = demFemaleShamMelt, graphTitle = "Female - Intact", yMax = 1, showLegend = F)
femaleGdxDemGraph <- demPrefGraphPre(dataSet = demFemaleGdxMelt, graphTitle = "Female - GDX+VEH", yMax = 1, showLegend = F)
femaleRepDemGraph <- demPrefGraphPre(dataSet = demFemaleRepMelt, graphTitle = "Female - GDX+EB", yMax = 1, showLegend = F)

demLegend <- getLegend(maleShamDemGraph)
maleShamDemGraph <- maleShamDemGraph + theme(legend.position = "none")

pdf(paste("output/PostPuberty/postpubDemPref.pdf"), 24, 18)
grid.arrange(maleShamDemGraph,
             maleGdxDemGraph,
             maleRepDemGraph,
             femaleShamDemGraph,
             femaleGdxDemGraph,
             femaleRepDemGraph,
             demLegend,
             ncol = 3, nrow = 3, widths = c(8, 8, 8))
dev.off()


# Total Consumption

maleShamConsGraph <- consumptionGraphPre(dataSet = conMaleShamMelt, graphTitle = "Male - Intact", yMax = 1.5)
maleGdxConsGraph <- consumptionGraphPre(dataSet = conMaleGdxMelt, graphTitle = "Male - GDX+VEH", yMax = 1.5, showLegend = F)
maleRepConsGraph <- consumptionGraphPre(dataSet = conMaleRepMelt, graphTitle = "Male - GDX+T", yMax = 1.5, showLegend = F)
femaleShamConsGraph <- consumptionGraphPre(dataSet = conFemaleShamMelt, graphTitle = "Female - Intact", yMax = 1.5, showLegend = F)
femaleGdxConsGraph <- consumptionGraphPre(dataSet = conFemaleGdxMelt, graphTitle = "Female - GDX+VEH", yMax = 1.5, showLegend = F)
femaleRepConsGraph <- consumptionGraphPre(dataSet = conFemaleRepMelt, graphTitle = "Female - GDX+EB", yMax = 1.5, showLegend = F)

conLegend <- getLegend(maleShamConsGraph)
maleShamConsGraph <- maleShamConsGraph + theme(legend.position = "none")

pdf(paste("output/PostPuberty/postpubConsumption.pdf"), 24, 18)
grid.arrange(maleShamConsGraph,
             maleGdxConsGraph,
             maleRepConsGraph,
             femaleShamConsGraph,
             femaleGdxConsGraph,
             femaleRepConsGraph,
             conLegend,
             ncol = 3, nrow = 3, widths = c(8, 8, 8))
dev.off()

# Flavour Rec

pdf(paste("output/PostPuberty/flav-rec.pdf"), 8, 4)
flavRecGraph(data = flavRecGraphing, "IR", graphTitle = "")
dev.off()

# summary for elena
malePost <- subset(postStfp, Sex == "M")
femalePost <- subset(postStfp, Sex == "F")

summaryPanel(malePost, "flavAPref2H", "Turmeric Preferece Ratio", "Male", 1, "maleSummary.pdf")
summaryPanel(femalePost, "flavAPref2H", "Turmeric Preferece Ratio", "Female", 1, "femaleSummary.pdf")




# Sample size

nrow(subset(subset(malePreC, Time == "flavAPref2H"), DemFood == "CIN"))
nrow(subset(subset(malePreC, Time == "flavAPref2H"), DemFood == "COC"))
nrow(subset(subset(femalePreC, Time == "flavAPref2H"), DemFood == "CIN"))
nrow(subset(subset(femalePreC, Time == "flavAPref2H"), DemFood == "COC"))
nrow(subset(subset(malePreD, Time == "flavAPref2H"), DemFood == "CIN"))
nrow(subset(subset(malePreD, Time == "flavAPref2H"), DemFood == "COC"))
nrow(subset(subset(femalePreD, Time == "flavAPref2H"), DemFood == "CIN"))
nrow(subset(subset(femalePreD, Time == "flavAPref2H"), DemFood == "COC"))


nrow(subset(subset(femaleShamC, Time == "flavAPref2H"), DemFood == "TUM"))
nrow(subset(subset(femaleShamC, Time == "flavAPref2H"), DemFood == "THY"))
nrow(subset(subset(femaleGdxC, Time == "flavAPref2H"), DemFood == "TUM"))
nrow(subset(subset(femaleGdxC, Time == "flavAPref2H"), DemFood == "THY"))
nrow(subset(subset(femaleRepC, Time == "flavAPref2H"), DemFood == "TUM"))
nrow(subset(subset(femaleRepC, Time == "flavAPref2H"), DemFood == "THY"))

nrow(subset(subset(femaleShamD, Time == "flavAPref2H"), DemFood == "TUM"))
nrow(subset(subset(femaleShamD, Time == "flavAPref2H"), DemFood == "THY"))
nrow(subset(subset(femaleGdxD, Time == "flavAPref2H"), DemFood == "TUM"))
nrow(subset(subset(femaleGdxD, Time == "flavAPref2H"), DemFood == "THY"))
nrow(subset(subset(femaleRepD, Time == "flavAPref2H"), DemFood == "TUM"))
nrow(subset(subset(femaleRepD, Time == "flavAPref2H"), DemFood == "THY"))

nrow(subset(subset(maleShamC, Time == "flavAPref2H"), DemFood == "TUM"))
nrow(subset(subset(maleShamC, Time == "flavAPref2H"), DemFood == "THY"))
nrow(subset(subset(maleGdxC, Time == "flavAPref2H"), DemFood == "TUM"))
nrow(subset(subset(maleGdxC, Time == "flavAPref2H"), DemFood == "THY"))
nrow(subset(subset(maleRepC, Time == "flavAPref2H"), DemFood == "TUM"))
nrow(subset(subset(maleRepC, Time == "flavAPref2H"), DemFood == "THY"))

nrow(subset(subset(maleShamD, Time == "flavAPref2H"), DemFood == "TUM"))
nrow(subset(subset(maleShamD, Time == "flavAPref2H"), DemFood == "THY"))
nrow(subset(subset(maleGdxD, Time == "flavAPref2H"), DemFood == "TUM"))
nrow(subset(subset(maleGdxD, Time == "flavAPref2H"), DemFood == "THY"))
nrow(subset(subset(maleRepD, Time == "flavAPref2H"), DemFood == "TUM"))
nrow(subset(subset(maleRepD, Time == "flavAPref2H"), DemFood == "THY"))

