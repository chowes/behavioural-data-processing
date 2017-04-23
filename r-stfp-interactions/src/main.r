library(ggplot2)
library(Hmisc)
library(grid)
library(ez)
library(car)
library(phia)
library(lsmeans)
library(gridExtra)
library(coin)
library(lsr)

source("src/data.r")

stfpInteractionData <- read.csv("data/stfpInt.csv")
stfpInteractionData <- createComposites(stfpInteractionData)

### Transform non-normal measures

stfpInteractionData$TransAgonisticTotalDuration <- lnTransform(stfpInteractionData$AgonisticTotalDuration)
stfpInteractionData$TransAgonisticTotalOccur <- lnTransform(stfpInteractionData$AgonisticTotalOccur)

stfpInteractionData$TransAgonisticDeliveredDuration <- lnTransform(stfpInteractionData$AgonisticDeliveredDuration)
stfpInteractionData$TransAgonisticDeliveredOccur <- lnTransform(stfpInteractionData$AgonisticDeliveredOccur)

stfpInteractionData$TransAgonisticReceivedDuration <- lnTransform(stfpInteractionData$AgonisticReceivedDuration)
stfpInteractionData$TransAgonisticReceivedOccur <- lnTransform(stfpInteractionData$AgonisticReceivedOccur)

stfpInteractionData$TransNonSocialDuration <- lnTransform(stfpInteractionData$NonSocialDuration)
stfpInteractionData$TransNonSocialOccur <- lnTransform(stfpInteractionData$NonSocialOccur)

stfpInteractionData$TransInactiveNonSocialDuration <- lnTransform(stfpInteractionData$InactiveNonSocialDuration)
stfpInteractionData$TransInactiveNonSocialOccur <- lnTransform(stfpInteractionData$InactiveNonSocialOccur)

stfpInteractionData$TransFollowDur <- lnTransform(stfpInteractionData$FollowDur)
stfpInteractionData$TransFollowEv <- lnTransform(stfpInteractionData$FollowEv)

stfpInteractionData$TransDomEv <- lnTransform(stfpInteractionData$DomEv)

stfpInteractionData$TransAttackDel <- lnTransform(stfpInteractionData$AttackDelEv)
stfpInteractionData$TransAttackRec <- lnTransform(stfpInteractionData$AttackRecEv)

stfpInteractionData$TransAvoidDur <- lnTransform(stfpInteractionData$AvoidDur)
stfpInteractionData$TransAvoidEv <- lnTransform(stfpInteractionData$AvoidEv)

stfpInteractionData$TransSubDur <- lnTransform(stfpInteractionData$SubDur)
stfpInteractionData$TransSubEv <- lnTransform(stfpInteractionData$SubEv)

stfpInteractionData$TransRitAggDur <- lnTransform(stfpInteractionData$RitAggDur)
stfpInteractionData$TransRitAggEv <- lnTransform(stfpInteractionData$RitAggEv)

stfpInteractionData$TransDefenseDur <- lnTransform(stfpInteractionData$DefenseDur)
stfpInteractionData$TransDefenseEv <- lnTransform(stfpInteractionData$DefenseEv)

stfpInteractionData$TransInactiveTogetherDur <- lnTransform(stfpInteractionData$InactiveTogetherDur)
stfpInteractionData$TransInactiveTogetherEv <- lnTransform(stfpInteractionData$InactiveTogetherEv)

stfpInteractionData$TransSniffHeadEv <- lnTransform(stfpInteractionData$SniffHeadEv)

stfpInteractionData$TransStretchEv <- lnTransform(stfpInteractionData$StretchEv)

stfpInteractionData$TransDigDur <- lnTransform(stfpInteractionData$DigDur)
stfpInteractionData$TransDigEv <- lnTransform(stfpInteractionData$DigEv)

stfpInteractionData$TransInactiveDur <- lnTransform(stfpInteractionData$InactiveDur)
stfpInteractionData$TransInactiveEv <- lnTransform(stfpInteractionData$InactiveEv)


### Subsets

stfpInteractionPre <- subset(stfpInteractionData, stfpInteractionData$Gonad == "PRE")
stfpInteractionPost <- subset(stfpInteractionData, stfpInteractionData$Gonad != "PRE")

stfpInteractionPost <- createFactors(stfpInteractionPost)

malePre <- subset(stfpInteractionPre, Sex == "M")
femalePre <- subset(stfpInteractionPre, Sex == "F")
controlPre <- subset(stfpInteractionPre, Treatment == "C")
testPre <- subset(stfpInteractionPre, Treatment == "T")

maleCSham <- subset(stfpInteractionPost, Sex == "M" & Treatment == "C" & Gonad == "SHAM")
maleCGdx <- subset(stfpInteractionPost, Sex == "M" & Treatment == "C" & Gonad == "GDX")
maleCRep <- subset(stfpInteractionPost, Sex == "M" & Treatment == "C" & Gonad == "REP")
femaleCSham <- subset(stfpInteractionPost, Sex == "F" & Treatment == "C" & Gonad == "SHAM")
femaleCGdx <- subset(stfpInteractionPost, Sex == "F" & Treatment == "C" & Gonad == "GDX")
femaleCRep <- subset(stfpInteractionPost, Sex == "F" & Treatment == "C" & Gonad == "REP")
maleTSham <- subset(stfpInteractionPost, Sex == "M" & Treatment == "T" & Gonad == "SHAM")
maleTGdx <- subset(stfpInteractionPost, Sex == "M" & Treatment == "T" & Gonad == "GDX")
maleTRep <- subset(stfpInteractionPost, Sex == "M" & Treatment == "T" & Gonad == "REP")
femaleTSham <- subset(stfpInteractionPost, Sex == "F" & Treatment == "T" & Gonad == "SHAM")
femaleTGdx <- subset(stfpInteractionPost, Sex == "F" & Treatment == "T" & Gonad == "GDX")
femaleTRep <- subset(stfpInteractionPost, Sex == "F" & Treatment == "T" & Gonad == "REP")

write.csv(stfpInteractionPost, file = "stfpInteractionPost.csv", na = "-999")
write.csv(stfpInteractionPre, file = "stfpInteractionPre.csv", na = "-999")

##################################
# Analysis
##################################

### Pre-Puberty

## Composite Measures
# Total Activity - Duration
# Main Model
preTransAgonisticTotalOccur <- lm(TransAgonisticTotalOccur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preTransAgonisticTotalOccur)

# Planned Comparisons - Treatment
wilcox.test(TransAgonisticTotalOccur ~ Treatment, data = malePre)
cohensD(TransAgonisticTotalOccur ~ Treatment, data = malePre)
wilcox.test(TransAgonisticTotalOccur ~ Treatment, data = femalePre)
cohensD(TransAgonisticTotalOccur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransAgonisticTotalOccur ~ Sex, data = controlPre)
cohensD(TransAgonisticTotalOccur ~ Sex, data = controlPre)
wilcox.test(TransAgonisticTotalOccur ~ Sex, data = testPre)
cohensD(TransAgonisticTotalOccur ~ Sex, data = testPre)

# Total Activity - Freq.
# Main Model
preTotalActivityFrequency <- lm(TotalActivityOccur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preTotalActivityFrequency)

# Planned Comparisons - Treatment
wilcox.test(TotalActivityOccur ~ Treatment, data = malePre)
cohensD(TotalActivityOccur ~ Treatment, data = malePre)
wilcox.test(TotalActivityOccur ~ Treatment, data = femalePre)
cohensD(TotalActivityOccur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TotalActivityOccur ~ Sex, data = controlPre)
cohensD(TotalActivityOccur ~ Sex, data = controlPre)
wilcox.test(TotalActivityOccur ~ Sex, data = testPre)
cohensD(TotalActivityOccur ~ Sex, data = testPre)


# Total Social - Duration
# Main Model
preTotalSocial <- lm(TotalSocialDuration ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preTotalSocial)

# Planned Comparisons - Treatment
wilcox.test(TotalSocialDuration ~ Treatment, data = malePre)
cohensD(TotalSocialDuration ~ Treatment, data = malePre)
wilcox.test(TotalSocialDuration ~ Treatment, data = femalePre)
cohensD(TotalSocialDuration ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TotalSocialDuration ~ Sex, data = controlPre)
cohensD(TotalSocialDuration ~ Sex, data = controlPre)
wilcox.test(TotalSocialDuration ~ Sex, data = testPre)
cohensD(TotalSocialDuration ~ Sex, data = testPre)

# Total Activity - Freq.
# Main Model
preTotalSocialFrequency <- lm(TotalSocialOccur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preTotalSocialFrequency)

# Planned Comparisons - Treatment
wilcox.test(TotalSocialOccur ~ Treatment, data = malePre)
cohensD(TotalSocialOccur ~ Treatment, data = malePre)
wilcox.test(TotalSocialOccur ~ Treatment, data = femalePre)
cohensD(TotalSocialOccur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TotalSocialOccur ~ Sex, data = controlPre)
cohensD(TotalSocialOccur ~ Sex, data = controlPre)
wilcox.test(TotalSocialOccur ~ Sex, data = testPre)
cohensD(TotalSocialOccur ~ Sex, data = testPre)


# Total Agonistic - Duration
# Main Model
preAgonisticTotalDuration <- lm(TransAgonisticTotalDuration ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preAgonisticTotalDuration)

# Planned Comparisons - Treatment
wilcox.test(TransAgonisticTotalDuration ~ Treatment, data = malePre)
wilcox.test(TransAgonisticTotalDuration ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransAgonisticTotalDuration ~ Sex, data = controlPre)
wilcox.test(TransAgonisticTotalDuration ~ Sex, data = testPre)


# Total Agonistic - Freq.
# Main Model
preAgonisticTotalFreq <- lm(TransAgonisticTotalOccur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preAgonisticTotalFreq)

# Planned Comparisons - Treatment
wilcox.test(TransAgonisticTotalOccur ~ Treatment, data = malePre)
wilcox.test(TransAgonisticTotalOccur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransAgonisticTotalOccur ~ Sex, data = controlPre)
wTest <- wilcox_test(TransAgonisticTotalOccur ~ Sex, data = controlPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(controlPre))
wilcox.test(TransAgonisticTotalOccur ~ Sex, data = testPre)
wTest <- wilcox_test(TransAgonisticTotalOccur ~ Sex, data = testPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testPre))


# Agonistic Delivered - Duration
# Main Model
preAgonisticDeliveredDuration <- lm(TransAgonisticDeliveredDuration ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preAgonisticDeliveredDuration)

# Planned Comparisons - Treatment
wilcox.test(TransAgonisticDeliveredDuration ~ Treatment, data = malePre)
wilcox.test(TransAgonisticDeliveredDuration ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransAgonisticDeliveredDuration ~ Sex, data = controlPre)
wilcox.test(TransAgonisticDeliveredDuration ~ Sex, data = testPre)


# Agonistic Delivered - Freq.
# Main Model
preAgonisticDeliveredOccur <- lm(TransAgonisticDeliveredOccur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preAgonisticDeliveredOccur)

# Planned Comparisons - Treatment
wilcox.test(TransAgonisticDeliveredOccur ~ Treatment, data = malePre)
wilcox.test(TransAgonisticDeliveredOccur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransAgonisticDeliveredOccur ~ Sex, data = controlPre)
wTest <- wilcox_test(TransAgonisticDeliveredOccur ~ Sex, data = controlPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(controlPre))
wilcox.test(TransAgonisticDeliveredOccur ~ Sex, data = testPre)
wTest <- wilcox_test(TransAgonisticDeliveredOccur ~ Sex, data = testPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testPre))


# Agonistic Received - Duration
# Main Model
preAgonisticReceivedDuration <- lm(TransAgonisticReceivedDuration ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preAgonisticReceivedDuration)

# Planned Comparisons - Treatment
wilcox.test(TransAgonisticReceivedDuration ~ Treatment, data = malePre)
wilcox.test(TransAgonisticReceivedDuration ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransAgonisticReceivedDuration ~ Sex, data = controlPre)
wilcox.test(TransAgonisticReceivedDuration ~ Sex, data = testPre)
wTest <- wilcox_test(TransAgonisticReceivedDuration ~ Sex, data = testPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testPre))

# Agonistic Received - Freq.
# Main Model
preAgonisticReceivedOccur <- lm(TransAgonisticReceivedOccur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preAgonisticReceivedOccur)

# Planned Comparisons - Treatment
wilcox.test(TransAgonisticReceivedOccur ~ Treatment, data = malePre)
wilcox.test(TransAgonisticReceivedOccur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransAgonisticReceivedOccur ~ Sex, data = controlPre)
wilcox.test(TransAgonisticReceivedOccur ~ Sex, data = testPre)


# Dominance Score - Duration
# Main Model
preDomScoreDuration <- lm(DominanceScoreDuration ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preDomScoreDuration)

# Planned Comparisons - Treatment
wilcox.test(DominanceScoreDuration ~ Treatment, data = malePre)
wilcox.test(DominanceScoreDuration ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(DominanceScoreDuration ~ Sex, data = controlPre)
wilcox.test(DominanceScoreDuration ~ Sex, data = testPre)
wTest <- wilcox_test(DominanceScoreDuration ~ Sex, data = testPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testPre))

# Dominance Score - Freq.
# Main Model
preDomScoreOccur <- lm(DominanceScoreOccur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preDomScoreOccur)

# Planned Comparisons - Treatment
wilcox.test(DominanceScoreOccur ~ Treatment, data = malePre)
wilcox.test(DominanceScoreOccur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(DominanceScoreOccur ~ Sex, data = controlPre)
wTest <- wilcox_test(DominanceScoreOccur ~ Sex, data = controlPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(controlPre))
wilcox.test(DominanceScoreOccur ~ Sex, data = testPre)


# Social Investigation - Duration
# Main Model
preSocialInvestigationDur <- lm(SocialInvestigationDuration ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preSocialInvestigationDur)

# Planned Comparisons - Treatment
wilcox.test(SocialInvestigationDuration ~ Treatment, data = malePre)
wilcox.test(SocialInvestigationDuration ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(SocialInvestigationDuration ~ Sex, data = controlPre)
wilcox.test(SocialInvestigationDuration ~ Sex, data = testPre)
cohensD(SocialInvestigationDuration ~ Sex, data = testPre)

# Social Investigation - Freq.
# Main Model
preSocialInvestigationOccur <- lm(SocialInvestigationOccur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preSocialInvestigationOccur)

# Planned Comparisons - Treatment
wilcox.test(SocialInvestigationOccur ~ Treatment, data = malePre)
wilcox.test(SocialInvestigationOccur ~ Treatment, data = femalePre)
cohensD(SocialInvestigationOccur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(SocialInvestigationOccur ~ Sex, data = controlPre)
wilcox.test(SocialInvestigationOccur ~ Sex, data = testPre)
cohensD(SocialInvestigationOccur ~ Sex, data = testPre)


# Social Investigation - Duration
# Main Model
preSocialInvestigationDur <- lm(SocialInvestigationDuration ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preSocialInvestigationDur)

# Planned Comparisons - Treatment
wilcox.test(SocialInvestigationDuration ~ Treatment, data = malePre)
wilcox.test(SocialInvestigationDuration ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(SocialInvestigationDuration ~ Sex, data = controlPre)
wilcox.test(SocialInvestigationDuration ~ Sex, data = testPre)
cohensD(SocialInvestigationDuration ~ Sex, data = testPre)

# Social Investigation - Freq.
# Main Model
preSocialInvestigationOccur <- lm(SocialInvestigationOccur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preSocialInvestigationOccur)

# Planned Comparisons - Treatment
wilcox.test(SocialInvestigationOccur ~ Treatment, data = malePre)
wilcox.test(SocialInvestigationOccur ~ Treatment, data = femalePre)
cohensD(SocialInvestigationOccur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(SocialInvestigationOccur ~ Sex, data = controlPre)
wilcox.test(SocialInvestigationOccur ~ Sex, data = testPre)
cohensD(SocialInvestigationOccur ~ Sex, data = testPre)


# Non-Social - Duration
# Main Model
preNonSocialDur <- lm(NonSocialDuration ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preNonSocialDur)

# Planned Comparisons - Treatment
wilcox.test(NonSocialDuration ~ Treatment, data = malePre)
cohensD(NonSocialDuration ~ Treatment, data = malePre)
wilcox.test(NonSocialDuration ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(NonSocialDuration ~ Sex, data = controlPre)
wilcox.test(NonSocialDuration ~ Sex, data = testPre)

# Non-Social - Freq.
# Main Model
preNonSocialOccur <- lm(NonSocialOccur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preNonSocialOccur)

# Planned Comparisons - Treatment
wilcox.test(NonSocialOccur ~ Treatment, data = malePre)
wilcox.test(NonSocialOccur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(NonSocialOccur ~ Sex, data = controlPre)
wilcox.test(NonSocialOccur ~ Sex, data = testPre)


# Active Non-Social - Duration
# Main Model
preActiveNonSocialDur <- lm(NonSocialLocomotorDuration ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preActiveNonSocialDur)

# Planned Comparisons - Treatment
wilcox.test(NonSocialLocomotorDuration ~ Treatment, data = malePre)
wilcox.test(NonSocialLocomotorDuration ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(NonSocialLocomotorDuration ~ Sex, data = controlPre)
wilcox.test(NonSocialLocomotorDuration ~ Sex, data = testPre)

# Active Non-Social - Freq.
# Main Model
preNonSocialOccur <- lm(NonSocialOccur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preNonSocialOccur)

# Planned Comparisons - Treatment
wilcox.test(NonSocialOccur ~ Treatment, data = malePre)
wilcox.test(NonSocialOccur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(NonSocialOccur ~ Sex, data = controlPre)
wilcox.test(NonSocialOccur ~ Sex, data = testPre)


# Inactive Non-Social - Duration
# Main Model
preInactiveNonSocialDur <- lm(TransInactiveNonSocialDuration ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preInactiveNonSocialDur)

# Planned Comparisons - Treatment
wilcox.test(TransInactiveNonSocialDuration ~ Treatment, data = malePre)
wilcox.test(TransInactiveNonSocialDuration ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransInactiveNonSocialDuration ~ Sex, data = controlPre)
wilcox.test(TransInactiveNonSocialDuration ~ Sex, data = testPre)

# Inactive Non-Social - Freq.
# Main Model
preInactiveNonSocialOccur <- lm(TransInactiveNonSocialOccur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preInactiveNonSocialOccur)

# Planned Comparisons - Treatment
wilcox.test(TransInactiveNonSocialOccur ~ Treatment, data = malePre)
wilcox.test(TransInactiveNonSocialOccur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransInactiveNonSocialOccur ~ Sex, data = controlPre)
wilcox.test(TransInactiveNonSocialOccur ~ Sex, data = testPre)


## Single Behaviours
# Follow - Duration
# Main Model
preFollowDuration <- lm(FollowDur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preFollowDuration)

# Planned Comparisons - Treatment
wilcox.test(FollowDur ~ Treatment, data = malePre)
wilcox.test(FollowDur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(FollowDur ~ Sex, data = controlPre)
wTest <- wilcox_test(FollowDur ~ Sex, data = controlPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(controlPre))
wilcox.test(FollowDur ~ Sex, data = testPre)

# Follow - Freq.
# Main Model
preFollowFreq <- lm(TransFollowEv ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preFollowFreq)

# Planned Comparisons - Treatment
wilcox.test(FollowEv ~ Treatment, data = malePre)
wilcox.test(FollowEv ~ Treatment, data = femalePre)
wTest <- wilcox_test(FollowEv ~ Treatment, data = femalePre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(femalePre))
# Planned Comparisons - Sex
wilcox.test(FollowEv ~ Sex, data = controlPre)
wTest <- wilcox_test(FollowEv ~ Sex, data = controlPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(controlPre))
wilcox.test(FollowDur ~ Sex, data = testPre)


# Dominant - Duration
# Main Model
preDominantDuration <- lm(DomDur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preDominantDuration)

# Planned Comparisons - Treatment
wilcox.test(DomDur ~ Treatment, data = malePre)
wilcox.test(DomDur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(DomDur ~ Sex, data = controlPre)
wilcox.test(DomDur ~ Sex, data = testPre)

# Dominant - Freq.
# Main Model
preDominantFreq <- lm(TransDomEv ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preDominantFreq)

# Planned Comparisons - Treatment
wilcox.test(TransDomEv ~ Treatment, data = malePre)
wilcox.test(TransDomEv ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransDomEv ~ Sex, data = controlPre)
wilcox.test(TransDomEv ~ Sex, data = testPre)


# Attack Del - Freq.
# Main Model
preAttackDelFreq <- lm(TransAttackDel ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preAttackDelFreq)

# Planned Comparisons - Treatment
wilcox.test(TransAttackDel ~ Treatment, data = malePre)
wilcox.test(TransAttackDel ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransAttackDel ~ Sex, data = controlPre)
wTest <- wilcox_test(TransAttackDel ~ Sex, data = controlPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(controlPre))
wilcox.test(TransAttackDel ~ Sex, data = testPre)
wTest <- wilcox_test(TransAttackDel ~ Sex, data = testPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testPre))

# Attack Rec - Freq.
# Main Model
preAttackRecFreq <- lm(TransAttackRec ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preAttackDelFreq)

# Planned Comparisons - Treatment
wilcox.test(TransAttackRec ~ Treatment, data = malePre)
wilcox.test(TransAttackRec ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransAttackRec ~ Sex, data = controlPre)
wTest <- wilcox_test(TransAttackRec ~ Sex, data = controlPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(controlPre))
wilcox.test(TransAttackRec ~ Sex, data = testPre)
wTest <- wilcox_test(TransAttackRec ~ Sex, data = testPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testPre))


# Open Aggression - Duration
# Main Model
preOpenAggDuration <- lm(OpenAggDur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preOpenAggDuration)

# Planned Comparisons - Treatment
wilcox.test(OpenAggDur ~ Treatment, data = malePre)
wilcox.test(OpenAggDur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(OpenAggDur ~ Sex, data = controlPre)
wTest <- wilcox_test(OpenAggDur ~ Sex, data = controlPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(controlPre))
wilcox.test(OpenAggDur ~ Sex, data = testPre)
wTest <- wilcox_test(OpenAggDur ~ Sex, data = testPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testPre))

# Open Aggression - Freq
# Main Model
preOpenAggFreq <- lm(OpenAggEv ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preOpenAggFreq)

# Planned Comparisons - Treatment
wilcox.test(OpenAggEv ~ Treatment, data = malePre)
wilcox.test(OpenAggEv ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(OpenAggEv ~ Sex, data = controlPre)
wTest <- wilcox_test(OpenAggEv ~ Sex, data = controlPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(controlPre))
wilcox.test(OpenAggEv ~ Sex, data = testPre)
wTest <- wilcox_test(OpenAggEv ~ Sex, data = testPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testPre))


# Ritualized Aggression - Dur
# Main Model
preRitualizedAggDur <- lm(TransRitAggDur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preRitualizedAggDur)

# Planned Comparisons - Treatment
wilcox.test(TransRitAggDur ~ Treatment, data = malePre)
wTest <- wilcox_test(TransRitAggDur ~ Treatment, data = malePre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(malePre))
wilcox.test(TransRitAggDur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransRitAggDur ~ Sex, data = controlPre)
wTest <- wilcox_test(TransRitAggDur ~ Sex, data = controlPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(controlPre))
wilcox.test(TransRitAggDur ~ Sex, data = testPre)
wTest <- wilcox_test(TransRitAggDur ~ Sex, data = testPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testPre))

# Ritualized Aggression - Freq
# Main Model
preRitualizedAggFreq <- lm(TransRitAggEv ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preRitualizedAggFreq)

# Planned Comparisons - Treatment
wilcox.test(TransRitAggEv ~ Treatment, data = malePre)
wTest <- wilcox_test(TransRitAggEv ~ Treatment, data = malePre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(malePre))
wilcox.test(TransRitAggEv ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransRitAggEv ~ Sex, data = controlPre)
wTest <- wilcox_test(TransRitAggEv ~ Sex, data = controlPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(controlPre))
wilcox.test(TransRitAggEv ~ Sex, data = testPre)
wTest <- wilcox_test(TransRitAggEv ~ Sex, data = testPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testPre))


# Submissive - Dur
# Main Model
preSubmissiveDur <- lm(TransSubDur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preSubmissiveDur)

# Planned Comparisons - Treatment
wilcox.test(TransSubDur ~ Treatment, data = malePre)
wilcox.test(TransSubDur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransSubDur ~ Sex, data = controlPre)
wilcox.test(TransSubDur ~ Sex, data = testPre)
wTest <- wilcox_test(TransSubDur ~ Sex, data = testPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testPre))

# Submissive - Freq
# Main Model
preSubmissiveFreq <- lm(TransSubEv ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preSubmissiveFreq)

# Planned Comparisons - Treatment
wilcox.test(TransSubEv ~ Treatment, data = malePre)
wilcox.test(TransSubEv ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransSubEv ~ Sex, data = controlPre)
wilcox.test(TransSubEv ~ Sex, data = testPre)
wTest <- wilcox_test(TransSubEv ~ Sex, data = testPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testPre))


# Defensive - Dur
# Main Model
preDefenseDur <- lm(TransDefenseDur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preDefenseDur)

# Planned Comparisons - Treatment
wilcox.test(TransDefenseDur ~ Treatment, data = malePre)
wilcox.test(TransDefenseDur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransDefenseDur ~ Sex, data = controlPre)
wTest <- wilcox_test(TransDefenseDur ~ Sex, data = controlPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(controlPre))
wilcox.test(TransDefenseDur ~ Sex, data = testPre)
wTest <- wilcox_test(TransDefenseDur ~ Sex, data = testPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testPre))

# Defensive - Freq
# Main Model
preDefensiveFreq <- lm(TransDefenseEv ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preDefensiveFreq)

# Planned Comparisons - Treatment
wilcox.test(TransDefenseEv ~ Treatment, data = malePre)
wilcox.test(TransDefenseEv ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransDefenseEv ~ Sex, data = controlPre)
wTest <- wilcox_test(TransDefenseEv ~ Sex, data = controlPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(controlPre))
wilcox.test(TransDefenseEv ~ Sex, data = testPre)
wTest <- wilcox_test(TransDefenseEv ~ Sex, data = testPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testPre))


# Inactive Together - Dur
# Main Model
preInactiveTogetherDur <- lm(TransInactiveTogetherDur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preInactiveTogetherDur)

# Planned Comparisons - Treatment
wilcox.test(TransInactiveTogetherDur ~ Treatment, data = malePre)
wilcox.test(TransInactiveTogetherDur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransInactiveTogetherDur ~ Sex, data = controlPre)
wilcox.test(TransInactiveTogetherDur ~ Sex, data = testPre)

# Inactive Together - Freq
# Main Model
preInactiveTogetherFreq <- lm(TransInactiveTogetherEv ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preInactiveTogetherFreq)

# Planned Comparisons - Treatment
wilcox.test(TransInactiveTogetherEv ~ Treatment, data = malePre)
wilcox.test(TransInactiveTogetherEv ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransInactiveTogetherEv ~ Sex, data = controlPre)
wilcox.test(TransInactiveTogetherEv ~ Sex, data = testPre)


# Sniff Head - Dur
# Main Model
preSniffHeadDur <- lm(SniffHeadDur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preSniffHeadDur)

# Planned Comparisons - Treatment
wilcox.test(SniffHeadDur ~ Treatment, data = malePre)
wilcox.test(SniffHeadDur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(SniffHeadDur ~ Sex, data = controlPre)
wilcox.test(SniffHeadDur ~ Sex, data = testPre)

# Sniff Head - Freq
# Main Model
preSniffHeadFreq <- lm(TransSniffHeadEv ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preSniffHeadFreq)

# Planned Comparisons - Treatment
wilcox.test(TransSniffHeadEv ~ Treatment, data = malePre)
wilcox.test(TransSniffHeadEv ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransSniffHeadEv ~ Sex, data = controlPre)
wilcox.test(TransSniffHeadEv ~ Sex, data = testPre)


# Sniff Head - Dur
# Main Model
preSniffBodyDur <- lm(SniffBodyDur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preSniffBodyDur)

# Planned Comparisons - Treatment
wilcox.test(SniffBodyDur ~ Treatment, data = malePre)
wilcox.test(SniffBodyDur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(SniffBodyDur ~ Sex, data = controlPre)
cohensD(SniffBodyDur ~ Sex, data = controlPre)
wilcox.test(SniffBodyDur ~ Sex, data = testPre)
cohensD(SniffBodyDur ~ Sex, data = testPre)

# Sniff Body - Freq
# Main Model
preSniffBodyFreq <- lm(SniffBodyEv ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preSniffHeadFreq)

# Planned Comparisons - Treatment
wilcox.test(SniffBodyEv ~ Treatment, data = malePre)
wilcox.test(SniffBodyEv ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(SniffBodyEv ~ Sex, data = controlPre)
wilcox.test(SniffBodyEv ~ Sex, data = testPre)
wTest <- wilcox_test(SniffBodyEv ~ Sex, data = testPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testPre))


# Sniff Anogenital - Dur
# Main Model
preSniffAnogenitalDur <- lm(SniffAnoDur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preSniffAnogenitalDur)

# Planned Comparisons - Treatment
wilcox.test(SniffAnoDur ~ Treatment, data = malePre)
wilcox.test(SniffAnoDur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(SniffAnoDur ~ Sex, data = controlPre)
wilcox.test(SniffAnoDur ~ Sex, data = testPre)
wTest <- wilcox_test(SniffAnoDur ~ Sex, data = testPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testPre))

# Sniff Anogenital - Freq
# Main Model
preSniffAnogenitalFreq <- lm(SniffAnoEv ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preSniffAnogenitalFreq)

# Planned Comparisons - Treatment
wilcox.test(SniffAnoEv ~ Treatment, data = malePre)
wilcox.test(SniffAnoEv ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(SniffAnoEv ~ Sex, data = controlPre)
wilcox.test(SniffAnoEv ~ Sex, data = testPre)
wTest <- wilcox_test(SniffAnoEv ~ Sex, data = testPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testPre))


# Stretch - Freq
# Main Model
preStretchFreq <- lm(TransStretchEv ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preStretchFreq)

# Planned Comparisons - Treatment
wilcox.test(TransStretchEv ~ Treatment, data = malePre)
wilcox.test(TransStretchEv ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransStretchEv ~ Sex, data = controlPre)
wilcox.test(TransStretchEv ~ Sex, data = testPre)


# Hor Activity - Dur
# Main Model
preHorAct <- lm(ActiveHDur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preHorAct)

# Planned Comparisons - Treatment
wilcox.test(ActiveHDur ~ Treatment, data = malePre)
wilcox.test(ActiveHDur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(ActiveHDur ~ Sex, data = controlPre)
wilcox.test(ActiveHDur ~ Sex, data = testPre)

# Sniff Anogenital - Freq
# Main Model
preHorActFreq <- lm(ActiveHEv ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preHorActFreq)

# Planned Comparisons - Treatment
wilcox.test(ActiveHEv ~ Treatment, data = malePre)
wilcox.test(ActiveHEv ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(ActiveHEv ~ Sex, data = controlPre)
wilcox.test(ActiveHEv ~ Sex, data = testPre)


# Vert Activity - Dur
# Main Model
preVertActDur <- lm(ActiveVDur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preVertActDur)

# Planned Comparisons - Treatment
wilcox.test(ActiveVDur ~ Treatment, data = malePre)
wilcox.test(ActiveVDur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(ActiveVDur ~ Sex, data = controlPre)
wilcox.test(ActiveVDur ~ Sex, data = testPre)

# Sniff Anogenital - Freq
# Main Model
preVertActFreq <- lm(ActiveVEv ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preVertActFreq)

# Planned Comparisons - Treatment
wilcox.test(ActiveVEv ~ Treatment, data = malePre)
wilcox.test(ActiveVEv ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(ActiveVEv ~ Sex, data = controlPre)
wilcox.test(ActiveVEv ~ Sex, data = testPre)


# Inactive - Dur
# Main Model
preInactiveDur <- lm(TransInactiveDur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preInactiveDur)

# Planned Comparisons - Treatment
wilcox.test(TransInactiveDur ~ Treatment, data = malePre)
wilcox.test(TransInactiveDur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransInactiveDur ~ Sex, data = controlPre)
wilcox.test(TransInactiveDur ~ Sex, data = testPre)

# Inactive - Freq
# Main Model
preInactiveFreq <- lm(TransInactiveEv ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preInactiveFreq)

# Planned Comparisons - Treatment
wilcox.test(TransInactiveEv ~ Treatment, data = malePre)
wilcox.test(TransInactiveEv ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransInactiveEv ~ Sex, data = controlPre)
wilcox.test(TransInactiveEv ~ Sex, data = testPre)


# Groom - Dur
# Main Model
preGroomDur <- lm(GroomDur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preGroomDur)

# Planned Comparisons - Treatment
wilcox.test(GroomDur ~ Treatment, data = malePre)
wilcox.test(GroomDur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(GroomDur ~ Sex, data = controlPre)
wilcox.test(GroomDur ~ Sex, data = testPre)

# Inactive - Freq
# Main Model
preGroomFreq <- lm(GroomEv ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preGroomFreq)

# Planned Comparisons - Treatment
wilcox.test(GroomEv ~ Treatment, data = malePre)
wilcox.test(GroomEv ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(GroomEv ~ Sex, data = controlPre)
wilcox.test(GroomEv ~ Sex, data = testPre)


# Dig - Dur
# Main Model
preDigDur <- lm(TransDigDur ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preDigDur)

# Planned Comparisons - Treatment
wilcox.test(TransDigDur ~ Treatment, data = malePre)
wilcox.test(TransDigDur ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransDigDur ~ Sex, data = controlPre)
wilcox.test(TransDigDur ~ Sex, data = testPre)

# Inactive - Freq
# Main Model
preDigEv <- lm(TransDigEv ~ Treatment * Sex, data = stfpInteractionPre)
Anova(preDigEv)

# Planned Comparisons - Treatment
wilcox.test(TransDigEv ~ Treatment, data = malePre)
wilcox.test(TransDigEv ~ Treatment, data = femalePre)
# Planned Comparisons - Sex
wilcox.test(TransDigEv ~ Sex, data = controlPre)
wilcox.test(TransDigEv ~ Sex, data = testPre)


#############################

### Post-Puberty

## Composite Measures
# Total Activity - Duration
# Main Model
postTransAgonisticTotalOccur <- lm(TransAgonisticTotalOccur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postTransAgonisticTotalOccur)

# planned comparisons - gonad
wilcox.test(femaleCSham$TransAgonisticTotalOccur, femaleCGdx$TransAgonisticTotalOccur)
wilcox.test(femaleCGdx$TransAgonisticTotalOccur, femaleCRep$TransAgonisticTotalOccur)

wilcox.test(femaleTSham$TransAgonisticTotalOccur, femaleTGdx$TransAgonisticTotalOccur)
wilcox.test(femaleTGdx$TransAgonisticTotalOccur, femaleTRep$TransAgonisticTotalOccur)

wilcox.test(maleCSham$TransAgonisticTotalOccur, maleCGdx$TransAgonisticTotalOccur)
wilcox.test(maleCGdx$TransAgonisticTotalOccur, maleCRep$TransAgonisticTotalOccur)

wilcox.test(maleTSham$TransAgonisticTotalOccur, maleTGdx$TransAgonisticTotalOccur)
wilcox.test(maleTGdx$TransAgonisticTotalOccur, maleTRep$TransAgonisticTotalOccur)


# planned comparisons - male to female
wilcox.test(femaleCSham$TransAgonisticTotalOccur, maleCSham$TransAgonisticTotalOccur)
wilcox.test(femaleCRep$TransAgonisticTotalOccur, maleCRep$TransAgonisticTotalOccur)
wilcox.test(femaleCGdx$TransAgonisticTotalOccur, maleCGdx$TransAgonisticTotalOccur)

wilcox.test(femaleTSham$TransAgonisticTotalOccur, maleTSham$TransAgonisticTotalOccur)
wilcox.test(femaleTRep$TransAgonisticTotalOccur, maleTRep$TransAgonisticTotalOccur)
wilcox.test(femaleTGdx$TransAgonisticTotalOccur, maleTGdx$TransAgonisticTotalOccur)


# planned comparisons - test to control
wilcox.test(femaleCSham$TransAgonisticTotalOccur, femaleTSham$TransAgonisticTotalOccur)
wilcox.test(femaleCRep$TransAgonisticTotalOccur, femaleTRep$TransAgonisticTotalOccur)
wilcox.test(femaleCGdx$TransAgonisticTotalOccur, femaleTGdx$TransAgonisticTotalOccur)

wilcox.test(maleCSham$TransAgonisticTotalOccur, maleTSham$TransAgonisticTotalOccur)
wilcox.test(maleCRep$TransAgonisticTotalOccur, maleTRep$TransAgonisticTotalOccur)
wilcox.test(maleCGdx$TransAgonisticTotalOccur, maleTGdx$TransAgonisticTotalOccur)

# Total Activity - Freq.
# Main Model
postTotalActivityFrequency <- lm(TotalActivityOccur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postTotalActivityFrequency)

wilcox.test(femaleCSham$TotalActivityOccur, femaleCGdx$TotalActivityOccur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TotalActivityOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TotalActivityOccur, femaleCRep$TotalActivityOccur)

wilcox.test(femaleTSham$TotalActivityOccur, femaleTGdx$TotalActivityOccur)
wilcox.test(femaleTGdx$TotalActivityOccur, femaleTRep$TotalActivityOccur)

wilcox.test(maleCSham$TotalActivityOccur, maleCGdx$TotalActivityOccur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TotalActivityOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TotalActivityOccur, maleCRep$TotalActivityOccur)

wilcox.test(maleTSham$TotalActivityOccur, maleTGdx$TotalActivityOccur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TotalActivityOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TotalActivityOccur, maleTRep$TotalActivityOccur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TotalActivityOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# planned comparisons - male to female
wilcox.test(femaleCSham$TotalActivityOccur, maleCSham$TotalActivityOccur)
wilcox.test(femaleCRep$TotalActivityOccur, maleCRep$TotalActivityOccur)
wilcox.test(femaleCGdx$TotalActivityOccur, maleCGdx$TotalActivityOccur)

wilcox.test(femaleTSham$TotalActivityOccur, maleTSham$TotalActivityOccur)
wilcox.test(femaleTRep$TotalActivityOccur, maleTRep$TotalActivityOccur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TotalActivityOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TotalActivityOccur, maleTGdx$TotalActivityOccur)


# planned comparisons - test to control
wilcox.test(femaleCSham$TotalActivityOccur, femaleTSham$TotalActivityOccur)
wilcox.test(femaleCRep$TotalActivityOccur, femaleTRep$TotalActivityOccur)
wilcox.test(femaleCGdx$TotalActivityOccur, femaleTGdx$TotalActivityOccur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TotalActivityOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TotalActivityOccur, maleTSham$TotalActivityOccur)
wilcox.test(maleCRep$TotalActivityOccur, maleTRep$TotalActivityOccur)
wilcox.test(maleCGdx$TotalActivityOccur, maleTGdx$TotalActivityOccur)



# Total Social - Duration
# Main Model
postTotalSocial <- lm(TotalSocialDuration ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postTotalSocial)

# planned comparisons - gonad
wilcox.test(femaleCSham$TotalSocialDuration, femaleCGdx$TotalSocialDuration)
wilcox.test(femaleCGdx$TotalSocialDuration, femaleCRep$TotalSocialDuration)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TotalSocialDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TotalSocialDuration, femaleTGdx$TotalSocialDuration)
wilcox.test(femaleTGdx$TotalSocialDuration, femaleTRep$TotalSocialDuration)

wilcox.test(maleCSham$TotalSocialDuration, maleCGdx$TotalSocialDuration)
wilcox.test(maleCGdx$TotalSocialDuration, maleCRep$TotalSocialDuration)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TotalSocialDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TotalSocialDuration, maleTGdx$TotalSocialDuration)
wilcox.test(maleTGdx$TotalSocialDuration, maleTRep$TotalSocialDuration)


# planned comparisons - male to female
wilcox.test(femaleCSham$TotalSocialDuration, maleCSham$TotalSocialDuration)
wilcox.test(femaleCRep$TotalSocialDuration, maleCRep$TotalSocialDuration)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TotalSocialDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TotalSocialDuration, maleCGdx$TotalSocialDuration)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TotalSocialDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TotalSocialDuration, maleTSham$TotalSocialDuration)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TotalSocialDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TotalSocialDuration, maleTRep$TotalSocialDuration)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TotalSocialDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TotalSocialDuration, maleTGdx$TotalSocialDuration)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TotalSocialDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# planned comparisons - test to control
wilcox.test(femaleCSham$TotalSocialDuration, femaleTSham$TotalSocialDuration)
wilcox.test(femaleCRep$TotalSocialDuration, femaleTRep$TotalSocialDuration)
wilcox.test(femaleCGdx$TotalSocialDuration, femaleTGdx$TotalSocialDuration)

wilcox.test(maleCSham$TotalSocialDuration, maleTSham$TotalSocialDuration)
wilcox.test(maleCRep$TotalSocialDuration, maleTRep$TotalSocialDuration)
wilcox.test(maleCGdx$TotalSocialDuration, maleTGdx$TotalSocialDuration)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TotalSocialDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Total Social - Freq.
# Main Model
postTotalSocialFrequency <- lm(TotalSocialOccur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postTotalSocialFrequency)

# planned comparisons - gonad
wilcox.test(femaleCSham$TotalSocialOccur, femaleCGdx$TotalSocialOccur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TotalSocialOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TotalSocialOccur, femaleCRep$TotalSocialOccur)

wilcox.test(femaleTSham$TotalSocialOccur, femaleTGdx$TotalSocialOccur)
wilcox.test(femaleTGdx$TotalSocialOccur, femaleTRep$TotalSocialOccur)

wilcox.test(maleCSham$TotalSocialOccur, maleCGdx$TotalSocialOccur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TotalSocialOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TotalSocialOccur, maleCRep$TotalSocialOccur)

wilcox.test(maleTSham$TotalSocialOccur, maleTGdx$TotalSocialOccur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TotalSocialOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TotalSocialOccur, maleTRep$TotalSocialOccur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TotalSocialOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# planned comparisons - male to female
wilcox.test(femaleCSham$TotalSocialOccur, maleCSham$TotalSocialOccur)
wilcox.test(femaleCRep$TotalSocialOccur, maleCRep$TotalSocialOccur)
wilcox.test(femaleCGdx$TotalSocialOccur, maleCGdx$TotalSocialOccur)

wilcox.test(femaleTSham$TotalSocialOccur, maleTSham$TotalSocialOccur)
wilcox.test(femaleTRep$TotalSocialOccur, maleTRep$TotalSocialOccur)
wilcox.test(femaleTGdx$TotalSocialOccur, maleTGdx$TotalSocialOccur)


# planned comparisons - test to control
wilcox.test(femaleCSham$TotalSocialOccur, femaleTSham$TotalSocialOccur)
wilcox.test(femaleCRep$TotalSocialOccur, femaleTRep$TotalSocialOccur)
wilcox.test(femaleCGdx$TotalSocialOccur, femaleTGdx$TotalSocialOccur)

wilcox.test(maleCSham$TotalSocialOccur, maleTSham$TotalSocialOccur)
wilcox.test(maleCRep$TotalSocialOccur, maleTRep$TotalSocialOccur)
wilcox.test(maleCGdx$TotalSocialOccur, maleTGdx$TotalSocialOccur)


# Total Agonistic - Duration
# Main Model
postAgonisticTotalDuration <- lm(TransAgonisticTotalDuration ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postAgonisticTotalDuration)

# planned comparisons - gonad
wilcox.test(femaleCSham$TransAgonisticTotalDuration, femaleCGdx$TransAgonisticTotalDuration)
wilcox.test(femaleCGdx$TransAgonisticTotalDuration, femaleCRep$TransAgonisticTotalDuration)

wilcox.test(femaleTSham$TransAgonisticTotalDuration, femaleTGdx$TransAgonisticTotalDuration)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransAgonisticTotalDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransAgonisticTotalDuration, femaleTRep$TransAgonisticTotalDuration)

wilcox.test(maleCSham$TransAgonisticTotalDuration, maleCGdx$TransAgonisticTotalDuration)
wilcox.test(maleCGdx$TransAgonisticTotalDuration, maleCRep$TransAgonisticTotalDuration)

wilcox.test(maleTSham$TransAgonisticTotalDuration, maleTGdx$TransAgonisticTotalDuration)
wilcox.test(maleTGdx$TransAgonisticTotalDuration, maleTRep$TransAgonisticTotalDuration)


# planned comparisons - male to female
wilcox.test(femaleCSham$TransAgonisticTotalDuration, maleCSham$TransAgonisticTotalDuration)
wilcox.test(femaleCRep$TransAgonisticTotalDuration, maleCRep$TransAgonisticTotalDuration)
wilcox.test(femaleCGdx$TransAgonisticTotalDuration, maleCGdx$TransAgonisticTotalDuration)

wilcox.test(femaleTSham$TransAgonisticTotalDuration, maleTSham$TransAgonisticTotalDuration)
wilcox.test(femaleTRep$TransAgonisticTotalDuration, maleTRep$TransAgonisticTotalDuration)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransAgonisticTotalDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransAgonisticTotalDuration, maleTGdx$TransAgonisticTotalDuration)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransAgonisticTotalDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransAgonisticTotalDuration, femaleTSham$TransAgonisticTotalDuration)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransAgonisticTotalDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransAgonisticTotalDuration, femaleTRep$TransAgonisticTotalDuration)
wilcox.test(femaleCGdx$TransAgonisticTotalDuration, femaleTGdx$TransAgonisticTotalDuration)

wilcox.test(maleCSham$TransAgonisticTotalDuration, maleTSham$TransAgonisticTotalDuration)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransAgonisticTotalDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransAgonisticTotalDuration, maleTRep$TransAgonisticTotalDuration)
wilcox.test(maleCGdx$TransAgonisticTotalDuration, maleTGdx$TransAgonisticTotalDuration)

# Total Agonistic - Freq.
# Main Model
postAgonisticTotalFreq <- lm(TransAgonisticTotalOccur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postAgonisticTotalFreq)

# planned comparisons - gonad
wilcox.test(femaleCSham$TransAgonisticTotalOccur, femaleCGdx$TransAgonisticTotalOccur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransAgonisticTotalOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransAgonisticTotalOccur, femaleCRep$TransAgonisticTotalOccur)

wilcox.test(femaleTSham$TransAgonisticTotalOccur, femaleTGdx$TransAgonisticTotalOccur)
wilcox.test(femaleTGdx$TransAgonisticTotalOccur, femaleTRep$TransAgonisticTotalOccur)

wilcox.test(maleCSham$TransAgonisticTotalOccur, maleCGdx$TransAgonisticTotalOccur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransAgonisticTotalOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransAgonisticTotalOccur, maleCRep$TransAgonisticTotalOccur)

wilcox.test(maleTSham$TransAgonisticTotalOccur, maleTGdx$TransAgonisticTotalOccur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransAgonisticTotalOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransAgonisticTotalOccur, maleTRep$TransAgonisticTotalOccur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransAgonisticTotalOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransAgonisticTotalOccur, maleCSham$TransAgonisticTotalOccur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransAgonisticTotalOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransAgonisticTotalOccur, maleCRep$TransAgonisticTotalOccur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransAgonisticTotalOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransAgonisticTotalOccur, maleCGdx$TransAgonisticTotalOccur)

wilcox.test(femaleTSham$TransAgonisticTotalOccur, maleTSham$TransAgonisticTotalOccur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransAgonisticTotalOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransAgonisticTotalOccur, maleTRep$TransAgonisticTotalOccur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransAgonisticTotalOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransAgonisticTotalOccur, maleTGdx$TransAgonisticTotalOccur)


# planned comparisons - test to control
wilcox.test(femaleCSham$TransAgonisticTotalOccur, femaleTSham$TransAgonisticTotalOccur)
wilcox.test(femaleCRep$TransAgonisticTotalOccur, femaleTRep$TransAgonisticTotalOccur)
wilcox.test(femaleCGdx$TransAgonisticTotalOccur, femaleTGdx$TransAgonisticTotalOccur)

wilcox.test(maleCSham$TransAgonisticTotalOccur, maleTSham$TransAgonisticTotalOccur)
wilcox.test(maleCRep$TransAgonisticTotalOccur, maleTRep$TransAgonisticTotalOccur)
wilcox.test(maleCGdx$TransAgonisticTotalOccur, maleTGdx$TransAgonisticTotalOccur)



# Agonistic Delivered - Duration
# Main Model
postAgonisticDeliveredDuration <- lm(TransAgonisticDeliveredDuration ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postAgonisticDeliveredDuration)

# planned comparisons - gonad
wilcox.test(femaleCSham$TransAgonisticDeliveredDuration, femaleCGdx$TransAgonisticDeliveredDuration)
wilcox.test(femaleCGdx$TransAgonisticDeliveredDuration, femaleCRep$TransAgonisticDeliveredDuration)

wilcox.test(femaleTSham$TransAgonisticDeliveredDuration, femaleTGdx$TransAgonisticDeliveredDuration)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransAgonisticDeliveredDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransAgonisticDeliveredDuration, femaleTRep$TransAgonisticDeliveredDuration)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransAgonisticDeliveredDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransAgonisticDeliveredDuration, maleCGdx$TransAgonisticDeliveredDuration)
wilcox.test(maleCGdx$TransAgonisticDeliveredDuration, maleCRep$TransAgonisticDeliveredDuration)

wilcox.test(maleTSham$TransAgonisticDeliveredDuration, maleTGdx$TransAgonisticDeliveredDuration)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransAgonisticDeliveredDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransAgonisticDeliveredDuration, maleTRep$TransAgonisticDeliveredDuration)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransAgonisticDeliveredDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# planned comparisons - male to female
wilcox.test(femaleCSham$TransAgonisticDeliveredDuration, maleCSham$TransAgonisticDeliveredDuration)
wilcox.test(femaleCRep$TransAgonisticDeliveredDuration, maleCRep$TransAgonisticDeliveredDuration)
wilcox.test(femaleCGdx$TransAgonisticDeliveredDuration, maleCGdx$TransAgonisticDeliveredDuration)

wilcox.test(femaleTSham$TransAgonisticDeliveredDuration, maleTSham$TransAgonisticDeliveredDuration)
wilcox.test(femaleTRep$TransAgonisticDeliveredDuration, maleTRep$TransAgonisticDeliveredDuration)
wilcox.test(femaleTGdx$TransAgonisticDeliveredDuration, maleTGdx$TransAgonisticDeliveredDuration)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransAgonisticDeliveredDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# planned comparisons - test to control
wilcox.test(femaleCSham$TransAgonisticDeliveredDuration, femaleTSham$TransAgonisticDeliveredDuration)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransAgonisticDeliveredDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransAgonisticDeliveredDuration, femaleTRep$TransAgonisticDeliveredDuration)
wilcox.test(femaleCGdx$TransAgonisticDeliveredDuration, femaleTGdx$TransAgonisticDeliveredDuration)

wilcox.test(maleCSham$TransAgonisticDeliveredDuration, maleTSham$TransAgonisticDeliveredDuration)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransAgonisticDeliveredDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransAgonisticDeliveredDuration, maleTRep$TransAgonisticDeliveredDuration)
wilcox.test(maleCGdx$TransAgonisticDeliveredDuration, maleTGdx$TransAgonisticDeliveredDuration)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransAgonisticDeliveredDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Agonistic Delivered - Freq.
# Main Model
postAgonisticDeliveredOccur <- lm(TransAgonisticDeliveredOccur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postAgonisticDeliveredOccur)

# planned comparisons - gonad
wilcox.test(femaleCSham$TransAgonisticDeliveredOccur, femaleCGdx$TransAgonisticDeliveredOccur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransAgonisticDeliveredOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransAgonisticDeliveredOccur, femaleCRep$TransAgonisticDeliveredOccur)

wilcox.test(femaleTSham$TransAgonisticDeliveredOccur, femaleTGdx$TransAgonisticDeliveredOccur)
wilcox.test(femaleTGdx$TransAgonisticDeliveredOccur, femaleTRep$TransAgonisticDeliveredOccur)

wilcox.test(maleCSham$TransAgonisticDeliveredOccur, maleCGdx$TransAgonisticDeliveredOccur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransAgonisticDeliveredOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransAgonisticDeliveredOccur, maleCRep$TransAgonisticDeliveredOccur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransAgonisticDeliveredOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransAgonisticDeliveredOccur, maleTGdx$TransAgonisticDeliveredOccur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransAgonisticDeliveredOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransAgonisticDeliveredOccur, maleTRep$TransAgonisticDeliveredOccur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransAgonisticDeliveredOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransAgonisticDeliveredOccur, maleCSham$TransAgonisticDeliveredOccur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransAgonisticDeliveredOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransAgonisticDeliveredOccur, maleCRep$TransAgonisticDeliveredOccur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransAgonisticDeliveredOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransAgonisticDeliveredOccur, maleCGdx$TransAgonisticDeliveredOccur)

wilcox.test(femaleTSham$TransAgonisticDeliveredOccur, maleTSham$TransAgonisticDeliveredOccur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransAgonisticDeliveredOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransAgonisticDeliveredOccur, maleTRep$TransAgonisticDeliveredOccur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransAgonisticDeliveredOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransAgonisticDeliveredOccur, maleTGdx$TransAgonisticDeliveredOccur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransAgonisticDeliveredOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransAgonisticDeliveredOccur, femaleTSham$TransAgonisticDeliveredOccur)
wilcox.test(femaleCRep$TransAgonisticDeliveredOccur, femaleTRep$TransAgonisticDeliveredOccur)
wilcox.test(femaleCGdx$TransAgonisticDeliveredOccur, femaleTGdx$TransAgonisticDeliveredOccur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransAgonisticDeliveredOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


wilcox.test(maleCSham$TransAgonisticDeliveredOccur, maleTSham$TransAgonisticDeliveredOccur)
wilcox.test(maleCRep$TransAgonisticDeliveredOccur, maleTRep$TransAgonisticDeliveredOccur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransAgonisticDeliveredOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransAgonisticDeliveredOccur, maleTGdx$TransAgonisticDeliveredOccur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransAgonisticDeliveredOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))



# Agonistic Received - Duration
# Main Model
postAgonisticReceivedDuration <- lm(TransAgonisticReceivedDuration ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postAgonisticReceivedDuration)

# planned comparisons - gonad
wilcox.test(femaleCSham$TransAgonisticReceivedDuration, femaleCGdx$TransAgonisticReceivedDuration)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransAgonisticReceivedDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransAgonisticReceivedDuration, femaleCRep$TransAgonisticReceivedDuration)

wilcox.test(femaleTSham$TransAgonisticReceivedDuration, femaleTGdx$TransAgonisticReceivedDuration)
wilcox.test(femaleTGdx$TransAgonisticReceivedDuration, femaleTRep$TransAgonisticReceivedDuration)

wilcox.test(maleCSham$TransAgonisticReceivedDuration, maleCGdx$TransAgonisticReceivedDuration)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransAgonisticReceivedDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransAgonisticReceivedDuration, maleCRep$TransAgonisticReceivedDuration)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransAgonisticReceivedDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransAgonisticReceivedDuration, maleTGdx$TransAgonisticReceivedDuration)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransAgonisticReceivedDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransAgonisticReceivedDuration, maleTRep$TransAgonisticReceivedDuration)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransAgonisticReceivedDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# planned comparisons - male to female
wilcox.test(femaleCSham$TransAgonisticReceivedDuration, maleCSham$TransAgonisticReceivedDuration)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransAgonisticReceivedDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransAgonisticReceivedDuration, maleCRep$TransAgonisticReceivedDuration)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransAgonisticReceivedDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransAgonisticReceivedDuration, maleCGdx$TransAgonisticReceivedDuration)

wilcox.test(femaleTSham$TransAgonisticReceivedDuration, maleTSham$TransAgonisticReceivedDuration)
wilcox.test(femaleTRep$TransAgonisticReceivedDuration, maleTRep$TransAgonisticReceivedDuration)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransAgonisticReceivedDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransAgonisticReceivedDuration, maleTGdx$TransAgonisticReceivedDuration)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransAgonisticReceivedDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# planned comparisons - test to control
wilcox.test(femaleCSham$TransAgonisticReceivedDuration, femaleTSham$TransAgonisticReceivedDuration)
wilcox.test(femaleCRep$TransAgonisticReceivedDuration, femaleTRep$TransAgonisticReceivedDuration)
wilcox.test(femaleCGdx$TransAgonisticReceivedDuration, femaleTGdx$TransAgonisticReceivedDuration)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransAgonisticReceivedDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


wilcox.test(maleCSham$TransAgonisticReceivedDuration, maleTSham$TransAgonisticReceivedDuration)
wilcox.test(maleCRep$TransAgonisticReceivedDuration, maleTRep$TransAgonisticReceivedDuration)
wilcox.test(maleCGdx$TransAgonisticReceivedDuration, maleTGdx$TransAgonisticReceivedDuration)




# Agonistic Received - Freq.
# Main Model
postAgonisticReceivedOccur <- lm(TransAgonisticReceivedOccur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postAgonisticReceivedOccur)

# planned comparisons - gonad
wilcox.test(femaleCSham$TransAgonisticReceivedOccur, femaleCGdx$TransAgonisticReceivedOccur)
wilcox.test(femaleCGdx$TransAgonisticReceivedOccur, femaleCRep$TransAgonisticReceivedOccur)

wilcox.test(femaleTSham$TransAgonisticReceivedOccur, femaleTGdx$TransAgonisticReceivedOccur)
wilcox.test(femaleTGdx$TransAgonisticReceivedOccur, femaleTRep$TransAgonisticReceivedOccur)

wilcox.test(maleCSham$TransAgonisticReceivedOccur, maleCGdx$TransAgonisticReceivedOccur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransAgonisticReceivedOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransAgonisticReceivedOccur, maleCRep$TransAgonisticReceivedOccur)

wilcox.test(maleTSham$TransAgonisticReceivedOccur, maleTGdx$TransAgonisticReceivedOccur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransAgonisticReceivedOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransAgonisticReceivedOccur, maleTRep$TransAgonisticReceivedOccur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransAgonisticReceivedOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransAgonisticReceivedOccur, maleCSham$TransAgonisticReceivedOccur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransAgonisticReceivedOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransAgonisticReceivedOccur, maleCRep$TransAgonisticReceivedOccur)
wilcox.test(femaleCGdx$TransAgonisticReceivedOccur, maleCGdx$TransAgonisticReceivedOccur)

wilcox.test(femaleTSham$TransAgonisticReceivedOccur, maleTSham$TransAgonisticReceivedOccur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransAgonisticReceivedOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransAgonisticReceivedOccur, maleTRep$TransAgonisticReceivedOccur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransAgonisticReceivedOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransAgonisticReceivedOccur, maleTGdx$TransAgonisticReceivedOccur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransAgonisticReceivedOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransAgonisticReceivedOccur, femaleTSham$TransAgonisticReceivedOccur)
wilcox.test(femaleCRep$TransAgonisticReceivedOccur, femaleTRep$TransAgonisticReceivedOccur)
wilcox.test(femaleCGdx$TransAgonisticReceivedOccur, femaleTGdx$TransAgonisticReceivedOccur)

wilcox.test(maleCSham$TransAgonisticReceivedOccur, maleTSham$TransAgonisticReceivedOccur)
wilcox.test(maleCRep$TransAgonisticReceivedOccur, maleTRep$TransAgonisticReceivedOccur)
wilcox.test(maleCGdx$TransAgonisticReceivedOccur, maleTGdx$TransAgonisticReceivedOccur)


# Dominance Score - Duration
# Main Model
postDomScoreDuration <- lm(DominanceScoreDuration ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postDomScoreDuration)

# planned comparisons - gonad
wilcox.test(femaleCSham$DominanceScoreDuration, femaleCGdx$DominanceScoreDuration)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(DominanceScoreDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$DominanceScoreDuration, femaleCRep$DominanceScoreDuration)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(DominanceScoreDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$DominanceScoreDuration, femaleTGdx$DominanceScoreDuration)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(DominanceScoreDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$DominanceScoreDuration, femaleTRep$DominanceScoreDuration)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(DominanceScoreDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$DominanceScoreDuration, maleCGdx$DominanceScoreDuration)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(DominanceScoreDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$DominanceScoreDuration, maleCRep$DominanceScoreDuration)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(DominanceScoreDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$DominanceScoreDuration, maleTGdx$DominanceScoreDuration)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(DominanceScoreDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$DominanceScoreDuration, maleTRep$DominanceScoreDuration)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(DominanceScoreDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$DominanceScoreDuration, maleCSham$DominanceScoreDuration)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(DominanceScoreDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$DominanceScoreDuration, maleCRep$DominanceScoreDuration)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(DominanceScoreDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$DominanceScoreDuration, maleCGdx$DominanceScoreDuration)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(DominanceScoreDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$DominanceScoreDuration, maleTSham$DominanceScoreDuration)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(DominanceScoreDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$DominanceScoreDuration, maleTRep$DominanceScoreDuration)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(DominanceScoreDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$DominanceScoreDuration, maleTGdx$DominanceScoreDuration)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(DominanceScoreDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$DominanceScoreDuration, femaleTSham$DominanceScoreDuration)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(DominanceScoreDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$DominanceScoreDuration, femaleTRep$DominanceScoreDuration)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(DominanceScoreDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$DominanceScoreDuration, femaleTGdx$DominanceScoreDuration)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(DominanceScoreDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$DominanceScoreDuration, maleTSham$DominanceScoreDuration)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(DominanceScoreDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$DominanceScoreDuration, maleTRep$DominanceScoreDuration)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(DominanceScoreDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$DominanceScoreDuration, maleTGdx$DominanceScoreDuration)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(DominanceScoreDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Dominance Score - Freq.
# Main Model
postDomScoreOccur <- lm(DominanceScoreOccur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postDomScoreOccur)

# planned comparisons - gonad
wilcox.test(femaleCSham$DominanceScoreOccur, femaleCGdx$DominanceScoreOccur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(DominanceScoreOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$DominanceScoreOccur, femaleCRep$DominanceScoreOccur)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(DominanceScoreOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$DominanceScoreOccur, femaleTGdx$DominanceScoreOccur)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(DominanceScoreOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$DominanceScoreOccur, femaleTRep$DominanceScoreOccur)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(DominanceScoreOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$DominanceScoreOccur, maleCGdx$DominanceScoreOccur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(DominanceScoreOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$DominanceScoreOccur, maleCRep$DominanceScoreOccur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(DominanceScoreOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$DominanceScoreOccur, maleTGdx$DominanceScoreOccur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(DominanceScoreOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$DominanceScoreOccur, maleTRep$DominanceScoreOccur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(DominanceScoreOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$DominanceScoreOccur, maleCSham$DominanceScoreOccur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(DominanceScoreOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$DominanceScoreOccur, maleCRep$DominanceScoreOccur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(DominanceScoreOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$DominanceScoreOccur, maleCGdx$DominanceScoreOccur)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(DominanceScoreOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$DominanceScoreOccur, maleTSham$DominanceScoreOccur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(DominanceScoreOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$DominanceScoreOccur, maleTRep$DominanceScoreOccur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(DominanceScoreOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$DominanceScoreOccur, maleTGdx$DominanceScoreOccur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(DominanceScoreOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$DominanceScoreOccur, femaleTSham$DominanceScoreOccur)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(DominanceScoreOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$DominanceScoreOccur, femaleTRep$DominanceScoreOccur)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(DominanceScoreOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$DominanceScoreOccur, femaleTGdx$DominanceScoreOccur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(DominanceScoreOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$DominanceScoreOccur, maleTSham$DominanceScoreOccur)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(DominanceScoreOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$DominanceScoreOccur, maleTRep$DominanceScoreOccur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(DominanceScoreOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$DominanceScoreOccur, maleTGdx$DominanceScoreOccur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(DominanceScoreOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Social Investigation - Duration
# Main Model
postSocialInvestigationDur <- lm(SocialInvestigationDuration ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postSocialInvestigationDur)

# planned comparisons - gonad
wilcox.test(femaleCSham$SocialInvestigationDuration, femaleCGdx$SocialInvestigationDuration)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(SocialInvestigationDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SocialInvestigationDuration, femaleCRep$SocialInvestigationDuration)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(SocialInvestigationDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$SocialInvestigationDuration, femaleTGdx$SocialInvestigationDuration)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(SocialInvestigationDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$SocialInvestigationDuration, femaleTRep$SocialInvestigationDuration)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(SocialInvestigationDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$SocialInvestigationDuration, maleCGdx$SocialInvestigationDuration)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(SocialInvestigationDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$SocialInvestigationDuration, maleCRep$SocialInvestigationDuration)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(SocialInvestigationDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$SocialInvestigationDuration, maleTGdx$SocialInvestigationDuration)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(SocialInvestigationDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$SocialInvestigationDuration, maleTRep$SocialInvestigationDuration)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(SocialInvestigationDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$SocialInvestigationDuration, maleCSham$SocialInvestigationDuration)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(SocialInvestigationDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$SocialInvestigationDuration, maleCRep$SocialInvestigationDuration)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(SocialInvestigationDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SocialInvestigationDuration, maleCGdx$SocialInvestigationDuration)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(SocialInvestigationDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$SocialInvestigationDuration, maleTSham$SocialInvestigationDuration)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(SocialInvestigationDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$SocialInvestigationDuration, maleTRep$SocialInvestigationDuration)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(SocialInvestigationDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$SocialInvestigationDuration, maleTGdx$SocialInvestigationDuration)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(SocialInvestigationDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$SocialInvestigationDuration, femaleTSham$SocialInvestigationDuration)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(SocialInvestigationDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$SocialInvestigationDuration, femaleTRep$SocialInvestigationDuration)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(SocialInvestigationDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SocialInvestigationDuration, femaleTGdx$SocialInvestigationDuration)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(SocialInvestigationDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$SocialInvestigationDuration, maleTSham$SocialInvestigationDuration)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(SocialInvestigationDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$SocialInvestigationDuration, maleTRep$SocialInvestigationDuration)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(SocialInvestigationDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$SocialInvestigationDuration, maleTGdx$SocialInvestigationDuration)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(SocialInvestigationDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))



# Social Investigation - Freq.
# Main Model
postSocialInvestigationOccur <- lm(SocialInvestigationOccur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postSocialInvestigationOccur)

# planned comparisons - gonad
wilcox.test(femaleCSham$SocialInvestigationOccur, femaleCGdx$SocialInvestigationOccur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(SocialInvestigationOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SocialInvestigationOccur, femaleCRep$SocialInvestigationOccur)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(SocialInvestigationOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$SocialInvestigationOccur, femaleTGdx$SocialInvestigationOccur)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(SocialInvestigationOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$SocialInvestigationOccur, femaleTRep$SocialInvestigationOccur)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(SocialInvestigationOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$SocialInvestigationOccur, maleCGdx$SocialInvestigationOccur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(SocialInvestigationOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$SocialInvestigationOccur, maleCRep$SocialInvestigationOccur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(SocialInvestigationOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$SocialInvestigationOccur, maleTGdx$SocialInvestigationOccur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(SocialInvestigationOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$SocialInvestigationOccur, maleTRep$SocialInvestigationOccur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(SocialInvestigationOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$SocialInvestigationOccur, maleCSham$SocialInvestigationOccur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(SocialInvestigationOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$SocialInvestigationOccur, maleCRep$SocialInvestigationOccur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(SocialInvestigationOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SocialInvestigationOccur, maleCGdx$SocialInvestigationOccur)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(SocialInvestigationOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$SocialInvestigationOccur, maleTSham$SocialInvestigationOccur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(SocialInvestigationOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$SocialInvestigationOccur, maleTRep$SocialInvestigationOccur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(SocialInvestigationOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$SocialInvestigationOccur, maleTGdx$SocialInvestigationOccur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(SocialInvestigationOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$SocialInvestigationOccur, femaleTSham$SocialInvestigationOccur)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(SocialInvestigationOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$SocialInvestigationOccur, femaleTRep$SocialInvestigationOccur)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(SocialInvestigationOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SocialInvestigationOccur, femaleTGdx$SocialInvestigationOccur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(SocialInvestigationOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$SocialInvestigationOccur, maleTSham$SocialInvestigationOccur)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(SocialInvestigationOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$SocialInvestigationOccur, maleTRep$SocialInvestigationOccur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(SocialInvestigationOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$SocialInvestigationOccur, maleTGdx$SocialInvestigationOccur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(SocialInvestigationOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# Non-Social - Duration
# Main Model
postNonSocialDur <- lm(NonSocialDuration ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postNonSocialDur)

# planned comparisons - gonad
wilcox.test(femaleCSham$NonSocialDuration, femaleCGdx$NonSocialDuration)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(NonSocialDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$NonSocialDuration, femaleCRep$NonSocialDuration)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(NonSocialDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$NonSocialDuration, femaleTGdx$NonSocialDuration)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(NonSocialDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$NonSocialDuration, femaleTRep$NonSocialDuration)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(NonSocialDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$NonSocialDuration, maleCGdx$NonSocialDuration)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(NonSocialDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$NonSocialDuration, maleCRep$NonSocialDuration)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(NonSocialDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$NonSocialDuration, maleTGdx$NonSocialDuration)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(NonSocialDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$NonSocialDuration, maleTRep$NonSocialDuration)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(NonSocialDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$NonSocialDuration, maleCSham$NonSocialDuration)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(NonSocialDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$NonSocialDuration, maleCRep$NonSocialDuration)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(NonSocialDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$NonSocialDuration, maleCGdx$NonSocialDuration)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(NonSocialDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$NonSocialDuration, maleTSham$NonSocialDuration)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(NonSocialDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$NonSocialDuration, maleTRep$NonSocialDuration)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(NonSocialDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$NonSocialDuration, maleTGdx$NonSocialDuration)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(NonSocialDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$NonSocialDuration, femaleTSham$NonSocialDuration)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(NonSocialDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$NonSocialDuration, femaleTRep$NonSocialDuration)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(NonSocialDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$NonSocialDuration, femaleTGdx$NonSocialDuration)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(NonSocialDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$NonSocialDuration, maleTSham$NonSocialDuration)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(NonSocialDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$NonSocialDuration, maleTRep$NonSocialDuration)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(NonSocialDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$NonSocialDuration, maleTGdx$NonSocialDuration)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(NonSocialDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Non-Social - Freq.
# Main Model
postNonSocialOccur <- lm(NonSocialOccur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postNonSocialOccur)

# planned comparisons - gonad
wilcox.test(femaleCSham$NonSocialOccur, femaleCGdx$NonSocialOccur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(NonSocialOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$NonSocialOccur, femaleCRep$NonSocialOccur)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(NonSocialOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$NonSocialOccur, femaleTGdx$NonSocialOccur)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(NonSocialOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$NonSocialOccur, femaleTRep$NonSocialOccur)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(NonSocialOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$NonSocialOccur, maleCGdx$NonSocialOccur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(NonSocialOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$NonSocialOccur, maleCRep$NonSocialOccur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(NonSocialOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$NonSocialOccur, maleTGdx$NonSocialOccur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(NonSocialOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$NonSocialOccur, maleTRep$NonSocialOccur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(NonSocialOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$NonSocialOccur, maleCSham$NonSocialOccur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(NonSocialOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$NonSocialOccur, maleCRep$NonSocialOccur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(NonSocialOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$NonSocialOccur, maleCGdx$NonSocialOccur)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(NonSocialOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$NonSocialOccur, maleTSham$NonSocialOccur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(NonSocialOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$NonSocialOccur, maleTRep$NonSocialOccur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(NonSocialOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$NonSocialOccur, maleTGdx$NonSocialOccur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(NonSocialOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$NonSocialOccur, femaleTSham$NonSocialOccur)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(NonSocialOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$NonSocialOccur, femaleTRep$NonSocialOccur)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(NonSocialOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$NonSocialOccur, femaleTGdx$NonSocialOccur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(NonSocialOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$NonSocialOccur, maleTSham$NonSocialOccur)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(NonSocialOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$NonSocialOccur, maleTRep$NonSocialOccur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(NonSocialOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$NonSocialOccur, maleTGdx$NonSocialOccur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(NonSocialOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# Active Non-Social - Duration
# Main Model
postActiveNonSocialDur <- lm(NonSocialLocomotorDuration ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postActiveNonSocialDur)

# planned comparisons - gonad
wilcox.test(femaleCSham$NonSocialLocomotorDuration, femaleCGdx$NonSocialLocomotorDuration)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(NonSocialLocomotorDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$NonSocialLocomotorDuration, femaleCRep$NonSocialLocomotorDuration)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(NonSocialLocomotorDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$NonSocialLocomotorDuration, femaleTGdx$NonSocialLocomotorDuration)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(NonSocialLocomotorDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$NonSocialLocomotorDuration, femaleTRep$NonSocialLocomotorDuration)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(NonSocialLocomotorDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$NonSocialLocomotorDuration, maleCGdx$NonSocialLocomotorDuration)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(NonSocialLocomotorDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$NonSocialLocomotorDuration, maleCRep$NonSocialLocomotorDuration)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(NonSocialLocomotorDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$NonSocialLocomotorDuration, maleTGdx$NonSocialLocomotorDuration)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(NonSocialLocomotorDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$NonSocialLocomotorDuration, maleTRep$NonSocialLocomotorDuration)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(NonSocialLocomotorDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$NonSocialLocomotorDuration, maleCSham$NonSocialLocomotorDuration)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(NonSocialLocomotorDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$NonSocialLocomotorDuration, maleCRep$NonSocialLocomotorDuration)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(NonSocialLocomotorDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$NonSocialLocomotorDuration, maleCGdx$NonSocialLocomotorDuration)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(NonSocialLocomotorDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$NonSocialLocomotorDuration, maleTSham$NonSocialLocomotorDuration)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(NonSocialLocomotorDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$NonSocialLocomotorDuration, maleTRep$NonSocialLocomotorDuration)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(NonSocialLocomotorDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$NonSocialLocomotorDuration, maleTGdx$NonSocialLocomotorDuration)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(NonSocialLocomotorDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$NonSocialLocomotorDuration, femaleTSham$NonSocialLocomotorDuration)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(NonSocialLocomotorDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$NonSocialLocomotorDuration, femaleTRep$NonSocialLocomotorDuration)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(NonSocialLocomotorDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$NonSocialLocomotorDuration, femaleTGdx$NonSocialLocomotorDuration)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(NonSocialLocomotorDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$NonSocialLocomotorDuration, maleTSham$NonSocialLocomotorDuration)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(NonSocialLocomotorDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$NonSocialLocomotorDuration, maleTRep$NonSocialLocomotorDuration)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(NonSocialLocomotorDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$NonSocialLocomotorDuration, maleTGdx$NonSocialLocomotorDuration)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(NonSocialLocomotorDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Active Non-Social - Freq.
# Main Model
postNonSocialOccur <- lm(NonSocialLocomotorOccur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postNonSocialOccur)

wilcox.test(femaleCSham$NonSocialLocomotorOccur, femaleCGdx$NonSocialLocomotorOccur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(NonSocialLocomotorOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$NonSocialLocomotorOccur, femaleCRep$NonSocialLocomotorOccur)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(NonSocialLocomotorOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$NonSocialLocomotorOccur, femaleTGdx$NonSocialLocomotorOccur)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(NonSocialLocomotorOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$NonSocialLocomotorOccur, femaleTRep$NonSocialLocomotorOccur)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(NonSocialLocomotorOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$NonSocialLocomotorOccur, maleCGdx$NonSocialLocomotorOccur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(NonSocialLocomotorOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$NonSocialLocomotorOccur, maleCRep$NonSocialLocomotorOccur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(NonSocialLocomotorOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$NonSocialLocomotorOccur, maleTGdx$NonSocialLocomotorOccur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(NonSocialLocomotorOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$NonSocialLocomotorOccur, maleTRep$NonSocialLocomotorOccur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(NonSocialLocomotorOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$NonSocialLocomotorOccur, maleCSham$NonSocialLocomotorOccur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(NonSocialLocomotorOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$NonSocialLocomotorOccur, maleCRep$NonSocialLocomotorOccur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(NonSocialLocomotorOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$NonSocialLocomotorOccur, maleCGdx$NonSocialLocomotorOccur)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(NonSocialLocomotorOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$NonSocialLocomotorOccur, maleTSham$NonSocialLocomotorOccur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(NonSocialLocomotorOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$NonSocialLocomotorOccur, maleTRep$NonSocialLocomotorOccur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(NonSocialLocomotorOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$NonSocialLocomotorOccur, maleTGdx$NonSocialLocomotorOccur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(NonSocialLocomotorOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$NonSocialLocomotorOccur, femaleTSham$NonSocialLocomotorOccur)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(NonSocialLocomotorOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$NonSocialLocomotorOccur, femaleTRep$NonSocialLocomotorOccur)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(NonSocialLocomotorOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$NonSocialLocomotorOccur, femaleTGdx$NonSocialLocomotorOccur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(NonSocialLocomotorOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$NonSocialLocomotorOccur, maleTSham$NonSocialLocomotorOccur)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(NonSocialLocomotorOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$NonSocialLocomotorOccur, maleTRep$NonSocialLocomotorOccur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(NonSocialLocomotorOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$NonSocialLocomotorOccur, maleTGdx$NonSocialLocomotorOccur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(NonSocialLocomotorOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# Inactive Non-Social - Duration
# Main Model
postInactiveNonSocialDur <- lm(TransInactiveNonSocialDuration ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postInactiveNonSocialDur)

wilcox.test(femaleCSham$TransInactiveNonSocialDuration, femaleCGdx$TransInactiveNonSocialDuration)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransInactiveNonSocialDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransInactiveNonSocialDuration, femaleCRep$TransInactiveNonSocialDuration)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TransInactiveNonSocialDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransInactiveNonSocialDuration, femaleTGdx$TransInactiveNonSocialDuration)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransInactiveNonSocialDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransInactiveNonSocialDuration, femaleTRep$TransInactiveNonSocialDuration)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransInactiveNonSocialDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransInactiveNonSocialDuration, maleCGdx$TransInactiveNonSocialDuration)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransInactiveNonSocialDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransInactiveNonSocialDuration, maleCRep$TransInactiveNonSocialDuration)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransInactiveNonSocialDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransInactiveNonSocialDuration, maleTGdx$TransInactiveNonSocialDuration)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransInactiveNonSocialDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransInactiveNonSocialDuration, maleTRep$TransInactiveNonSocialDuration)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransInactiveNonSocialDuration ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransInactiveNonSocialDuration, maleCSham$TransInactiveNonSocialDuration)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransInactiveNonSocialDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransInactiveNonSocialDuration, maleCRep$TransInactiveNonSocialDuration)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransInactiveNonSocialDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransInactiveNonSocialDuration, maleCGdx$TransInactiveNonSocialDuration)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TransInactiveNonSocialDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransInactiveNonSocialDuration, maleTSham$TransInactiveNonSocialDuration)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransInactiveNonSocialDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransInactiveNonSocialDuration, maleTRep$TransInactiveNonSocialDuration)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransInactiveNonSocialDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransInactiveNonSocialDuration, maleTGdx$TransInactiveNonSocialDuration)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransInactiveNonSocialDuration ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransInactiveNonSocialDuration, femaleTSham$TransInactiveNonSocialDuration)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransInactiveNonSocialDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransInactiveNonSocialDuration, femaleTRep$TransInactiveNonSocialDuration)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(TransInactiveNonSocialDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransInactiveNonSocialDuration, femaleTGdx$TransInactiveNonSocialDuration)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransInactiveNonSocialDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransInactiveNonSocialDuration, maleTSham$TransInactiveNonSocialDuration)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransInactiveNonSocialDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransInactiveNonSocialDuration, maleTRep$TransInactiveNonSocialDuration)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransInactiveNonSocialDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransInactiveNonSocialDuration, maleTGdx$TransInactiveNonSocialDuration)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransInactiveNonSocialDuration ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Inactive Non-Social - Freq.
# Main Model
postInactiveNonSocialOccur <- lm(TransInactiveNonSocialOccur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postInactiveNonSocialOccur)

wilcox.test(femaleCSham$TransInactiveNonSocialOccur, femaleCGdx$TransInactiveNonSocialOccur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransInactiveNonSocialOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransInactiveNonSocialOccur, femaleCRep$TransInactiveNonSocialOccur)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TransInactiveNonSocialOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransInactiveNonSocialOccur, femaleTGdx$TransInactiveNonSocialOccur)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransInactiveNonSocialOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransInactiveNonSocialOccur, femaleTRep$TransInactiveNonSocialOccur)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransInactiveNonSocialOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransInactiveNonSocialOccur, maleCGdx$TransInactiveNonSocialOccur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransInactiveNonSocialOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransInactiveNonSocialOccur, maleCRep$TransInactiveNonSocialOccur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransInactiveNonSocialOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransInactiveNonSocialOccur, maleTGdx$TransInactiveNonSocialOccur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransInactiveNonSocialOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransInactiveNonSocialOccur, maleTRep$TransInactiveNonSocialOccur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransInactiveNonSocialOccur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransInactiveNonSocialOccur, maleCSham$TransInactiveNonSocialOccur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransInactiveNonSocialOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransInactiveNonSocialOccur, maleCRep$TransInactiveNonSocialOccur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransInactiveNonSocialOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransInactiveNonSocialOccur, maleCGdx$TransInactiveNonSocialOccur)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TransInactiveNonSocialOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransInactiveNonSocialOccur, maleTSham$TransInactiveNonSocialOccur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransInactiveNonSocialOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransInactiveNonSocialOccur, maleTRep$TransInactiveNonSocialOccur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransInactiveNonSocialOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransInactiveNonSocialOccur, maleTGdx$TransInactiveNonSocialOccur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransInactiveNonSocialOccur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransInactiveNonSocialOccur, femaleTSham$TransInactiveNonSocialOccur)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransInactiveNonSocialOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransInactiveNonSocialOccur, femaleTRep$TransInactiveNonSocialOccur)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(TransInactiveNonSocialOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransInactiveNonSocialOccur, femaleTGdx$TransInactiveNonSocialOccur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransInactiveNonSocialOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransInactiveNonSocialOccur, maleTSham$TransInactiveNonSocialOccur)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransInactiveNonSocialOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransInactiveNonSocialOccur, maleTRep$TransInactiveNonSocialOccur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransInactiveNonSocialOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransInactiveNonSocialOccur, maleTGdx$TransInactiveNonSocialOccur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransInactiveNonSocialOccur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

## Single Behaviours
# Follow - Duration
# Main Model
postFollowDuration <- lm(FollowDur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postFollowDuration)

wilcox.test(femaleCSham$FollowDur, femaleCGdx$FollowDur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(FollowDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$FollowDur, femaleCRep$FollowDur)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(FollowDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$FollowDur, femaleTGdx$FollowDur)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(FollowDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$FollowDur, femaleTRep$FollowDur)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(FollowDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$FollowDur, maleCGdx$FollowDur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(FollowDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$FollowDur, maleCRep$FollowDur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(FollowDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$FollowDur, maleTGdx$FollowDur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(FollowDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$FollowDur, maleTRep$FollowDur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(FollowDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$FollowDur, maleCSham$FollowDur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(FollowDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$FollowDur, maleCRep$FollowDur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(FollowDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$FollowDur, maleCGdx$FollowDur)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(FollowDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$FollowDur, maleTSham$FollowDur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(FollowDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$FollowDur, maleTRep$FollowDur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(FollowDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$FollowDur, maleTGdx$FollowDur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(FollowDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$FollowDur, femaleTSham$FollowDur)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(FollowDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$FollowDur, femaleTRep$FollowDur)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(FollowDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$FollowDur, femaleTGdx$FollowDur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(FollowDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$FollowDur, maleTSham$FollowDur)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(FollowDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$FollowDur, maleTRep$FollowDur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(FollowDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$FollowDur, maleTGdx$FollowDur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(FollowDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# Follow - Freq.
# Main Model
postFollowFreq <- lm(TransFollowEv ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postFollowFreq)


# gonad
wilcox.test(femaleCSham$TransFollowEv, femaleCGdx$TransFollowEv)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransFollowEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransFollowEv, femaleCRep$TransFollowEv)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TransFollowEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransFollowEv, femaleTGdx$TransFollowEv)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransFollowEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransFollowEv, femaleTRep$TransFollowEv)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransFollowEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransFollowEv, maleCGdx$TransFollowEv)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransFollowEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransFollowEv, maleCRep$TransFollowEv)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransFollowEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransFollowEv, maleTGdx$TransFollowEv)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransFollowEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransFollowEv, maleTRep$TransFollowEv)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransFollowEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransFollowEv, maleCSham$TransFollowEv)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransFollowEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransFollowEv, maleCRep$TransFollowEv)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransFollowEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransFollowEv, maleCGdx$TransFollowEv)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TransFollowEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransFollowEv, maleTSham$TransFollowEv)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransFollowEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransFollowEv, maleTRep$TransFollowEv)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransFollowEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransFollowEv, maleTGdx$TransFollowEv)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransFollowEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransFollowEv, femaleTSham$TransFollowEv)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransFollowEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransFollowEv, femaleTRep$TransFollowEv)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(TransFollowEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransFollowEv, femaleTGdx$TransFollowEv)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransFollowEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransFollowEv, maleTSham$TransFollowEv)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransFollowEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransFollowEv, maleTRep$TransFollowEv)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransFollowEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransFollowEv, maleTGdx$TransFollowEv)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransFollowEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# Dominant - Duration
# Main Model
postDominantDuration <- lm(DomDur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postDominantDuration)

# planned comparisons - gonad
wilcox.test(femaleCSham$DomDur, femaleCGdx$DomDur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(DomDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$DomDur, femaleCRep$DomDur)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(DomDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$DomDur, femaleTGdx$DomDur)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(DomDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$DomDur, femaleTRep$DomDur)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(DomDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$DomDur, maleCGdx$DomDur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(DomDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$DomDur, maleCRep$DomDur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(DomDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$DomDur, maleTGdx$DomDur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(DomDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$DomDur, maleTRep$DomDur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(DomDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$DomDur, maleCSham$DomDur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(DomDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$DomDur, maleCRep$DomDur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(DomDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$DomDur, maleCGdx$DomDur)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(DomDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$DomDur, maleTSham$DomDur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(DomDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$DomDur, maleTRep$DomDur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(DomDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$DomDur, maleTGdx$DomDur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(DomDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$DomDur, femaleTSham$DomDur)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(DomDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$DomDur, femaleTRep$DomDur)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(DomDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$DomDur, femaleTGdx$DomDur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(DomDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$DomDur, maleTSham$DomDur)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(DomDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$DomDur, maleTRep$DomDur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(DomDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$DomDur, maleTGdx$DomDur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(DomDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# Dominant - Freq.
# Main Model
postDominantFreq <- lm(TransDomEv ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postDominantFreq)

# planned comparisons - gonad
wilcox.test(femaleCSham$TransDomEv, femaleCGdx$TransDomEv)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransDomEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransDomEv, femaleCRep$TransDomEv)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TransDomEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransDomEv, femaleTGdx$TransDomEv)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransDomEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransDomEv, femaleTRep$TransDomEv)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransDomEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransDomEv, maleCGdx$TransDomEv)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransDomEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransDomEv, maleCRep$TransDomEv)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransDomEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransDomEv, maleTGdx$TransDomEv)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransDomEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransDomEv, maleTRep$TransDomEv)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransDomEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransDomEv, maleCSham$TransDomEv)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransDomEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransDomEv, maleCRep$TransDomEv)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransDomEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransDomEv, maleCGdx$TransDomEv)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TransDomEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransDomEv, maleTSham$TransDomEv)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransDomEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransDomEv, maleTRep$TransDomEv)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransDomEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransDomEv, maleTGdx$TransDomEv)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransDomEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransDomEv, femaleTSham$TransDomEv)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransDomEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransDomEv, femaleTRep$TransDomEv)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(TransDomEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransDomEv, femaleTGdx$TransDomEv)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransDomEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransDomEv, maleTSham$TransDomEv)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransDomEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransDomEv, maleTRep$TransDomEv)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransDomEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransDomEv, maleTGdx$TransDomEv)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransDomEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))



# Attack Del - Freq.
# Main Model
postAttackDelFreq <- lm(TransAttackDel ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postAttackDelFreq)

# planned comparisons - gonad
wilcox.test(femaleCSham$TransAttackDel, femaleCGdx$TransAttackDel)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransAttackDel ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransAttackDel, femaleCRep$TransAttackDel)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TransAttackDel ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransAttackDel, femaleTGdx$TransAttackDel)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransAttackDel ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransAttackDel, femaleTRep$TransAttackDel)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransAttackDel ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransAttackDel, maleCGdx$TransAttackDel)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransAttackDel ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransAttackDel, maleCRep$TransAttackDel)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransAttackDel ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransAttackDel, maleTGdx$TransAttackDel)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransAttackDel ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransAttackDel, maleTRep$TransAttackDel)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransAttackDel ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransAttackDel, maleCSham$TransAttackDel)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransAttackDel ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransAttackDel, maleCRep$TransAttackDel)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransAttackDel ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransAttackDel, maleCGdx$TransAttackDel)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TransAttackDel ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransAttackDel, maleTSham$TransAttackDel)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransAttackDel ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransAttackDel, maleTRep$TransAttackDel)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransAttackDel ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransAttackDel, maleTGdx$TransAttackDel)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransAttackDel ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransAttackDel, femaleTSham$TransAttackDel)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransAttackDel ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransAttackDel, femaleTRep$TransAttackDel)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(TransAttackDel ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransAttackDel, femaleTGdx$TransAttackDel)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransAttackDel ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransAttackDel, maleTSham$TransAttackDel)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransAttackDel ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransAttackDel, maleTRep$TransAttackDel)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransAttackDel ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransAttackDel, maleTGdx$TransAttackDel)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransAttackDel ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Attack Rec - Freq.
# Main Model
postAttackRecFreq <- lm(TransAttackRec ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postAttackRecFreq)

wilcox.test(femaleCSham$TransAttackRec, femaleCGdx$TransAttackRec)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransAttackRec ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransAttackRec, femaleCRep$TransAttackRec)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TransAttackRec ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransAttackRec, femaleTGdx$TransAttackRec)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransAttackRec ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransAttackRec, femaleTRep$TransAttackRec)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransAttackRec ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransAttackRec, maleCGdx$TransAttackRec)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransAttackRec ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransAttackRec, maleCRep$TransAttackRec)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransAttackRec ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransAttackRec, maleTGdx$TransAttackRec)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransAttackRec ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransAttackRec, maleTRep$TransAttackRec)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransAttackRec ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransAttackRec, maleCSham$TransAttackRec)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransAttackRec ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransAttackRec, maleCRep$TransAttackRec)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransAttackRec ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransAttackRec, maleCGdx$TransAttackRec)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TransAttackRec ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransAttackRec, maleTSham$TransAttackRec)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransAttackRec ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransAttackRec, maleTRep$TransAttackRec)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransAttackRec ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransAttackRec, maleTGdx$TransAttackRec)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransAttackRec ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransAttackRec, femaleTSham$TransAttackRec)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransAttackRec ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransAttackRec, femaleTRep$TransAttackRec)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(TransAttackRec ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransAttackRec, femaleTGdx$TransAttackRec)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransAttackRec ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransAttackRec, maleTSham$TransAttackRec)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransAttackRec ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransAttackRec, maleTRep$TransAttackRec)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransAttackRec ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransAttackRec, maleTGdx$TransAttackRec)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransAttackRec ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# Open Aggression - Duration
# Main Model
postOpenAggDuration <- lm(OpenAggDur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postOpenAggDuration)

wilcox.test(femaleCSham$OpenAggDur, femaleCGdx$OpenAggDur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(OpenAggDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$OpenAggDur, femaleCRep$OpenAggDur)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(OpenAggDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$OpenAggDur, femaleTGdx$OpenAggDur)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(OpenAggDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$OpenAggDur, femaleTRep$OpenAggDur)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(OpenAggDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$OpenAggDur, maleCGdx$OpenAggDur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(OpenAggDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$OpenAggDur, maleCRep$OpenAggDur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(OpenAggDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$OpenAggDur, maleTGdx$OpenAggDur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(OpenAggDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$OpenAggDur, maleTRep$OpenAggDur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(OpenAggDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$OpenAggDur, maleCSham$OpenAggDur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(OpenAggDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$OpenAggDur, maleCRep$OpenAggDur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(OpenAggDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$OpenAggDur, maleCGdx$OpenAggDur)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(OpenAggDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$OpenAggDur, maleTSham$OpenAggDur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(OpenAggDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$OpenAggDur, maleTRep$OpenAggDur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(OpenAggDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$OpenAggDur, maleTGdx$OpenAggDur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(OpenAggDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$OpenAggDur, femaleTSham$OpenAggDur)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(OpenAggDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$OpenAggDur, femaleTRep$OpenAggDur)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(OpenAggDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$OpenAggDur, femaleTGdx$OpenAggDur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(OpenAggDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$OpenAggDur, maleTSham$OpenAggDur)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(OpenAggDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$OpenAggDur, maleTRep$OpenAggDur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(OpenAggDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$OpenAggDur, maleTGdx$OpenAggDur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(OpenAggDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Open Aggression - Freq
# Main Model
postOpenAggFreq <- lm(OpenAggEv ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postOpenAggFreq)


wilcox.test(femaleCSham$OpenAggEv, femaleCGdx$OpenAggEv)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(OpenAggEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$OpenAggEv, femaleCRep$OpenAggEv)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(OpenAggEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$OpenAggEv, femaleTGdx$OpenAggEv)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(OpenAggEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$OpenAggEv, femaleTRep$OpenAggEv)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(OpenAggEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$OpenAggEv, maleCGdx$OpenAggEv)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(OpenAggEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$OpenAggEv, maleCRep$OpenAggEv)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(OpenAggEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$OpenAggEv, maleTGdx$OpenAggEv)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(OpenAggEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$OpenAggEv, maleTRep$OpenAggEv)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(OpenAggEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$OpenAggEv, maleCSham$OpenAggEv)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(OpenAggEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$OpenAggEv, maleCRep$OpenAggEv)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(OpenAggEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$OpenAggEv, maleCGdx$OpenAggEv)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(OpenAggEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$OpenAggEv, maleTSham$OpenAggEv)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(OpenAggEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$OpenAggEv, maleTRep$OpenAggEv)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(OpenAggEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$OpenAggEv, maleTGdx$OpenAggEv)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(OpenAggEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$OpenAggEv, femaleTSham$OpenAggEv)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(OpenAggEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$OpenAggEv, femaleTRep$OpenAggEv)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(OpenAggEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$OpenAggEv, femaleTGdx$OpenAggEv)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(OpenAggEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$OpenAggEv, maleTSham$OpenAggEv)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(OpenAggEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$OpenAggEv, maleTRep$OpenAggEv)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(OpenAggEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$OpenAggEv, maleTGdx$OpenAggEv)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(OpenAggEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# Ritualized Aggression - Dur
# Main Model
postRitualizedAggDur <- lm(TransRitAggDur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postRitualizedAggDur)

wilcox.test(femaleCSham$TransRitAggDur, femaleCGdx$TransRitAggDur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransRitAggDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransRitAggDur, femaleCRep$TransRitAggDur)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TransRitAggDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransRitAggDur, femaleTGdx$TransRitAggDur)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransRitAggDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransRitAggDur, femaleTRep$TransRitAggDur)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransRitAggDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransRitAggDur, maleCGdx$TransRitAggDur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransRitAggDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransRitAggDur, maleCRep$TransRitAggDur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransRitAggDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransRitAggDur, maleTGdx$TransRitAggDur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransRitAggDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransRitAggDur, maleTRep$TransRitAggDur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransRitAggDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransRitAggDur, maleCSham$TransRitAggDur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransRitAggDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransRitAggDur, maleCRep$TransRitAggDur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransRitAggDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransRitAggDur, maleCGdx$TransRitAggDur)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TransRitAggDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransRitAggDur, maleTSham$TransRitAggDur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransRitAggDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransRitAggDur, maleTRep$TransRitAggDur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransRitAggDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransRitAggDur, maleTGdx$TransRitAggDur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransRitAggDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransRitAggDur, femaleTSham$TransRitAggDur)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransRitAggDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransRitAggDur, femaleTRep$TransRitAggDur)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(TransRitAggDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransRitAggDur, femaleTGdx$TransRitAggDur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransRitAggDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransRitAggDur, maleTSham$TransRitAggDur)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransRitAggDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransRitAggDur, maleTRep$TransRitAggDur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransRitAggDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransRitAggDur, maleTGdx$TransRitAggDur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransRitAggDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Ritualized Aggression - Freq
# Main Model
postRitualizedAggFreq <- lm(TransRitAggEv ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postRitualizedAggFreq)

wilcox.test(femaleCSham$TransRitAggEv, femaleCGdx$TransRitAggEv)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransRitAggEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransRitAggEv, femaleCRep$TransRitAggEv)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TransRitAggEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransRitAggEv, femaleTGdx$TransRitAggEv)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransRitAggEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransRitAggEv, femaleTRep$TransRitAggEv)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransRitAggEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransRitAggEv, maleCGdx$TransRitAggEv)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransRitAggEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransRitAggEv, maleCRep$TransRitAggEv)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransRitAggEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransRitAggEv, maleTGdx$TransRitAggEv)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransRitAggEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransRitAggEv, maleTRep$TransRitAggEv)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransRitAggEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransRitAggEv, maleCSham$TransRitAggEv)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransRitAggEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransRitAggEv, maleCRep$TransRitAggEv)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransRitAggEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransRitAggEv, maleCGdx$TransRitAggEv)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TransRitAggEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransRitAggEv, maleTSham$TransRitAggEv)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransRitAggEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransRitAggEv, maleTRep$TransRitAggEv)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransRitAggEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransRitAggEv, maleTGdx$TransRitAggEv)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransRitAggEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransRitAggEv, femaleTSham$TransRitAggEv)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransRitAggEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransRitAggEv, femaleTRep$TransRitAggEv)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(TransRitAggEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransRitAggEv, femaleTGdx$TransRitAggEv)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransRitAggEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransRitAggEv, maleTSham$TransRitAggEv)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransRitAggEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransRitAggEv, maleTRep$TransRitAggEv)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransRitAggEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransRitAggEv, maleTGdx$TransRitAggEv)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransRitAggEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Submissive - Dur
# Main Model
postSubmissiveDur <- lm(TransSubDur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postSubmissiveDur)

wilcox.test(femaleCSham$TransSubDur, femaleCGdx$TransSubDur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransSubDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransSubDur, femaleCRep$TransSubDur)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TransSubDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransSubDur, femaleTGdx$TransSubDur)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransSubDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransSubDur, femaleTRep$TransSubDur)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransSubDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransSubDur, maleCGdx$TransSubDur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransSubDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransSubDur, maleCRep$TransSubDur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransSubDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransSubDur, maleTGdx$TransSubDur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransSubDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransSubDur, maleTRep$TransSubDur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransSubDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransSubDur, maleCSham$TransSubDur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransSubDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransSubDur, maleCRep$TransSubDur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransSubDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransSubDur, maleCGdx$TransSubDur)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TransSubDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransSubDur, maleTSham$TransSubDur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransSubDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransSubDur, maleTRep$TransSubDur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransSubDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransSubDur, maleTGdx$TransSubDur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransSubDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransSubDur, femaleTSham$TransSubDur)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransSubDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransSubDur, femaleTRep$TransSubDur)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(TransSubDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransSubDur, femaleTGdx$TransSubDur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransSubDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransSubDur, maleTSham$TransSubDur)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransSubDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransSubDur, maleTRep$TransSubDur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransSubDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransSubDur, maleTGdx$TransSubDur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransSubDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Submissive - Freq
# Main Model
postSubmissiveFreq <- lm(TransSubEv ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postSubmissiveFreq)

wilcox.test(femaleCSham$TransSubEv, femaleCGdx$TransSubEv)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransSubEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransSubEv, femaleCRep$TransSubEv)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TransSubEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransSubEv, femaleTGdx$TransSubEv)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransSubEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransSubEv, femaleTRep$TransSubEv)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransSubEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransSubEv, maleCGdx$TransSubEv)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransSubEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransSubEv, maleCRep$TransSubEv)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransSubEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransSubEv, maleTGdx$TransSubEv)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransSubEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransSubEv, maleTRep$TransSubEv)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransSubEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransSubEv, maleCSham$TransSubEv)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransSubEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransSubEv, maleCRep$TransSubEv)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransSubEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransSubEv, maleCGdx$TransSubEv)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TransSubEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransSubEv, maleTSham$TransSubEv)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransSubEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransSubEv, maleTRep$TransSubEv)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransSubEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransSubEv, maleTGdx$TransSubEv)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransSubEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransSubEv, femaleTSham$TransSubEv)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransSubEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransSubEv, femaleTRep$TransSubEv)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(TransSubEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransSubEv, femaleTGdx$TransSubEv)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransSubEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransSubEv, maleTSham$TransSubEv)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransSubEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransSubEv, maleTRep$TransSubEv)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransSubEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransSubEv, maleTGdx$TransSubEv)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransSubEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Defensive - Dur
# Main Model
postDefenseDur <- lm(TransDefenseDur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postDefenseDur)

wilcox.test(femaleCSham$TransDefenseDur, femaleCGdx$TransDefenseDur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransDefenseDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransDefenseDur, femaleCRep$TransDefenseDur)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TransDefenseDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransDefenseDur, femaleTGdx$TransDefenseDur)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransDefenseDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransDefenseDur, femaleTRep$TransDefenseDur)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransDefenseDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransDefenseDur, maleCGdx$TransDefenseDur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransDefenseDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransDefenseDur, maleCRep$TransDefenseDur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransDefenseDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransDefenseDur, maleTGdx$TransDefenseDur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransDefenseDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransDefenseDur, maleTRep$TransDefenseDur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransDefenseDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransDefenseDur, maleCSham$TransDefenseDur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransDefenseDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransDefenseDur, maleCRep$TransDefenseDur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransDefenseDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransDefenseDur, maleCGdx$TransDefenseDur)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TransDefenseDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransDefenseDur, maleTSham$TransDefenseDur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransDefenseDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransDefenseDur, maleTRep$TransDefenseDur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransDefenseDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransDefenseDur, maleTGdx$TransDefenseDur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransDefenseDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransDefenseDur, femaleTSham$TransDefenseDur)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransDefenseDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransDefenseDur, femaleTRep$TransDefenseDur)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(TransDefenseDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransDefenseDur, femaleTGdx$TransDefenseDur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransDefenseDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransDefenseDur, maleTSham$TransDefenseDur)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransDefenseDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransDefenseDur, maleTRep$TransDefenseDur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransDefenseDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransDefenseDur, maleTGdx$TransDefenseDur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransDefenseDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Defensive - Freq
# Main Model
postDefensiveFreq <- lm(TransDefenseEv ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postDefensiveFreq)

wilcox.test(femaleCSham$TransDefenseEv, femaleCGdx$TransDefenseEv)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransDefenseEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransDefenseEv, femaleCRep$TransDefenseEv)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TransDefenseEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransDefenseEv, femaleTGdx$TransDefenseEv)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransDefenseEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransDefenseEv, femaleTRep$TransDefenseEv)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransDefenseEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransDefenseEv, maleCGdx$TransDefenseEv)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransDefenseEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransDefenseEv, maleCRep$TransDefenseEv)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransDefenseEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransDefenseEv, maleTGdx$TransDefenseEv)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransDefenseEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransDefenseEv, maleTRep$TransDefenseEv)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransDefenseEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransDefenseEv, maleCSham$TransDefenseEv)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransDefenseEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransDefenseEv, maleCRep$TransDefenseEv)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransDefenseEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransDefenseEv, maleCGdx$TransDefenseEv)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TransDefenseEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransDefenseEv, maleTSham$TransDefenseEv)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransDefenseEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransDefenseEv, maleTRep$TransDefenseEv)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransDefenseEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransDefenseEv, maleTGdx$TransDefenseEv)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransDefenseEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransDefenseEv, femaleTSham$TransDefenseEv)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransDefenseEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransDefenseEv, femaleTRep$TransDefenseEv)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(TransDefenseEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransDefenseEv, femaleTGdx$TransDefenseEv)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransDefenseEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransDefenseEv, maleTSham$TransDefenseEv)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransDefenseEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransDefenseEv, maleTRep$TransDefenseEv)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransDefenseEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransDefenseEv, maleTGdx$TransDefenseEv)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransDefenseEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# Inactive Together - Dur
# Main Model
postInactiveTogetherDur <- lm(TransInactiveTogetherDur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postInactiveTogetherDur)

wilcox.test(femaleCSham$TransInactiveTogetherDur, femaleCGdx$TransInactiveTogetherDur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransInactiveTogetherDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransInactiveTogetherDur, femaleCRep$TransInactiveTogetherDur)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TransInactiveTogetherDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransInactiveTogetherDur, femaleTGdx$TransInactiveTogetherDur)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransInactiveTogetherDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransInactiveTogetherDur, femaleTRep$TransInactiveTogetherDur)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransInactiveTogetherDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransInactiveTogetherDur, maleCGdx$TransInactiveTogetherDur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransInactiveTogetherDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransInactiveTogetherDur, maleCRep$TransInactiveTogetherDur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransInactiveTogetherDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransInactiveTogetherDur, maleTGdx$TransInactiveTogetherDur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransInactiveTogetherDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransInactiveTogetherDur, maleTRep$TransInactiveTogetherDur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransInactiveTogetherDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransInactiveTogetherDur, maleCSham$TransInactiveTogetherDur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransInactiveTogetherDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransInactiveTogetherDur, maleCRep$TransInactiveTogetherDur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransInactiveTogetherDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransInactiveTogetherDur, maleCGdx$TransInactiveTogetherDur)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TransInactiveTogetherDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransInactiveTogetherDur, maleTSham$TransInactiveTogetherDur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransInactiveTogetherDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransInactiveTogetherDur, maleTRep$TransInactiveTogetherDur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransInactiveTogetherDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransInactiveTogetherDur, maleTGdx$TransInactiveTogetherDur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransInactiveTogetherDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransInactiveTogetherDur, femaleTSham$TransInactiveTogetherDur)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransInactiveTogetherDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransInactiveTogetherDur, femaleTRep$TransInactiveTogetherDur)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(TransInactiveTogetherDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransInactiveTogetherDur, femaleTGdx$TransInactiveTogetherDur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransInactiveTogetherDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransInactiveTogetherDur, maleTSham$TransInactiveTogetherDur)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransInactiveTogetherDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransInactiveTogetherDur, maleTRep$TransInactiveTogetherDur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransInactiveTogetherDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransInactiveTogetherDur, maleTGdx$TransInactiveTogetherDur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransInactiveTogetherDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Inactive Together - Freq
# Main Model
postInactiveTogetherFreq <- lm(TransInactiveTogetherEv ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postInactiveTogetherFreq)

wilcox.test(femaleCSham$TransInactiveTogetherEv, femaleCGdx$TransInactiveTogetherEv)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransInactiveTogetherEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransInactiveTogetherEv, femaleCRep$TransInactiveTogetherEv)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TransInactiveTogetherEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransInactiveTogetherEv, femaleTGdx$TransInactiveTogetherEv)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransInactiveTogetherEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransInactiveTogetherEv, femaleTRep$TransInactiveTogetherEv)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransInactiveTogetherEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransInactiveTogetherEv, maleCGdx$TransInactiveTogetherEv)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransInactiveTogetherEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransInactiveTogetherEv, maleCRep$TransInactiveTogetherEv)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransInactiveTogetherEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransInactiveTogetherEv, maleTGdx$TransInactiveTogetherEv)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransInactiveTogetherEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransInactiveTogetherEv, maleTRep$TransInactiveTogetherEv)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransInactiveTogetherEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransInactiveTogetherEv, maleCSham$TransInactiveTogetherEv)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransInactiveTogetherEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransInactiveTogetherEv, maleCRep$TransInactiveTogetherEv)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransInactiveTogetherEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransInactiveTogetherEv, maleCGdx$TransInactiveTogetherEv)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TransInactiveTogetherEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransInactiveTogetherEv, maleTSham$TransInactiveTogetherEv)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransInactiveTogetherEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransInactiveTogetherEv, maleTRep$TransInactiveTogetherEv)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransInactiveTogetherEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransInactiveTogetherEv, maleTGdx$TransInactiveTogetherEv)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransInactiveTogetherEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransInactiveTogetherEv, femaleTSham$TransInactiveTogetherEv)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransInactiveTogetherEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransInactiveTogetherEv, femaleTRep$TransInactiveTogetherEv)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(TransInactiveTogetherEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransInactiveTogetherEv, femaleTGdx$TransInactiveTogetherEv)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransInactiveTogetherEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransInactiveTogetherEv, maleTSham$TransInactiveTogetherEv)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransInactiveTogetherEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransInactiveTogetherEv, maleTRep$TransInactiveTogetherEv)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransInactiveTogetherEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransInactiveTogetherEv, maleTGdx$TransInactiveTogetherEv)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransInactiveTogetherEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# Sniff Head - Dur
# Main Model
postSniffHeadDur <- lm(SniffHeadDur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postSniffHeadDur)

wilcox.test(femaleCSham$SniffHeadDur, femaleCGdx$SniffHeadDur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(SniffHeadDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SniffHeadDur, femaleCRep$SniffHeadDur)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(SniffHeadDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$SniffHeadDur, femaleTGdx$SniffHeadDur)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(SniffHeadDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$SniffHeadDur, femaleTRep$SniffHeadDur)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(SniffHeadDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$SniffHeadDur, maleCGdx$SniffHeadDur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(SniffHeadDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$SniffHeadDur, maleCRep$SniffHeadDur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(SniffHeadDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$SniffHeadDur, maleTGdx$SniffHeadDur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(SniffHeadDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$SniffHeadDur, maleTRep$SniffHeadDur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(SniffHeadDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$SniffHeadDur, maleCSham$SniffHeadDur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(SniffHeadDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$SniffHeadDur, maleCRep$SniffHeadDur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(SniffHeadDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SniffHeadDur, maleCGdx$SniffHeadDur)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(SniffHeadDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$SniffHeadDur, maleTSham$SniffHeadDur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(SniffHeadDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$SniffHeadDur, maleTRep$SniffHeadDur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(SniffHeadDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$SniffHeadDur, maleTGdx$SniffHeadDur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(SniffHeadDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$SniffHeadDur, femaleTSham$SniffHeadDur)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(SniffHeadDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$SniffHeadDur, femaleTRep$SniffHeadDur)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(SniffHeadDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SniffHeadDur, femaleTGdx$SniffHeadDur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(SniffHeadDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$SniffHeadDur, maleTSham$SniffHeadDur)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(SniffHeadDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$SniffHeadDur, maleTRep$SniffHeadDur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(SniffHeadDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$SniffHeadDur, maleTGdx$SniffHeadDur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(SniffHeadDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Sniff Head - Freq
# Main Model
postSniffHeadFreq <- lm(TransSniffHeadEv ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postSniffHeadFreq)

wilcox.test(femaleCSham$TransSniffHeadEv, femaleCGdx$TransSniffHeadEv)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransSniffHeadEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransSniffHeadEv, femaleCRep$TransSniffHeadEv)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TransSniffHeadEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransSniffHeadEv, femaleTGdx$TransSniffHeadEv)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransSniffHeadEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransSniffHeadEv, femaleTRep$TransSniffHeadEv)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransSniffHeadEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransSniffHeadEv, maleCGdx$TransSniffHeadEv)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransSniffHeadEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransSniffHeadEv, maleCRep$TransSniffHeadEv)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransSniffHeadEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransSniffHeadEv, maleTGdx$TransSniffHeadEv)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransSniffHeadEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransSniffHeadEv, maleTRep$TransSniffHeadEv)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransSniffHeadEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransSniffHeadEv, maleCSham$TransSniffHeadEv)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransSniffHeadEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransSniffHeadEv, maleCRep$TransSniffHeadEv)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransSniffHeadEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransSniffHeadEv, maleCGdx$TransSniffHeadEv)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TransSniffHeadEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransSniffHeadEv, maleTSham$TransSniffHeadEv)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransSniffHeadEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransSniffHeadEv, maleTRep$TransSniffHeadEv)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransSniffHeadEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransSniffHeadEv, maleTGdx$TransSniffHeadEv)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransSniffHeadEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransSniffHeadEv, femaleTSham$TransSniffHeadEv)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransSniffHeadEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransSniffHeadEv, femaleTRep$TransSniffHeadEv)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(TransSniffHeadEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransSniffHeadEv, femaleTGdx$TransSniffHeadEv)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransSniffHeadEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransSniffHeadEv, maleTSham$TransSniffHeadEv)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransSniffHeadEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransSniffHeadEv, maleTRep$TransSniffHeadEv)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransSniffHeadEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransSniffHeadEv, maleTGdx$TransSniffHeadEv)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransSniffHeadEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Sniff Body - Dur
# Main Model
postSniffBodyDur <- lm(SniffBodyDur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postSniffBodyDur)

wilcox.test(femaleCSham$SniffBodyDur, femaleCGdx$SniffBodyDur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(SniffBodyDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SniffBodyDur, femaleCRep$SniffBodyDur)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(SniffBodyDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$SniffBodyDur, femaleTGdx$SniffBodyDur)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(SniffBodyDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$SniffBodyDur, femaleTRep$SniffBodyDur)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(SniffBodyDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$SniffBodyDur, maleCGdx$SniffBodyDur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(SniffBodyDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$SniffBodyDur, maleCRep$SniffBodyDur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(SniffBodyDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$SniffBodyDur, maleTGdx$SniffBodyDur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(SniffBodyDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$SniffBodyDur, maleTRep$SniffBodyDur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(SniffBodyDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$SniffBodyDur, maleCSham$SniffBodyDur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(SniffBodyDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$SniffBodyDur, maleCRep$SniffBodyDur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(SniffBodyDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SniffBodyDur, maleCGdx$SniffBodyDur)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(SniffBodyDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$SniffBodyDur, maleTSham$SniffBodyDur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(SniffBodyDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$SniffBodyDur, maleTRep$SniffBodyDur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(SniffBodyDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$SniffBodyDur, maleTGdx$SniffBodyDur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(SniffBodyDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$SniffBodyDur, femaleTSham$SniffBodyDur)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(SniffBodyDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$SniffBodyDur, femaleTRep$SniffBodyDur)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(SniffBodyDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SniffBodyDur, femaleTGdx$SniffBodyDur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(SniffBodyDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$SniffBodyDur, maleTSham$SniffBodyDur)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(SniffBodyDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$SniffBodyDur, maleTRep$SniffBodyDur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(SniffBodyDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$SniffBodyDur, maleTGdx$SniffBodyDur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(SniffBodyDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Sniff Body - Freq
# Main Model
postSniffBodyFreq <- lm(SniffBodyEv ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postSniffHeadFreq)

wilcox.test(femaleCSham$SniffBodyEv, femaleCGdx$SniffBodyEv)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(SniffBodyEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SniffBodyEv, femaleCRep$SniffBodyEv)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(SniffBodyEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$SniffBodyEv, femaleTGdx$SniffBodyEv)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(SniffBodyEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$SniffBodyEv, femaleTRep$SniffBodyEv)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(SniffBodyEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$SniffBodyEv, maleCGdx$SniffBodyEv)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(SniffBodyEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$SniffBodyEv, maleCRep$SniffBodyEv)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(SniffBodyEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$SniffBodyEv, maleTGdx$SniffBodyEv)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(SniffBodyEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$SniffBodyEv, maleTRep$SniffBodyEv)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(SniffBodyEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$SniffBodyEv, maleCSham$SniffBodyEv)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(SniffBodyEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$SniffBodyEv, maleCRep$SniffBodyEv)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(SniffBodyEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SniffBodyEv, maleCGdx$SniffBodyEv)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(SniffBodyEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$SniffBodyEv, maleTSham$SniffBodyEv)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(SniffBodyEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$SniffBodyEv, maleTRep$SniffBodyEv)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(SniffBodyEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$SniffBodyEv, maleTGdx$SniffBodyEv)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(SniffBodyEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$SniffBodyEv, femaleTSham$SniffBodyEv)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(SniffBodyEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$SniffBodyEv, femaleTRep$SniffBodyEv)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(SniffBodyEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SniffBodyEv, femaleTGdx$SniffBodyEv)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(SniffBodyEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$SniffBodyEv, maleTSham$SniffBodyEv)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(SniffBodyEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$SniffBodyEv, maleTRep$SniffBodyEv)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(SniffBodyEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$SniffBodyEv, maleTGdx$SniffBodyEv)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(SniffBodyEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# Sniff Anogenital - Dur
# Main Model
postSniffAnogenitalDur <- lm(SniffAnoDur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postSniffAnogenitalDur)

wilcox.test(femaleCSham$SniffAnoDur, femaleCGdx$SniffAnoDur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(SniffAnoDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SniffAnoDur, femaleCRep$SniffAnoDur)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(SniffAnoDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$SniffAnoDur, femaleTGdx$SniffAnoDur)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(SniffAnoDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$SniffAnoDur, femaleTRep$SniffAnoDur)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(SniffAnoDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$SniffAnoDur, maleCGdx$SniffAnoDur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(SniffAnoDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$SniffAnoDur, maleCRep$SniffAnoDur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(SniffAnoDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$SniffAnoDur, maleTGdx$SniffAnoDur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(SniffAnoDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$SniffAnoDur, maleTRep$SniffAnoDur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(SniffAnoDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$SniffAnoDur, maleCSham$SniffAnoDur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(SniffAnoDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$SniffAnoDur, maleCRep$SniffAnoDur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(SniffAnoDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SniffAnoDur, maleCGdx$SniffAnoDur)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(SniffAnoDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$SniffAnoDur, maleTSham$SniffAnoDur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(SniffAnoDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$SniffAnoDur, maleTRep$SniffAnoDur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(SniffAnoDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$SniffAnoDur, maleTGdx$SniffAnoDur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(SniffAnoDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$SniffAnoDur, femaleTSham$SniffAnoDur)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(SniffAnoDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$SniffAnoDur, femaleTRep$SniffAnoDur)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(SniffAnoDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SniffAnoDur, femaleTGdx$SniffAnoDur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(SniffAnoDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$SniffAnoDur, maleTSham$SniffAnoDur)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(SniffAnoDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$SniffAnoDur, maleTRep$SniffAnoDur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(SniffAnoDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$SniffAnoDur, maleTGdx$SniffAnoDur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(SniffAnoDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Sniff Anogenital - Freq
# Main Model
postSniffAnogenitalFreq <- lm(SniffAnoEv ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postSniffAnogenitalFreq)

wilcox.test(femaleCSham$SniffAnoEv, femaleCGdx$SniffAnoEv)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(SniffAnoEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SniffAnoEv, femaleCRep$SniffAnoEv)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(SniffAnoEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$SniffAnoEv, femaleTGdx$SniffAnoEv)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(SniffAnoEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$SniffAnoEv, femaleTRep$SniffAnoEv)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(SniffAnoEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$SniffAnoEv, maleCGdx$SniffAnoEv)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(SniffAnoEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$SniffAnoEv, maleCRep$SniffAnoEv)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(SniffAnoEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$SniffAnoEv, maleTGdx$SniffAnoEv)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(SniffAnoEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$SniffAnoEv, maleTRep$SniffAnoEv)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(SniffAnoEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$SniffAnoEv, maleCSham$SniffAnoEv)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(SniffAnoEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$SniffAnoEv, maleCRep$SniffAnoEv)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(SniffAnoEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SniffAnoEv, maleCGdx$SniffAnoEv)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(SniffAnoEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$SniffAnoEv, maleTSham$SniffAnoEv)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(SniffAnoEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$SniffAnoEv, maleTRep$SniffAnoEv)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(SniffAnoEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$SniffAnoEv, maleTGdx$SniffAnoEv)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(SniffAnoEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$SniffAnoEv, femaleTSham$SniffAnoEv)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(SniffAnoEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$SniffAnoEv, femaleTRep$SniffAnoEv)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(SniffAnoEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$SniffAnoEv, femaleTGdx$SniffAnoEv)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(SniffAnoEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$SniffAnoEv, maleTSham$SniffAnoEv)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(SniffAnoEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$SniffAnoEv, maleTRep$SniffAnoEv)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(SniffAnoEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$SniffAnoEv, maleTGdx$SniffAnoEv)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(SniffAnoEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Stretch - Freq
# Main Model
postStretchFreq <- lm(TransStretchEv ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postStretchFreq)

wilcox.test(femaleCSham$TransStretchEv, femaleCGdx$TransStretchEv)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransStretchEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransStretchEv, femaleCRep$TransStretchEv)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TransStretchEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransStretchEv, femaleTGdx$TransStretchEv)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransStretchEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransStretchEv, femaleTRep$TransStretchEv)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransStretchEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransStretchEv, maleCGdx$TransStretchEv)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransStretchEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransStretchEv, maleCRep$TransStretchEv)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransStretchEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransStretchEv, maleTGdx$TransStretchEv)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransStretchEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransStretchEv, maleTRep$TransStretchEv)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransStretchEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransStretchEv, maleCSham$TransStretchEv)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransStretchEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransStretchEv, maleCRep$TransStretchEv)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransStretchEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransStretchEv, maleCGdx$TransStretchEv)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TransStretchEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransStretchEv, maleTSham$TransStretchEv)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransStretchEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransStretchEv, maleTRep$TransStretchEv)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransStretchEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransStretchEv, maleTGdx$TransStretchEv)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransStretchEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransStretchEv, femaleTSham$TransStretchEv)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransStretchEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransStretchEv, femaleTRep$TransStretchEv)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(TransStretchEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransStretchEv, femaleTGdx$TransStretchEv)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransStretchEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransStretchEv, maleTSham$TransStretchEv)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransStretchEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransStretchEv, maleTRep$TransStretchEv)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransStretchEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransStretchEv, maleTGdx$TransStretchEv)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransStretchEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# Hor Activity - Dur
# Main Model
postHorAct <- lm(ActiveHDur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postHorAct)

wilcox.test(femaleCSham$ActiveHDur, femaleCGdx$ActiveHDur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(ActiveHDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$ActiveHDur, femaleCRep$ActiveHDur)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(ActiveHDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$ActiveHDur, femaleTGdx$ActiveHDur)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(ActiveHDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$ActiveHDur, femaleTRep$ActiveHDur)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(ActiveHDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$ActiveHDur, maleCGdx$ActiveHDur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(ActiveHDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$ActiveHDur, maleCRep$ActiveHDur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(ActiveHDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$ActiveHDur, maleTGdx$ActiveHDur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(ActiveHDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$ActiveHDur, maleTRep$ActiveHDur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(ActiveHDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$ActiveHDur, maleCSham$ActiveHDur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(ActiveHDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$ActiveHDur, maleCRep$ActiveHDur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(ActiveHDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$ActiveHDur, maleCGdx$ActiveHDur)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(ActiveHDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$ActiveHDur, maleTSham$ActiveHDur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(ActiveHDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$ActiveHDur, maleTRep$ActiveHDur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(ActiveHDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$ActiveHDur, maleTGdx$ActiveHDur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(ActiveHDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$ActiveHDur, femaleTSham$ActiveHDur)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(ActiveHDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$ActiveHDur, femaleTRep$ActiveHDur)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(ActiveHDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$ActiveHDur, femaleTGdx$ActiveHDur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(ActiveHDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$ActiveHDur, maleTSham$ActiveHDur)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(ActiveHDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$ActiveHDur, maleTRep$ActiveHDur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(ActiveHDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$ActiveHDur, maleTGdx$ActiveHDur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(ActiveHDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Hor Act - Freq
# Main Model
postHorActFreq <- lm(ActiveHEv ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postHorActFreq)

wilcox.test(femaleCSham$ActiveHEv, femaleCGdx$ActiveHEv)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(ActiveHEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$ActiveHEv, femaleCRep$ActiveHEv)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(ActiveHEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$ActiveHEv, femaleTGdx$ActiveHEv)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(ActiveHEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$ActiveHEv, femaleTRep$ActiveHEv)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(ActiveHEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$ActiveHEv, maleCGdx$ActiveHEv)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(ActiveHEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$ActiveHEv, maleCRep$ActiveHEv)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(ActiveHEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$ActiveHEv, maleTGdx$ActiveHEv)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(ActiveHEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$ActiveHEv, maleTRep$ActiveHEv)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(ActiveHEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$ActiveHEv, maleCSham$ActiveHEv)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(ActiveHEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$ActiveHEv, maleCRep$ActiveHEv)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(ActiveHEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$ActiveHEv, maleCGdx$ActiveHEv)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(ActiveHEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$ActiveHEv, maleTSham$ActiveHEv)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(ActiveHEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$ActiveHEv, maleTRep$ActiveHEv)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(ActiveHEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$ActiveHEv, maleTGdx$ActiveHEv)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(ActiveHEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$ActiveHEv, femaleTSham$ActiveHEv)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(ActiveHEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$ActiveHEv, femaleTRep$ActiveHEv)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(ActiveHEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$ActiveHEv, femaleTGdx$ActiveHEv)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(ActiveHEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$ActiveHEv, maleTSham$ActiveHEv)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(ActiveHEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$ActiveHEv, maleTRep$ActiveHEv)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(ActiveHEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$ActiveHEv, maleTGdx$ActiveHEv)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(ActiveHEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Vert Activity - Dur
# Main Model
postVertActDur <- lm(ActiveVDur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postVertActDur)

wilcox.test(femaleCSham$ActiveVDur, femaleCGdx$ActiveVDur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(ActiveVDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$ActiveVDur, femaleCRep$ActiveVDur)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(ActiveVDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$ActiveVDur, femaleTGdx$ActiveVDur)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(ActiveVDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$ActiveVDur, femaleTRep$ActiveVDur)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(ActiveVDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$ActiveVDur, maleCGdx$ActiveVDur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(ActiveVDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$ActiveVDur, maleCRep$ActiveVDur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(ActiveVDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$ActiveVDur, maleTGdx$ActiveVDur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(ActiveVDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$ActiveVDur, maleTRep$ActiveVDur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(ActiveVDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$ActiveVDur, maleCSham$ActiveVDur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(ActiveVDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$ActiveVDur, maleCRep$ActiveVDur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(ActiveVDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$ActiveVDur, maleCGdx$ActiveVDur)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(ActiveVDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$ActiveVDur, maleTSham$ActiveVDur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(ActiveVDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$ActiveVDur, maleTRep$ActiveVDur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(ActiveVDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$ActiveVDur, maleTGdx$ActiveVDur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(ActiveVDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$ActiveVDur, femaleTSham$ActiveVDur)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(ActiveVDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$ActiveVDur, femaleTRep$ActiveVDur)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(ActiveVDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$ActiveVDur, femaleTGdx$ActiveVDur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(ActiveVDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$ActiveVDur, maleTSham$ActiveVDur)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(ActiveVDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$ActiveVDur, maleTRep$ActiveVDur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(ActiveVDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$ActiveVDur, maleTGdx$ActiveVDur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(ActiveVDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Active Vert - Freq
# Main Model
postVertActFreq <- lm(ActiveVEv ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postVertActFreq)

wilcox.test(femaleCSham$ActiveVEv, femaleCGdx$ActiveVEv)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(ActiveVEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$ActiveVEv, femaleCRep$ActiveVEv)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(ActiveVEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$ActiveVEv, femaleTGdx$ActiveVEv)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(ActiveVEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$ActiveVEv, femaleTRep$ActiveVEv)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(ActiveVEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$ActiveVEv, maleCGdx$ActiveVEv)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(ActiveVEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$ActiveVEv, maleCRep$ActiveVEv)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(ActiveVEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$ActiveVEv, maleTGdx$ActiveVEv)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(ActiveVEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$ActiveVEv, maleTRep$ActiveVEv)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(ActiveVEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$ActiveVEv, maleCSham$ActiveVEv)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(ActiveVEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$ActiveVEv, maleCRep$ActiveVEv)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(ActiveVEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$ActiveVEv, maleCGdx$ActiveVEv)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(ActiveVEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$ActiveVEv, maleTSham$ActiveVEv)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(ActiveVEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$ActiveVEv, maleTRep$ActiveVEv)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(ActiveVEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$ActiveVEv, maleTGdx$ActiveVEv)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(ActiveVEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$ActiveVEv, femaleTSham$ActiveVEv)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(ActiveVEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$ActiveVEv, femaleTRep$ActiveVEv)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(ActiveVEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$ActiveVEv, femaleTGdx$ActiveVEv)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(ActiveVEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$ActiveVEv, maleTSham$ActiveVEv)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(ActiveVEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$ActiveVEv, maleTRep$ActiveVEv)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(ActiveVEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$ActiveVEv, maleTGdx$ActiveVEv)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(ActiveVEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Inactive - Dur
# Main Model
postInactiveDur <- lm(TransInactiveDur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postInactiveDur)

wilcox.test(femaleCSham$TransInactiveDur, femaleCGdx$TransInactiveDur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransInactiveDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransInactiveDur, femaleCRep$TransInactiveDur)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TransInactiveDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransInactiveDur, femaleTGdx$TransInactiveDur)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransInactiveDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransInactiveDur, femaleTRep$TransInactiveDur)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransInactiveDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransInactiveDur, maleCGdx$TransInactiveDur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransInactiveDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransInactiveDur, maleCRep$TransInactiveDur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransInactiveDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransInactiveDur, maleTGdx$TransInactiveDur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransInactiveDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransInactiveDur, maleTRep$TransInactiveDur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransInactiveDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransInactiveDur, maleCSham$TransInactiveDur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransInactiveDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransInactiveDur, maleCRep$TransInactiveDur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransInactiveDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransInactiveDur, maleCGdx$TransInactiveDur)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TransInactiveDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransInactiveDur, maleTSham$TransInactiveDur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransInactiveDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransInactiveDur, maleTRep$TransInactiveDur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransInactiveDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransInactiveDur, maleTGdx$TransInactiveDur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransInactiveDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransInactiveDur, femaleTSham$TransInactiveDur)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransInactiveDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransInactiveDur, femaleTRep$TransInactiveDur)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(TransInactiveDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransInactiveDur, femaleTGdx$TransInactiveDur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransInactiveDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransInactiveDur, maleTSham$TransInactiveDur)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransInactiveDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransInactiveDur, maleTRep$TransInactiveDur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransInactiveDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransInactiveDur, maleTGdx$TransInactiveDur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransInactiveDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# Inactive - Freq
# Main Model
postInactiveFreq <- lm(TransInactiveEv ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postInactiveFreq)

wilcox.test(femaleCSham$TransInactiveEv, femaleCGdx$TransInactiveEv)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransInactiveEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransInactiveEv, femaleCRep$TransInactiveEv)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TransInactiveEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransInactiveEv, femaleTGdx$TransInactiveEv)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransInactiveEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransInactiveEv, femaleTRep$TransInactiveEv)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransInactiveEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransInactiveEv, maleCGdx$TransInactiveEv)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransInactiveEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransInactiveEv, maleCRep$TransInactiveEv)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransInactiveEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransInactiveEv, maleTGdx$TransInactiveEv)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransInactiveEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransInactiveEv, maleTRep$TransInactiveEv)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransInactiveEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransInactiveEv, maleCSham$TransInactiveEv)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransInactiveEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransInactiveEv, maleCRep$TransInactiveEv)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransInactiveEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransInactiveEv, maleCGdx$TransInactiveEv)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TransInactiveEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransInactiveEv, maleTSham$TransInactiveEv)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransInactiveEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransInactiveEv, maleTRep$TransInactiveEv)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransInactiveEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransInactiveEv, maleTGdx$TransInactiveEv)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransInactiveEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransInactiveEv, femaleTSham$TransInactiveEv)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransInactiveEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransInactiveEv, femaleTRep$TransInactiveEv)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(TransInactiveEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransInactiveEv, femaleTGdx$TransInactiveEv)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransInactiveEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransInactiveEv, maleTSham$TransInactiveEv)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransInactiveEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransInactiveEv, maleTRep$TransInactiveEv)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransInactiveEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransInactiveEv, maleTGdx$TransInactiveEv)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransInactiveEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# Groom - Dur
# Main Model
postGroomDur <- lm(GroomDur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postGroomDur)

wilcox.test(femaleCSham$GroomDur, femaleCGdx$GroomDur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(GroomDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$GroomDur, femaleCRep$GroomDur)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(GroomDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$GroomDur, femaleTGdx$GroomDur)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(GroomDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$GroomDur, femaleTRep$GroomDur)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(GroomDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$GroomDur, maleCGdx$GroomDur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(GroomDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$GroomDur, maleCRep$GroomDur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(GroomDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$GroomDur, maleTGdx$GroomDur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(GroomDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$GroomDur, maleTRep$GroomDur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(GroomDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$GroomDur, maleCSham$GroomDur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(GroomDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$GroomDur, maleCRep$GroomDur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(GroomDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$GroomDur, maleCGdx$GroomDur)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(GroomDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$GroomDur, maleTSham$GroomDur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(GroomDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$GroomDur, maleTRep$GroomDur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(GroomDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$GroomDur, maleTGdx$GroomDur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(GroomDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$GroomDur, femaleTSham$GroomDur)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(GroomDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$GroomDur, femaleTRep$GroomDur)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(GroomDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$GroomDur, femaleTGdx$GroomDur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(GroomDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$GroomDur, maleTSham$GroomDur)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(GroomDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$GroomDur, maleTRep$GroomDur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(GroomDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$GroomDur, maleTGdx$GroomDur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(GroomDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Groom - Freq
# Main Model
postGroomFreq <- lm(GroomEv ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postGroomFreq)

wilcox.test(femaleCSham$GroomEv, femaleCGdx$GroomEv)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(GroomEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$GroomEv, femaleCRep$GroomEv)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(GroomEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$GroomEv, femaleTGdx$GroomEv)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(GroomEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$GroomEv, femaleTRep$GroomEv)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(GroomEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$GroomEv, maleCGdx$GroomEv)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(GroomEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$GroomEv, maleCRep$GroomEv)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(GroomEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$GroomEv, maleTGdx$GroomEv)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(GroomEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$GroomEv, maleTRep$GroomEv)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(GroomEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$GroomEv, maleCSham$GroomEv)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(GroomEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$GroomEv, maleCRep$GroomEv)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(GroomEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$GroomEv, maleCGdx$GroomEv)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(GroomEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$GroomEv, maleTSham$GroomEv)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(GroomEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$GroomEv, maleTRep$GroomEv)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(GroomEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$GroomEv, maleTGdx$GroomEv)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(GroomEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$GroomEv, femaleTSham$GroomEv)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(GroomEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$GroomEv, femaleTRep$GroomEv)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(GroomEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$GroomEv, femaleTGdx$GroomEv)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(GroomEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$GroomEv, maleTSham$GroomEv)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(GroomEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$GroomEv, maleTRep$GroomEv)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(GroomEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$GroomEv, maleTGdx$GroomEv)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(GroomEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# Dig - Dur
# Main Model
postDigDur <- lm(TransDigDur ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postDigDur)

wilcox.test(femaleCSham$TransDigDur, femaleCGdx$TransDigDur)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransDigDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransDigDur, femaleCRep$TransDigDur)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TransDigDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransDigDur, femaleTGdx$TransDigDur)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransDigDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransDigDur, femaleTRep$TransDigDur)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransDigDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransDigDur, maleCGdx$TransDigDur)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransDigDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransDigDur, maleCRep$TransDigDur)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransDigDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransDigDur, maleTGdx$TransDigDur)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransDigDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransDigDur, maleTRep$TransDigDur)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransDigDur ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransDigDur, maleCSham$TransDigDur)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransDigDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransDigDur, maleCRep$TransDigDur)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransDigDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransDigDur, maleCGdx$TransDigDur)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TransDigDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransDigDur, maleTSham$TransDigDur)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransDigDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransDigDur, maleTRep$TransDigDur)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransDigDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransDigDur, maleTGdx$TransDigDur)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransDigDur ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransDigDur, femaleTSham$TransDigDur)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransDigDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransDigDur, femaleTRep$TransDigDur)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(TransDigDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransDigDur, femaleTGdx$TransDigDur)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransDigDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransDigDur, maleTSham$TransDigDur)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransDigDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransDigDur, maleTRep$TransDigDur)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransDigDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransDigDur, maleTGdx$TransDigDur)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransDigDur ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

# Dig - Freq
# Main Model
postDigEv <- lm(TransDigEv ~ Treatment * Sex * Gonad, data = stfpInteractionPost)
Anova(postDigEv)

wilcox.test(femaleCSham$TransDigEv, femaleCGdx$TransDigEv)
testData <- rbind(femaleCSham, femaleCGdx)
wTest <- wilcox_test(TransDigEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransDigEv, femaleCRep$TransDigEv)
testData <- rbind(femaleCGdx, femaleCRep)
wTest <- wilcox_test(TransDigEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransDigEv, femaleTGdx$TransDigEv)
testData <- rbind(femaleTSham, femaleTGdx)
wTest <- wilcox_test(TransDigEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransDigEv, femaleTRep$TransDigEv)
testData <- rbind(femaleTGdx, femaleTRep)
wTest <- wilcox_test(TransDigEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransDigEv, maleCGdx$TransDigEv)
testData <- rbind(maleCSham, maleCGdx)
wTest <- wilcox_test(TransDigEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransDigEv, maleCRep$TransDigEv)
testData <- rbind(maleCGdx, maleCRep)
wTest <- wilcox_test(TransDigEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleTSham$TransDigEv, maleTGdx$TransDigEv)
testData <- rbind(maleTSham, maleTGdx)
wTest <- wilcox_test(TransDigEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleTGdx$TransDigEv, maleTRep$TransDigEv)
testData <- rbind(maleTGdx, maleTRep)
wTest <- wilcox_test(TransDigEv ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - male to female
wilcox.test(femaleCSham$TransDigEv, maleCSham$TransDigEv)
testData <- rbind(femaleCSham, maleCSham)
wTest <- wilcox_test(TransDigEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransDigEv, maleCRep$TransDigEv)
testData <- rbind(femaleCRep, maleCRep)
wTest <- wilcox_test(TransDigEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransDigEv, maleCGdx$TransDigEv)
testData <- rbind(femaleCGdx, maleCGdx)
wTest <- wilcox_test(TransDigEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(femaleTSham$TransDigEv, maleTSham$TransDigEv)
testData <- rbind(femaleTSham, maleTSham)
wTest <- wilcox_test(TransDigEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTRep$TransDigEv, maleTRep$TransDigEv)
testData <- rbind(femaleTRep, maleTRep)
wTest <- wilcox_test(TransDigEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTGdx$TransDigEv, maleTGdx$TransDigEv)
testData <- rbind(femaleTGdx, maleTGdx)
wTest <- wilcox_test(TransDigEv ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))


# planned comparisons - test to control
wilcox.test(femaleCSham$TransDigEv, femaleTSham$TransDigEv)
testData <- rbind(femaleCSham, femaleTSham)
wTest <- wilcox_test(TransDigEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCRep$TransDigEv, femaleTRep$TransDigEv)
testData <- rbind(femaleCRep, femaleTRep)
wTest <- wilcox_test(TransDigEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleCGdx$TransDigEv, femaleTGdx$TransDigEv)
testData <- rbind(femaleCGdx, femaleTGdx)
wTest <- wilcox_test(TransDigEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

wilcox.test(maleCSham$TransDigEv, maleTSham$TransDigEv)
testData <- rbind(maleCSham, maleTSham)
wTest <- wilcox_test(TransDigEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCRep$TransDigEv, maleTRep$TransDigEv)
testData <- rbind(maleCRep, maleTRep)
wTest <- wilcox_test(TransDigEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleCGdx$TransDigEv, maleTGdx$TransDigEv)
testData <- rbind(maleCGdx, maleTGdx)
wTest <- wilcox_test(TransDigEv ~ Treatment, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))

### quick


wilcox.test(subset(malePre, Treatment == "C")$DominanceScoreOccur, mu = 0)


##################################
# Graphing
##################################

### Pre-puberty

TotalSocialDurGraph <- interactionGraphPre(stfpInteractionPre, "(TotalSocialDuration/1000)", "Duration (Seconds)", "Social Behaviour - Duration", 1200, nonParametric = F, showLegend = T, "total-social-duration.pdf")
TotalSocialFreqGraph <- interactionGraphPre(stfpInteractionPre, "TotalSocialOccur", "Frequency", "Social Behaviour - Frequency", 300, nonParametric = F, showLegend = F, "total-social-frequency.pdf")
NonSocialDurGraph <- interactionGraphPre(stfpInteractionPre, "(NonSocialDuration/1000)", "Duration (Seconds)", "Non-Social Behaviour - Duration", 1200, nonParametric = F, showLegend = F, "nonsocial-behaviour-duration.pdf")
legend <- getLegend(TotalSocialDurGraph)
TotalSocialDurGraph <- TotalSocialDurGraph + theme(legend.position = "none")
pdf("output/PrePuberty/social-composite.pdf", 10, 10)
grid.arrange(TotalSocialFreqGraph, TotalSocialDurGraph, NonSocialDurGraph, legend, nrow = 2, ncol = 2, widths = c(5, 5), heights = c(5, 5))
dev.off()

DomScoreDurGraph <- dominanceGraphPre(stfpInteractionPre, "(DominanceScoreDuration/1000)", "Duration (Seconds)", "B: Dominance Score - Duration", 0, 120, nonParametric = F, showLegend = F, "dominance-score-duration.pdf")
DomScoreFreqGraph <- dominanceGraphPre(stfpInteractionPre, "DominanceScoreOccur", "Frequency", "C: Dominance Score - Frequency", 0, 30, nonParametric = T, showLegend = F, "dominance-score-frequency.pdf")
AgonisticFreqGraph <- interactionGraphPre(stfpInteractionPre, "AgonisticTotalOccur", "Frequency", "A: Agonistic Behaviour - Frequency", 80, nonParametric = F, showLegend = F, "agonistic-behaviour-frequency.pdf")
AgonisticDelFreqGraph <- interactionGraphPre(stfpInteractionPre, "AgonisticDeliveredOccur", "Frequency", "D: Agonistic Behaviour Delivered - Frequency", 80, nonParametric = F, showLegend = F, "agonistic-delivered-frequency.pdf")
AgonisticRecDurGraph <- interactionGraphPre(stfpInteractionPre, "(AgonisticReceivedDuration/1000)", "Duration (Seconds)", "E: Agonistic Behaviour Received - Duration", 100, nonParametric = F, showLegend = F, "agonistic-received-duration.pdf")
SubmissiveGraph <- interactionGraphPre(stfpInteractionPre, "(SubDur/1000)", "Duration (Seconds)", "F: Submissive Behaviour", 80, nonParametric = F, showLegend = F, "submissive-duration.pdf")
AttackDelGraph <- interactionGraphPre(stfpInteractionPre, "AttackDelEv", "Frequency", "I: Attacks Delivered", 20, nonParametric = F, showLegend = F, "attacks-delivered.pdf")
AttackRecGraph <- interactionGraphPre(stfpInteractionPre, "AttackRecEv", "Frequency", "J: Attacks Received", 20, nonParametric = F, showLegend = F, "attacks-received.pdf")
OpenAggDurGraph <- interactionGraphPre(stfpInteractionPre, "(OpenAggDur/1000)", "Duration (Seconds)", "G: Open Aggression - Duration", 60, nonParametric = F, showLegend = F, "open-aggression-duration.pdf")
RitAggDurGraph <- interactionGraphPre(stfpInteractionPre, "(RitAggDur/1000)", "Duration (Seconds)", "H: Ritualized Aggression - Duration", 100, nonParametric = F, showLegend = F, "ritualized-aggression-duration.pdf")
pdf("output/PrePuberty/agonistic-composite.pdf", 21, 20)
grid.arrange(AgonisticFreqGraph, DomScoreDurGraph, DomScoreFreqGraph, AgonisticDelFreqGraph, AgonisticRecDurGraph, SubmissiveGraph, OpenAggDurGraph, RitAggDurGraph, AttackDelGraph, AttackRecGraph, nrow = 4, ncol = 3, widths = c(7, 7, 7), heights = c(5, 5, 5, 5))
dev.off()

FollowGraph <- interactionGraphPre(stfpInteractionPre, "(FollowDur/1000)", "Duration (Seconds)", "Follow Opponent", 15, nonParametric = F, showLegend = T, "follow-opponent-duration.pdf")
DefenseGraph <- interactionGraphPre(stfpInteractionPre, "(DefenseDur/1000)", "Duration (Seconds)", "Defensive Upright Posture", 30, nonParametric = F, showLegend = F, "defensive-upright-duration.pdf")
SubmissiveGraph <- interactionGraphPre(stfpInteractionPre, "(SubDur/1000)", "Duration (Seconds)", "Submissive Behaviour", 80, nonParametric = F, showLegend = F, "submissive-duration.pdf")
legend <- getLegend(FollowGraph)
FollowGraph <- FollowGraph + theme(legend.position = "none")
pdf("output/PrePuberty/submissive-composite.pdf", 10, 10)
grid.arrange(FollowGraph, DefenseGraph, SubmissiveGraph, legend, nrow = 2, ncol = 2, widths = c(5, 5))
dev.off()

InvestigationDurGraph <- interactionGraphPre(stfpInteractionPre, "(SocialInvestigationDuration/1000)", "Duration (Seconds)", "A: Social Investigation - Duration", 600, nonParametric = F, showLegend = F, "social-investigation-duration.pdf")
SniffAnogenitalGraph <- interactionGraphPre(stfpInteractionPre, "SniffAnoEv", "Freqency", "D: Anogenital Investigation - Frequency", 60, nonParametric = F, showLegend = F, "sniff-anogenital-duration.pdf")
SniffBodyGraph <- interactionGraphPre(stfpInteractionPre, "(SniffBodyDur/1000)", "Duration (Seconds)", "C: Body Investigation - Duration", 300, nonParametric = F, showLegend = F, "sniff-body-duration.pdf")
SniffHeadGraph <- interactionGraphPre(stfpInteractionPre, "(SniffHeadDur/1000)", "Duration (Seconds)", "B: Oronasal Investigation - Duration", 300, nonParametric = F, showLegend = F, "sniff-oronasal-duration.pdf")
pdf("output/PrePuberty/investigation-composite.pdf", 12, 8)
grid.arrange(InvestigationDurGraph, SniffHeadGraph, SniffBodyGraph, SniffAnogenitalGraph, nrow = 2, ncol = 2, widths = c(6, 6), heights = c(4, 4))
dev.off()


### Post-puberty

## Social/NonSocial

interactionPanel(stfpInteractionPost, "(TotalSocialDuration/1000)", "Duration (Seconds)", "Social Behaviour - Duration", 1500, "total-social-duration.pdf", T)
interactionPanel(stfpInteractionPost, "TotalSocialOccur", "Frequency", "Social Behaviour - Frequency", 500, "total-social-frequency.pdf", T)

interactionPanel(stfpInteractionPost, "(NonSocialDuration/1000)", "Duration (Seconds)", "Non-Social Behaviour - Duration", 1000, "nonsocial-behaviour-duration.pdf", T)
interactionPanel(stfpInteractionPost, "NonSocialOccur", "Frequency", "Non-Social Behaviour - Frequency", 400, "nonsocial-behaviour-frequency.pdf", T)

interactionPanel(stfpInteractionPost, "(NonSocialLocomotorDuration/1000)", "Duration (Seconds)", "Non-Social Locomotor Behaviour - Duration", 600, "nonsocial-locomotive-duration.pdf", T)
interactionPanel(stfpInteractionPost, "NonSocialLocomotorOccur", "Frequency", "Non-Social Locomotor Behaviour - Frequency", 300, "nonsocial-locomotive-frequency.pdf", T)

## Agonistic

dominancePanel(stfpInteractionPost, "(DominanceScoreDuration/1000)", "Duration (Seconds)", "Dominance Score - Duration", -200, 300, "dominance-score-duration.pdf", T)
dominancePanel(stfpInteractionPost, "DominanceScoreOccur", "Frequency", "Dominance Score - Frequency", -100, 100, "dominance-score-frequency.pdf", T)


interactionPanel(stfpInteractionPost, "(AgonisticTotalDuration/1000)", "Duration (Seconds)", "Agonistic Behaviour - Duration", 500, "agonistic-behaviour-duration.pdf", T)
interactionPanel(stfpInteractionPost, "AgonisticTotalOccur", "Frequency", "Agonistic Behaviour - Frequency", 100, "agonistic-behaviour-frequency.pdf", T)


interactionPanel(stfpInteractionPost, "(AgonisticDeliveredDuration/1000)", "Duration (Seconds)", "Agonistic Behaviour Delivered - Duration", 300, "agonistic-delivered-duration.pdf", T)
interactionPanel(stfpInteractionPost, "AgonisticDeliveredOccur", "Frequency", "Agonistic Behaviour Delivered - Frequency", 100, "agonistic-delivered-frequency.pdf", T)


interactionPanel(stfpInteractionPost, "(AgonisticReceivedDuration/1000)", "Duration (Seconds)", "Agonistic Behaviour Received - Duration", 300, "agonistic-received-duration.pdf", T)


## Aggression

interactionPanel(stfpInteractionPost, "AttackDelEv", "Frequency", "Attacks Delivered", 50, "attacks-delivered.pdf", T)

interactionPanel(stfpInteractionPost, "(OpenAggDur/1000)", "Duration (Seconds)", "Open Aggression - Duration", 25, "open-aggression-duration.pdf", T)

interactionPanel(stfpInteractionPost, "(RitAggDur/1000)", "Duration (Seconds)", "Ritualized Aggression - Duration", 100, "ritualized-aggression-duration.pdf", T)

## Dominance/Submissive

interactionPanel(stfpInteractionPost, "(DomDur/1000)", "Duration (Seconds)", "Dominance Behaviour - Duration", 300, "dominance-behaviour-duration.pdf", T)

interactionPanel(stfpInteractionPost, "(SubDur/1000)", "Duration (Seconds)", "Submissive Behaviour - Duration", 300, "submissive-behaviour-duration.pdf", T)
interactionPanel(stfpInteractionPost, "(SubDur/1000)", "Duration (Seconds)", "Submissive Behaviour - Duration", 300, "submissive-behaviour-duration.pdf", T)


## Social Investigation

interactionPanel(stfpInteractionPost, "(SniffAnoDur/1000)", "Duration (Seconds)", " Anogenital Investigation - Duration", 300, "sniff-anogenital-duration.pdf", T)

SniffHeadDurationPanel <- interactionPanel(stfpInteractionPost, "(SniffHeadDur/1000)", "Duration (Seconds)", "Oronasal Investigation - Duration", 400, "sniff-head-duration.pdf", T)
SniffHeadFreqPanel <- interactionPanel(stfpInteractionPost, "SniffHeadEv", "Frequency", "Oronasal Investigation - Frequency", 120, "sniff-head-frequency.pdf", T)

interactionPanel(stfpInteractionPost, "(SniffBodyDur/1000)", "Duration (Seconds)", "Investigation of Demonstrator's Body - Duration", 500, "sniff-body-duration.pdf", T)
interactionPanel(stfpInteractionPost, "SniffBodyEv", "Frequency", "Investigation of Demonstrator's Body - Frequency", 200, "sniff-body-frequency", T)


interactionPanel(stfpInteractionPost, "(SocialInvestigationDuration/1000)", "Duration (Seconds)", "Social Investigation - Duration", 1000, "social-investigation-duration.pdf", T)
interactionPanel(stfpInteractionPost, "SocialInvestigationOccur", "Frequency", "Social Investigation - Frequency", 400, "social-investigation-frequency.pdf", T)


## Activity

interactionPanel(stfpInteractionPost, "(TotalActivityDuration/1000)", "Duration (Seconds)", "Total Activity - Duration", 1500, "total-activity-duration.pdf", T)
interactionPanel(stfpInteractionPost, "TotalActivityOccur", "Frequency", "Total Activity - Frequency", 800, "total-activity-frequency.pdf", T)

interactionPanel(stfpInteractionPost, "(InactiveTogetherDur/1000)", "Duration (Seconds)", "Social Inactivity - Duration", 400, "inactive-together-duration.pdf", T)

interactionPanel(stfpInteractionPost, "(InactiveDur/1000)", "Duration (Seconds)", "Solitary Inactivity - Duration", 150, "inactive-duration.pdf", T)

interactionPanel(stfpInteractionPost, "(ActiveHDur/1000)", "Duration (Seconds)", "Horizontal Exploration - Duration", 300, "active-horizontal-duration.pdf", T)
interactionPanel(stfpInteractionPost, "ActiveHEv", "Frequency", "Horizontal Exploration - Frequency", 200, "active-horizontal-frequency.pdf", T)

interactionPanel(stfpInteractionPost, "(ActiveVDur/1000)", "Duration (Seconds)", "Vertical Exploration - Duration", 300, "active-vertical-duration.pdf", T)
interactionPanel(stfpInteractionPost, "ActiveVEv", "Frequency", "Vertical Exploration - Frequency", 150, "active-vertical-frequency.pdf", T)

interactionPanel(stfpInteractionPost, "(GroomDur/1000)", "Duration (Seconds)", "Groom", 400, "Activity/groom-duration.pdf", F)
interactionPanel(stfpInteractionPost, "GroomEv", "Frequency", "Groom", 60, "Activity/groom-frequency.pdf", F)

interactionPanel(stfpInteractionPost, "(DigDur/1000)", "Duration (Seconds)", "Dig", 15, "Activity/dig-duration.pdf", F)
interactionPanel(stfpInteractionPost, "DigEv", "Frequency", "Dig", 5, "Activity/dig-frequency.pdf", F)
