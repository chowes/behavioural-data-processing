library(ggplot2)
library(grid)
library(ez)
library(car)
library(phia)
library(lsmeans)
library(gridExtra)

source("src/data.r")

intruderData <- read.csv("data/intruder.csv")
intruderData <- createComposites(intruderData)

### Data Cleaning

boxplot(intruderData$TransTotalActivityDur)
boxplot(log(intruderPost$GroomEv + 1))
boxplot(sqrt(intruderPost$GroomEv + 1))


shapiro.test(intruderPost$SniffAnoEv)
shapiro.test(sqrt(intruderPost$NonSocialLocomotorOccur + 1))

intruderData$TransTotalActivityDur <- sqrtTransform(intruderData$TotalActivityDuration)
intruderData$TransTotalAgonisticDur <- lnTransform(intruderData$AgonisticTotalDuration)
intruderData$TransAgonisticDeliveredDur <- sqrtTransform(intruderData$AgonisticDeliveredDuration)
intruderData$TransAgonisticDeliveredFreq <- lnTransform(intruderData$AgonisticDeliveredOccur)
intruderData$TransAgonisticReceivedDur <- sqrtTransform(intruderData$AgonisticReceivedDuration)
intruderData$TransAgonisticReceivedFreq <- sqrtTransform(intruderData$AgonisticReceivedOccur)
intruderData$TransNonSocialDur <- sqrtTransform(intruderData$NonSocialDuration)
intruderData$TransNonSocialFreq <- sqrtTransform(intruderData$NonSocialOccur)
intruderData$TransActiveNonSocialDur <- sqrtTransform(intruderData$NonSocialLocomotorDuration)
intruderData$TransActiveNonSocialFreq <- sqrtTransform(intruderData$NonSocialLocomotorOccur)
intruderData$TransInactiveNonSocialDur <- lnTransform(intruderData$InactiveNonSocialDuration)
intruderData$TransInactiveNonSocialFreq <- lnTransform(intruderData$InactiveNonSocialOccur)
intruderData$TransRitualizedAggressionDur <- lnTransform(intruderData$RitAggDur)
intruderData$TransRitualizedAggressionFreq <- lnTransform(intruderData$RitAggEv)
intruderData$TransAttacksReceived <- lnTransform(intruderData$AttackRecEv)
intruderData$TransDominanceDur <- sqrtTransform(intruderData$DomDur)
intruderData$TransDominanceFreq <- lnTransform(intruderData$DomEv)
intruderData$TransFollowDur <- sqrtTransform(intruderData$FollowDur)
intruderData$TransFollowFreq <- lnTransform(intruderData$FollowEv)
intruderData$TransSubmissiveDur <- sqrtTransform(intruderData$SubDur)
intruderData$TransSubmissiveFreq <- sqrtTransform(intruderData$SubEv)
intruderData$TransAnogenitalDur <- lnTransform(intruderData$SniffAnoDur)
intruderData$TransAnogenitalFreq <- lnTransform(intruderData$SniffAnoEv)
intruderData$TransSocialInactiveDur <- lnTransform(intruderData$InactiveTogetherDur)
intruderData$TransSocialInactiveFreq <- lnTransform(intruderData$InactiveTogetherEv)
intruderData$TransDigDur <- lnTransform(intruderData$DigDur)
intruderData$TransDigFreq <- lnTransform(intruderData$DigEv)
intruderData$TransInactiveDur <- lnTransform(intruderData$InactiveDur)
intruderData$TransInactiveFreq <- lnTransform(intruderData$InactiveEv)
intruderData$TransGroomDur <- lnTransform(intruderData$GroomDur)
intruderData$TransGroomFreq <- lnTransform(intruderData$GroomEv)


### subsets

intruderPre <- subset(intruderData, intruderData$Gonad == "PRE")
intruderPost <- subset(intruderData, intruderData$Gonad != "PRE")
intruderEstrus <- subset(intruderData, !is.na(intruderData$Estrus))

intruderPost <- createFactors(intruderPost)

maleCSham <- subset(intruderPost, Sex == "M" & Treatment == "C" & Gonad == "SHAM")
maleCGdx <- subset(intruderPost, Sex == "M" & Treatment == "C" & Gonad == "GDX")
maleCRep <- subset(intruderPost, Sex == "M" & Treatment == "C" & Gonad == "REP")
femaleCSham <- subset(intruderPost, Sex == "F" & Treatment == "C" & Gonad == "SHAM")
femaleCGdx <- subset(intruderPost, Sex == "F" & Treatment == "C" & Gonad == "GDX")
femaleCRep <- subset(intruderPost, Sex == "F" & Treatment == "C" & Gonad == "REP")
maleTSham <- subset(intruderPost, Sex == "M" & Treatment == "T" & Gonad == "SHAM")
maleTGdx <- subset(intruderPost, Sex == "M" & Treatment == "T" & Gonad == "GDX")
maleTRep <- subset(intruderPost, Sex == "M" & Treatment == "T" & Gonad == "REP")
femaleTSham <- subset(intruderPost, Sex == "F" & Treatment == "T" & Gonad == "SHAM")
femaleTGdx <- subset(intruderPost, Sex == "F" & Treatment == "T" & Gonad == "GDX")
femaleTRep <- subset(intruderPost, Sex == "F" & Treatment == "T" & Gonad == "REP")





##################################
# Graphing
##################################

### Pre-puberty


### Post-puberty

# social 

intruderPanel(intruderPost, "(TotalSocialDuration/1000)", "Duration (Seconds)", "Total Social", 1000, "total-social-duration.pdf", T)
intruderPanel(intruderPost, "TotalSocialOccur", "Frequency", "Total Social", 500, "total-social-frequency.pdf", T)

intruderPanel(intruderPost, "(SniffAnoDur/1000)", "Duration (Seconds)", "Sniff Anogenital Area", 300, "sniff-anogenital-duration.pdf", T)
intruderPanel(intruderPost, "SniffAnoEv", "Frequency", "Sniff Anogenital Area", 150, "sniff-anogenital-frequency.pdf", T)

intruderPanel(intruderPost, "(SniffHeadDur/1000)", "Duration (Seconds)", "Sniff Head", 200, "sniff-head-duration.pdf", T)
intruderPanel(intruderPost, "SniffHeadEv", "Frequency", "Sniff Head", 80, "sniff-head-frequency.pdf", T)

intruderPanel(intruderPost, "(SniffBodyDur/1000)", "Duration (Seconds)", "Sniff Body", 400, "sniff-body-duration.pdf", T)
intruderPanel(intruderPost, "SniffBodyEv", "Frequency", "Sniff Body", 200, "sniff-body-frequency.pdf", T)

intruderPanel(intruderPost, "(SocialInvestigationDuration/1000)", "Duration (Seconds)", "Social Investigation", 800, "social-investigation-duration.pdf", T)
intruderPanel(intruderPost, "SocialInvestigationOccur", "Frequency", "Social Investigation", 400, "social-investigation-frequency.pdf", T)

intruderPanel(intruderPost, "(SubDur/1000)", "Duration (Seconds)", "Submissive Behaviour", 150, "submissive-behaviour-duration.pdf", T)
intruderPanel(intruderPost, "SubEv", "Frequency", "Submissive Behaviour", 50, "submissive-behaviour-frequency.pdf", T)

intruderPanel(intruderPost, "AttackDelEv", "Frequency", "Attacks Delivered", 25, "attacks-delivered.pdf", T)
intruderPanel(intruderPost, "AttackRecEv", "Frequency", "Attacks Received", 25, "attacks-received.pdf", T)

intruderPanel(intruderPost, "(AgonisticTotalDuration/1000)", "Duration (Seconds)", "Agonistic Behaviour", 200, "agonistic-behaviour-duration.pdf", T)
intruderPanel(intruderPost, "AgonisticTotalOccur", "Frequency", "Agonistic Behaviour", 100, "agonistic-behaviour-frequency.pdf", T)

intruderPanel(intruderPost, "(AgonisticDeliveredDuration/1000)", "Duration (Seconds)", "Agonistic Behaviour Delivered", 150, "agonistic-delivered-duration.pdf", T)
intruderPanel(intruderPost, "AgonisticDeliveredOccur", "Frequency", "Agonistic Behaviour Delivered", 60, "agonistic-delivered-frequency.pdf", T)

intruderPanel(intruderPost, "(AgonisticReceivedDuration/1000)", "Duration (Seconds)", "Agonistic Behaviour Received", 150, "agonistic-received-duration.pdf", T)
intruderPanel(intruderPost, "AgonisticReceivedOccur", "Frequency", "Agonistic Behaviour Received", 60, "agonistic-received-frequency.pdf", T)

intruderPanel(intruderPost, "(DomDur/1000)", "Duration (Seconds)", "Dominance Behaviour", 100, "dominance-behaviour-duration.pdf", T)
intruderPanel(intruderPost, "DomEv", "Frequency", "Dominance Behaviour", 30, "dominance-behaviour-frequency.pdf", T)

dominancePanel(intruderPost, "(DominanceScoreDuration/1000)", "Duration (Seconds)", "Dominance Score", -150, 150, "dominance-score-duration.pdf", T)
dominancePanel(intruderPost, "DominanceScoreOccur", "Frequency", "Dominance Score", -50, 50, "dominance-score-frequency.pdf", T)

intruderPanel(intruderPost, "(AvoidDur/1000)", "Duration (Seconds)", "Avoid Opponent", 10, "avoid-duration.pdf", T)
intruderPanel(intruderPost, "AvoidEv", "Frequency", "Avoid Opponent", 5, "avoid-frequency.pdf", T)

intruderPanel(intruderPost, "(DefenseDur/1000)", "Duration (Seconds)", "Defensive Upright Posture", 15, "defensive-upright-duration.pdf", T)
intruderPanel(intruderPost, "DefenseEv", "Frequency", "Defensive Upright Posture", 5, "defensive-upright-frequency.pdf", T)

intruderPanel(intruderPost, "(OpenAggDur/1000)", "Duration (Seconds)", "Open Aggression", 25, "open-aggression-duration.pdf", T)
intruderPanel(intruderPost, "OpenAggEv", "Frequency", "Open Aggression", 5, "open-aggression-frequency.pdf", T)

intruderPanel(intruderPost, "(RitAggDur/1000)", "Duration (Seconds)", "Ritualized Aggression", 30, "ritualized-aggression-duration.pdf", T)
intruderPanel(intruderPost, "RitAggEv", "Frequency", "Ritualized Aggression", 10, "ritualized-aggression-frequency.pdf", T)

intruderPanel(intruderPost, "(ApproachDur/1000)", "Duration (Seconds)", "Approach Intruder", 100, "approach-intruder-duration.pdf", T)
intruderPanel(intruderPost, "ApproachEv", "Frequency", "Approach Intruder", 30, "approach-intruder-frequency.pdf", T)

intruderPanel(intruderPost, "(InactiveTogetherDur/1000)", "Duration (Seconds)", "Inactive Together", 60, "inactive-together-duration.pdf", T)
intruderPanel(intruderPost, "InactiveTogetherEv", "Frequency", "Inactive Together", 10, "inactive-together-frequency.pdf", T)

intruderPanel(intruderPost, "(FollowDur/1000)", "Duration (Seconds)", "Follow Opponent", 30, "follow-opponent-duration.pdf", T)
intruderPanel(intruderPost, "FollowEv", "Frequency", "Follow Opponent", 30, "follow-opponent-frequency.pdf", T)

intruderPanel(intruderPost, "StretchEv", "Frequency", "Stretch Approach", 5, "stretch-approach-frequency.pdf", T)

# non social

intruderPanel(intruderPost, "(ActiveHDur/1000)", "Duration (Seconds)", "Active Horizontal", 200, "active-horizontal-duration.pdf", T)
intruderPanel(intruderPost, "ActiveHEv", "Frequency", "Active Horizontal", 200, "active-horizontal-frequency.pdf", T)

intruderPanel(intruderPost, "(ActiveVDur/1000)", "Duration (Seconds)", "Active Vertical", 250, "active-vertical-duration.pdf", T)
intruderPanel(intruderPost, "ActiveVEv", "Frequency", "Active Vertical", 100, "active-vertical-frequency.pdf", T)

intruderPanel(intruderPost, "(InactiveDur/1000)", "Duration (Seconds)", "Inactive", 50, "inactive-duration.pdf", T)
intruderPanel(intruderPost, "InactiveEv", "Frequency", "Inactive", 15, "inactive-frequency.pdf", T)

intruderPanel(intruderPost, "(GroomDur/1000)", "Duration (Seconds)", "Groom", 100, "groom-duration.pdf", T)
intruderPanel(intruderPost, "GroomEv", "Frequency", "Groom", 30, "groom-frequency.pdf", T)

intruderPanel(intruderPost, "(DigDur/1000)", "Duration (Seconds)", "Dig", 30, "dig-duration.pdf", T)
intruderPanel(intruderPost, "DigEv", "Frequency", "Dig", 10, "dig-frequency.pdf", T)

intruderPanel(intruderPost, "(StereoDur/1000)", "Duration (Seconds)", "Stereotypies", 100, "stero-duration.pdf", T)
intruderPanel(intruderPost, "StereoEv", "Frequency", "Stereotypies", 10, "stero-frequency.pdf", T)

intruderPanel(intruderPost, "(NonSocialDuration/1000)", "Duration (Seconds)", "Non-Social Behaviour", 500, "nonsocial-behaviour-duration.pdf", T)
intruderPanel(intruderPost, "NonSocialOccur", "Frequency", "Non-Social Behaviour", 300, "nonsocial-behaviour-frequency.pdf", T)

intruderPanel(intruderPost, "(NonSocialLocomotorDuration/1000)", "Duration (Seconds)", "Non-Social Locomotive Behaviour", 400, "nonsocial-locomotive-duration.pdf", T)
intruderPanel(intruderPost, "NonSocialLocomotorOccur", "Frequency", "Non-Social Locomotive Behaviour", 300, "nonsocial-locomotive-frequency.pdf", T)

intruderPanel(intruderPost, "(InactiveNonSocialDuration/1000)", "Duration (Seconds)", "Inactive Non-Social Behaviour", 100, "inactive-nonsocial-duration.pdf", T)
intruderPanel(intruderPost, "InactiveNonSocialOccur", "Frequency", "Inactive Non-Social Behaviour", 30, "inactive-nonsocial-frequency.pdf", T)

intruderPanel(intruderPost, "(TotalActivityDuration/1000)", "Duration (Seconds)", "Total Activity", 1000, "total-activity-duration.pdf", T)
intruderPanel(intruderPost, "TotalActivityOccur", "Frequency", "Total Activity", 600, "total-activity-frequency.pdf", T)


