library(car)
library(phia)
library(ez)
library(lsr)
library(ggplot2)
library(grid)
library(lsmeans)
library(gridExtra)
library(coin)

source("src/data.r")

ldData <- read.csv(file = "data/lightdark.csv", na.strings = c("NA", "-999"), stringsAsFactors=FALSE)
ldData <- ldClean(ldData)


ldDataPre <- subset(ldData, Time == "PRE")
ldDataPost <- subset(ldData, Time == "POST")



########################
# Analysis - Pre-Puberty
########################

ldMalePre <- subset(ldDataPre, ldDataPre$Sex == "M")
ldFemalePre <- subset(ldDataPre, ldDataPre$Sex == "F")

ldTestPre <- subset(ldDataPre, ldDataPre$Treatment == "D")
ldControlPre <- subset(ldDataPre, ldDataPre$Treatment == "C")

nrow(subset(ldMalePre, ldMalePre$Treatment == "D"))
nrow(subset(ldMalePre, ldMalePre$Treatment == "C"))

nrow(subset(ldFemalePre, ldFemalePre$Treatment == "D"))
nrow(subset(ldFemalePre, ldFemalePre$Treatment == "C"))

qqnorm(ldDataPre$Light_ENTRIES_W)
qqnorm(ldDataPre$Light_DURATION_W)
qqnorm(ldDataPre$Light_LATENCY_W)
qqnorm(ldDataPre$Light_HACTV_W)
qqnorm(ldDataPre$Light_ENTRIES_W_1MIN)
qqnorm(ldDataPre$Light_DURATION_W_1MIN)

# horizontal activity
preHActModel <- lm(HACTV ~ Treatment * Sex, data = ldDataPre)
Anova(preHActModel)

t.test(HACTV ~ Treatment, data = ldMalePre)
cohensD(HACTV ~ Treatment, data = ldMalePre)
t.test(HACTV ~ Treatment, data = ldFemalePre)
cohensD(HACTV ~ Treatment, data = ldFemalePre)

t.test(HACTV ~ Sex, data = ldTestPre)
cohensD(HACTV ~ Sex, data = ldTestPre)
t.test(HACTV ~ Sex, data = ldControlPre)
cohensD(HACTV ~ Sex, data = ldControlPre)

# latency
preLatencyModel <- lm(Light_LATENCY_W ~ Treatment * Sex, data = ldDataPre)
Anova(preLatencyModel)

wilcox.test(Light_LATENCY_W ~ Treatment, data = ldMalePre)
wTest <- wilcox_test(Light_LATENCY_W ~ Treatment, data = ldMalePre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(ldMalePre))
wilcox.test(Light_LATENCY_W ~ Treatment, data = ldFemalePre)
wTest <- wilcox_test(Light_LATENCY_W ~ Treatment, data = ldFemalePre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(ldFemalePre))

wilcox.test(Light_LATENCY_W ~ Sex, data = ldTestPre)
wTest <- wilcox_test(Light_LATENCY_W ~ Sex, data = ldTestPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(ldTestPre))
wilcox.test(Light_LATENCY_W ~ Sex, data = ldControlPre)
wTest <- wilcox_test(Light_LATENCY_W ~ Sex, data = ldControlPre, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(ldControlPre))

# total entries

preEntriesModel <- lm(Light_ENTRIES_W ~ Treatment * Sex, data = ldDataPre)
Anova(preEntriesModel)

t.test(Light_ENTRIES_W ~ Treatment, data = ldMalePre)
cohensD(Light_ENTRIES_W ~ Treatment, data = ldMalePre)
t.test(Light_ENTRIES_W ~ Treatment, data = ldFemalePre)
cohensD(Light_ENTRIES_W ~ Treatment, data = ldFemalePre)

t.test(Light_ENTRIES_W ~ Sex, data = ldTestPre)
cohensD(Light_ENTRIES_W ~ Sex, data = ldTestPre)
t.test(Light_ENTRIES_W ~ Sex, data = ldControlPre)
cohensD(Light_ENTRIES_W ~ Sex, data = ldControlPre)

# 1 min entries
preEntries1MinModel <- lm(Light_ENTRIES_W_1MIN ~ Treatment * Sex, data = ldDataPre)
Anova(preEntries1MinModel)

t.test(Light_ENTRIES_W_1MIN ~ Treatment, data = ldMalePre)
cohensD(Light_ENTRIES_W_1MIN ~ Treatment, data = ldMalePre)
t.test(Light_ENTRIES_W_1MIN ~ Treatment, data = ldFemalePre)
cohensD(Light_ENTRIES_W_1MIN ~ Treatment, data = ldFemalePre)

t.test(Light_ENTRIES_W_1MIN ~ Sex, data = ldTestPre)
cohensD(Light_ENTRIES_W_1MIN ~ Sex, data = ldTestPre)
t.test(Light_ENTRIES_W_1MIN ~ Sex, data = ldControlPre)
cohensD(Light_ENTRIES_W_1MIN ~ Sex, data = ldControlPre)

# total duration
preDurationModel <- lm(Light_DURATION_W ~ Treatment * Sex, data = ldDataPre)
Anova(preDurationModel)

t.test(Light_DURATION_W ~ Treatment, data = ldMalePre)
cohensD(Light_DURATION_W ~ Treatment, data = ldMalePre)
t.test(Light_DURATION_W ~ Treatment, data = ldFemalePre)
cohensD(Light_DURATION_W ~ Treatment, data = ldFemalePre)

t.test(Light_DURATION_W ~ Sex, data = ldTestPre)
cohensD(Light_DURATION_W ~ Sex, data = ldTestPre)
t.test(Light_DURATION_W ~ Sex, data = ldControlPre)
cohensD(Light_DURATION_W ~ Sex, data = ldControlPre)

# 1 min duration
preDuration1MinModel <- lm(Light_DURATION_W_1MIN ~ Treatment * Sex, data = ldDataPre)
Anova(preDuration1MinModel)

t.test(Light_DURATION_W_1MIN ~ Treatment, data = ldMalePre)
cohensD(Light_DURATION_W_1MIN ~ Treatment, data = ldMalePre)
t.test(Light_DURATION_W_1MIN ~ Treatment, data = ldFemalePre)
cohensD(Light_DURATION_W_1MIN ~ Treatment, data = ldFemalePre)

t.test(Light_DURATION_W_1MIN ~ Sex, data = ldTestPre)
cohensD(Light_DURATION_W_1MIN ~ Sex, data = ldTestPre)
t.test(Light_DURATION_W_1MIN ~ Sex, data = ldControlPre)
cohensD(Light_DURATION_W_1MIN ~ Sex, data = ldControlPre)



#########################
# Analysis - Post-Puberty
#########################

femaleControlSham <- subset(subset(subset(ldDataPost, Gonad == "SHAM"), Sex == "F"), Treatment == "C")
femaleControlRep <- subset(subset(subset(ldDataPost, Gonad == "REP"), Sex == "F"), Treatment == "C")
femaleControlGdx <- subset(subset(subset(ldDataPost, Gonad == "GDX"), Sex == "F"), Treatment == "C")

maleControlSham <- subset(subset(subset(ldDataPost, Gonad == "SHAM"), Sex == "M"), Treatment == "C")
maleControlRep <- subset(subset(subset(ldDataPost, Gonad == "REP"), Sex == "M"), Treatment == "C")
maleControlGdx <- subset(subset(subset(ldDataPost, Gonad == "GDX"), Sex == "M"), Treatment == "C")

femaleTestSham <- subset(subset(subset(ldDataPost, Gonad == "SHAM"), Sex == "F"), Treatment == "D")
femaleTestRep <- subset(subset(subset(ldDataPost, Gonad == "REP"), Sex == "F"), Treatment == "D")
femaleTestGdx <- subset(subset(subset(ldDataPost, Gonad == "GDX"), Sex == "F"), Treatment == "D")

maleTestSham <- subset(subset(subset(ldDataPost, Gonad == "SHAM"), Sex == "M"), Treatment == "D")
maleTestRep <- subset(subset(subset(ldDataPost, Gonad == "REP"), Sex == "M"), Treatment == "D")
maleTestGdx <- subset(subset(subset(ldDataPost, Gonad == "GDX"), Sex == "M"), Treatment == "D")

# sample size
nrow(femaleControlSham)
nrow(femaleControlRep)
nrow(femaleControlGdx)

nrow(maleControlSham)
nrow(maleControlRep)
nrow(maleControlGdx)

nrow(femaleTestSham)
nrow(femaleTestRep)
nrow(femaleTestGdx)

nrow(maleTestSham)
nrow(maleTestRep)
nrow(maleTestGdx)

# assumptions

qqnorm(ldDataPost$Light_ENTRIES_W)
qqnorm(ldDataPost$Light_DURATION_W)
qqnorm(ldDataPost$Light_LATENCY_W)
qqnorm(ldDataPost$Light_HACTV_W)
qqnorm(ldDataPost$Light_ENTRIES_W_1MIN)
qqnorm(ldDataPost$Light_DURATION_W_1MIN)


### horizontal activity

# main model
postHActModel <- lm(HACTV ~ Treatment * Sex * Gonad, data = ldDataPost)
Anova(postHActModel)


# planned comparisons - hormone condition
t.test(femaleControlSham$HACTV, femaleControlGdx$HACTV) # sham - gdx
cohensD(femaleControlSham$HACTV, femaleControlGdx$HACTV)
t.test(femaleControlGdx$HACTV, femaleControlRep$HACTV) # rep - gdx
cohensD(femaleControlGdx$HACTV, femaleControlRep$HACTV)

t.test(femaleTestSham$HACTV, femaleTestGdx$HACTV)
cohensD(femaleTestSham$HACTV, femaleTestGdx$HACTV)
t.test(femaleTestGdx$HACTV, femaleTestRep$HACTV)
cohensD(femaleTestGdx$HACTV, femaleTestRep$HACTV)

t.test(maleControlSham$HACTV, maleControlGdx$HACTV)
cohensD(maleControlSham$HACTV, maleControlGdx$HACTV)
t.test(maleControlGdx$HACTV, maleControlRep$HACTV)
cohensD(maleControlGdx$HACTV, maleControlRep$HACTV)

t.test(maleTestSham$HACTV, maleTestGdx$HACTV)
cohensD(maleTestSham$HACTV, maleTestGdx$HACTV)
t.test(maleTestGdx$HACTV, maleTestRep$HACTV)
cohensD(maleTestGdx$HACTV, maleTestRep$HACTV)


# planned comparisons - male to female
t.test(femaleControlSham$HACTV, maleControlSham$HACTV)
cohensD(femaleControlSham$HACTV, maleControlSham$HACTV)
t.test(femaleControlRep$HACTV, maleControlRep$HACTV)
cohensD(femaleControlRep$HACTV, maleControlRep$HACTV)
t.test(femaleControlGdx$HACTV, maleControlGdx$HACTV)
cohensD(femaleControlGdx$HACTV, maleControlGdx$HACTV)

t.test(femaleTestSham$HACTV, maleTestSham$HACTV)
cohensD(femaleTestSham$HACTV, maleTestSham$HACTV)
t.test(femaleTestRep$HACTV, maleTestRep$HACTV)
cohensD(femaleTestRep$HACTV, maleTestRep$HACTV)
t.test(femaleTestGdx$HACTV, maleTestGdx$HACTV)
cohensD(femaleTestGdx$HACTV, maleTestGdx$HACTV)


# planned comparisons - test to control
t.test(femaleControlSham$HACTV, femaleTestSham$HACTV)
cohensD(femaleControlSham$HACTV, femaleTestSham$HACTV)
t.test(femaleControlRep$HACTV, femaleTestRep$HACTV)
cohensD(femaleControlRep$HACTV, femaleTestRep$HACTV)
t.test(femaleControlGdx$HACTV, femaleTestGdx$HACTV)
cohensD(femaleControlGdx$HACTV, femaleTestGdx$HACTV)

t.test(maleControlSham$HACTV, maleTestSham$HACTV)
cohensD(maleControlSham$HACTV, maleTestSham$HACTV)
t.test(maleControlRep$HACTV, maleTestRep$HACTV)
cohensD(maleControlRep$HACTV, maleTestRep$HACTV)
t.test(maleControlGdx$HACTV, maleTestGdx$HACTV)
cohensD(maleControlGdx$HACTV, maleTestGdx$HACTV)



### latency - must use non parametric tests

# main model
postLatencyModel <- lm(Light_LATENCY_W ~ Treatment * Sex * Gonad, data = ldDataPost)
Anova(postLatencyModel)


# planned comparisons - gonad
wilcox.test(femaleControlSham$Light_LATENCY_W, femaleControlGdx$Light_LATENCY_W)
wilcox.test(femaleControlGdx$Light_LATENCY_W, femaleControlRep$Light_LATENCY_W)

wilcox.test(femaleTestSham$Light_LATENCY_W, femaleTestGdx$Light_LATENCY_W)
wilcox.test(femaleTestGdx$Light_LATENCY_W, femaleTestRep$Light_LATENCY_W)

wilcox.test(maleControlSham$Light_LATENCY_W, maleControlGdx$Light_LATENCY_W)
testData <- rbind(maleControlSham, maleControlGdx)
wTest <- wilcox_test(Light_LATENCY_W ~ Gonad, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(maleControlGdx$Light_LATENCY_W, maleControlRep$Light_LATENCY_W)

wilcox.test(maleTestSham$Light_LATENCY_W, maleTestGdx$Light_LATENCY_W)
wilcox.test(maleTestGdx$Light_LATENCY_W, maleTestRep$Light_LATENCY_W)


# planned comparisons - male to female
wilcox.test(femaleControlSham$Light_LATENCY_W, maleControlSham$Light_LATENCY_W)
testData <- rbind(femaleControlSham, maleControlSham)
wTest <- wilcox_test(Light_LATENCY_W ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleControlRep$Light_LATENCY_W, maleControlRep$Light_LATENCY_W)
wilcox.test(femaleControlGdx$Light_LATENCY_W, maleControlGdx$Light_LATENCY_W)

wilcox.test(femaleTestSham$Light_LATENCY_W, maleTestSham$Light_LATENCY_W)
testData <- rbind(femaleTestSham, maleTestSham)
wTest <- wilcox_test(Light_LATENCY_W ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTestRep$Light_LATENCY_W, maleTestRep$Light_LATENCY_W)
testData <- rbind(femaleTestRep, maleTestRep)
wTest <- wilcox_test(Light_LATENCY_W ~ Sex, data = testData, exact = T)
wTest
wTest@statistic@teststatistic / sqrt(nrow(testData))
wilcox.test(femaleTestGdx$Light_LATENCY_W, maleTestGdx$Light_LATENCY_W)


# planned comparisons - test to control
wilcox.test(femaleControlSham$Light_LATENCY_W, femaleTestSham$Light_LATENCY_W)
wilcox.test(femaleControlRep$Light_LATENCY_W, femaleTestRep$Light_LATENCY_W)
wilcox.test(femaleControlGdx$Light_LATENCY_W, femaleTestGdx$Light_LATENCY_W)

wilcox.test(maleControlSham$Light_LATENCY_W, maleTestSham$Light_LATENCY_W)
wilcox.test(maleControlRep$Light_LATENCY_W, maleTestRep$Light_LATENCY_W)
wilcox.test(maleControlGdx$Light_LATENCY_W, maleTestGdx$Light_LATENCY_W)



### light entries

# main model
postEntriesModel <- lm(Light_ENTRIES_W ~ Treatment * Sex * Gonad, data = ldDataPost)
Anova(postEntriesModel)


# planned comparisons - gonad
t.test(femaleControlSham$Light_ENTRIES_W, femaleControlGdx$Light_ENTRIES_W)
cohensD(femaleControlSham$Light_ENTRIES_W, femaleControlGdx$Light_ENTRIES_W)
t.test(femaleControlGdx$Light_ENTRIES_W, femaleControlRep$Light_ENTRIES_W)
cohensD(femaleControlGdx$Light_ENTRIES_W, femaleControlRep$Light_ENTRIES_W)

t.test(femaleTestSham$Light_ENTRIES_W, femaleTestGdx$Light_ENTRIES_W)
cohensD(femaleTestSham$Light_ENTRIES_W, femaleTestGdx$Light_ENTRIES_W)
t.test(femaleTestGdx$Light_ENTRIES_W, femaleTestRep$Light_ENTRIES_W)
cohensD(femaleTestGdx$Light_ENTRIES_W, femaleTestRep$Light_ENTRIES_W)

t.test(maleControlSham$Light_ENTRIES_W, maleControlGdx$Light_ENTRIES_W)
cohensD(maleControlSham$Light_ENTRIES_W, maleControlGdx$Light_ENTRIES_W)
t.test(maleControlGdx$Light_ENTRIES_W, maleControlRep$Light_ENTRIES_W)
cohensD(maleControlGdx$Light_ENTRIES_W, maleControlRep$Light_ENTRIES_W)

t.test(maleTestSham$Light_ENTRIES_W, maleTestGdx$Light_ENTRIES_W)
cohensD(maleTestSham$Light_ENTRIES_W, maleTestGdx$Light_ENTRIES_W)
t.test(maleTestGdx$Light_ENTRIES_W, maleTestRep$Light_ENTRIES_W)
cohensD(maleTestGdx$Light_ENTRIES_W, maleTestRep$Light_ENTRIES_W)


# planned comparisons - male to female
t.test(femaleControlSham$Light_ENTRIES_W, maleControlSham$Light_ENTRIES_W)
cohensD(femaleControlSham$Light_ENTRIES_W, maleControlSham$Light_ENTRIES_W)
t.test(femaleControlRep$Light_ENTRIES_W, maleControlRep$Light_ENTRIES_W)
cohensD(femaleControlRep$Light_ENTRIES_W, maleControlRep$Light_ENTRIES_W)
t.test(femaleControlGdx$Light_ENTRIES_W, maleControlGdx$Light_ENTRIES_W)
cohensD(femaleControlGdx$Light_ENTRIES_W, maleControlGdx$Light_ENTRIES_W)

t.test(femaleTestSham$Light_ENTRIES_W, maleTestSham$Light_ENTRIES_W)
cohensD(femaleTestSham$Light_ENTRIES_W, maleTestSham$Light_ENTRIES_W)
t.test(femaleTestRep$Light_ENTRIES_W, maleTestRep$Light_ENTRIES_W)
cohensD(femaleTestRep$Light_ENTRIES_W, maleTestRep$Light_ENTRIES_W)
t.test(femaleTestGdx$Light_ENTRIES_W, maleTestGdx$Light_ENTRIES_W)
cohensD(femaleTestGdx$Light_ENTRIES_W, maleTestGdx$Light_ENTRIES_W)


# planned comparisons - test to control
t.test(femaleControlSham$Light_ENTRIES_W, femaleTestSham$Light_ENTRIES_W)
cohensD(femaleControlSham$Light_ENTRIES_W, femaleTestSham$Light_ENTRIES_W)
t.test(femaleControlRep$Light_ENTRIES_W, femaleTestRep$Light_ENTRIES_W)
cohensD(femaleControlRep$Light_ENTRIES_W, femaleTestRep$Light_ENTRIES_W)
t.test(femaleControlGdx$Light_ENTRIES_W, femaleTestGdx$Light_ENTRIES_W)
cohensD(femaleControlGdx$Light_ENTRIES_W, femaleTestGdx$Light_ENTRIES_W)

t.test(maleControlSham$Light_ENTRIES_W, maleTestSham$Light_ENTRIES_W)
cohensD(maleControlSham$Light_ENTRIES_W, maleTestSham$Light_ENTRIES_W)
t.test(maleControlRep$Light_ENTRIES_W, maleTestRep$Light_ENTRIES_W)
cohensD(maleControlRep$Light_ENTRIES_W, maleTestRep$Light_ENTRIES_W)
t.test(maleControlGdx$Light_ENTRIES_W, maleTestGdx$Light_ENTRIES_W)
cohensD(maleControlGdx$Light_ENTRIES_W, maleTestGdx$Light_ENTRIES_W)



### light duration

# main model
postDurationModel <- lm(Light_DURATION_W ~ Treatment * Sex * Gonad, data = ldDataPost)
Anova(postDurationModel)


# planned comparisons - gonad
t.test(femaleControlSham$Light_DURATION_W, femaleControlGdx$Light_DURATION_W)
cohensD(femaleControlSham$Light_DURATION_W, femaleControlGdx$Light_DURATION_W)
t.test(femaleControlGdx$Light_DURATION_W, femaleControlRep$Light_DURATION_W)
cohensD(femaleControlGdx$Light_DURATION_W, femaleControlRep$Light_DURATION_W)

t.test(femaleTestSham$Light_DURATION_W, femaleTestGdx$Light_DURATION_W)
cohensD(femaleTestSham$Light_DURATION_W, femaleTestGdx$Light_DURATION_W)
t.test(femaleTestGdx$Light_DURATION_W, femaleTestRep$Light_DURATION_W)
cohensD(femaleTestGdx$Light_DURATION_W, femaleTestRep$Light_DURATION_W)

t.test(maleControlSham$Light_DURATION_W, maleControlGdx$Light_DURATION_W)
cohensD(maleControlSham$Light_DURATION_W, maleControlGdx$Light_DURATION_W)
t.test(maleControlGdx$Light_DURATION_W, maleControlRep$Light_DURATION_W)
cohensD(maleControlGdx$Light_DURATION_W, maleControlRep$Light_DURATION_W)

t.test(maleTestSham$Light_DURATION_W, maleTestGdx$Light_DURATION_W)
cohensD(maleTestSham$Light_DURATION_W, maleTestGdx$Light_DURATION_W)
t.test(maleTestGdx$Light_DURATION_W, maleTestRep$Light_DURATION_W)
cohensD(maleTestGdx$Light_DURATION_W, maleTestRep$Light_DURATION_W)


# planned comparisons - male to female
t.test(femaleControlSham$Light_DURATION_W, maleControlSham$Light_DURATION_W)
cohensD(femaleControlSham$Light_DURATION_W, maleControlSham$Light_DURATION_W)
t.test(femaleControlRep$Light_DURATION_W, maleControlRep$Light_DURATION_W)
cohensD(femaleControlRep$Light_DURATION_W, maleControlRep$Light_DURATION_W)
t.test(femaleControlGdx$Light_DURATION_W, maleControlGdx$Light_DURATION_W)
cohensD(femaleControlGdx$Light_DURATION_W, maleControlGdx$Light_DURATION_W)

t.test(femaleTestSham$Light_DURATION_W, maleTestSham$Light_DURATION_W)
cohensD(femaleTestSham$Light_DURATION_W, maleTestSham$Light_DURATION_W)
t.test(femaleTestRep$Light_DURATION_W, maleTestRep$Light_DURATION_W)
cohensD(femaleTestRep$Light_DURATION_W, maleTestRep$Light_DURATION_W)
t.test(femaleTestGdx$Light_DURATION_W, maleTestGdx$Light_DURATION_W)
cohensD(femaleTestGdx$Light_DURATION_W, maleTestGdx$Light_DURATION_W)


# planned comparisons - test to control
t.test(femaleControlSham$Light_DURATION_W, femaleTestSham$Light_DURATION_W)
cohensD(femaleControlSham$Light_DURATION_W, femaleTestSham$Light_DURATION_W)
t.test(femaleControlRep$Light_DURATION_W, femaleTestRep$Light_DURATION_W)
cohensD(femaleControlRep$Light_DURATION_W, femaleTestRep$Light_DURATION_W)
t.test(femaleControlGdx$Light_DURATION_W, femaleTestGdx$Light_DURATION_W)
cohensD(femaleControlGdx$Light_DURATION_W, femaleTestGdx$Light_DURATION_W)

t.test(maleControlSham$Light_DURATION_W, maleTestSham$Light_DURATION_W)
cohensD(maleControlSham$Light_DURATION_W, maleTestSham$Light_DURATION_W)
t.test(maleControlRep$Light_DURATION_W, maleTestRep$Light_DURATION_W)
cohensD(maleControlRep$Light_DURATION_W, maleTestRep$Light_DURATION_W)
t.test(maleControlGdx$Light_DURATION_W, maleTestGdx$Light_DURATION_W)
cohensD(maleControlGdx$Light_DURATION_W, maleTestGdx$Light_DURATION_W)



### 1 min entries

# main model
postEntries1MinModel <- lm(Light_ENTRIES_W_1MIN ~ Treatment * Sex * Gonad, data = ldDataPost)
Anova(postEntries1MinModel)


# planned comparisons - gonad
t.test(femaleControlSham$Light_ENTRIES_W_1MIN, femaleControlGdx$Light_ENTRIES_W_1MIN)
cohensD(femaleControlSham$Light_ENTRIES_W_1MIN, femaleControlGdx$Light_ENTRIES_W_1MIN)
t.test(femaleControlGdx$Light_ENTRIES_W_1MIN, femaleControlRep$Light_ENTRIES_W_1MIN)
cohensD(femaleControlGdx$Light_ENTRIES_W_1MIN, femaleControlRep$Light_ENTRIES_W_1MIN)

t.test(femaleTestSham$Light_ENTRIES_W_1MIN, femaleTestGdx$Light_ENTRIES_W_1MIN)
cohensD(femaleTestSham$Light_ENTRIES_W_1MIN, femaleTestGdx$Light_ENTRIES_W_1MIN)
t.test(femaleTestGdx$Light_ENTRIES_W_1MIN, femaleTestRep$Light_ENTRIES_W_1MIN)
cohensD(femaleTestGdx$Light_ENTRIES_W_1MIN, femaleTestRep$Light_ENTRIES_W_1MIN)

t.test(maleControlSham$Light_ENTRIES_W_1MIN, maleControlGdx$Light_ENTRIES_W_1MIN)
cohensD(maleControlSham$Light_ENTRIES_W_1MIN, maleControlGdx$Light_ENTRIES_W_1MIN)
t.test(maleControlGdx$Light_ENTRIES_W_1MIN, maleControlRep$Light_ENTRIES_W_1MIN)
cohensD(maleControlGdx$Light_ENTRIES_W_1MIN, maleControlRep$Light_ENTRIES_W_1MIN)

t.test(maleTestSham$Light_ENTRIES_W_1MIN, maleTestGdx$Light_ENTRIES_W_1MIN)
cohensD(maleTestSham$Light_ENTRIES_W_1MIN, maleTestGdx$Light_ENTRIES_W_1MIN)
t.test(maleTestGdx$Light_ENTRIES_W_1MIN, maleTestRep$Light_ENTRIES_W_1MIN)
cohensD(maleTestGdx$Light_ENTRIES_W_1MIN, maleTestRep$Light_ENTRIES_W_1MIN)


# planned comparisons - male to female
t.test(femaleControlSham$Light_ENTRIES_W_1MIN, maleControlSham$Light_ENTRIES_W_1MIN)
cohensD(femaleControlSham$Light_ENTRIES_W_1MIN, maleControlSham$Light_ENTRIES_W_1MIN)
t.test(femaleControlRep$Light_ENTRIES_W_1MIN, maleControlRep$Light_ENTRIES_W_1MIN)
cohensD(femaleControlRep$Light_ENTRIES_W_1MIN, maleControlRep$Light_ENTRIES_W_1MIN)
t.test(femaleControlGdx$Light_ENTRIES_W_1MIN, maleControlGdx$Light_ENTRIES_W_1MIN)
cohensD(femaleControlGdx$Light_ENTRIES_W_1MIN, maleControlGdx$Light_ENTRIES_W_1MIN)

t.test(femaleTestSham$Light_ENTRIES_W_1MIN, maleTestSham$Light_ENTRIES_W_1MIN)
cohensD(femaleTestSham$Light_ENTRIES_W_1MIN, maleTestSham$Light_ENTRIES_W_1MIN)
t.test(femaleTestRep$Light_ENTRIES_W_1MIN, maleTestRep$Light_ENTRIES_W_1MIN)
cohensD(femaleTestRep$Light_ENTRIES_W_1MIN, maleTestRep$Light_ENTRIES_W_1MIN)
t.test(femaleTestGdx$Light_ENTRIES_W_1MIN, maleTestGdx$Light_ENTRIES_W_1MIN)
cohensD(femaleTestGdx$Light_ENTRIES_W_1MIN, maleTestGdx$Light_ENTRIES_W_1MIN)


# planned comparisons - test to control
t.test(femaleControlSham$Light_ENTRIES_W_1MIN, femaleTestSham$Light_ENTRIES_W_1MIN)
cohensD(femaleControlSham$Light_ENTRIES_W_1MIN, femaleTestSham$Light_ENTRIES_W_1MIN)
t.test(femaleControlRep$Light_ENTRIES_W_1MIN, femaleTestRep$Light_ENTRIES_W_1MIN)
cohensD(femaleControlRep$Light_ENTRIES_W_1MIN, femaleTestRep$Light_ENTRIES_W_1MIN)
t.test(femaleControlGdx$Light_ENTRIES_W_1MIN, femaleTestGdx$Light_ENTRIES_W_1MIN)
cohensD(femaleControlGdx$Light_ENTRIES_W_1MIN, femaleTestGdx$Light_ENTRIES_W_1MIN)

t.test(maleControlSham$Light_ENTRIES_W_1MIN, maleTestSham$Light_ENTRIES_W_1MIN)
cohensD(maleControlSham$Light_ENTRIES_W_1MIN, maleTestSham$Light_ENTRIES_W_1MIN)
t.test(maleControlRep$Light_ENTRIES_W_1MIN, maleTestRep$Light_ENTRIES_W_1MIN)
cohensD(maleControlRep$Light_ENTRIES_W_1MIN, maleTestRep$Light_ENTRIES_W_1MIN)
t.test(maleControlGdx$Light_ENTRIES_W_1MIN, maleTestGdx$Light_ENTRIES_W_1MIN)
cohensD(maleControlGdx$Light_ENTRIES_W_1MIN, maleTestGdx$Light_ENTRIES_W_1MIN)


### 1 min duration

# main model
postDuration1MinModel <- lm(Light_DURATION_W_1MIN ~ Treatment * Sex * Gonad, data = ldDataPost)
Anova(postDuration1MinModel)


# planned comparisons - gonad
t.test(femaleControlSham$Light_DURATION_W_1MIN, femaleControlGdx$Light_DURATION_W_1MIN)
cohensD(femaleControlSham$Light_DURATION_W_1MIN, femaleControlGdx$Light_DURATION_W_1MIN)
t.test(femaleControlGdx$Light_DURATION_W_1MIN, femaleControlRep$Light_DURATION_W_1MIN)
cohensD(femaleControlGdx$Light_DURATION_W_1MIN, femaleControlRep$Light_DURATION_W_1MIN)

t.test(femaleTestSham$Light_DURATION_W_1MIN, femaleTestGdx$Light_DURATION_W_1MIN)
cohensD(femaleTestSham$Light_DURATION_W_1MIN, femaleTestGdx$Light_DURATION_W_1MIN)
t.test(femaleTestGdx$Light_DURATION_W_1MIN, femaleTestRep$Light_DURATION_W_1MIN)
cohensD(femaleTestGdx$Light_DURATION_W_1MIN, femaleTestRep$Light_DURATION_W_1MIN)

t.test(maleControlSham$Light_DURATION_W_1MIN, maleControlGdx$Light_DURATION_W_1MIN)
cohensD(maleControlSham$Light_DURATION_W_1MIN, maleControlGdx$Light_DURATION_W_1MIN)
t.test(maleControlGdx$Light_DURATION_W_1MIN, maleControlRep$Light_DURATION_W_1MIN)
cohensD(maleControlGdx$Light_DURATION_W_1MIN, maleControlRep$Light_DURATION_W_1MIN)

t.test(maleTestSham$Light_DURATION_W_1MIN, maleTestGdx$Light_DURATION_W_1MIN)
cohensD(maleTestSham$Light_DURATION_W_1MIN, maleTestGdx$Light_DURATION_W_1MIN)
t.test(maleTestGdx$Light_DURATION_W_1MIN, maleTestRep$Light_DURATION_W_1MIN)
cohensD(maleTestGdx$Light_DURATION_W_1MIN, maleTestRep$Light_DURATION_W_1MIN)


# planned comparisons - male to female
t.test(femaleControlSham$Light_DURATION_W_1MIN, maleControlSham$Light_DURATION_W_1MIN)
cohensD(femaleControlSham$Light_DURATION_W_1MIN, maleControlSham$Light_DURATION_W_1MIN)
t.test(femaleControlRep$Light_DURATION_W_1MIN, maleControlRep$Light_DURATION_W_1MIN)
cohensD(femaleControlRep$Light_DURATION_W_1MIN, maleControlRep$Light_DURATION_W_1MIN)
t.test(femaleControlGdx$Light_DURATION_W_1MIN, maleControlGdx$Light_DURATION_W_1MIN)
cohensD(femaleControlGdx$Light_DURATION_W_1MIN, maleControlGdx$Light_DURATION_W_1MIN)

t.test(femaleTestSham$Light_DURATION_W_1MIN, maleTestSham$Light_DURATION_W_1MIN)
cohensD(femaleTestSham$Light_DURATION_W_1MIN, maleTestSham$Light_DURATION_W_1MIN)
t.test(femaleTestRep$Light_DURATION_W_1MIN, maleTestRep$Light_DURATION_W_1MIN)
cohensD(femaleTestRep$Light_DURATION_W_1MIN, maleTestRep$Light_DURATION_W_1MIN)
t.test(femaleTestGdx$Light_DURATION_W_1MIN, maleTestGdx$Light_DURATION_W_1MIN)
cohensD(femaleTestGdx$Light_DURATION_W_1MIN, maleTestGdx$Light_DURATION_W_1MIN)


# planned comparisons - test to control
t.test(femaleControlSham$Light_DURATION_W_1MIN, femaleTestSham$Light_DURATION_W_1MIN)
cohensD(femaleControlSham$Light_DURATION_W_1MIN, femaleTestSham$Light_DURATION_W_1MIN)
t.test(femaleControlRep$Light_DURATION_W_1MIN, femaleTestRep$Light_DURATION_W_1MIN)
cohensD(femaleControlRep$Light_DURATION_W_1MIN, femaleTestRep$Light_DURATION_W_1MIN)
t.test(femaleControlGdx$Light_DURATION_W_1MIN, femaleTestGdx$Light_DURATION_W_1MIN)
cohensD(femaleControlGdx$Light_DURATION_W_1MIN, femaleTestGdx$Light_DURATION_W_1MIN)

t.test(maleControlSham$Light_DURATION_W_1MIN, maleTestSham$Light_DURATION_W_1MIN)
cohensD(maleControlSham$Light_DURATION_W_1MIN, maleTestSham$Light_DURATION_W_1MIN)
t.test(maleControlRep$Light_DURATION_W_1MIN, maleTestRep$Light_DURATION_W_1MIN)
cohensD(maleControlRep$Light_DURATION_W_1MIN, maleTestRep$Light_DURATION_W_1MIN)
t.test(maleControlGdx$Light_DURATION_W_1MIN, maleTestGdx$Light_DURATION_W_1MIN)
cohensD(maleControlGdx$Light_DURATION_W_1MIN, maleTestGdx$Light_DURATION_W_1MIN)



#############
# Graphing
#############

# pre-puberty

preEntriesGraph <- lightDarkGraphPre(ldDataPre, "Light_ENTRIES_W", "Light-Side Entries", "Light-Side Entries (Total)", 30, F, "PrePubLightEntries.pdf")
preDurationGraph <- lightDarkGraphPre(ldDataPre, "Light_DURATION_W", "Duration (seconds)", "Time in Light-Side (Total)", 150, F, "PrePubLightDuration.pdf")
pre1MinEntriesGraph <- lightDarkGraphPre(ldDataPre, "Light_ENTRIES_W_1MIN", "Light-Side Entries", "Light-Side Entries (1st Minute)", 5, F, "PrePubLightEntries1Min.pdf")
pre1MinDurationGraph <- lightDarkGraphPre(ldDataPre, "Light_DURATION_W_1MIN", "Duration (seconds)", "Time in Light-Side (1st Minute)", 30, F, "PrePubLightEntries1Min.pdf")
preLatencyGraph <- lightDarkGraphPre(ldDataPre, "Light_LATENCY_W", "Latency (seconds)", "Latency to Enter Light-Side", 50, F, "PrePubLatency.pdf")
preHActGraph <- lightDarkGraphPre(ldDataPre, "HACTV", "Number of Beam Breaks", "Total Horizontal Activity", 3000, F, "PrePubHorAct.pdf")

preGraphLegend <- getLegend(preEntriesGraph);
preEntriesGraph <- preEntriesGraph + theme(legend.position = "none") 
preDurationGraph <- preDurationGraph + theme(legend.position = "none")
pre1MinEntriesGraph <- pre1MinEntriesGraph + theme(legend.position = "none")
pre1MinDurationGraph <- pre1MinDurationGraph + theme(legend.position = "none")
preLatencyGraph <- preLatencyGraph + theme(legend.position = "none")
preHActGraph <- preHActGraph + theme(legend.position = "none")

pdf(paste("output/", "ld-prepub"), 8, 11)
grid.arrange(preEntriesGraph, pre1MinEntriesGraph, preDurationGraph, pre1MinDurationGraph, preLatencyGraph, preHActGraph, ncol = 2, nrow = 3, widths = c(7, 7))
dev.off()

pdf(paste("output/", "fileName"), 8, 11)
grid.arrange(preGraphLegend, ncol = 1, nrow = 1, widths = c(1.5))
dev.off()



# Post-Puberty

lightDarkGraph(subset(ldDataPost, ldDataPost$Sex == "F"), "Light_ENTRIES_W", "Light Side Entries", "Light Side Entries (Total)", 20, "LightSideEntries.pdf")

lightDarkPanel(ldDataPost, "Light_ENTRIES_W", "Light Side Entries", "Light Side Entries (Total)", 20, "LightSideEntries.pdf")
lightDarkPanel(ldDataPost, "Light_DURATION_W", "Duration (seconds)", "Time in Light Side (Total)", 150, "LightSideDuration.pdf")
lightDarkPanel(ldDataPost, "Light_ENTRIES_W_1MIN", "Light Side Entries", "Light Side Entries (1st Minute)", 5, "LightSideEntriesFirstMin.pdf")
lightDarkPanel(ldDataPost, "Light_DURATION_W_1MIN", "Duration (seconds)", "Time in Light Side (1st Minute)", 25, "LightSideDurationFirstMin.pdf")
lightDarkPanel(ldDataPost, "Light_LATENCY_W", "Latency (seconds)", "Latency to Enter Light Side", 100, "LightSideLatency.pdf")
lightDarkPanel(ldDataPost, "HACTV", "Number of Beam Breaks", "Total Horizontal Activity", 3000, "HorizontalActivity.pdf")

