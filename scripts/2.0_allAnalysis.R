##############################
#                            #
#         Analysis  V2       #
#   Self-Report & Acoustics  #
#    Res. Breathing study    #
#           x tDCS           #
#                            #
#############################
#
# Author: Xander Cornelis & Mitchel Kappen
# 09-02-2023

# Load in packages
library(car)
library(lme4)
library(arrow) # Parquets
library(readr) # Delims
library(reshape2)
library(plotly)
library(dplyr)
library(emmeans)
library(ggplot2)
library(effects)

##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console # # Or ctrl + l in VSCode
dev.off() # Clear plot window

# Set and Get directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set WD to script location
source("functions.R") # Load document where functions are stored
options(contrasts = c("contr.sum","contr.poly")) #use this for the p value of the t test
nAGQ = 1
plotDirectory <- "../figures/"
pvalues = c() # Create a variable to store all p-values to correct later
cbPalette <- c("#F0E442", "#0072B2", "#D55E00") # Define Colorblind proof plotting colors

# Load in data ####
questionData <- read_delim("../loc_data/Data_SF.txt")
audioData <- as.data.frame(read_parquet("../loc_data/df_gemaps_func_16khz_noisy.parquet"))

# Start data Merge
questionData <- questionData %>% rename(participantNum = Subject) # Give same Identifier name
audioData <- audioData %>% rename(Phase = taskType) # Give same Phase name
audioData <- audioData %>% mutate(Phase = substring(Phase, 3)) # Rename Phase variables for matching
questionData$Phase[questionData$Phase == "Stress"] <- "Calculus" # Rename Phase variables for matching

# Clean up - Take out irrelevant phases
audioData <- audioData[audioData$Phase != "SART" & audioData$Phase != "PassiveViewing", ] # Kick out final two phases
questionData <- questionData[questionData$Phase != "SART" & questionData$Phase != "PassiveViewing", ] # Kick out final two phases

# Merge data and put four identifier columns up front
allData = full_join(audioData, questionData, by = c("participantNum","Phase"))
allData <- allData[, c("participantNum", "Phase", "tDCSGroup", "Breathing_Condition", setdiff(names(allData), c("participantNum", "Phase", "tDCSGroup", "Breathing_Condition")))]

#### TRANSCRIPTS #####
# Breathing
breathing <- read.csv("../loc_data/sentiment/Breathing_features.csv")
breathing$Phase = "Breathing"
colnames(breathing)[colnames(breathing) == "Patient"] <- "participantNum"

# Habituation
habituation <- read.csv("../loc_data/sentiment/Habituation_features.csv")
habituation$Phase = "Habituation"
colnames(habituation)[colnames(habituation) == "Patient"] <- "participantNum"

# Calculus
calculus <- read.csv("../loc_data/sentiment/Calculus_features.csv")
calculus$Phase = "Calculus"
colnames(calculus)[colnames(calculus) == "Patient"] <- "participantNum"

# Merge the dataframes into transcriptData
transcriptData <- rbind(breathing, habituation, calculus)

# Merge with allData
allData = full_join(allData, transcriptData, by = c("participantNum","Phase"))
write.csv(allData, file = "../loc_data/allData.csv", row.names = FALSE)

# Factorize stuff
allData <- allData %>% # Factorize relevant variables
  transform(participantNum = as.factor(participantNum),
            Breathing_Condition = as.factor(Breathing_Condition),
            tDCSGroup = as.factor(tDCSGroup))

allData$Phase <- ordered(allData$Phase, levels = c("Habituation", "Breathing", "Calculus")) # Factorize (ordered) moment

####### Clear up more?
# To decide; include tDCS or take out completely?
# Include or exclude the baseline/Habituation

####### Acoustic Speech features #######
# Acoustic Speech features: F0 ######
formula <- 'F0semitoneFrom27.5Hz_sma3nz_amean ~ Phase * Breathing_Condition + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in
plot(effect("Phase", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase | Breathing_Condition, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

figure = behaviorplot(emm0.1, Phase, Breathing_Condition, "F0 (Pitch)") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "F0") # Display and save plot
figureF0 = figure

emmeans0.3<- emmeans(chosenModel[[1]], pairwise ~ Breathing_Condition | Phase, adjust ="none", type = "response") # Pairwise comparisons

# Acoustic Speech features: Jitter ######
formula <- 'jitterLocal_sma3nz_amean ~ Phase * Breathing_Condition + (1|participantNum)' # Declare formula

dataModel <- allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in
plot(effect("Phase", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase | Breathing_Condition, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

figure = behaviorplot(emm0.1, Phase, Breathing_Condition, "Jitter") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "Jitter") # Display and save plot
figureJitter = figure

emmeans0.3<- emmeans(chosenModel[[1]], pairwise ~ Breathing_Condition | Phase, adjust ="none", type = "response") # Pairwise comparisons


# Acoustic Speech features: Shimmer ######
formula <- 'shimmerLocaldB_sma3nz_amean ~ Phase * Breathing_Condition + (1|participantNum)' # Declare formula

dataModel <- allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in
plot(effect("Phase", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase | Breathing_Condition, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure = behaviorplot(emm0.1, Phase, Breathing_Condition, "Shimmer") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "Shimmer") # Display and save plot
figureShimmer = figure

emmeans0.3<- emmeans(chosenModel[[1]], pairwise ~ Breathing_Condition | Phase, adjust ="none", type = "response") # Pairwise comparisons

# Acoustic Speech features: HNR ######
formula <- 'HNRdBACF_sma3nz_amean ~ Phase * Breathing_Condition + (1|participantNum)' # Declare formula

dataModel <- allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in
plot(effect("Phase", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase | Breathing_Condition, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure = behaviorplot(emm0.1, Phase, Breathing_Condition, "HNR") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "HNR") # Display and save plot
figureHNR = figure

emmeans0.3<- emmeans(chosenModel[[1]], pairwise ~ Breathing_Condition | Phase, adjust ="none", type = "response") # Pairwise comparisons

# Acoustic Speech features: MeanSegLength ######
formula <- 'MeanVoicedSegmentLengthSec ~ Phase * Breathing_Condition + (1|participantNum)' # Declare formula

dataModel <- allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in
plot(effect("Phase", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase | Breathing_Condition, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure = behaviorplot(emm0.1, Phase, Breathing_Condition, "MeanSegLength") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "MeanSegLength") # Display and save plot
figureMeanSegLength = figure

emmeans0.3<- emmeans(chosenModel[[1]], pairwise ~ Breathing_Condition | Phase, adjust ="none", type = "response") # Pairwise comparisons

# Acoustic Speech features: VoicedSegmentsPerSec ######
formula <- 'VoicedSegmentsPerSec ~ Phase * Breathing_Condition + (1|participantNum)' # Declare formula

dataModel <- allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in
plot(effect("Phase", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase | Breathing_Condition, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure = behaviorplot(emm0.1, Phase, Breathing_Condition, "VoicedSegmentsPerSec") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "VoicedSegmentsPerSec") # Display and save plot
figureVoicedSegments = figure

emmeans0.3<- emmeans(chosenModel[[1]], pairwise ~ Breathing_Condition | Phase, adjust ="none", type = "response") # Pairwise comparisons

####### Sentiment Speech features #######
# Sentiment Speech features: Valence ######
formula <- 'valence ~ Phase * Breathing_Condition + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in
plot(effect("Phase", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase | Breathing_Condition, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure = behaviorplot(emm0.1, Phase, Breathing_Condition, "Valence") # Create plot
figure = addpvalues(figure, emmeans0.1)
savePlot(figure, "Valence") # Display and save plot
figureValence = figure

emmeans0.3<- emmeans(chosenModel[[1]], pairwise ~ Breathing_Condition | Phase, adjust ="none", type = "response") # Pairwise comparisons


# Sentiment Speech features: Arousal ######
formula <- 'arousal ~ Phase * Breathing_Condition + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in
plot(effect("Phase", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase | Breathing_Condition, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure = behaviorplot(emm0.1, Phase, Breathing_Condition, "arousal") # Create plot
figure = addpvalues(figure, emmeans0.1)
savePlot(figure, "arousal") # Display and save plot
figureArousal = figure

emmeans0.3<- emmeans(chosenModel[[1]], pairwise ~ Breathing_Condition | Phase, adjust ="none", type = "response") # Pairwise comparisons

# Sentiment Speech features: Dominance ######
formula <- 'dominance ~ Phase * Breathing_Condition + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in
plot(effect("Phase", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase | Breathing_Condition, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure = behaviorplot(emm0.1, Phase, Breathing_Condition, "Dominance") # Create plot
figure = addpvalues(figure, emmeans0.1)
savePlot(figure, "Dominance") # Display and save plot
figureDominance = figure

emmeans0.3<- emmeans(chosenModel[[1]], pairwise ~ Breathing_Condition | Phase, adjust ="none", type = "response") # Pairwise comparisons

# Sentiment Speech features: Joy ######
formula <- 'joy ~ Phase * Breathing_Condition + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in
plot(effect("Phase", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase | Breathing_Condition, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure = behaviorplot(emm0.1, Phase, Breathing_Condition, "Joy") # Create plot
figure = addpvalues(figure, emmeans0.1)
savePlot(figure, "Joy") # Display and save plot
figureDominance = figure

emmeans0.3<- emmeans(chosenModel[[1]], pairwise ~ Breathing_Condition | Phase, adjust ="none", type = "response") # Pairwise comparisons

# Sentiment Speech features: Anger ######
formula <- 'anger ~ Phase * Breathing_Condition + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in
plot(effect("Phase", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase | Breathing_Condition, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure = behaviorplot(emm0.1, Phase, Breathing_Condition, "Anger") # Create plot
figure = addpvalues(figure, emmeans0.1)
savePlot(figure, "Anger") # Display and save plot
figureDominance = figure

emmeans0.3<- emmeans(chosenModel[[1]], pairwise ~ Breathing_Condition | Phase, adjust ="none", type = "response") # Pairwise comparisons

# Sentiment Speech features: Sadness ######
formula <- 'sadness ~ Phase * Breathing_Condition + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in
plot(effect("Phase", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase | Breathing_Condition, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure = behaviorplot(emm0.1, Phase, Breathing_Condition, "Sadness") # Create plot
figure = addpvalues(figure, emmeans0.1)
savePlot(figure, "Sadness") # Display and save plot
figureDominance = figure

emmeans0.3<- emmeans(chosenModel[[1]], pairwise ~ Breathing_Condition | Phase, adjust ="none", type = "response") # Pairwise comparisons

# Sentiment Speech features: Fear ######
formula <- 'fear ~ Phase * Breathing_Condition + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in
plot(effect("Phase", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase | Breathing_Condition, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure = behaviorplot(emm0.1, Phase, Breathing_Condition, "Fear") # Create plot
figure = addpvalues(figure, emmeans0.1)
savePlot(figure, "Fear") # Display and save plot
figureDominance = figure

emmeans0.3<- emmeans(chosenModel[[1]], pairwise ~ Breathing_Condition | Phase, adjust ="none", type = "response") # Pairwise comparisons

# Sentiment Speech features: Disgust ######
formula <- 'disgust ~ Phase * Breathing_Condition + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in
plot(effect("Phase", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase | Breathing_Condition, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure = behaviorplot(emm0.1, Phase, Breathing_Condition, "Disgust") # Create plot
figure = addpvalues(figure, emmeans0.1)
savePlot(figure, "Disgust") # Display and save plot
figureDominance = figure

emmeans0.3<- emmeans(chosenModel[[1]], pairwise ~ Breathing_Condition | Phase, adjust ="none", type = "response") # Pairwise comparisons

####### Behavioral #######
# Behavioral: NegativeAffect ######
formula <- 'AF_NegativeAffect ~ Phase * Breathing_Condition + (1|participantNum)' # Declare formula

dataModel <- allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in
plot(effect("Phase", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase | Breathing_Condition, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure = behaviorplot(emm0.1, Phase, Breathing_Condition, "Negative Affect") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "NegativeAffect") # Display and save plot
figureNegAffect = figure

emmeans0.3<- emmeans(chosenModel[[1]], pairwise ~ Breathing_Condition | Phase, adjust ="none", type = "response") # Pairwise comparisons

# Behavioral: Activating Positive Affect ######
formula <- 'AF_ActivatingPositiveAffect ~ Phase * Breathing_Condition + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in
plot(effect("Phase", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase | Breathing_Condition, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure<- plotfunction(emm0.1, "Activating Positive Affect")
figure<- figure + annotate('text', x=1.5, y=mean(emm0.1$emmean) + (max(emm0.1$emmean) - min(emm0.1$emmean)) / 2, label='', size=7)
figure 

figure = behaviorplot(emm0.1, Phase, Breathing_Condition, "Activating Positive Affect") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "ActivatingPositiveAffect") # Display and save plot
figureActivatingPositiveAffect = figure

emmeans0.3<- emmeans(chosenModel[[1]], pairwise ~ Breathing_Condition | Phase, adjust ="none", type = "response") # Pairwise comparisons

# Behavioral: SoothingPositiveAffect ######
formula <- 'AF_SoothingPositiveAffect ~ Phase * Breathing_Condition + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in
plot(effect("Phase", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase | Breathing_Condition, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure = behaviorplot(emm0.1, Phase, Breathing_Condition, "Soothing Positive Affect") # Create plot
figure = addpvalues(figure, emmeans0.1)
savePlot(figure, "SoothingPositiveAffect") # Display and save plot
figureSoothingPositiveAffect = figure

emmeans0.3<- emmeans(chosenModel[[1]], pairwise ~ Breathing_Condition | Phase, adjust ="none", type = "response") # Pairwise comparisons





