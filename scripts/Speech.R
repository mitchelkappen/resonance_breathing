
# Load in packages #### 
library(car)
library(lme4)
library(reshape2)
library(readr)
library(plotly)
library(dplyr)
library(emmeans)
library(ggplot2)
library(effects) 
library(arrow)
library(ggthemes) 
library(ggsignif)


#Set environment ####
rm(list = ls()) # Clear environment
cat("\014") # Clear console # # Or ctrl + l in VSCode
dev.off() # Clear plot window
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set WD to script location

options(contrasts = c("contr.sum","contr.poly")) # Use this for the p value of the t test
nAGQ = 1
pvalues = c() # Create a variable to store all p-values to correct later
cbPalette <- c("#F0E442", "#0072B2", "#D55E00") # Define Colorblind proof plotting colors

# function for plot
plotfunction <-
  function(emmean_dataframe, title){
    ggplot(emmean_dataframe, aes(x=phaseName, y=emmean, colour = Breathing_Cond)) +
      geom_point(aes(group = Breathing_Cond), size = 4, position = position_dodge(width = 0.3)) + 
      geom_line(aes(group = Breathing_Cond), size = 1, position = position_dodge(width = 0.3)) +
      geom_errorbar(width=.25, size = 1, aes(ymin=emmean-SE, ymax=emmean+SE), position = position_dodge(width = 0.3)) +
      labs(y = title, x = "Phase")+
      scale_colour_manual(values=cbPalette)+
      theme_apa()
  }


# Load in data ####
data <- read_delim("../loc_data/Data_SF.txt")
audioData <- as.data.frame(read_parquet("../loc_data/df_gemaps_func.parquet"))

# Factorize
audioData$participantNum = as.factor(audioData$participantNum)
audioData$phaseName = as.factor(audioData$phaseName)

# Removing the last two phases (and all related data) + habituation
audioData = audioData[audioData$phaseName != "SART", ]
audioData = audioData[audioData$phaseName != "PassiveViewing", ]
audioData = audioData[audioData$phaseName != "Habituation", ]

# Subset of columns
audioData = subset(audioData, select = c(F0semitoneFrom27.5Hz_sma3nz_amean,
                                         jitterLocal_sma3nz_amean,
                                         shimmerLocaldB_sma3nz_amean,
                                         HNRdBACF_sma3nz_amean,
                                         MeanVoicedSegmentLengthSec,
                                         VoicedSegmentsPerSec,
                                         participantNum, phaseName)) 

# Add column with breathing condition
audioData$Breathing_Cond<- 0
for (i in 1:nrow(audioData)){
  audioData$Breathing_Cond[i]<- data$Breathing_Condition[audioData$participantNum[i] == data$Subject] 
}
audioData$Breathing_Cond<- as.factor(audioData$Breathing_Cond)

# Visualization of the data
densityPlot(audioData$F0semitoneFrom27.5Hz_sma3nz_amean)
densityPlot(audioData$jitterLocal_sma3nz_amean)
densityPlot(audioData$shimmerLocaldB_sma3nz_amean)
densityPlot(audioData$HNRdBACF_sma3nz_amean)
densityPlot(audioData$MeanVoicedSegmentLengthSec)
densityPlot(audioData$VoicedSegmentsPerSec)



# testing #####


# 1) F0semitoneFrom27.5Hz_sma3nz_amean ####
formula <- 'F0semitoneFrom27.5Hz_sma3nz_amean ~ phaseName*Breathing_Cond + (1|participantNum)' # Declare formula

dataModel = audioData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("phaseName:Breathing_Cond", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ phaseName | Breathing_Cond, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure<- plotfunction(emm0.1, "F0")
figure<- figure + annotate('text', x=1.5, y=mean(emm0.1$emmean) + (max(emm0.1$emmean) - min(emm0.1$emmean)) / 2, label='', size=7)
figure<- figure + 
  geom_text(x=1.5, y=28.7, label="*", colour = "#0072B2")
figure 


# 2) jitterLocal_sma3nz_amean ####
formula <- 'jitterLocal_sma3nz_amean ~ phaseName*Breathing_Cond + (1|participantNum)' # Declare formula

dataModel = audioData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("phaseName:Breathing_Cond", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ phaseName | Breathing_Cond, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure<- plotfunction(emm0.1, "Jitter")
figure<- figure + annotate('text', x=1.5, y=mean(emm0.1$emmean) + (max(emm0.1$emmean) - min(emm0.1$emmean)) / 2, label='', size=7)
figure 


# 3) shimmerLocaldB_sma3nz_amean ####
formula <- 'shimmerLocaldB_sma3nz_amean ~ phaseName*Breathing_Cond + (1|participantNum)' # Declare formula

dataModel = audioData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("phaseName:Breathing_Cond", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ phaseName | Breathing_Cond, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure<- plotfunction(emm0.1, "Shimmer")
figure<- figure + annotate('text', x=1.5, y=mean(emm0.1$emmean) + (max(emm0.1$emmean) - min(emm0.1$emmean)) / 2, label='', size=7)
figure 


# 4) HNRdBACF_sma3nz_amean ####
formula <- 'HNRdBACF_sma3nz_amean ~ phaseName*Breathing_Cond + (1|participantNum)' # Declare formula

dataModel = audioData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
#d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
#d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1)
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("phaseName:Breathing_Cond", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ phaseName | Breathing_Cond, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure<- plotfunction(emm0.1, "HNR")
figure<- figure + annotate('text', x=1.5, y=mean(emm0.1$emmean) + (max(emm0.1$emmean) - min(emm0.1$emmean)) / 2, label='', size=7)
figure 


# 5) MeanVoicedSegmentLengthSec ####
formula <- 'MeanVoicedSegmentLengthSec ~ phaseName*Breathing_Cond + (1|participantNum)' # Declare formula

dataModel = audioData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("phaseName:Breathing_Cond", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ phaseName | Breathing_Cond, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure<- plotfunction(emm0.1, "MeanVoiced")
figure<- figure + annotate('text', x=1.5, y=mean(emm0.1$emmean) + (max(emm0.1$emmean) - min(emm0.1$emmean)) / 2, label='', size=7)
figure<- figure + 
   geom_text(x=1.5, y=0.162, label="*", colour = "#F0E442") + 
   geom_text(x=1.5, y=0.157, label="***", colour = "#0072B2")
figure 


# 6) VoicedSegmentsPerSec ####
formula <- 'VoicedSegmentsPerSec ~ phaseName*Breathing_Cond + (1|participantNum)' # Declare formula

dataModel = audioData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("phaseName:Breathing_Cond", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ phaseName | Breathing_Cond, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure<- plotfunction(emm0.1, "Voiced")
figure<- figure + annotate('text', x=1.5, y=mean(emm0.1$emmean) + (max(emm0.1$emmean) - min(emm0.1$emmean)) / 2, label='', size=7)
figure 


# Correction for multiple comparisons ####
names = c('F0_Control', 'F0_Slow', 'Jitter_Control', 'Jitter_Slow', 'Shimmer_Control', 'Shimmer_Slow', 'HNR_Control', 'HNR_Slow', 'MeanVoiced_Control', 'MeanVoiced_Slow', 'Voiced_Control', 'Voiced_Slow')
ps = list()
ps[names] = p.adjust(pvalues, method = "fdr", length(pvalues)) # Create list containing fdr corrected pvalues
ps



