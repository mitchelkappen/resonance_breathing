
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
# One general theme to clean up code
plot_theme_apa <-
  function(...){
    theme(
      # legend.key.size=unit(1.3, 'cm'),
      # legend.text=element_text(size=13),
      legend.position = "none",
      plot.title = element_text(size=rel(2)),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.grid.major.y = element_line( size=.1, color="#dedede" ),
      axis.text.x=element_text(size=rel(2)),
      axis.title.y=element_text(size=rel(1.5)),
      axis.title.x = element_text(size=rel(1.5)))
  }


plotfunction <-
  function(emmean_dataframe, title){
    ggplot(emmean_dataframe, aes(x=taskType, y=emmean, colour = Breathing_Cond)) +
      geom_point(aes(group = Breathing_Cond), size = 4, position = position_dodge(width = 0.3)) + 
      geom_line(aes(group = Breathing_Cond), size = 1, position = position_dodge(width = 0.3)) +
      geom_errorbar(width=.25, size = 1, aes(ymin=emmean-SE, ymax=emmean+SE), position = position_dodge(width = 0.3)) +
      labs(y = title, x = "Phase")+
      scale_colour_manual(values=cbPalette)+
      plot_theme_apa()
  }


# Load in data ####
data <- read_delim("../loc_data/Data_SF.txt")
audioData <- as.data.frame(read_parquet("../loc_data/df_gemaps_func_16khz_noisy.parquet"))

# Clean up
audioData <- audioData %>% mutate(taskType = substring(taskType, 3)) # Rename taskType variables
audioData <- audioData[audioData$taskType != "SART" & audioData$taskType != "PassiveViewing", ] # Kick out final two phases

# Factorize
audioData$participantNum = as.factor(audioData$participantNum)
audioData$taskType = factor(audioData$taskType, levels = c("Habituation", "Breathing", "Calculus"))

# Subset of columns
backupData = audioData
audioData = subset(audioData, select = c(F0semitoneFrom27.5Hz_sma3nz_amean,
                                         jitterLocal_sma3nz_amean,
                                         shimmerLocaldB_sma3nz_amean,
                                         HNRdBACF_sma3nz_amean,
                                         MeanVoicedSegmentLengthSec,
                                         VoicedSegmentsPerSec,
                                         participantNum, taskType)) 

# Add column with breathing condition
audioData$Breathing_Cond <- 0
for (i in 1:nrow(audioData)){
  audioData$Breathing_Cond[i]<- data$Breathing_Condition[audioData$participantNum[i] == data$Subject][1]
}
audioData$Breathing_Cond <- as.factor(audioData$Breathing_Cond)

# Visualization of the data
densityPlot(audioData$F0semitoneFrom27.5Hz_sma3nz_amean)
densityPlot(audioData$jitterLocal_sma3nz_amean)
densityPlot(audioData$shimmerLocaldB_sma3nz_amean)
densityPlot(audioData$HNRdBACF_sma3nz_amean)
densityPlot(audioData$MeanVoicedSegmentLengthSec)
densityPlot(audioData$VoicedSegmentsPerSec)

# Statistics #####
# Speech #####
# 1) F0semitoneFrom27.5Hz_sma3nz_amean ####
formula <- 'F0semitoneFrom27.5Hz_sma3nz_amean ~ taskType*Breathing_Cond + (1|participantNum)' # Declare formula

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
plot(effect("taskType:Breathing_Cond", chosenModel[[1]])) # Visualize the three way interaction we are interested in
plot(effect("taskType", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.2<- emmeans(chosenModel[[1]], pairwise ~ taskType | Breathing_Cond, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure<- plotfunction(emm0.1, "F0")
figure<- figure + annotate('text', x=1.5, y=mean(emm0.1$emmean) + (max(emm0.1$emmean) - min(emm0.1$emmean)) / 2, label='', size=7)
figure 


emmeans0.3<- emmeans(chosenModel[[1]], pairwise ~ Breathing_Cond | taskType, adjust ="none", type = "response") # Pairwise comparisons


# 2) jitterLocal_sma3nz_amean ####
formula <- 'jitterLocal_sma3nz_amean ~ taskType*Breathing_Cond + (1|participantNum)' # Declare formula

dataModel = audioData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
# d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
# d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1)
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("taskType:Breathing_Cond", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ taskType | Breathing_Cond, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure<- plotfunction(emm0.1, "Jitter")
figure<- figure + annotate('text', x=1.5, y=mean(emm0.1$emmean) + (max(emm0.1$emmean) - min(emm0.1$emmean)) / 2, label='', size=7)
figure 


# 3) shimmerLocaldB_sma3nz_amean ####
formula <- 'shimmerLocaldB_sma3nz_amean ~ taskType*Breathing_Cond + (1|participantNum)' # Declare formula

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
plot(effect("taskType:Breathing_Cond", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ taskType | Breathing_Cond, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure<- plotfunction(emm0.1, "Shimmer")
figure<- figure + annotate('text', x=1.5, y=mean(emm0.1$emmean) + (max(emm0.1$emmean) - min(emm0.1$emmean)) / 2, label='', size=7)
figure<- figure + 
  geom_text(x=1.5, y=1.185, label="*", colour = "#F0E442") + 
  geom_text(x=1.5, y=1.215, label="*", colour = "#0072B2")
figure  


# 4) HNRdBACF_sma3nz_amean ####
formula <- 'HNRdBACF_sma3nz_amean ~ taskType*Breathing_Cond + (1|participantNum)' # Declare formula

dataModel = audioData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
# d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
# d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1)
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("taskType:Breathing_Cond", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ taskType | Breathing_Cond, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure<- plotfunction(emm0.1, "HNR")
figure<- figure + annotate('text', x=1.5, y=mean(emm0.1$emmean) + (max(emm0.1$emmean) - min(emm0.1$emmean)) / 2, label='', size=7)
figure 


# 5) MeanVoicedSegmentLengthSec ####
formula <- 'MeanVoicedSegmentLengthSec ~ taskType*Breathing_Cond + (1|participantNum)' # Declare formula

dataModel = audioData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
# d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
# d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1)
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("taskType:Breathing_Cond", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ taskType | Breathing_Cond, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure<- plotfunction(emm0.1, "MeanVoiced")
figure<- figure + annotate('text', x=1.5, y=mean(emm0.1$emmean) + (max(emm0.1$emmean) - min(emm0.1$emmean)) / 2, label='', size=7)
figure<- figure + 
   geom_text(x=1.65, y=0.153, label="*", colour = "#0072B2") + 
   geom_text(x=2.45, y=0.153, label="*", colour = "#0072B2")
figure 


# 6) VoicedSegmentsPerSec ####
formula <- 'VoicedSegmentsPerSec ~ taskType*Breathing_Cond + (1|participantNum)' # Declare formula

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
plot(effect("taskType:Breathing_Cond", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ taskType | Breathing_Cond, adjust ="none", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Store Pvalues to correct for multiple corrections later

# Figure
figure<- plotfunction(emm0.1, "Voiced")
figure<- figure + annotate('text', x=1.5, y=mean(emm0.1$emmean) + (max(emm0.1$emmean) - min(emm0.1$emmean)) / 2, label='', size=7)
figure 


# Correction for multiple comparisons ####
names = c('F0_Control', 'F0_Control', 'F0_Control', 'F0_Slow', 'F0_Slow', 'F0_Slow', 
          'Jitter_Control', 'Jitter_Control', 'Jitter_Control', 'Jitter_Slow',  'Jitter_Slow', 'Jitter_Slow', 
          'Shimmer_Control', 'Shimmer_Control', 'Shimmer_Control', 'Shimmer_Slow', 'Shimmer_Slow', 'Shimmer_Slow', 
          'HNR_Control', 'HNR_Control', 'HNR_Control', 'HNR_Slow', 'HNR_Slow', 'HNR_Slow', 
          'MeanVoiced_Control', 'MeanVoiced_Control', 'MeanVoiced_Control', 'MeanVoiced_Slow', 'MeanVoiced_Slow', 'MeanVoiced_Slow', 
          'Voiced_Control', 'Voiced_Control', 'Voiced_Control', 'Voiced_Slow', 'Voiced_Slow', 'Voiced_Slow')
ps = list()
ps[names] = p.adjust(pvalues, method = "fdr", length(pvalues)) # Create list containing fdr corrected pvalues
ps
pvalues
p.adjust(pvalues, method = "fdr", length(pvalues))
