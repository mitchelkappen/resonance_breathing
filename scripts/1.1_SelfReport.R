# Load in packages
# install.packages("egg")
# install.packages("multcompView")
# install.packages("ggthemes")
# install.packages("ggsignif")

library(car)
library(lme4)
library(reshape2)
library(readr)
library(plotly)
library(dplyr)
library(emmeans)
library(ggplot2)
library(effects) 
library(egg)
library(multcompView)
library(ggthemes) 
library(ggsignif)

##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console # # Or ctrl + l in VSCode
dev.off() # Clear plot window

# Set and Get directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set WD to script location
# source("functions.R") # Load document where functions are stored
options(contrasts = c("contr.sum","contr.poly")) # Use this for the p value of the t test

nAGQ = 1
plotPrefix <- "/../figures/"

##### Loading data ##### 
# Reading in data
data <- read_delim("data_SF.txt")
data$tDCSGroup[data$tDCSGroup == 'Active '] = 'Active'
data$tDCSGroup[data$tDCSGroup == 'Sham '] = 'Sham'
# Remove unnecessary columns
data = subset(data, select = -c(PTQ_Total:SERI_Acceptance, PASA_Threat:PASA_StressIndex)) #removing unnecessary columns

# Removing the last two phases (and all related data)
data = data[data$Phase != "SART", ]
data = data[data$Phase != "PassiveViewing", ]

# Change factors to numeric or factor
data$Subject<- as.factor(data$Subject)
data$AF_NegativeAffect<- as.numeric(data$AF_NegativeAffect)
data$AF_ActivatingPositiveAffect<- as.numeric(data$AF_ActivatingPositiveAffect)
data$AF_SoothingPositiveAffect<- as.numeric(data$AF_SoothingPositiveAffect)
data$tDCSGroup<- as.factor(as.factor(data$tDCSGroup))
data$Phase<- ordered(data$Phase, levels = c('Habituation', 'Breathing', 'Stress')) # Factorize (ordered) moment
data$Breathing_Condition<- as.factor(data$Breathing_Condition)

# Visualization
hist(data$AF_NegativeAffect)
hist(data$AF_ActivatingPositiveAffect)
hist(data$AF_SoothingPositiveAffect)


####### Stats #######

# 1) Negative Affect ######
formula <- 'AF_NegativeAffect ~ Phase*tDCSGroup*Breathing_Condition + (1|Subject)' # Declare formula

dataModel = data # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
# d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
# d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

modelNames = c(d0.1)
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:tDCSGroup:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans(chosenModel[[1]], pairwise ~ Phase, adjust ="fdr", type = "response") # Pairwise comparisons

# 2) AF_ActivatingPositiveAffect ######
formula <- 'AF_ActivatingPositiveAffect ~ Phase*tDCSGroup*Breathing_Condition + (1|Subject)' # Declare formula

dataModel = data # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
# d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
# d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

modelNames = c(d0.1)
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:tDCSGroup:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans(chosenModel[[1]], pairwise ~ Phase, adjust ="fdr", type = "response") # Pairwise comparisons

# 3) AF_SoothingPositiveAffect ######
formula <- 'AF_SoothingPositiveAffect ~ Phase*tDCSGroup*Breathing_Condition + (1|Subject)' # Declare formula

dataModel = data # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
# d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
# d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

modelNames = c(d0.1)
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:tDCSGroup:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans(chosenModel[[1]], pairwise ~ Phase, adjust ="fdr", type = "response")# Pairwise comparisons


####### plots #######


# 1) AF_NegativeAffect

# Table with the mean and the standard deviation for every combination
dt1 <- group_by(data, Phase, Breathing_Condition, tDCSGroup) %>%
  summarise(NegAffect_mean=mean(AF_NegativeAffect), sd=sd(AF_NegativeAffect)) %>%
  arrange(desc(NegAffect_mean))
dt1

# Barplot based on dt
ggplot(dt1, aes(x = Phase, y = NegAffect_mean, fill = tDCSGroup:Breathing_Condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  # geom_errorbar(aes(ymax = NegAffect_mean + sd, ymin = NegAffect_mean - sd),
  #               position = position_dodge(0.9), width = 0.25, color = "Gray25") + # Include errorbar
  geom_signif(comparisons = list(c("Stress", "Breathing")), map_signif_level=T) +  # Include significance lines
  geom_signif(comparisons = list(c("Habituation", "Stress")), map_signif_level=T) +  # Include significance lines
  scale_fill_brewer(palette = "Greens") +
  theme_few()

# 2) AF_ActivatingPositiveAffect

# Table with the mean and the standard deviation for every combination
dt2 <- group_by(data, Phase, Breathing_Condition, tDCSGroup) %>%
  summarise(APosAffect_mean=mean(AF_ActivatingPositiveAffect), sd=sd(AF_ActivatingPositiveAffect)) %>%
  arrange(desc(APosAffect_mean))
dt2

# Barplot based on dt
ggplot(dt2, aes(x = Phase, y = APosAffect_mean, fill = tDCSGroup:Breathing_Condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  # geom_errorbar(aes(ymax = APosAffect_mean + sd, ymin = APosAffect_mean - sd),
  #               position = position_dodge(0.9), width = 0.25, color = "Gray25") + # Include errorbar
 geom_signif(comparisons = list(c("Habituation", "Breathing")), map_signif_level=T) +  # Include significance lines
  scale_fill_brewer(palette = "Greens") +
  theme_few()

# 3) AF_SoothingPositiveAffect

# Table with the mean and the standard deviation for every combination
dt3 <- group_by(data, Phase, Breathing_Condition, tDCSGroup) %>%
  summarise(SPosAffect_mean=mean(AF_SoothingPositiveAffect), sd=sd(AF_SoothingPositiveAffect)) %>%
  arrange(desc(SPosAffect_mean))
dt3

# Barplot based on dt
ggplot(dt3, aes(x = Phase, y = SPosAffect_mean, fill = tDCSGroup:Breathing_Condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  # geom_errorbar(aes(ymax = SPosAffect_mean + sd, ymin = SPosAffect_mean - sd),
  #               position = position_dodge(0.9), width = 0.25, color = "Gray25") + # Include errorbar
  geom_signif(comparisons = list(c("Stress", "Breathing")), map_signif_level=T) +   # Include significance lines
  geom_signif(comparisons = list(c("Habituation", "Stress")), map_signif_level=T) +  # Include significance lines
  scale_fill_brewer(palette = "Greens") +
  theme_few()

