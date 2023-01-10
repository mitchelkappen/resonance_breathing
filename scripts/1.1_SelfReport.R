##############################
#                            #
#  Self Report Analysis  V1  #
#    Res. Breathing study    #
#           x tDCS           #
#                            #
#############################
# 
# Author: Xander Cornelis & Mitchel Kappen 
# 12-12-2022

# Load in packages
# install.packages("egg")
# install.packages("multcompView")
# install.packages("ggthemes")
# install.packages("ggsignif")
# install.packages("papaja")

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
library(papaja)

##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console # # Or ctrl + l in VSCode
dev.off() # Clear plot window

cbPalette <- c("#F0E442", "#0072B2", "#D55E00") # Define Colorblind proof plotting colors

# Set and Get directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set WD to script location
# source("functions.R") # Load document where functions are stored
options(contrasts = c("contr.sum","contr.poly")) # Use this for the p value of the t test

nAGQ = 1
plotPrefix <- "/../figures/"

# function for plot
plotfunction <-
  function(emmean_dataframe, title){
    ggplot(emmean_dataframe, aes(x=Phase, y=emmean, colour = Breathing_Condition)) +
      geom_point(aes(group = Breathing_Condition), size = 4) + 
      geom_line(aes(group = Breathing_Condition),size = 1, colour = "black", linetype = "dotted")+
      geom_errorbar(width=.25, size = 1, aes(ymin=emmean-SE, ymax=emmean+SE))+
      labs(y = title, x = "Phase")+
      scale_colour_manual(values=cbPalette)+
      theme_apa()
  }

##### Loading data ##### 
# Reading in data
data <- read_delim("data_SF.txt")
data$tDCSGroup[data$tDCSGroup == 'Active '] = 'Active'
data$tDCSGroup[data$tDCSGroup == 'Sham '] = 'Sham'
# Remove unnecessary columns
data = subset(data, select = -c(PTQ_Total:SERI_Acceptance, PASA_Threat:PASA_StressIndex)) #removing unnecessary columns

# Removing active tDCS
data = data[data$tDCSGroup != "Active", ]
data = data[data$Phase != "Habituation", ]

# Removing the last two phases (and all related data)
data = data[data$Phase != "SART", ]
data = data[data$Phase != "PassiveViewing", ]

# Change factors to numeric or factor
data$Subject<- as.factor(data$Subject)

data$tDCSGroup<- as.factor(as.factor(data$tDCSGroup))
data$Phase<- ordered(data$Phase, levels = c('Habituation', 'Breathing', 'Stress')) # Factorize (ordered) moment
data$Breathing_Condition<- as.factor(data$Breathing_Condition)

# Visualization
densityPlot(data$AF_NegativeAffect)
densityPlot(data$AF_ActivatingPositiveAffect)
densityPlot(data$AF_SoothingPositiveAffect)

summary(data) # I like to do this because it is easy to check the variable types


####### Stats #######

# 1) Negative Affect ######
formula <- 'AF_NegativeAffect ~ Phase*Breathing_Condition + (1|Subject)' # Declare formula

dataModel = data # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

modelNames = c(d0.1)
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase*Breathing_Condition, adjust ="fdr", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure<- plotfunction(emm0.1, "Negative Affect")
figure<- figure + annotate('text', x=1.5, y=mean(emm0.1$emmean) + (max(emm0.1$emmean) - min(emm0.1$emmean)) / 2, label='', size=7)
figure<- figure + 
  geom_signif(comparisons = list(c("Breathing", "Stress")), annotations="***",
              y_position = 28, tip_length = 0.1, vjust=0.4, color = "black")
figure 

# 2) AF_ActivatingPositiveAffect ######
formula <- 'AF_ActivatingPositiveAffect ~ Phase*Breathing_Condition + (1|Subject)' # Declare formula

dataModel = data # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
# d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
# d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

modelNames = c(d0.1)
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase*Breathing_Condition, adjust ="fdr", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure<- plotfunction(emm0.1, "Activating Positive Affect")
figure<- figure + annotate('text', x=1.5, y=mean(emm0.1$emmean) + (max(emm0.1$emmean) - min(emm0.1$emmean)) / 2, label='', size=7)
figure

# 3) AF_SoothingPositiveAffect ######
formula <- 'AF_SoothingPositiveAffect ~ Phase*Breathing_Condition + (1|Subject)' # Declare formula

dataModel = data # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
# d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
# d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

modelNames = c(d0.1)
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase*Breathing_Condition, adjust ="fdr", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure<- plotfunction(emm0.1, "Soothing Positive Affect")
figure<- figure + annotate('text', x=1.5, y=mean(emm0.1$emmean) + (max(emm0.1$emmean) - min(emm0.1$emmean)) / 2, label='', size=7)
figure<- figure + 
  geom_signif(comparisons = list(c("Breathing", "Stress")), annotations="***",
              y_position = 85, tip_length = 0.1, vjust=0.4, color = "black")
figure


