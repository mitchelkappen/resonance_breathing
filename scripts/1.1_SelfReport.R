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
    ggplot(emmean_dataframe, aes(x=Phase, y=emmean, colour = Phase)) +
      geom_point(size = 5) + 
      geom_line(aes(group = 1),size = 1, colour = "black", linetype = "dotted")+
      geom_errorbar(width=.25, size = 1.4, aes(ymin=emmean-SE, ymax=emmean+SE))+
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
densityPlot(data$AF_NegativeAffect)
densityPlot(data$AF_ActivatingPositiveAffect)
densityPlot(data$AF_SoothingPositiveAffect)

summary(data) # I like to do this because it is easy to check the variable types


####### Stats #######

# 1) Negative Affect ######
formula <- 'AF_NegativeAffect ~ Phase*tDCSGroup*Breathing_Condition + (1|Subject)' # Declare formula

dataModel = data # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

modelNames = c(d0.1)
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:tDCSGroup:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase, adjust ="fdr", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

NegAffectPlot<- plotfunction(emm0.1, "Negative Affect")
NegAffectPlot<- NegAffectPlot + annotate('text', x=1.5, y=mean(emm0.1$emmean) + (max(emm0.1$emmean) - min(emm0.1$emmean)) / 2, label='', size=7)
NegAffectPlot 

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

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase, adjust ="fdr", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

ActPositivePlot<- plotfunction(emm0.1, "Activating Positive Affect")
ActPositivePlot<- ActPositivePlot + annotate('text', x=1.5, y=mean(emm0.1$emmean) + (max(emm0.1$emmean) - min(emm0.1$emmean)) / 2, label='', size=7)
ActPositivePlot

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

emmeans0.1<- emmeans(chosenModel[[1]], pairwise ~ Phase, adjust ="fdr", type = "response") # Pairwise comparisons
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

SootPositivePlot<- plotfunction(emm0.1, "Soothing Positive Affect")
SootPositivePlot<- SootPositivePlot + annotate('text', x=1.5, y=mean(emm0.1$emmean) + (max(emm0.1$emmean) - min(emm0.1$emmean)) / 2, label='', size=7)
SootPositivePlot


