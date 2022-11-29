# Load in packages
# install.packages("foreign")
# install.packages("ggplot2")
# install.packages("MASS")

library(car)
library(lme4)
# library(lattice)
library(reshape2)
# library(rio)
# library(writexl)
# library(splitstackshape)
library(readr)
library(plotly)
library(dplyr)
library(emmeans)
# library(foreign)
library(ggplot2)
# library(MASS)
library(effects) # Needed for plotting of effects from Anova

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
data <- read_delim("../loc_data/data_SF.txt")
data$tDCSGroup[data$tDCSGroup == 'Active '] = 'Active'
data$tDCSGroup[data$tDCSGroup == 'Sham '] = 'Sham'
# Remove unnecessary columns
data = subset(data, select = -c(PTQ_Total:SERI_Acceptance, PASA_Threat:PASA_StressIndex)) #removing unnecessary columns
data <- na.omit(data) #removing data containing NA's

# Removing the last two phases (and all related data)
data = data[data$Phase != "SART", ]

# Change factors to numeric or factor
data$Subject<- as.factor(data$Subject)
data$AF_NegativeAffect<- as.numeric(data$AF_NegativeAffect)
data$AF_ActivatingPositiveAffect<- as.numeric(data$AF_ActivatingPositiveAffect)
data$AF_SoothingPositiveAffect<- as.numeric(data$AF_SoothingPositiveAffect)
data$tDCSGroup<- as.factor(as.factor(data$tDCSGroup))
data$Phase<- ordered(data$Phase, levels = c('Habituation', 'Breathing', 'Stress', 'PassiveViewing')) # Factorize (ordered) moment
data$Breathing_Condition<- as.factor(data$Breathing_Condition)
 
# Visualization
hist(data$AF_NegativeAffect)
hist(data$AF_ActivatingPositiveAffect)
hist(data$AF_SoothingPositiveAffect)


####### Stats #######
# Stats: Negative Affect ######
formula <- 'AF_NegativeAffect ~ Phase*tDCSGroup*Breathing_Condition + (1|Subject)' # Declare formula

dataModel = data # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
# The following is what I would normally do, when choosing from three models. In this case, we couldn't do d0.2 and d0.3 because there are zeroes present. I will comment this segment, and show you how to use the code otherwise.
# modelNames = c(d0.1,d0.2,d0.3)
# tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
# chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

# This is what we do when only d0.2 can be made:
#    Indeed, you could also just use d0.1 directly in the Anova, but this makes it easier to copy and paste all your code throughout. 
modelNames = c(d0.1)
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')
plot(effect("Phase:tDCSGroup:Breathing_Condition", chosenModel[[1]])) # Visualize the three way interaction we are interested in

######### This is where I end now #############
# Do the same, and follow the same structure as here, for all the different variables. 
# Then you will have significance for each of the effects of interest. 
# And also already some rough visualisations

# Next steps will be:
#    1) pairwise comparisons with emmeans
#    2) decent visualisations, custom made for this cause, also showing significance!


########### Under here is your code #############
# Testing

# 1) NegativeAffect

test1A <- glmer(AF_NegativeAffect ~ Phase + (1|Subject), data=data) #Linear Mixed Model 
summary(test1A)

test1B <- glmer(AF_NegativeAffect ~ Phase + tDCSGroup + Breathing_Condition + (1|Subject),data=data) #Linear Mixed Model with extra predictors
summary(test1B)

test1C <- glm.nb(AF_NegativeAffect ~ Phase, data = data) #Negative Binomial 
summary(test1C)

test1D <- glm.nb(AF_NegativeAffect ~ Phase + tDCSGroup + Breathing_Condition, data = data) #Negative Binomial with extra predictors
summary(test1D)

AIC(test1A)
AIC(test1B)
AIC(test1C)
AIC(test1D)

Anova(test1D, type= "III") #anova with the best model (lowest AIC)

# 2) ActivatingPositiveAffect

test1A <- glmer(AF_ActivatingPositiveAffect ~ Phase + (1|Subject), data=data) #Linear Mixed Model 
summary(test1A)

test1B <- glmer(AF_ActivatingPositiveAffect ~ Phase + tDCSGroup + Breathing_Condition + (1|Subject),data=data) #Linear Mixed Model with extra predictors
summary(test1B)

test1C <- glm.nb(AF_ActivatingPositiveAffect ~ Phase, data = data) #Negative Binomial 
summary(test1C)

test1D <- glm.nb(AF_ActivatingPositiveAffect ~ Phase + tDCSGroup + Breathing_Condition, data = data) #Negative Binomial with extra predictors
summary(test1D)

AIC(test1A)
AIC(test1B)
AIC(test1C)
AIC(test1D)

Anova(test1B, type= "III") #anova with the best model (lowest AIC)

# 3) SoothingPositiveAffect

test1A <- glmer(AF_SoothingPositiveAffect ~ Phase + (1|Subject), data=data) #Linear Mixed Model 
summary(test1A)

test1B <- glmer(AF_SoothingPositiveAffect ~ Phase + tDCSGroup + Breathing_Condition + (1|Subject),data=data) #Linear Mixed Model with extra predictors
summary(test1B)

test1C <- glm.nb(AF_SoothingPositiveAffect ~ Phase, data = data) #Negative Binomial 
summary(test1C)

test1D <- glm.nb(AF_SoothingPositiveAffect ~ Phase + tDCSGroup + Breathing_Condition, data = data) #Negative Binomial with extra predictors
summary(test1D)

AIC(test1A)
AIC(test1B)
AIC(test1C)
AIC(test1D)

Anova(test1B, type= "III") #anova with the best model (lowest AIC)




