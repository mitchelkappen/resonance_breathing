# Load in packages
install.packages("foreign")
install.packages("ggplot2")
install.packages("MASS")
library(car)
library(lme4)
library(lattice)
library(reshape2)
library(rio)
library(writexl)
library(splitstackshape)
library(readr)
library(plotly)
library(dplyr)
library(emmeans)
library(foreign)
library(ggplot2)
library(MASS)

# Reading in data
Data <- read_delim("C:/Users/Gebruiker/Desktop/Psychologie/Psycho 22-23/Stage 22-23/Mitchel/Data_SF.txt", 
                   delim = "\t" , escape_double = FALSE, 
                   col_names = TRUE, trim_ws = TRUE)

# Remove unnecessary columns
Data = subset(Data, select = -c(PTQ_Total:SERI_Acceptance, PASA_Threat:PASA_StressIndex)) #removing unnecessary columns
Data<-na.omit(Data) #removing data containing NA's

# Removing the last two phases (and all related data)
Data$DataCheck<- grepl("SART", Data$Phase) 
which(Data$DataCheck == TRUE)[1] #find first row were phase is equal to "SART"
Data <- Data[c(1 : ((which(Data$DataCheck == TRUE)[1]))-1), ] #remove all rows after the first "SART" (including first "SART")
Data = subset(Data, select = -c(DataCheck))

# Change factors to numeric or factor
Data$Subject<- as.factor(Data$Subject)
Data$AF_NegativeAffect<- as.numeric(Data$AF_NegativeAffect)
Data$AF_ActivatingPositiveAffect<- as.numeric(Data$AF_ActivatingPositiveAffect)
Data$AF_SoothingPositiveAffect<- as.numeric(Data$AF_SoothingPositiveAffect)
Data$tDCSGroup<- as.factor(as.numeric(as.factor(Data$tDCSGroup)))
Data$Phase<- as.factor(as.numeric(as.factor(Data$Phase)))
Data$Breathing_Condition<- as.factor(as.numeric(as.factor(Data$Breathing_Condition)))
 
# Visualization
hist(Data$AF_NegativeAffect)
plot(Data$Phase, Data$AF_NegativeAffect)
hist(Data$AF_ActivatingPositiveAffect)
plot(Data$Phase, Data$AF_ActivatingPositiveAffect)
hist(Data$AF_SoothingPositiveAffect)
plot(Data$Phase, Data$AF_SoothingPositiveAffect)

bwplot(AF_NegativeAffect ~ tDCSGroup | Breathing_Condition, data = Data)
bwplot(AF_NegativeAffect ~ Phase, data = Data)
bwplot(AF_ActivatingPositiveAffect ~ Phase, data = Data)
bwplot(AF_SoothingPositiveAffect ~ Phase, data = Data)

# Testing

# 1) NegativeAffect

test1A <- glmer(AF_NegativeAffect ~ Phase + (1|Subject), data=Data) #Linear Mixed Model 
summary(test1A)

test1B <- glmer(AF_NegativeAffect ~ Phase + tDCSGroup + Breathing_Condition + (1|Subject),data=Data) #Linear Mixed Model with extra predictors
summary(test1B)

test1C <- glm.nb(AF_NegativeAffect ~ Phase, data = Data) #Negative Binomial 
summary(test1C)

test1D <- glm.nb(AF_NegativeAffect ~ Phase + tDCSGroup + Breathing_Condition, data = Data) #Negative Binomial with extra predictors
summary(test1D)

AIC(test1A)
AIC(test1B)
AIC(test1C)
AIC(test1D)

Anova(test1D, type= "III") #anova with the best model (lowest AIC)

# 2) ActivatingPositiveAffect

test1A <- glmer(AF_ActivatingPositiveAffect ~ Phase + (1|Subject), data=Data) #Linear Mixed Model 
summary(test1A)

test1B <- glmer(AF_ActivatingPositiveAffect ~ Phase + tDCSGroup + Breathing_Condition + (1|Subject),data=Data) #Linear Mixed Model with extra predictors
summary(test1B)

test1C <- glm.nb(AF_ActivatingPositiveAffect ~ Phase, data = Data) #Negative Binomial 
summary(test1C)

test1D <- glm.nb(AF_ActivatingPositiveAffect ~ Phase + tDCSGroup + Breathing_Condition, data = Data) #Negative Binomial with extra predictors
summary(test1D)

AIC(test1A)
AIC(test1B)
AIC(test1C)
AIC(test1D)

Anova(test1B, type= "III") #anova with the best model (lowest AIC)

# 3) SoothingPositiveAffect

test1A <- glmer(AF_SoothingPositiveAffect ~ Phase + (1|Subject), data=Data) #Linear Mixed Model 
summary(test1A)

test1B <- glmer(AF_SoothingPositiveAffect ~ Phase + tDCSGroup + Breathing_Condition + (1|Subject),data=Data) #Linear Mixed Model with extra predictors
summary(test1B)

test1C <- glm.nb(AF_SoothingPositiveAffect ~ Phase, data = Data) #Negative Binomial 
summary(test1C)

test1D <- glm.nb(AF_SoothingPositiveAffect ~ Phase + tDCSGroup + Breathing_Condition, data = Data) #Negative Binomial with extra predictors
summary(test1D)

AIC(test1A)
AIC(test1B)
AIC(test1C)
AIC(test1D)

Anova(test1B, type= "III") #anova with the best model (lowest AIC)




