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

# Merge data and put two identifier columns up front
allData = full_join(audioData, questionData, by = c("participantNum","Phase"))
allData <- allData[, c("participantNum", "Phase", setdiff(names(allData), c("participantNum", "Phase")))]

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


