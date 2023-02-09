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
plotDirectory = dirname(rstudioapi::getActiveDocumentContext()$path)
source("functions.R") # Load document where functions are stored
options(contrasts = c("contr.sum","contr.poly")) #use this for the p value of the t test

nAGQ = 1
plotPrefix <- "/../figures/"
pvalues = c() # Create a variable to store all p-values to correct later

# Load in data ####
questionData <- read_delim("../loc_data/Data_SF.txt")
audioData <- as.data.frame(read_parquet("../loc_data/df_gemaps_func_16khz_noisy.parquet"))

# Merge data
questionData <- questionData %>% rename(participantNum = Subject) # Give same Identifier name
audioData <- audioData %>% rename(Phase = taskType) # Give same Phase name
audioData <- audioData %>% mutate(Phase = substring(Phase, 3)) # Rename Phase variables for matching
questionData$Phase[questionData$Phase == "Stress"] <- "Calculus" # Rename Phase variables for matching

# Clean up - Take out irrelevant phases
audioData <- audioData[audioData$Phase != "SART" & audioData$Phase != "PassiveViewing", ] # Kick out final two phases
questionData <- questionData[questionData$Phase != "SART" & questionData$Phase != "PassiveViewing", ] # Kick out final two phases

allData = merge(audioData, questionData, by = "participantNum") # Merge audioData with trait questionnaires

allData = merge(audioData, questionData, by = c("participantNum","Phase"))
allData = full_join(audioData, questionData, by = c("participantNum","Phase"))

#### TRANSCRIPTS #####
breathing <- read.csv("../loc_data/sentiment/Breathing_features.csv")
transcripts <- read.csv("../loc_data/df_transcripts.csv")
temp <- transcripts[transcripts$phaseName == "Breathing", ]
