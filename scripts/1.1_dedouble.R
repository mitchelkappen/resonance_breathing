# Set and Get directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set WD to script location
plotDirectory = dirname(rstudioapi::getActiveDocumentContext()$path)
source("functions.R") # Load document where functions are stored
options(contrasts = c("contr.sum","contr.poly")) #use this for the p value of the t test

nAGQ = 1
plotPrefix <- "/../figures/"
pvalues = c() # Create a variable to store all p-values to correct later

# De-double transcripts
transcriptData <- as.data.frame(read_parquet("E:/Data/2020_ResonanceBreathing/Data/Interim/Audio/df_transcripts.parquet"))
uniqueData <- transcriptData[!duplicated(transcriptData[c("pptNum", "phaseNum")]), ]
# When files came from share the phaseName was incorrect
uniqueData$phaseName[uniqueData$phaseNum == 1] = 'Habituation'
uniqueData$phaseName[uniqueData$phaseNum == 2] = 'Breathing'
uniqueData$phaseName[uniqueData$phaseNum == 3] = 'Calculus'
uniqueData$phaseName[uniqueData$phaseNum == 4] = 'SART'
uniqueData$phaseName[uniqueData$phaseNum == 5] = 'PassiveViewing'
# Write file
write_parquet(uniqueData, "E:/Data/2020_ResonanceBreathing/Data/Interim/Audio/df_transcripts_cleaned.parquet")
