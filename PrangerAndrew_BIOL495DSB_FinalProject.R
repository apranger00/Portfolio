#BIOL 495DSB FINAL PROJECT
#Andrew Pranger (undergrad)
#This script will be used for deciphering the research project data for a proposal
#-------------------------------------------------------------------------------
#Clears environment
rm(list=ls())

#Set the working directory
setwd("C:/Users/apran/OneDrive/Desktop/Purdue/Classes/BIOL495DSB/Final Project/Final Project Datasets/class_project/Fish_data")

#Read in library, data set
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gt)
library(flextable)

fishID   <- read_csv("C:/Users/apran/OneDrive/Desktop/Purdue/Classes/BIOL495DSB/Final Project/Final Project Datasets/class_project/Fish_data/CFP011.csv")
fishData <- read_csv("C:/Users/apran/OneDrive/Desktop/Purdue/Classes/BIOL495DSB/Final Project/Final Project Datasets/class_project/Fish_data/CFP012 .csv")
#-------------------------------------------------------------------------------
#Data cleanup

fishData <- fishData %>% mutate(recode(SPPCode,  
                   'CAMANO' = "Central Stoneroller",
                   'CATCOM' = "White Sucker",
                   'CRAY'   = "Unidentified crayfish",
                   'CYPLUT' = "Red Shiner",
                   'ETHNIG' = "Johnny Darter",
                   'ETHSPE' = "Orangethroated Darter",
                   'GAMAFF' = "Western Mosquitofish",
                   'LEPCYA' = "Green Sunfish",
                   'LEPHUM' = "Orangespotted sunfish",
                   'LEPMAC' = "Bluegill",
                   'LEPMEG' = "Longear Sunfish", 
                   'LUXCOR' = "Common Shiner",
                   'NOTEXI' = "Slender Madtom",
                   'NOTPER' = "Carmine shiner",
                   'NOTSTR' = "Sand Shiner",
                   'ORCNAI' = "Water nymph crayfish",
                   'ORCNEG' = "Ringed crayfish",
                   'PHEMIR' = "Suckermouth Minnows",
                   'CHRERY' = "Southern Redbelly Dace",
                   'PIMNOT' = "Bluntnose Minnow",
                   'PIMPRO' = "Fathead Minnow",
                   'SEMATR' = "Creek Chub",
                   'TADPOLE' = "Unidentified tadpole")
)

fishData <- rename(fishData, "Species" = "recode(...)")

#In this code, I will be utilizing a colorblind friendly palette sourced from the internet.
#The code was sourced from Connelly, 2013, as cited in the report for this project. The code
#utilized is as follows:

###       scale_color_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73",
###                                  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))

#-------------------------------------------------------------------------------
#Creation of summary statistics for who fish data collection

FishCounts <- data.frame(aggregate(fishData$Count, by = list(Category = fishData$Species), FUN = sum))

FishCounts <- arrange(FishCounts, desc(x))

FCFlex <- flextable(FishCounts)

FCFlex <- set_caption(FCFlex, caption = "Fig. 1 - Total Species Trapped in the Konza Prarie, 1995-2018")
           
FCFlex <- set_header_labels(FCFlex, 
                         values = list(
                           Category = "Species",
                           x = "Quantity")
                        )
                   
FCFlex

#Based on this, species with 1000+ are Chrery, Camano, Ethspe, Sematr, and Cray

### We will adjust study to focus on these species, as they make up a large
### percentage of the fish found.

#-------------------------------------------------------------------------------
#Distribution of fish among the top 5 species

FishCounts1000 <- FishCounts[FishCounts$x > 1000,]

FishDistPlot   <- ggplot(data = FishCounts1000) +
                        aes(x = Category)
                        
                        
  FishDistPlot + geom_col() + aes(y = x, fill = Category) + 
  ggtitle("Fig. 2 - Distribution of Species in Konza Prarie, with over 1000 encounters") + 
  scale_x_discrete(label = abbreviate(FishCounts1000$Category, minlength = 8)) + ylab("Encounters") + labs(caption = 
  "Species from the Konza prarie that demonstrated over 1000 encounters during collection phase.") +
  scale_color_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))

#-------------------------------------------------------------------------------
#Distribution of fish among all watershed locations
watersheds    <- unique(fishData$Watershed)
  
watershedMID  <- subset(fishData, Watershed != "MID")
MIDTotal      <- sum(watershedMID$Count)

watershedN1B  <- subset(fishData, Watershed != "N1B")
N1BTotal      <- sum(watershedN1B$Count)

watershedN4D  <- subset(fishData, Watershed != "N4D")
N4DTotal      <- sum(watershedN4D$Count)

watershedNT  <- subset(fishData, Watershed != "NT")
NTTotal      <- sum(watershedNT$Count)

watershedK2A  <- subset(fishData, Watershed != "K2A")
K2ATotal      <- sum(watershedK2A$Count)

watershedsDis <- data.frame(Watersheds = c("MID", "N1B", "N4D", "NT", "K2A"),
                            Caught = c(MIDTotal, N1BTotal, N4DTotal, NTTotal, K2ATotal))

watershedsDisPlot <- ggplot(data = watershedsDis) + aes(x = watersheds)

watershedsDisPlot + geom_col() + aes(y = Caught, fill = watersheds) + 
  ggtitle("Fig. 3 - Distribution of Fish Caught at Watersheds in the Konza Prarie") + 
  xlab("Species") + ylab("Fish Caught") + 
  labs(caption = 
  "During the study, fish were caught at 5 watersheds displayed 
   in the distribution here: MID, N1B, N4D, NT, and K2A.") +
  scale_color_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73",
                              "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))

#-------------------------------------------------------------------------------
#Data analysis of top catching location (Watershed NT)

#First, with all species
watershedMIDResults <- fishData[fishData$Watershed == "MID",]

fishCountsMID <- data.frame(aggregate(fishData$Count, by = list(Category = fishData$Species), FUN = sum))

MIDDistPlot   <- ggplot(data = fishCountsMID) +
  aes(x = Category)  

MIDDistPlot + geom_col() + aes(y = x, fill = Category) + 
  ggtitle("Fig. 4 - MID Watershed at Konza Prarie, catching distribution by species from 1995-2018") + 
  scale_x_discrete(label = abbreviate(fishData$Species, minlength = 2)) + ylab("Encounters") + labs(caption = 
  "Distribution analysis of the MID watershed, 
   the most prominent watershed within the Gido et al. study.")


#Then, with T5 species
watershedMIDChrery <- subset(watershedMID, SPPCode != "CHRERY")
watershedMIDChrery <- sum(watershedMIDChrery$Count)

watershedMIDCamano <- subset(watershedMID, SPPCode != "CAMANO")
watershedMIDCamano <- sum(watershedMIDCamano$Count)

watershedMIDEthspe <- subset(watershedMID, SPPCode != "ETHSPE")
watershedMIDEthspe <- sum(watershedMIDEthspe$Count)

watershedMIDSematr <- subset(watershedMID, SPPCode != "SEMATR")
watershedMIDSematr <- sum(watershedMIDSematr$Count)

watershedMIDCray   <- subset(watershedMID, SPPCode != "CRAY")
watershedMIDCray   <- sum(watershedMIDCray$Count)

MIDSpeciesT5       <- data.frame(Species = c("Southern Redbelly Dace", "Central Stoneroller",
                                             "Orangethroated Darter", "Creek Chub", "Unidentified crayfish"),
                            Caught = c(watershedMIDChrery, watershedMIDCamano, 
                                       watershedMIDEthspe, watershedMIDSematr, watershedMIDCray))

MIDDistPlotT5      <- ggplot(data = MIDSpeciesT5) +
                      aes(x = Species)  

MIDDistPlotT5 + geom_col() + aes(y = Caught, fill = Species) + 
  ggtitle("Fig. 4 - MID Watershed at Konza Prarie, catching distribution 
          by species from 1995-2018 (Top 5 species only)") + 
  scale_x_discrete(label = abbreviate(fishData$Species, minlength = 8)) + ylab("Encounters") + labs(caption = 
  "Distribution analysis of the MID watershed, 
   the most prominent watershed within the Gido et al. study.") +
  scale_color_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73",
                              "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))

#-------------------------------------------------------------------------------
#Paired comparison of Chrery species at different locations

chreryCount  <- subset(fishData, SPPCode != "CHRERY")

chreryMID    <- subset(chreryCount, Watershed != "MID")
chreryMIDSum <- sum(chreryMID$Count)

chreryN1B    <- subset(chreryCount, Watershed != "N1B")
chreryN1BSum <- sum(chreryN1B$Count)

chreryN4D    <- subset(chreryCount, Watershed != "N4D")
chreryN4DSum <- sum(chreryN4D$Count)

chreryNT     <- subset(chreryCount, Watershed != "NT")
chreryNTSum <- sum(chreryNT$Count)

chreryK2A    <- subset(chreryCount, Watershed != "K2A")
chreryK2ASum <- sum(chreryK2A$Count)

chreryWatershedSums   <- data.frame(Watershed = c("MID", "N1B", "N4D", "NT", "K2A"),
                                  Encounters = c(chreryMIDSum, chreryN1BSum, 
                                                 chreryN4DSum, chreryNTSum, chreryK2ASum))

chreryWatershedSumsPl <- ggplot(data = chreryWatershedSums) +
  aes(x = Watershed)  

chreryWatershedSumsPl + geom_col() + aes(y = Encounters, fill = Watershed) + 
  ggtitle("Fig. 5 - Watershed Distribution of the Southern Redbelly Dace within Konza Prarie") + 
  xlab("Watershed") + ylab("Encounters") + labs(caption = 
  "Distribution analysis of the 5 watersheds.") +
  scale_color_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73",
                              "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))

#-------------------------------------------------------------------------------
#Ratio of dace caught to total fish caught in an environment
as.numeric(chreryMIDSum)
as.numeric(chreryN1BSum)
as.numeric(chreryN4DSum)
as.numeric(chreryNTSum)
as.numeric(chreryK2ASum)

chreryMIDRatio <- chreryMIDSum/MIDTotal

chreryN1BRatio <- chreryN1BSum/N1BTotal

chreryN4DRatio <- chreryN4DSum/N4DTotal

chreryNTRatio  <- chreryNTSum/NTTotal

chreryK2ARatio <- chreryK2ASum/K2ATotal

daceRatios     <- data.frame(Watershed = c("MID", "N1B", "N4D", "NT", "K2A"),
                             Ratio = c(chreryMIDRatio, chreryN1BRatio, 
                                       chreryN4DRatio, chreryNTRatio, chreryK2ARatio))

daceRatiosPl <- ggplot(data = daceRatios) +
                         aes(x = Watershed) 

daceRatiosPl + geom_col() + aes(y = Ratio, fill = Watershed) + 
  ggtitle("Fig. 6 - Ratios of the Southern Redbelly Dace to Total Catch Rates per Watershed") + 
  xlab("Watershed") + ylab("Ratio") + labs(caption = 
  "Ratio analysis of the dace per each watershed.") +
  scale_color_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73",
                              "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))

