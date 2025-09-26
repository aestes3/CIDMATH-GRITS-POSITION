library(here)
library(tidyverse)
library(DT)


#Load NIS dataset
load(here("data/NISPUF23.RData"))

#View Head of dataset
datatable(head(NISPUF23))

#Create Subset of data for MMR
#Includes SEQNUMC (ID), AGEGRP (Age range), PU4313313 (Complete series UTD),
#P_NUMMMR (P_UTDMMX) (MMR UTD), DMMR1-3(MMR ONLY UTD SHOTS)
all_mmr_strict <- subset(NISPUF23, select = c(SEQNUMC, AGEGRP, P_NUMMMR, P_UTDMMX, DMMR1, DMMR2, DMMR3))

#Use Pivot to convert from Wide to Long format
#all_mmr_strict_long <- all_mmr_strict |> 
#    pivot_longer(
 #       cols = c("AGEGRP", "P_NUMMMR", "DMMR1", "DMMR2", "DMMR3"),
 #       names_to = "Variable",
 #      values_to = "DMMR3"
 #   )

all_mmr_strict_long <- all_mmr_strict |> 
    pivot_longer(
        cols = starts_with("P_UTD"),
        names_to = "variable",
        values_to = "UTD"
    ) |> 
    # Check number of shots
    mutate(observation = gsub("UTD", "", variable)) |> 
    select(-variable) |> 
    pivot_longer(cols = starts_with("P_NUM"),
                 names_to = "variable",
                 values_to = "SHOT"
    ) |> 
    mutate(observation = gsub("SHOT", "", variable)) |> 
    select(-variable) |> 
    pivot_longer(cols = starts_with("DMMR"),
                 names_to = "variable",
                 values_to = "DMMR") |> 
    mutate(observation = gsub("DMMR", "", variable)) |> 
    select(SEQNUMC, UTD, SHOT, DMMR)

#Create birthdate 
