# Loading Libs and data------------------------------------------------------------
library(tidyverse)
library(Rspotify)
library(lubridate)
library(tidytext)
library(geniusr)
library(stringr)
library(rvest)
library(remotes)
library(genius)
library(stringi)
library(here)

data <- read_rds(here("03_Obtain_Top-200-Data/data/Full_Top_200_Feat_Lyrics_Data.rds"))
data <- data %>% mutate(Playlist=as.character(year(date)))


# Unweighted Comparison ---------------------------------------------------
source(here("03_Obtain_Top-200-Data/functions/04-Compare_Top200_Playlists_Function.R"))
comparison_data <- data %>% Playlist_Comparison_Function()

saveRDS(comparison_data,here("03_Obtain_Top-200-Data/data/Top200_Playlist_Data.rds"))
saveRDS(comparison_data,here("data/Top200_Playlist_Data.rds"))



# Weighting By Streams ----------------------------------------------------
source(here("03_Obtain_Top-200-Data/functions/05-Weighted-Compare_Top200_Playlists_Function.R"))
Jan_2017_Dec_2020 <- read_rds(here("03_Obtain_Top-200-Data/data/Jan_2017_Dec_2020.rds"))
streams <- Jan_2017_Dec_2020 %>% select(date,"Id"=URI,Streams) %>% distinct()
data <- data %>% left_join(streams)
data <- data %>% mutate(Streams=as.numeric(Streams))
weighted_comparison_data <- data %>% Weighted_Playlist_Comparison_Function()

saveRDS(weighted_comparison_data,here("03_Obtain_Top-200-Data/data/Top200_Weighted_Playlist_Data.rds"))
saveRDS(weighted_comparison_data,here("data/Top200_Weighted_Playlist_Data.rds"))
