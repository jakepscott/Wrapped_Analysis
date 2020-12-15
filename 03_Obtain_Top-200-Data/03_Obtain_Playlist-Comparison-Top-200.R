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

source(here("03_Obtain_Top-200-Data/functions/04-Compare_Top200_Playlists_Function.R"))
data <- read_rds(here("03_Obtain_Top-200-Data/data/Full_Top_200_Feat_Lyrics_Data.rds"))
data <- data %>% mutate(Playlist=as.character(year(date)))

comparison_data <- data %>% Playlist_Comparison_Function()

saveRDS(comparison_data,here("03_Obtain_Top-200-Data/data/Top200_Playlist_Data"))
saveRDS(comparison_data,here("data/Top200_Playlist_Data"))
