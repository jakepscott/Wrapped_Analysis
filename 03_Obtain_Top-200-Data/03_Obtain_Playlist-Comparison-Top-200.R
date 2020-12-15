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

source("03_Obtain_Top-200-Data/functions/Playlist_Comparison_Function.R")
data <- read_rds(here("data/Full_Top_200_Feat_Lyrics_Data.rds"))
data_clean <- data %>% select(-URL,-uri,-analysis_url,-artist_id,-Artist_id,-album_id,-Album_id,-label,
                -album_popularity,-album_type,-followers,-album_dummy,-time_signature_dummy,-Playlist,-Streams,-album)
names(data_clean) <- str_to_title(names(data_clean))

comparison_data <- data_clean %>% mutate(Playlist=year(Date)) %>% Playlist_Comparison_Function()

saveRDS(comparison_data,here("03_Obtain_Top-200-Data/data/Top200_Playlist_Data"))
saveRDS(comparison_data,here("data/Top200_Playlist_Data"))
