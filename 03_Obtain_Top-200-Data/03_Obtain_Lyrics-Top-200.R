# Loading Libs ------------------------------------------------------------
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

# Loading Necessary Data and Functions ------------------------------------
load(here("03_Obtain_Top-200-Data/functions/keys"))
source(here("03_Obtain_Top-200-Data/functions/Gen_Top200_Features.R"))
source(here("03_Obtain_Top-200-Data/functions/Analyze_Top200_Lyrics.R"))
source(here("03_Obtain_Top-200-Data/functions/Gen_Top200_Lyrics.R"))
source("03_Obtain_Top-200-Data/functions/Playlist_Comparison_Function.R")


# Load_Data ---------------------------------------------------------------
#NOTE: This data already has feature information, so do not need to use the Features_Function below. 
all_songs <- read_rds(here("03_Obtain_Top-200-Data/data/Jan_2017_Dec_2020.rds")) %>% #Getting consistent names
  rename("Song"=Track_Name,"Id"=URI) 
  

#Getting just distinct songs, so I only get lyrics and lyrics features for each song once
data <- data %>% 
  #Playlists here are the years
  mutate(Playlist=year(date)) %>% 
  #Selecting only those columns I'd get from Tracks_Function
  select(Song,Id, Artist,"Artist_id"=artist_id,"Album"=album,"Album_id" = album_id,Playlist) %>% 
  distinct(Id,.keep_all = T)



# Get Features (Unnecessary Here, already have them) -----------------------------------------
#Features <- Features_Function(track_data = data)
#saveRDS(Features,here("03_Obtain_Top-200-Data/data_new/Top200_Features.rds"))


# Get Lyrics and Lyric Features --------------------------------------------------------------
#Raw features
Lyrics <- Lyric_Generation_Function(data)
#saveRDS(Lyrics,here("03_Obtain_Top-200-Data/data_new/Top200_Lyrics.rds"))


#Getting features of the lyrics
Lyric_Features <- Lyric_Analysis_Function(Lyrics) %>% select(-Lyrics)
Lyric_Features <- Lyric_Features %>% distinct()
#saveRDS(Lyric_Features,here("03_Obtain_Top-200-Data/data_new/Top200_Lyric_Features.rds"))

#Getting a full lyrics column

#Joining
Full_Data <-  all_songs %>% left_join(Lyrics) %>% left_join(Lyric_Features) 
saveRDS(Full_Data,here("03_Obtain_Top-200-Data/data_new/Full_Top_200_Data.rds"))

