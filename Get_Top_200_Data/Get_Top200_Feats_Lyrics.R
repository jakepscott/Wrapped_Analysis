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
load(here("Get_Top_200_Data/functions/keys"))
source(here("Get_Top_200_Data/functions/Gen_Top200_Features.R"))
source(here("Get_Top_200_Data/functions/Analyze_Top200_Lyrics.R"))
source(here("Get_Top_200_Data/functions/Gen_Top200_Lyrics.R"))
source("Get_Top_200_Data/functions/Playlist_Comparison_Function.R")


# Load_Data ---------------------------------------------------------------
all_songs <- read_rds(here("Get_Top_200_Data/data/Jan_2017_Dec_2020.rds")) %>% #Getting consistent names
  rename("Song"=Track_Name,"Id"=URI) 
  
data <- data %>% 
  #Playlists here are the years
  mutate(Playlist=year(date)) %>% 
  #Selecting only those columns I'd get from Tracks_Function
  select(Song,Id, Artist,"Artist_id"=artist_id,"Album"=album,"Album_id" = album_id,Playlist) %>% 
  distinct(Id,.keep_all = T)


#Get Features
#Features <- Features_Function(track_data = data)
#saveRDS(Features,here("Get_Top_200_Data/data_new/Top200_Features.rds"))
#Get lyrics using function
Lyrics <- Lyric_Generation_Function(data)
#saveRDS(Lyrics,here("Get_Top_200_Data/data_new/Top200_Lyrics.rds"))


#Getting features of the lyrics
Lyric_Features <- Lyric_Analysis_Function(Lyrics) %>% select(-Lyrics)
Lyric_Features <- Lyric_Features %>% distinct()
#saveRDS(Lyric_Features,here("Get_Top_200_Data/data_new/Top200_Lyric_Features.rds"))


#Joining
Full_Data <-  all_songs %>% left_join(Lyrics) %>% left_join(Lyric_Features) 
saveRDS(Full_Data,here("Get_Top_200_Data/data_new/Full_Top_200_Data.rds"))

