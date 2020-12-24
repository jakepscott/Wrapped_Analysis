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
#First use functiion
source(here("03_Obtain_Top-200-Data/functions/04-Compare_Top200_Playlists_Function.R"))
comparison_data <- data %>% Playlist_Comparison_Function()

#Now manually get adjusted release date
Features <- read_rds(here("03_Obtain_Top-200-Data/data/Top200_Features_with_Ids.rds"))

#All songs from Jan 2017
Jan_to_Dec <- read_rds(here("03_Obtain_Top-200-Data/data/Jan_2017_Dec_2020.rds"))

#Rename columns to make them consistent
Jan_to_Dec <- Jan_to_Dec %>% rename("Song"=`Track Name`,"Id"=URI)

#Join the album release dates to every song from every day
Adj_Release_Dates <- Jan_to_Dec %>% left_join(Features)

#Take the day the song was on the top 200 list and subject the day it was released
Adj_Release_Dates <- Adj_Release_Dates %>% mutate(date=as.Date(date),
                                                  Album_Release_Date=as.Date(`Album Release Date`),
                                                  year=year(date),
                                                  `Days Since Release (Adj)`=date-Album_Release_Date)

#Summarise release date by year (playlist)
Adj_Release_Dates_Grouped <- Adj_Release_Dates %>% 
  mutate(Playlist=as.character(year)) %>% 
  select(-year) %>% 
  group_by(Playlist) %>% 
  summarise(`Median Days Since Release (Adj)`=median(`Days Since Release (Adj)`,na.rm = T)) 

comparison_data <- comparison_data %>% 
  left_join(Adj_Release_Dates_Grouped) %>%
  mutate(`Median Days Since Release (Adj)`=as.double(`Median Days Since Release (Adj)`))

#saveRDS(comparison_data,here("03_Obtain_Top-200-Data/data/Top200_Playlist_Data.rds"))
#saveRDS(comparison_data,here("data/Top200_Playlist_Data.rds"))



# Weighting By Streams ----------------------------------------------------
#First use the function
source(here("03_Obtain_Top-200-Data/functions/05-Weighted-Compare_Top200_Playlists_Function.R"))
Jan_2017_Dec_2020 <- read_rds(here("03_Obtain_Top-200-Data/data/Jan_2017_Dec_2020.rds"))
streams <- Jan_2017_Dec_2020 %>% select(date,"Id"=URI,Streams) %>% distinct()
data <- data %>% left_join(streams)
data <- data %>% mutate(Streams=as.numeric(Streams))
weighted_comparison_data <- data %>% Weighted_Playlist_Comparison_Function()


#Getting days since release weighted by streams
Weighted_Adj_Release_Dates_Grouped <- Adj_Release_Dates %>% 
  mutate(Playlist=as.character(year)) %>% 
  select(-year) %>% 
  group_by(Playlist) %>% 
  summarise(`Median Days Since Release (Adj)`=weighted.median(`Days Since Release (Adj)`,na.rm=T,w=Streams))

#Joining with full comparison data
weighted_comparison_data <- weighted_comparison_data %>% left_join(Weighted_Adj_Release_Dates_Grouped)

#saveRDS(weighted_comparison_data,here("03_Obtain_Top-200-Data/data/Top200_Weighted_Playlist_Data.rds"))
#saveRDS(weighted_comparison_data,here("data/Top200_Weighted_Playlist_Data.rds"))
