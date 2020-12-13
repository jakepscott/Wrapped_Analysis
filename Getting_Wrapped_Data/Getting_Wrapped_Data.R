
# Loading Libs ------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(shinycssloaders)
library(stringr)
library(Rspotify)
library(tidyverse)
library(shinybusy)
library(here)


# Loading Necessary Data and Functions ------------------------------------
load(here("Getting_Wrapped_Data/functions/keys"))
source(here("Getting_Wrapped_Data/functions/User_Tracks_Function.R"))
source(here("Getting_Wrapped_Data/functions/User_Features_Function.R"))
source(here("Getting_Wrapped_Data/functions/User_Lyric_Analysis_Function.R"))
source(here("Getting_Wrapped_Data/functions/User_Lyric_Generation_Function.R"))
source(here("Getting_Wrapped_Data/functions/Overview_Figure_Function.R"))
source("Getting_Wrapped_Data/functions/Playlist_Comparison_Function.R")



#Get my playlists
user_playlists <- getPlaylists("jakerocksalot",token = keys) %>% as_tibble() %>% filter(tracks>0) 

#Get track info for my wrapped playlists
tracks <- Tracks_Function(user = "jakerocksalot",playlists=c("Your Top Songs 2017",
                                                             "Your Top Songs 2018",
                                                             "Your Top Songs 2019",
                                                             "Your Top Songs 2020"))    

#Get Features
Features <- Features_Function(track_data = tracks)
#Get lyrics using function
Lyrics <- Lyric_Generation_Function(tracks)

#Get rest of lyrics more manually
source(here("Getting_Wrapped_Data/Getting_Some_Lyrics_Manually.R"))
#Note: this creates Full_Lyrics object
#Then I can left_join() on the full data after I do the Lyrics analysis
#saveRDS(Full_Lyrics,here("data/Full_Lyrics.rds"))

#Getting features of the lyrics
Lyric_Features <- Lyric_Analysis_Function(Full_Lyrics) %>% select(-Lyrics)

#Joining
Full_Data <- left_join(left_join(tracks,Features),Lyric_Features) %>% distinct()
#saveRDS(Full_Data,here("data/Full_Data.rds"))

#Playlist analysis
Full_Data %>% 
  select(!c(Artist_full,Id,Artist_id,Album_id)) %>% 
  Playlist_Comparison_Function()
