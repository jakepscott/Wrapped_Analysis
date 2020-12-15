
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
load(here("01_Obtain_Wrapped-Data/data/keys"))
source(here("01_Obtain_Wrapped-Data/functions/01-Obtain_Tracks_Function.R"))
source(here("01_Obtain_Wrapped-Data/functions/02-Obtain_Features_Function.R"))
source(here("01_Obtain_Wrapped-Data/functions/03-Obtain_Lyrics_Function.R"))
source(here("01_Obtain_Wrapped-Data/functions/04-Analyze_Lyrics_Function.R"))
source("01_Obtain_Wrapped-Data/functions/05-Compare_Playlists_Function.R")




# Getting User Wrapped Playlists ------------------------------------------
user_playlists <- getPlaylists("jakerocksalot",token = keys) %>% as_tibble() %>% filter(tracks>0 & str_detect(name,"Your Top Songs")==T)

#Get track info for my wrapped playlists
tracks <- Tracks_Function(user = "jakerocksalot",playlists=c("Your Top Songs 2017",
                                                             "Your Top Songs 2018",
                                                             "Your Top Songs 2019",
                                                             "Your Top Songs 2020"))    


# Getting Features of Wrapped Playlists -----------------------------------
Features <- Features_Function(track_data = tracks)


# Getting Lyrics for Wrapped Songs ----------------------------------------
Lyrics <- Lyric_Generation_Function(tracks)


# *Specific to Me*: Getting some songs manually ---------------------------
source(here("01_Obtain_Wrapped-Data/02_Obtain_Missed-Lyrics-Manually.R"))
#Note: this creates Full_Lyrics object
#Then I can left_join() on the full data after I do the Lyrics analysis
#saveRDS(Full_Lyrics,here("01_Obtain_Wrapped-Data/data/Full_Wrapped_Lyrics.rds"))


# Getting Lyric Features for Wrapped Songs --------------------------------
Lyric_Features <- Lyric_Analysis_Function(Full_Lyrics) %>% select(-Lyrics)


# Getting unnested lyrics column ------------------------------------------
Lyrics_Column <- Lyrics %>% select(Id,Lyrics) %>% mutate(full_lyrics="a")

for (i in 1:nrow(Lyrics)) {
  Lyrics_Column$full_lyrics[i] <- Lyrics_Column$Lyrics[[i]] %>% paste(collapse = " ")
}



# Joining the Data All Together -------------------------------------------
Full_Wrapped_Feat_Lyrics_Data <- Features %>% left_join(Lyrics) %>% left_join(Lyric_Features) %>% left_join(Lyrics_Column) %>% distinct()
#saveRDS(Full_Wrapped_Feat_Lyrics_Data,here("data/Full_Wrapped_Feat_Lyrics_Data.rds"))
#saveRDS(Full_Wrapped_Feat_Lyrics_Data,here("01_Obtain_Wrapped-Data/data/Full_Wrapped_Feat_Lyrics_Data.rds"))


# Comparing the Wrapped Playlists -----------------------------------------
Wrapped_Playlist_Data <- Full_Wrapped_Feat_Lyrics_Data %>% Playlist_Comparison_Function(wrapped = T)
#saveRDS(Wrapped_Playlist_Data,here("01_Obtain_Wrapped-Data/data/Wrapped_Playlist_Data.rds"))
#saveRDS(Wrapped_Playlist_Data,here("data/Wrapped_Playlist_Data.rds"))
