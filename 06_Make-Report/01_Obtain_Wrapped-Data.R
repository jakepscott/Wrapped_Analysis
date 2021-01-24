
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
source(here("01_Obtain_Wrapped-Data/functions/05-Compare_Playlists_Function.R"))


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


# Getting Lyric Features for Wrapped Songs --------------------------------
Lyric_Features <- Lyric_Analysis_Function(Lyrics) %>% select(-Lyrics)
Lyric_Features_To_Join <- Lyric_Features %>% select(!c(Song,Artist,Album))


# Joining the Data All Together -------------------------------------------
Full_Wrapped_Feat_Lyrics_Data <- Features %>% left_join(Lyric_Features_To_Join) %>% distinct(Id,.keep_all = T)

# Comparing the Wrapped Playlists -----------------------------------------
Wrapped_Playlist_Data <- Full_Wrapped_Feat_Lyrics_Data %>% Playlist_Comparison_Function(wrapped = T)



################
# Getting Relative Importance ---------------------------------------------
################
#Just keep columns I need
Lyrics <- data %>% select(Playlist,Id,Song,full_lyrics)

#Get a row for every word in the data (so now I have playlist-word pairs)
words <- Lyrics %>% 
  unnest_tokens(input = full_lyrics,output = "word",token="words") %>% 
  #Removing stop words
  anti_join(stop_words) %>% 
  #Removing more stop words
  filter(!(word %in% c("ya","yea","yeah","oh","ohh","ooh","ay","ayy","uh","gon"))) #Could also remove ("ah","em","nah","na","yah")

#Getting the sum of the playlist-word pairs. That way I can just sum rather than use nrow in the function below
playlist_word_sums <- words %>% count(Playlist,word)

#Getting just distinct playlist-word pairs
distinct_words <- words %>% distinct(Playlist,word)

# Function to Calculate Outside Percent -----------------------------------
calc_percent_outside <- function(playlist, curr_word) {
  #Get the number of words outside the album
  total_outside_words <- playlist_word_sums %>%  #*******NOTE: Sadly, this "playlist_word_sums" is something I need to define in the environment:(, see above
    filter(Playlist != playlist) %>% 
    pull(n) %>% 
    sum()
  
  #Get the number of times the given word appears outside the given album
  total_word_of_interest_outside <- playlist_word_sums %>% 
    filter(Playlist!=playlist,
           word==curr_word) %>% 
    pull(n) %>% 
    sum()
  
  prop <- (total_word_of_interest_outside/total_outside_words)*100
  return(prop)
}


# Calculating Outside Percent ---------------------------------------------
#This sets a plan for furrr
plan(multiprocess)

#Calculating and saving outside percent
tictoc::tic()
distinct_words_outside <- distinct_words %>% 
  mutate(percent_outside = future_pmap_dbl(list(Playlist, word), ~calc_percent_outside(..1, ..2),.progress = T)) 
tictoc::toc()

#saveRDS(needed_outside,here("03_Obtain_Top-200-Data/data/full_outside_percent.rds"))

#Joining this outside percent data with the full interesting words data
Relative_Importance <- words %>% 
  left_join(distinct_words_outside,by=c("Playlist","word"))

# Getting Within Album Proportion -----------------------------------------
Relative_Importance <- Relative_Importance %>% 
  group_by(Playlist,word) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  group_by(Playlist) %>% 
  mutate(total_word=n()) %>% 
  ungroup() %>%
  mutate(percent_inside=(n/total_word)*100)



# Getting Difference ------------------------------------------------------
Relative_Importance <- Relative_Importance %>% mutate(difference=percent_inside-percent_outside)

#Just keeping distinct playlist word pairs
Relative_Importance <- Relative_Importance %>% select(Playlist,word,difference) %>% distinct()



