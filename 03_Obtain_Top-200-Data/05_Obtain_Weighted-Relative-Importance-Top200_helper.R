# Load Libraries and Data -------------------------------------------------
library(tidyverse)
library(readr)
library(tidytext)
library(here)
library(furrr)

#This is the full features and lyrics data
data <- read_rds(here("03_Obtain_Top-200-Data/data/Full_Top_200_Feat_Lyrics_Data.rds"))

#Getting Streams column
Jan_2017_Dec_2020 <- read_rds(here("03_Obtain_Top-200-Data/data/Jan_2017_Dec_2020.rds"))
streams <- Jan_2017_Dec_2020 %>% select(date,"Id"=URI,Streams) %>% distinct()
data <- data %>% left_join(streams)
data <- data %>% mutate(Streams=as.numeric(Streams))

rm(Jan_2017_Dec_2020)
rm(streams)

#Just keep columns I need
Lyrics <- data %>% select(Playlist,Id,Song,Streams,full_lyrics)
rm(data)
#Get a row for every word in the data (so now I have playlist-word pairs)
words <- Lyrics %>% 
  unnest_tokens(input = full_lyrics,output = "words",token="words",to_lower = F) %>% 
  filter(words!="NA") %>%  #Removing "NA" from the lyrics. This is an artifact of the genius API, which sometimes has the first line as missing
  mutate(words= tolower(words))

#Getting rid of stop words
interesting_words <- words %>% 
  filter(!is.na(words)) %>% 
  rename("word"=words) %>% 
  #Removing stop words
  anti_join(stop_words) %>%
  #Removing more stop words
  filter(!(word %in% c("ya","yea","yeah","oh","ohh","ooh","ay","ayy","uh","gon"))) #Could also remove ("ah","em","nah","na","yah")

#Getting the sum of the playlist-word pairs. That way I can just sum rather than use nrow in the function below.
playlist_word_sums <- interesting_words %>% count(Playlist,word,wt=Streams)


#Getting just distinct playlist-word pairs
distinct_words <- interesting_words %>% distinct(Playlist,word)



# Function to Calculate Outside Percent -----------------------------------
calc_percent_outside <- function(playlist, curr_word) {
  #Get the number of words outside the album
  total_outside_words <- playlist_word_sums %>%  #*******NOTE: Sadly, this "playlist_word_sums" is something I need to define in the environment:(
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

#saveRDS(distinct_words_outside,here("03_Obtain_Top-200-Data/data/weighted_full_outside_percent.rds"))

#Joining this outside percent data with the full interesting words data
Relative_Importance <- interesting_words %>% 
  left_join(distinct_words_outside,by=c("Playlist","word"))

# Getting Within Album Proportion -----------------------------------------
Relative_Importance <- Relative_Importance %>% 
  group_by(Playlist,word) %>% 
  mutate(n=sum(Streams)) %>% 
  ungroup() %>% 
  group_by(Playlist) %>% 
  mutate(total_word=sum(Streams)) %>% 
  ungroup() %>%
  mutate(percent_inside=(n/total_word)*100)



# Getting Difference ------------------------------------------------------
Relative_Importance <- Relative_Importance %>% mutate(difference=percent_inside-percent_outside)


# Saving ------------------------------------------------------------------
#Just keeping distinct playlist word pairs
Relative_Importance <- Relative_Importance %>% select(Playlist,word,difference) %>% distinct()


#Saving so I don't need to run the above for loop each time
saveRDS(Relative_Importance,here("03_Obtain_Top-200-Data/data/Weighted_Top200_Relative_Importance.rds"))
saveRDS(Relative_Importance,here("data/Weighted_Top200_Relative_Importance.rds"))


