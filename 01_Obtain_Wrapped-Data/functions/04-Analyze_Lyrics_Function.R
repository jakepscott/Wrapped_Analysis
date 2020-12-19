
# Load Libs and Data ------------------------------------------------------
library(tidyverse)
library(readr)
library(furrr)
library(here)

Lyrics <- read_rds(here("01_Obtain_Wrapped-Data/data/Full_Wrapped_Lyrics.rds"))



# Getting Sentiment Data --------------------------------------------------
nrc_data <- read_rds(here("01_Obtain_Wrapped-Data/data/nrc_data.rds"))
Lyric_Analysis_Function <- function(Lyrics){
  
  #only works if there is at least 1 song in the track_data object
  if(nrow(Lyrics)==0){
    return("Sorry, no lyrics in this input!")
  }
  
  #Using the EmoLex to get word sentiments
  word_sentiment <- nrc_data
  
  ##Getting it into useable form, so you can see which emotions a given word does or does not match with
  word_sentiment <- word_sentiment %>% mutate(na=NA) %>% pivot_wider(names_from = sentiment, values_from = sentiment) %>%
    dplyr::select(-na)
  
  #Changing to dummy vars with 1s and 0s
  word_sentiment <- word_sentiment %>% mutate(trust=ifelse(is.na(trust),0,1),
                                              fear=ifelse(is.na(fear),0,1),
                                              negative=ifelse(is.na(negative),0,1),
                                              sadness=ifelse(is.na(sadness),0,1),
                                              anger=ifelse(is.na(anger),0,1),
                                              surprise=ifelse(is.na(surprise),0,1),
                                              positive=ifelse(is.na(positive),0,1),
                                              disgust=ifelse(is.na(disgust),0,1),
                                              joy=ifelse(is.na(joy),0,1),
                                              anticipation=ifelse(is.na(anticipation),0,1))
  
  # Create Functions-------------------------------------------------------
  Num_Words <- function(full_lyrics){
    full_lyrics %>% 
      tibble(word=.) %>% 
      unnest_tokens(output = "word",input = "word",token = "words") %>% 
      nrow()
    
  }
  
  Sentiment_Function <- function(full_lyrics,total_words) {
    full_lyrics %>% 
      tibble(word=.) %>% 
      unnest_tokens(output = "word",input = "word",token = "words") %>% 
      left_join(., word_sentiment, by="word") %>% 
      na.omit %>% 
      mutate_if(.predicate = is.double, .funs= ~sum(.)) %>% 
      mutate_if(is.double, funs(`percent` = round(./total_words*100,1))) %>% 
      head(1)
    
  }
  
  
  
  # Use Functions -----------------------------------------------------------
  plan(multiprocess)
  
  #Calculating and saving outside percent
  tictoc::tic()
  data <- Lyrics %>% 
    mutate(total_words = future_pmap_dbl(list(full_lyrics=full_lyrics), ~Num_Words(..1),.progress = T)) 
  tictoc::toc()
  
  #Calculating and saving outside percent
  tictoc::tic()
  data <- data %>% 
    mutate(features = future_pmap(list(full_lyrics=full_lyrics,total_words=total_words), ~Sentiment_Function(..1,..2),.progress = T)) 
  tictoc::toc()
  
  data <- data %>% unnest(features)
  return(data)
}
