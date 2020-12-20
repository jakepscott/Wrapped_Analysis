# Load Libraries and Data -------------------------------------------------
library(tidyverse)
library(readr)
library(tidytext)

#Load Data
data <- read_rds(here("01_Obtain_Wrapped-Data/data/Full_Wrapped_Feat_Lyrics_Data.rds"))

Lyrics <- data %>% select(Playlist,Id,Song,full_lyrics)

words <- Lyrics %>% 
  unnest_tokens(input = full_lyrics,output = "words",token="words")

interesting_words <- words %>% 
  rename("word"=words) %>% 
  #Removing stop words
  anti_join(stop_words) %>% 
  #Removing racist and profane words
  anti_join(tibble(word=lexicon::profanity_racist)) %>% 
  anti_join(tibble(word=lexicon::profanity_alvarez)) %>% 
  #Removing more stop words
  filter(!(word %in% c("ya","yea","yeah","oh","ohh","ooh","ay","ayy","uh","gon"))) 


# Getting Outside Proportion ----------------------------------------------
#First I make an empty tibble which will eventually contain the album, word, and proportion
#of all words made up by that word outside the given album. So if the album is Your Top Songs 2020 and the word
#is run, the percent_outside column will be the proportion of words outside Your Top Songs 2020 that are "run"
outside_values <- tibble(Playlist=character(0),word=character(0),percent_outside=double(0))
tictoc::tic()
for (i in unique(data$Playlist)) {
  print(i)
  for (z in interesting_words %>% filter(Playlist==i) %>% distinct(word) %>% pull(word)) {
    #Get the number of words outside the album
    total_outside_words <- interesting_words %>% 
      filter(Playlist!=i) %>% 
      #distinct(words) %>% 
      nrow() 
    #Get the number of times the given word appears outside the given album
    total_word_of_interest_outside <- interesting_words %>% 
      filter(Playlist!=i,
             word==z) %>% 
      nrow()
    #Make the proportion
    percent_outside_to_paste <- (total_word_of_interest_outside/total_outside_words)*100
    #Make into a tibble
    to_bind <- tibble(Playlist=i,word=z,percent_outside=percent_outside_to_paste)
    #Bind onto a big tibble which will have each album-word pair and the corresponding percent_outside column
    outside_values <- outside_values %>% bind_rows(to_bind)
  }
}
tictoc::toc()
#Join the outside_values tibble to the RTJ_lyrics tibble, so for each word with will now have the value
#for the proportion of words outside the given album are made up by that given word. So if the album is
#RTJ4 and the word is week, the percent_outside column, which is .029, means that week makes up .029% of words in 
#RTJ1,RTJ2,and RTJ3
Relative_Importance <- interesting_words %>% 
  left_join(outside_values,by=c("Playlist","word"))


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

#Saving so I don't need to run the above for loop each time
saveRDS(Relative_Importance,here("01_Obtain_Wrapped-Data/data/Relative_Importance.rds"))
saveRDS(Relative_Importance,here("data/Relative_Importance.rds"))
