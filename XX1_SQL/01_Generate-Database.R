# Load Libs and Data ------------------------------------------------------
library(tidyverse)
library(dbplyr)
library(RSQLite)
library(DBI)
library(here)
library(tidytext)

Full_Data_Raw <- read_rds(here("data/Full_Top_200_Feat_Lyrics_Data.rds"))


# Create database ---------------------------------------------------------
mydb <- DBI::dbConnect(RSQLite::SQLite(), "XX1_SQL/Top-200.sqlite")
mydb


# Make Reference Tables ---------------------------------------------------

##########
#Song name
##########
#Get song table
songs <- Full_Data_Raw %>% distinct(Song) %>% mutate(song_id=row_number())
#Match IDs to main table
Full_Data <- Full_Data_Raw %>% left_join(songs,by="Song") %>% select(-Song)
#Write table to database
DBI::dbWriteTable(conn = mydb, name = "songs", songs, overwrite=TRUE)
#List the tables to check it worked
DBI::dbListTables(mydb)

###
#Artist name
###
#Get artist table
artists <- Full_Data %>% distinct(Artist) %>% mutate(artist_id=row_number())
#Match IDs to main table
Full_Data <- Full_Data %>% left_join(artists,by="Artist") %>% select(-Artist)
#Write table to database
DBI::dbWriteTable(conn = mydb, name = "artists", artists, overwrite=TRUE)
#List the tables to check it worked
DBI::dbListTables(mydb)


###
#Album name
###
#Get artist table
album <- Full_Data %>% distinct(Album) %>% mutate(album_id=row_number())
#Match IDs to main table
Full_Data <- Full_Data %>% left_join(album,by="Album") %>% select(-Album)
#Write table to database
DBI::dbWriteTable(conn = mydb, name = "album", album, overwrite=TRUE)
#List the tables to check it worked
DBI::dbListTables(mydb)

#####
#Lyrics Table
#####
#Selecting just the cols I need
Lyrics <- Full_Data %>% 
  distinct(song_id,album_id,artist_id,.keep_all = T) %>% 
  select(song_id,album_id,artist_id, Lyrics) %>% 
  mutate(full_lyrics="a")

#Taking nested lyric entries and making them a single string
for (i in 1:nrow(test)) {
  Lyrics$full_lyrics[i] <- Lyrics$Lyrics[[i]] %>% paste(collapse = " ")
}

#Unnesting lyrics, pivoting to long tidy format
Lyrics_Unnested <- Lyrics %>% select(-Lyrics) %>% unnest_tokens(word,full_lyrics) %>% filter(word!="NA")
#Uniquely IDing each word 
Words_Table <- Lyrics_Unnested %>% 
  select(-song_id,-artist_id,-album_id) %>% 
  distinct(word,.keep_all = T) %>% 
  mutate(word_id=row_number())

#Replacing character words in unnested lyrics dataset with their numeric identifiers
Lyrics_Table <- Lyrics_Unnested %>% left_join(Lyrics_Table) %>% select(-word)

#This results in two tables. One has all unique words in the lyrics and an ID associated (Lyr)
#The other has all words for all songs, but instead of being represented by characters, 
#they are represented by numbers


#Write tables to database
DBI::dbWriteTable(conn = mydb, name = "words", Words_Table,overwrite=TRUE)
DBI::dbWriteTable(conn = mydb, name = "lyrics", Lyrics_Table, overwrite=TRUE)
#List the tables to check it worked
DBI::dbListTables(mydb)

#Now I can remove lyrics from Full Data. I can reconstruct them joining based on 
#song-artist-album triplet IDs
Full_Data <-Full_Data %>% select(-Lyrics)

####
#Make table for Full Data
####
#Rename some columns
Full_Data <- Full_Data %>% rename("Artist_uri"=Artist_id,
                     "Album_uri"=Album_id)
#Write table to database
DBI::dbWriteTable(conn = mydb, name = "full_data", Full_Data,overwrite=TRUE)
#List the tables to check it worked
DBI::dbListTables(mydb)



# Querying the data -------------------------------------------------------
# Open a queryable connection with the database
Q_full_data <- tbl(mydb, "full_data")

library(tictoc)
tic()
Q_full_data %>% 
  count(date)
toc()

tic()
Full_Data_Raw %>% count(date)
toc()