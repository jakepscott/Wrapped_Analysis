library(tidyverse)
library(here)
library(readr)
source(here("Get_Top_200_Data/functions/Gen_Top200_Lyrics.R"))
source(here("Get_Top_200_Data/functions/User_Lyric_Analysis_Function.R"))


#All songs from 2017 to 2020
full_data_distinct <- read_rds(here("Get_Top_200_Data/data/Jan_2017_Dec_2020.rds")) %>% select("Song"=Track_Name,Artist,URI) %>% distinct()

lyrics_jan2017_may2020 <- read_rds(here("Get_Top_200_Data/data/lyrics_jan2017_may2020.rds")) %>% rename("Song"=Track_Name)

need_lyrics <- full_data_distinct %>% anti_join(lyrics_jan2017_may2020) %>% rename("Id"=URI)


lyrics_may2020_dec2020 <- Lyric_Generation_Function(need_lyrics)
saveRDS(lyrics_may2020_dec2020,here("Get_Top_200_Data/data/lyrics_may2020_dec2020.rds"))

Full_top200_Lyrics <- lyrics_jan2017_may2020 %>% rename("Lyrics"=lyrics) %>% bind_rows(lyrics_may2020_dec2020) %>% select(-song_names,Id)
saveRDS(Full_top200_Lyrics,here("Get_Top_200_Data/data/Full_top200_Lyrics.rds"))

Top200_Lyric_Features <- Lyric_Analysis_Function(Full_top200_Lyrics)
distinct_Top200_Lyric_Features <- Top200_Lyric_Features %>% distinct()

saveRDS(Top200_Lyric_Features,here("Get_Top_200_Data/data/Top200_Lyric_Features.rds"))

