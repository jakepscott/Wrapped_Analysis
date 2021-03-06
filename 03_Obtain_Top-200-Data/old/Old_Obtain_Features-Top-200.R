# Loading Libraries and data -------------------------------------------------------
library(RSelenium)
library(tidyverse)
library(Rspotify)
library(stringr)
library(readr)
library(here)
library(sjmisc)


# Loading Data ------------------------------------------------------------
data_raw <- read_rds(here("03_Obtain_Top-200-Data/data/May5_to_Dec5_raw.rds")) %>% rename("Track_Name"=`Track Name`) #Data from May 5th to Dec 5th
need_data <- data_raw %>% distinct(URI,.keep_all = T) %>% select(-date,-Streams,-Position)


#********************************************
# Getting the Features of the Top 200 Songs -------------------------------
#********************************************
#I use my keys here, but you'll need to use your own of course
source(here("03_Obtain_Top-200-Data/Spotify_Key.R"))

#Getting a list of the URIs for the songs I need data for
URIs <- tibble(URI=need_data$URI)

#Pre-specifying an object to fill with all the song features
features <- as.data.frame(matrix(NA, nrow = nrow(URIs), ncol = 16))

tictoc::tic()
#I fill each row of the pre-specified features object with the set of features for each unique song
for(i in 1:nrow(URIs)){
  print(i)
  print(URIs[i,])
  tryCatch({
    Sys.sleep(.1)
    features[i,]<-getFeatures(URIs[i,],token=keys)
  }, error=function(e){test <- print(e)})
}
tictoc::toc()
features <- features %>% as_tibble() 

#Getting the names right
names(features) <- names(getFeatures("0nbXyq5TXYPCO7pr3N8S4I",token = keys)) 
features <- features %>% rename("URI"=id)


#********************************************
# Getting Track Data ------------------------------------------------------
#********************************************
#Get one row and then drop it, just to get column names and types to bind to
track_info <- as_tibble(getTrack("0nbXyq5TXYPCO7pr3N8S4I",token = keys))
track_info <- track_info[-1,]

#Get track info for all songs. Using bind_rows is about as fast a pre-specifying
for(i in 1:nrow(URIs)){#for each song
  tryCatch({print(i)
    print(URIs[i,])
    Sys.sleep(.1)
    track_info<-bind_rows(track_info,getTrack(URIs[i,],token=keys))
  }, error=function(e){})
}

#Cleaning the data to just keep the URI column, explicit status, artist_id, album, and album_id
track_info <- track_info %>% rename("URI"=track_id) %>% separate(artists_id,into = c("main_id","feat_id"), sep = ";") %>% select(-feat_id) %>%
  separate(artists, into = c("main_artist", "feat_artist"), sep = ";") %>% dplyr::select(-feat_artist) %>% 
  dplyr::select(-main_artist) %>% rename("artist_id"=main_id) %>% dplyr::select(-name) %>% mutate(URI=as.character(URI)) %>% 
  dplyr::select(-popularity)

#********************************************
# Getting Album Data ------------------------------------------------------
#********************************************
Album_URIs <- track_info %>% select(album_id) %>% distinct() %>% mutate(album_id=as.character(album_id))

Album_info <- as_tibble(getAlbumInfo("6eV8O8De1mibErM1XM1tEc",token = keys))
Album_info <- Album_info[-1,]

for(i in 1:nrow(Album_URIs)){
  tryCatch({print(i)
    print(URIs[i,])
    Sys.sleep(.1)
    Album_info<-as_tibble(rbind(Album_info,getAlbumInfo(Album_URIs[i,],token=keys)[1,]))
  }, error=function(e){})
}

Album_info <- Album_info %>% rename("album_id"=id, "album_popularity"=popularity, "album_release_date"=release_date, 
                                    "album_name" = name) %>% 
  dplyr::select(-artist) %>% dplyr::select(-album_name)


#********************************************
# Fixing Album Release Dates ----------------------------------------------
#********************************************
fixed_released_dates <- Album_info %>% 
  select(album_release_date) %>% 
  distinct() %>%
  mutate(album_release_date=as.character(album_release_date), 
         nchar=nchar(album_release_date)) %>%
  mutate(album_rd_correct=ymd("2025-01-01"))

fixed_released_dates <- fixed_released_dates %>% 
  mutate(album_rd_correct=case_when(nchar==10~ymd(album_release_date),
                                    nchar==7~as.Date(paste(album_release_date, "1", sep = "-")),
                                    nchar==4~as.Date(paste(album_release_date,"1","1",sep = "-")))) %>% 
  select(-nchar)

###Joining
Album_info <- left_join(Album_info,fixed_released_dates, by=c("album_release_date")) 

###Getting number of days since release
Album_info <- Album_info %>% mutate(Days_Since_Release=as.numeric(Sys.Date()-album_rd_correct))

#********************************************
# Getting Artist Info -----------------------------------------------------
#********************************************
#Only doing it once for each artist  
distinct_artists <- track_info %>% dplyr::select(artist_id) %>% distinct(artist_id)

#Get the first row, just to get the column names for artist info
Artist_info <- as_tibble(getArtist("757aE44tKEUQEqRuT6GnEB",token = keys))
Artist_info <- Artist_info[-1,]

for(i in 1:nrow(distinct_artists)){#for each song
  tryCatch({print(i)
    print(distinct_artists$artist_id[i])
    Sys.sleep(.1)
    Artist_info<-as_tibble(bind_rows(Artist_info,getArtist(distinct_artists$artist_id[i],token=keys)))
  }, error=function(e){})
}

Artist_info <- Artist_info %>% rename("artist_id"=id) %>% select(-popularity,-name)

#********************************************
# Joining Data ------------------------------------------------------------
#********************************************
#All features of each unique song from May to Dec 2020
distinct_data <- need_data %>% 
  left_join(features,by="URI") %>% 
  left_join(track_info,by ="URI") %>% 
  left_join(Album_info,by="album_id") %>% 
  left_join(Artist_info)

#Joining on full data of all songs each day from May to Dec
May_to_Dec <- data_raw %>% left_join(distinct_data)


#********************************************
##Last minute cleaning
#********************************************
May_to_Dec <- May_to_Dec %>% mutate(Streams=as.numeric(Streams))

##Making a dummy that says whether song is from an album or a single/other
May_to_Dec <- May_to_Dec %>% mutate(album_dummy=if_else(album_type=="album",TRUE,FALSE)) 

##Making a time signature dummy that is 1 if time signature is 4 zero otherwise
May_to_Dec <- May_to_Dec %>% mutate(time_signature_dummy=if_else(time_signature==4,1,0)) 

#making date a date column
May_to_Dec <- May_to_Dec %>% mutate(date=as.Date(date))

#Make position numeric
May_to_Dec <- May_to_Dec %>% mutate(Position=as.numeric(Position))
#********************************************
##Making Genre Variables
#********************************************
##Making the columns themselves: This creates a true false column for every genre
May_to_Dec <- May_to_Dec %>% mutate(hip_hop=str_detect(genres,"hip|hop"),
                                                pop = str_detect(genres,("pop")),
                                                rap=str_detect(genres,("rap")),
                                                trap = str_detect(genres,"trap"),
                                                teen = str_detect(genres,"teen"),
                                                electronic = str_detect(genres,"electronic|electro|edm|dub"),
                                                rock=str_detect(genres,"rock|metal|alternative"),
                                                sad_feelings=str_detect(genres,"blues|jazz|emo|punk|soul|reggae"),
                                                RandB=str_detect(genres,"R&B"),
                                                conscious=str_detect(genres,"conscious|progressive"),
                                                country=str_detect(genres,"country"),
                                                other_genre=str_detect(genres,
                                                                       "hip|hop|pop|rap|trap|teen|electronic|electro|edm|dub|rock|metal|alternative|blues|jazz|emo|punk|soul|reggae|R&B|conscious|progressive|country"),
                                                other_genre=ifelse(other_genre==T,F,T))



#********************************************
####Saving Data
#********************************************
saveRDS(May_to_Dec, file = "03_Obtain_Top-200-Data/data/May_to_Dec_Full.rds")

#********************************************
####Appending May 2020 to Dec 2020 with Jan 2017 to May 2020
#********************************************
Jan_to_May <- read_rds(here("03_Obtain_Top-200-Data/data/US_Data.rds")) %>% rename("date"=Date)
May_to_Dec <- May_to_Dec %>% select(-genres)

Jan_to_May <- Jan_to_May %>% select(names(May_to_Dec))

Jan_2017_Dec_2020 <- bind_rows(Jan_to_May,May_to_Dec)

saveRDS(Jan_2017_Dec_2020,here("03_Obtain_Top-200-Data/data/Jan_2017_Dec_2020.rds"))
