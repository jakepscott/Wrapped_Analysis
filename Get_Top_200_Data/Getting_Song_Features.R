# Loading Libraries and data -------------------------------------------------------
library(RSelenium)
library(tidyverse)
library(Rspotify)
library(stringr)
library(readr)
library(here)
library(sjmisc)


# Loading Data ------------------------------------------------------------
data_raw <- read_rds(here("Get_Top_200_Data/data/May5_to_Dec5_raw.rds")) %>% rename("Track_Name"=`Track Name`) #Data from May 5th to Dec 5th
US_Data <- read_rds(here("Get_Top_200_Data/data/US_Data.rds")) #This is the US Data I already had from Jan 1st 2017 to May 4th 2020


# Checking which songs I already have data for and which I need to get data for --------
distinct_already <- US_Data %>% distinct(Track_Name) #Just getting the distinct songs, so I know which songs I already have data for
distinct_new <- data_raw %>% distinct(Track_Name,.keep_all = T)
need_data <- anti_join(distinct_new,distinct_already,by="Track_Name")


#********************************************
# Getting the Features of the Top 200 Songs -------------------------------
#********************************************
#I use my keys here, but you'll need to use your own of course
source(here("Get_Top_200_Data/Spotify_Key.R"))

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
#First need to get the artist_id's, not including the featured artist
distinct_artists <- track_info %>% 
  separate(artists_id, c(into = "artist_id","to_drop"),sep = ";") %>% 
  select(-to_drop) %>% 
  separate(artists, c(into = "artist","to_drop"),sep = ";") %>% 
  select(artist,artist_id)
#Only doing it once for each artist  
distinct_artists <- distinct_artists %>% distinct()

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

Artist_info <- Artist_info %>% rename("artist_id"=id) %>% select(-popularity,-Track_Name)

#********************************************
# Joining Data ------------------------------------------------------------
#********************************************
full_needed_data <- need_data %>% 
  left_join(features,by="URI") %>% 
  left_join(track_info,by ="URI") %>% 
  left_join(Album_info,by="album_id") %>% 
  left_join(Artist_info)


#********************************************
##Last minute cleaning
#********************************************
full_needed_data <- full_needed_data %>% mutate(Streams=as.numeric(Streams))

##Stupidly I split the genre column. Should not have done that. This rectifies that mistake
full_needed_data <- full_needed_data %>% unite(col = "genre", genre1:genre15, sep="; ", remove = TRUE, na.rm = TRUE)

##Making a dummy that says whether song is from an album or a single/other
full_needed_data <- full_needed_data %>% mutate(album_dummy=if_else(album_type=="album",TRUE,FALSE)) 

##Making a time signature dummy that is 1 if time signature is 4 zero otherwise
full_needed_data <- full_needed_data %>% mutate(time_signature_dummy=if_else(time_signature==4,1,0)) 



#********************************************
##Making Genre Variables
#********************************************
##Making the columns themselves: This creates a true false column for every genre
full_needed_data <- full_needed_data %>% mutate(hip_hop=str_detect(genres,"hip|hop"),
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
saveRDS(full_needed_data, file = "Get_Top_200_Data/data/full_needed_data.rds")
