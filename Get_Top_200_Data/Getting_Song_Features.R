# Loading Libraries and data -------------------------------------------------------
library(RSelenium)
library(tidyverse)
library(Rspotify)
library(stringr)
library(readr)
library(here)


# Loading Data ------------------------------------------------------------
data_raw <- read_rds(here("Get_Top_200_Data/data/May5_to_Dec5_raw.rds")) %>% rename("Track_Name"=`Track Name`) #Data from May 5th to Dec 5th
US_Data <- read_rds(here("Get_Top_200_Data/data/US_Data.rds")) #This is the US Data I already had from Jan 1st 2017 to May 4th 2020


# Checking which songs I already have data for and which I need to get data for --------
distinct_already <- US_Data %>% distinct(Track_Name) #Just getting the distinct songs, so I know which songs I already have data for
distinct_new <- data_raw %>% distinct(Track_Name,.keep_all = T)
need_data <- anti_join(distinct_new,distinct_already,by="Track_Name")

#############
# Getting the Features of the Top 200 Songs -------------------------------
#############
#I use my keys here, but you'll need to use your own of course
source(here("Get_Top_200_Data/Spotify_Key.R"))

#Getting a list of the URIs for the songs I need data for
URIs <- tibble(URI=need_data$URI)

##Download features about the tracks
US_Features <- as_tibble(getFeatures("0nbXyq5TXYPCO7pr3N8S4I",token = keys))
US_Features <- US_Features[-1,]

for(i in 1:nrow(URIs)){
  print(i)
  print(URIs[i,])
  tryCatch({
    Sys.sleep(.1)
    US_Features<-as_tibble(rbind(US_Features,getFeatures(URIs[i,],token=keys)))
  }, error=function(e){test <- print(e)})
}

US_Features <- US_Features %>% dplyr::rename("URI"=id)

#############
##Combining Top 200 Songs Data and Features Data
#############
Us_Data_5.4.2020_2.2 <- left_join(Us_Data_5.4.2020_2.2,US_Features, by = "URI")

#############
##Combining Top 200 Songs Data and Track Data
#############
setwd("..")
setwd("Set_Up_Files")
unique_songs <- Us_Data_5.4.2020_2.2 %>% select(`Track Name`, Artist, URI) %>% distinct()
source(file="Track_Info_Code.r")
Us_Data_5.4.2020_2.2 <- left_join(Us_Data_5.4.2020_2.2, track_info, by = "URI")

#############
##Combining Top 200 Songs Data and Album Data
#############
Album_URIs <- Us_Data_5.4.2020_2.2 %>% select(album_id) %>% distinct() %>% mutate(album_id=as.character(album_id))
source(file="Album_Info_Code.r")
Us_Data_5.4.2020_2.2 <- left_join(Us_Data_5.4.2020_2.2, Album_info, by = "album_id")


#############
##Combining Top 200 Songs Data and Artist Data 
#############
Artist_Songs_Combo <- Us_Data_5.4.2020_2.2 %>% dplyr::select(Artist, URI) %>% unique(.)
source(file="Artist_Info_Code.r")
Us_Data_5.4.2020_2.2 <- left_join(Us_Data_5.4.2020_2.2, Artist_info, by="Artist")

#############
##Fixing Album Release Dates 
#############
unique_album_rds <- Us_Data_5.4.2020_2.2 %>% select(album_release_date) %>% distinct() %>%
  mutate(album_release_date=as.character(album_release_date), nchar=nchar(album_release_date))
unique_album_rds <- unique_album_rds %>% mutate(album_rd_correct=ymd("2020-01-10")) 
source(file="fixingAlbumRDs.r")
###Joining
Us_Data_5.4.2020_2.2 <- left_join(Us_Data_5.4.2020_2.2,unique_album_rds, by=c("album_release_date"))

###Getting number of days since release
Us_Data_5.4.2020_2.2 <- Us_Data_5.4.2020_2.2 %>% mutate(album_rd_correct=date(album_rd_correct),
                                                        Days_Since_Release=as.numeric(Date-album_rd_correct))
#############
##Last minute cleaning
#############
Us_Data_5.4.2020_2.2 <- Us_Data_5.4.2020_2.2 %>% mutate(Streams=as.numeric(Streams))
##Adding quantil of position (so top 50, middle top 50, middle bottom 50, and bottom 50)
Us_Data_5.4.2020_2.2 <- Us_Data_5.4.2020_2.2 %>% mutate(Position=as.numeric(Position),
                                                        position_quantile = case_when((Position>0 & Position<=50)~"Q4",
                                                                                      (Position>50 & Position<=100)~"Q3",
                                                                                      (Position>100 & Position<=150)~"Q2",
                                                                                      (Position>150)~"Q1"))
Us_Data_5.4.2020_2.2$position_quantile <- factor(Us_Data_5.4.2020_2.2$position_quantile)

##Stupidly I split the genre column. Should not have done that. This rectifies that mistake
Us_Data_5.4.2020_2.2 <- Us_Data_5.4.2020_2.2 %>% unite(col = "genre", genre1:genre15, sep="; ", remove = TRUE, na.rm = TRUE)

##Making a dummy that says whether song is from an album or a single/other
Us_Data_5.4.2020_2.2 <- Us_Data_5.4.2020_2.2 %>% mutate(album_dummy=if_else(album_type=="album",TRUE,FALSE)) 

##Making a time signature dummy that is 1 if time signature is 4 zero otherwise
Us_Data_5.4.2020_2.2 <- Us_Data_5.4.2020_2.2 %>% mutate(time_signature_dummy=if_else(time_signature==4,1,0)) 

##Making album release data a character, not a factor (I will turn it to a date var seperately)
Us_Data_5.4.2020_2.2 <- Us_Data_5.4.2020_2.2 %>% mutate(album_release_date=as.character(album_release_date))

##Renaming `Track Name` to Track_Name
Us_Data_5.4.2020_2.2 <- Us_Data_5.4.2020_2.2 %>% dplyr::rename("Track_Name"=`Track Name`)

############################################################
##Making Genre Variables
############################################################
##Making the columns themselves: This creates a true false column for every genre
Us_Data_5.4.2020_2.2 <- Us_Data_5.4.2020_2.2 %>% mutate(hip_hop=map_lgl(Us_Data_5.4.2020_2.2$genre, str_contains, pattern=c("hip", "hop"), ignore.case=TRUE, logic="or"),
                                                        pop = map_lgl(Us_Data_5.4.2020_2.2$genre, str_contains, pattern=c("pop"), ignore.case=TRUE),
                                                        rap=map_lgl(Us_Data_5.4.2020_2.2$genre, str_contains, pattern=c("rap"), ignore.case=TRUE),
                                                        trap = (map_lgl(Us_Data_5.4.2020_2.2$genre, str_contains, pattern=c("trap"), ignore.case=TRUE)),
                                                        teen = map_lgl(Us_Data_5.4.2020_2.2$genre, str_contains, pattern=c("teen"), ignore.case=TRUE, logic="or"),
                                                        electronic = map_lgl(Us_Data_5.4.2020_2.2$genre, str_contains, 
                                                                             pattern=c("electronic", "electro", "edm", "dub"), ignore.case=TRUE, logic="or"),
                                                        rock=map_lgl(Us_Data_5.4.2020_2.2$genre, str_contains, 
                                                                     pattern=c("rock", "metal", "alternative"), ignore.case=TRUE, logic="or"),
                                                        sad_feelings=map_lgl(Us_Data_5.4.2020_2.2$genre, str_contains, 
                                                                             pattern=c("blues", "jazz", "emo", "punk", "soul", "reggae"), 
                                                                             ignore.case=TRUE, logic="or"),
                                                        RandB=map_lgl(Us_Data_5.4.2020_2.2$genre, str_contains, pattern=c("R&B"), ignore.case=TRUE),
                                                        conscious=map_lgl(Us_Data_5.4.2020_2.2$genre, str_contains, pattern=c("conscious", "progressive"), 
                                                                          ignore.case=TRUE, logic="or"),
                                                        country=map_lgl(Us_Data_5.4.2020_2.2$genre, str_contains, pattern=c("country"), ignore.case=TRUE),
                                                        other_genre=map_lgl(Us_Data_5.4.2020_2.2$genre, str_contains, 
                                                                            pattern=c("hip", "hop", "pop", "rap", "rock", "metal", "classic", "R&B",
                                                                                      "electronic", "electro", "blues", "jazz",
                                                                                      "emo", "punk", "soul", "reggae",
                                                                                      "edm", "dub", "country", "trap", "conscious",
                                                                                      "progressive", "teen", "alternative"), 
                                                                            ignore.case=TRUE, logic="not"))

##Getting weeks that line up with the initial claims weeks. The initial claims go from Sat to the next Sat.
##So week 1 of initial claims data in a year has all the days up to and including the first saturday. Since it
##includes that saturday, Week 1 would be the days up to saturday, and week 2 would start with the Sunday. Thus
##the flip from week 1 to 2 would come on the sunday. Thus, I have weeks increment every sunday
Us_Data_5.4.2020_2.2 <- Us_Data_5.4.2020_2.2 %>% distinct(Date) %>% arrange(Date) %>% mutate(sun=ifelse(wday(Date)==1, 1,0),
                                                                                             Year=year(Date), Month=month(Date), 
                                                                                             Day=day(Date)) %>% 
  group_by(Year) %>% mutate(Week_Adj=cumsum(sun)+1) %>% ungroup() %>% 
  right_join(., Us_Data_5.4.2020_2.2, by="Date") %>% select(-sun)

##############################################
####Saving Data
##############################################
setwd("..")
setwd("Clean_DataFrames")
saveRDS(Us_Data_5.4.2020_2.2, file = "Us_Data_5.4.2020_2.2.rds")