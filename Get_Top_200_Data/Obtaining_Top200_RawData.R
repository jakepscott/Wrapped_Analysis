# Loading Libraries -------------------------------------------------------
library(RSelenium)
library(lubridate)
library(tidyverse)
library(Rspotify)
library(stringr)
library(here)

##
# Setting Up Dates --------------------------------------------------------
##
#2020-05-03 is the last day for which I had data before running this, hence why it is from
dates <- seq.Date(from = as_date("2020-05-03"),to = Sys.Date()-1, by = "day") %>% tibble("date"=.) %>% mutate(date=as.character(date))

##
# Getting URLs for the top 200s for the above dates -----------------------
##
#since the spotify url is consistent, it is just "https://spotifycharts.com/regional/us/daily/" and then the date,
#I can get a list of all the urls
urls <- vector(mode = "character", length = nrow(dates))
for (i in 1:nrow(dates)) {
  urls[i] <- paste("https://spotifycharts.com/regional/us/daily/",dates[i,], sep = "")
}
urls <-tibble(urls)

###########################################################
# Downloading CSVs --------------------------------------------------------
###########################################################

#***Here I use RSelenium to download the CSVs***

#First, I designate some settings, specifically I set where donwloads should go
options <- list(
  chromeOptions = 
    list(prefs = list(
      "profile.default_content_settings.popups" = 0L,
      "download.prompt_for_download" = FALSE,
      "download.default_directory" = here("spotify_csvs")
    )
    )
) #I get the syntax for this from https://stackoverflow.com/questions/35504731/specify-download-folder-in-rselenium

#First, need to set up a server and browser
remDr <- rsDriver(
  port = 4454L,
  browser = c("chrome", "firefox", "phantomjs", "internet explorer"),
  version = "latest",
  chromever = "87.0.4280.88")#Make sure to set this to the right version of Chrome


##Downloading all CSVs to Downloads folder (then move them to Project directory)
for (i in urls$urls) {
  tryCatch({
    print(i)
    remDr$client$navigate(as.character(i)) #This goes to the url
    webElem <- remDr$client$findElement(using = "class name", "header-csv") #This finds the download buton
    webElem$clickElement() #This clicks the download button
  }, error=function(e){test <- print(e)})
}

#Moving CSVs to spotify_csvs folder
desired_dir <- here("spotify_csvs/") #Directory I want to move folders to
default_dir <- "/Users/Jake Scott/Downloads" #Folder where the csvs were downloaded

files <- vector(mode = "character", length = nrow(dates)) #Get the names of the files you want to move
for (i in 1:nrow(dates)) {
  files[i] <- paste("regional-us-daily-",dates[i,], ".csv", sep = "")
}

# move the file to the desired directory. 
#This idea from second answer here: https://stackoverflow.com/questions/35504731/specify-download-folder-in-rselenium
for (i in files) {
  file.rename(file.path(default_dir, i), file.path(desired_dir, i))
}

###########################################################
# Reading CSVs into one big dataframe -------------------------------------
###########################################################
##Test is used for appending then is eliminated
test <- tibble(Position="a",`Track Name`="b", Artist="c",Streams="d",URL="e",date="f")
for (i in 1:length(files)) {
  tryCatch({  
    append <- read_csv(paste(here("spotify_csvs/",files[i],sep="")),skip = 1)
    append <- append %>% dplyr::mutate(date = as.character(dates[i,]))
    test <- rbind(test,append)}, error=function(e){print(e)})
}
top_data <- test[-1,]


#Getting URI column
top_data <- top_data %>% mutate(url2=URL) %>% separate(url2, into = c(1,2,3,4,"URI"), sep="/") %>%
  dplyr::select(date, Position, `Track Name`,Artist, Streams,URL,URI) 

#############
# Getting the Features of the Top 200 Songs -------------------------------
#############
#I use my keys here, but you'll need to use your own of course
source(here("Get_Top_200_Data/Spotify_Key.R"))


unique_US_songs <- Us_Data_5.4.2020_2.2 %>% select(`Track Name`, Artist, URI) %>% distinct()
URIs <- tibble(URI=unique_US_songs$URI)

##Download features about the tracks
US_Features <- as_tibble(getFeatures("0nbXyq5TXYPCO7pr3N8S4I",token = keys))
US_Features <- US_Features[-1,]

for(i in 1:nrow(unique_US_songs)){
  print(i)
  print(URIs[i,])
  Sys.sleep(.1)
  US_Features<-as_tibble(rbind(US_Features,getFeatures(URIs[i,],token=keys)))
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


