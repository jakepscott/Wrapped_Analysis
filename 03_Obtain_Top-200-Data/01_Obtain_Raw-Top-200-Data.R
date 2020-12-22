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
dates <- seq.Date(from = as_date("2017-01-01"),to = as_date("2020-12-05")-1, by = "day") %>% tibble("date"=.) %>% mutate(date=as.character(date))

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
    test <- bind_rows(test,append)}, error=function(e){print(e)})
}
top_data <- test[-1,]


#Getting URI column
top_data <- top_data %>% mutate(url2=URL) %>% separate(url2, into = c(1,2,3,4,"URI"), sep="/") %>%
  dplyr::select(date, Position, `Track Name`,Artist, Streams,URL,URI) 


# Saving RDS of this raw song data ----------------------------------------
saveRDS(top_data,here("03_Obtain_Top-200-Data/data/Jan_2017_Dec_2020.rds"))
