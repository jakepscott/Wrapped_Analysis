#Get my playlists
user_playlists <- getPlaylists("jakerocksalot",token = keys) %>% as_tibble() %>% filter(tracks>0) 

#Get track info
tracks <- Tracks_Function(user = "jakerocksalot",playlists=c("Your Top Songs 2017",
                                                             "Your Top Songs 2018",
                                                             "Your Top Songs 2019",
                                                             "Your Top Songs 2020"))    #You need to get rid of the playlist column so that it doesn't try to join by that, and you need to do distinct because if a song
#appears in 2+ playlists it will get added twice to each corresponding column in the lefthand side

#Get Features
Features <- Features_Function(track_data = tracks)
#Get lyrics
Lyrics <- Lyric_Generation_Function(tracks)
#Getting features of the lyrics
Lyric_Features <- Lyric_Analysis_Function(Lyrics) %>% select(-Lyrics)

#Joining
Full_Data <- left_join(left_join(tracks,Features),Lyric_Features)

#Playlist analysis
Full_Data %>% 
  select(!c(Artist_full,Id,Artist_id,Album_id)) %>% 
  Playlist_Comparison_Function()
