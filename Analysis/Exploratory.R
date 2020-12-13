# Loading Libraries and Data-------------------------------------------------------
library(readr)
library(tidyverse)
library(tidytext)
library(ggridges)
library(lexicon)
library(here)
source(here("Getting_Wrapped_Data/functions/Playlist_Comparison_Function.R"))

data <- read_rds(here("data/Full_Data.rds"))

# Exploratory Analysis ----------------------------------------------------
data %>% 
  ggplot(aes(x=Danceability, y=Playlist,fill=Playlist)) +
  stat_density_ridges(quantile_lines = TRUE,alpha=.5) + 
  guides(fill="none") +
  theme_minimal() +
  theme(axis.title.y = element_blank())

data %>% 
  ggplot(aes(x=Danceability,y=Playlist,fill=Playlist)) +
  geom_boxplot() + 
  guides(fill="none") +
  theme_minimal() +
  theme(axis.title.y = element_blank())


#Geom_run to show distribution of points
library(ggforce)
#Danceability
data %>% 
  ggplot(aes(y=Danceability,x=fct_rev(Playlist),fill=Playlist)) +
  geom_sina(alpha = .5,aes(color=Playlist)) +
  geom_violin(size = 1,alpha=.25,draw_quantiles = 0.5,color="black") +
  guides(color="none",
         fill="none") +
  coord_flip() +
  theme_minimal() +
  theme(axis.title.y = element_blank())

#energy
data %>% 
  ggplot(aes(y=Energy,x=fct_rev(Playlist),fill=Playlist)) +
  geom_sina(alpha = .5,aes(color=Playlist)) +
  geom_violin(size = 1,alpha=.25,draw_quantiles = 0.5,color="black") +
  guides(color="none",
         fill="none") +
  coord_flip() +
  theme_minimal() +
  theme(axis.title.y = element_blank())

#Valence
data %>% 
  ggplot(aes(y=Valence,x=fct_rev(Playlist),fill=Playlist)) +
  geom_sina(alpha = .5,aes(color=Playlist)) +
  geom_violin(size = 1,alpha=.25,draw_quantiles = 0.5,color="black") +
  guides(color="none",
         fill="none") +
  coord_flip() +
  theme_minimal() +
  theme(axis.title.y = element_blank())


# Using Playlist_Comparison_Function --------------------------------------
comparison_data <- Playlist_Comparison_Function(data %>% select(-full_lyrics))

#Danceability
comparison_data %>% 
  ggplot(aes(x=Playlist,y=`Median Danceability`,group=1)) +
  geom_line(color="#1DB954",size=1.5) +
  geom_point(color="#1DB954",size=5) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30,hjust = 1))

#Energy
comparison_data %>% 
  ggplot(aes(x=Playlist,y=`Median Energy`,group=1)) +
  geom_line(color="#1DB954",size=1.5) +
  geom_point(color="#1DB954",size=5) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30,hjust = 1))

#Valence
comparison_data %>% 
  ggplot(aes(x=Playlist,y=`Median Valence`,group=1)) +
  geom_line(color="#1DB954",size=1.5) +
  geom_point(color="#1DB954",size=5) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30,hjust = 1))

#Duration
comparison_data %>% 
  ggplot(aes(x=Playlist,y=`Median Song Duration (Minutes)`,group=1)) +
  geom_line(color="#1DB954",size=1.5) +
  geom_point(color="#1DB954",size=5) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30,hjust = 1))

#"Median Years Since Release"
comparison_data %>% 
  ggplot(aes(x=Playlist,y=`Median Years Since Release`,group=1)) +
  geom_line(color="#1DB954",size=1.5) +
  geom_point(color="#1DB954",size=5) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30,hjust = 1))

#Percent_Male
comparison_data %>% 
  ggplot(aes(x=Playlist,y=`Percent Male`,group=1)) +
  geom_line(color="#1DB954",size=1.5) +
  geom_point(color="#1DB954",size=5) +
  labs(title="Percent of Artists Who are Male") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30,hjust = 1),
        plot.title.position = "plot",
        title = element_text(size=18))

#total words
comparison_data %>% 
  ggplot(aes(x=Playlist,y=`Total Words`,group=1)) +
  geom_line(color="#1DB954",size=1.5) +
  geom_point(color="#1DB954",size=5) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30,hjust = 1))

#Median Loudness
comparison_data %>% 
  ggplot(aes(x=Playlist,y=`Median Loudness`,group=1)) +
  geom_line(color="#1DB954",size=1.5) +
  geom_point(color="#1DB954",size=5) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30,hjust = 1))

#Median Tempo
comparison_data %>% 
  ggplot(aes(x=Playlist,y=`Median Tempo`,group=1)) +
  geom_line(color="#1DB954",size=1.5) +
  geom_point(color="#1DB954",size=5) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30,hjust = 1))

#Percent in Major
comparison_data %>% 
  ggplot(aes(x=Playlist,y=`Percent in Major`,group=1)) +
  geom_line(color="#1DB954",size=1.5) +
  geom_point(color="#1DB954",size=5) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30,hjust = 1))


# Genres ------------------------------------------------------------------
genres <- comparison_data %>% select(Playlist,`Percent \`Hip Hop\``:`Percent Other`)

genres %>% pivot_longer(cols=`Percent \`Hip Hop\``:`Percent Other`,names_to="Genre",values_to="Percent") %>% 
  ggplot(aes(x=Playlist,y=Percent,group=1)) +
  geom_line(color="#1DB954",size=1.5) +
  geom_point(color="#1DB954",size=5) +
  facet_wrap(~Genre,scales = "free_y") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30,hjust = 1))



# Sentiments --------------------------------------------------------------
#All sentiments
sentiments <- comparison_data %>% select(Playlist,`Percent of Words in Trust Category`:`Percent of Words in Anticipation Category`)

sentiments %>% 
  pivot_longer(cols=`Percent of Words in Trust Category`:`Percent of Words in Anticipation Category`,
               names_to="Genre",
               values_to="Percent") %>% 
  ggplot(aes(x=Playlist,y=Percent,group=1)) +
  geom_line(color="#1DB954",size=1.5) +
  geom_point(color="#1DB954",size=5) +
  facet_wrap(~Genre,scales = "free_y") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30,hjust = 1))

#Average Overall Sentiment
comparison_data %>% 
  ggplot(aes(x=Playlist,y=`Average Overall Sentiment`,group=1)) +
  geom_line(color="#1DB954",size=1.5) +
  geom_point(color="#1DB954",size=5) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30,hjust = 1))


# Top Artists -------------------------------------------------------------
data %>% count(Artist) %>% arrange(desc(n))
Artists <- data %>% count(Playlist,Artist) %>% arrange(desc(n)) %>% 
  group_by(Playlist) %>% 
  top_n(5, n) %>% 
  mutate(Artist=reorder_within(x = Artist,within = Playlist,by = n))

ggplot(Artists, aes(Artist, n, fill = Playlist)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "count") +
  facet_wrap(~Playlist, ncol = 2, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  theme_minimal()

#Number distinct artists
data %>% select(Playlist,Artist) %>% distinct(Playlist,Artist) %>% 
  group_by(Playlist) %>% 
  summarise(Distinct_Artists=n()) %>% 
  ggplot(aes(x=Playlist,y=Distinct_Artists,group=1)) +
  geom_line(color="#1DB954",size=1.5) +
  geom_point(color="#1DB954",size=5) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 30,hjust = 1))



# Lyric Analysis ----------------------------------------------------------
Lyrics <- data %>% select(Playlist,Id,Song,full_lyrics)

words <- Lyrics %>% 
  unnest_tokens(input = full_lyrics,output = "words",token="words")

# Top 15 non-stop words, censoring swears and slurs ---------------------------------------------------
interesting_words <- words %>% 
  #Removing stop words
  anti_join(stop_words, by=c("words"="word")) %>% 
  #Removing more stop words
  filter(!(words %in% c("ya","yea","yeah","oh","ohh","ooh","ay","ayy","uh","gon"))) %>% 
  mutate(words_clean=case_when(
    words %in% lexicon::profanity_racist~str_replace_all(string = words,pattern = "a|e|i|o|u",replacement = "*"),
    words %in% lexicon::profanity_alvarez~str_replace_all(string = words,pattern = "a|e|i|o|u",replacement = "*"),
    TRUE~words))


top_15_words <- interesting_words %>% 
  count(Playlist,words_clean) %>% 
  arrange(desc(n)) %>% 
  group_by(Playlist) %>% 
  top_n(15, n) %>% 
  mutate(words_clean=reorder_within(x = words_clean,within = Playlist,by = n))


ggplot(top_15_words, aes(words_clean, n, fill = Playlist)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "count") +
  facet_wrap(~Playlist, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# Top 15 non-stop words, censoring swears and removing slurs ---------------------------------------------------
interesting_words <- words %>% 
  #Removing stop words
  anti_join(stop_words, by=c("words"="word")) %>% 
  anti_join(tibble(words=lexicon::profanity_racist)) %>% 
  #Removing more stop words
  filter(!(words %in% c("ya","yea","yeah","oh","ohh","ooh","ay","ayy","uh","gon"))) %>% 
  mutate(words_clean=case_when(
    words %in% lexicon::profanity_alvarez~str_replace_all(string = words,pattern = "a|e|i|o|u",replacement = "*"),
    TRUE~words))

top_15_words <- interesting_words %>% 
  count(Playlist,words_clean) %>% 
  arrange(desc(n)) %>% 
  group_by(Playlist) %>% 
  top_n(15, n) %>% 
  mutate(words_clean=reorder_within(x = words_clean,within = Playlist,by = n))


ggplot(top_15_words, aes(words_clean, n, fill = Playlist)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "count") +
  facet_wrap(~Playlist, ncol = 2, scales = "free_y") +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  scale_x_reordered()


top_5_words <- interesting_words %>% 
  count(Playlist,words_clean) %>% 
  arrange(desc(n)) %>% 
  group_by(Playlist) %>% 
  top_n(5, n) %>% 
  mutate(words_clean=reorder_within(x = words_clean,within = Playlist,by = n))


ggplot(top_5_words, aes(words_clean, n, fill = Playlist)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "count") +
  facet_wrap(~Playlist, ncol = 2, scales = "free_y") +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  scale_x_reordered() +
  theme_minimal()

# Top 15 non-stop words, censoring swears and removing slurs ---------------------------------------------------
interesting_words <- words %>% 
  #Removing stop words
  anti_join(stop_words, by=c("words"="word")) %>% 
  anti_join(tibble(words=lexicon::profanity_racist)) %>% 
  anti_join(tibble(words=lexicon::profanity_alvarez)) %>% 
  #Removing more stop words
  filter(!(words %in% c("ya","yea","yeah","oh","ohh","ooh","ay","ayy","uh","gon"))) 

top_5_words <- interesting_words %>% 
  count(Playlist,words) %>% 
  arrange(desc(n)) %>% 
  group_by(Playlist) %>% 
  top_n(5, n) %>% 
  mutate(words=reorder_within(x = words,within = Playlist,by = n))


ggplot(top_5_words, aes(words, n, fill = Playlist)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "count") +
  facet_wrap(~Playlist, ncol = 2, scales = "free_y") +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  scale_x_reordered() +
  theme_minimal()


# Most Unique Words -------------------------------------------------------
Relative_Importance <- read_rds(here("data/Relative_Importance.rds"))
Top_10_Relative_Importance <- Relative_Importance %>% 
  select(Playlist,words,difference) %>%
  distinct() %>% 
  group_by(Playlist) %>% 
  top_n(10, difference) %>% 
  mutate(words=reorder_within(x = words,within = Playlist,by = difference))  

Top_10_Relative_Importance %>%  
  ggplot(aes(words, difference, fill = Playlist)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "count") +
  facet_wrap(~Playlist, ncol = 2, scales = "free_y") +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  scale_x_reordered() +
  theme_minimal()
