# Loading Libraries and Data-------------------------------------------------------
library(readr)
library(tidyverse)
library(tidytext)
library(ggridges)
library(lexicon)
library(ggtext)
library(here)
library(patchwork)
library(stringr)
library(scales)
library(tools)
theme_set(theme_minimal(base_size = 12))



# Getting Wrapped Data ----------------------------------------------------
#Full wrapped data
Full_Wrapped <- read_rds(here("data/Full_Wrapped_Feat_Lyrics_Data.rds")) %>% 
  mutate(Wrapped="Yes")

#Playlist comparison wrapped data
Comparison_Wrapped <- read_rds(here("data/Wrapped_Playlist_Data.rds"))
Comparison_Wrapped <- Comparison_Wrapped %>% rename("Median Loudness (dB)"=`Median Loudness`,
                                              "Median Tempo (BPM)"=`Median Tempo`,
                                              "Percent of Songs That Are Explicit"=`Percent Explicit`) %>% 
  mutate(Wrapped="Yes",
         Playlist=case_when(Playlist=="Your Top Songs 2017"~"2017",
                            Playlist=="Your Top Songs 2018"~"2018",
                            Playlist=="Your Top Songs 2019"~"2019",
                            Playlist=="Your Top Songs 2020"~"2020"))


# Getting Top 200 Data -----------------------------------------------------
#Getting full top 200 data
Full_Top_200 <- read_rds(here("data/Full_Top_200_Feat_Lyrics_Data.rds"))
#Adding number of streams
Jan_2017_Dec_2020 <- read_rds(here("03_Obtain_Top-200-Data/data/Jan_2017_Dec_2020.rds"))
streams <- Jan_2017_Dec_2020 %>% select(date,"Id"=URI,Streams) %>% distinct()
Full_Top_200 <- Full_Top_200 %>% left_join(streams)
Full_Top_200 <- Full_Top_200 %>% mutate(Streams=as.numeric(Streams),
                                        Wrapped="No",
                                        Playlist=as.character(Playlist))

#Playlist comparison top 200 data
Comparison_Top_200 <- read_rds(here("data/Top200_Weighted_Playlist_Data.rds")) %>% 
  rename("Median Loudness (dB)"=`Median Loudness`,
         "Median Tempo (BPM)"=`Median Tempo`,
         "Percent of Songs That Are Explicit"=`Percent Explicit`) %>% 
  mutate(Wrapped="No",
         `Median Years Since Release (Adj)`=`Median Days Since Release (Adj)`/365) %>% 
  select(-`Median Days Since Release (Adj)`)



# Joining Wrapped and Top 200 ---------------------------------------------
Full_Data <- Full_Wrapped %>% bind_rows(Full_Top_200)
Full_Comparison <- Comparison_Wrapped %>% bind_rows(Comparison_Top_200)



# Features ----------------------------------------------------------------
Feats_Data <- Full_Comparison %>% 
  select(Wrapped,Playlist,`Median Danceability`:`Median Years Since Release (Adj)`,
         `Average Words Per Song`,
         `Percent of Songs That Are Explicit`) %>% 
  select(-`Median Key (0 is C)`) %>% 
  pivot_longer(`Median Danceability`:`Percent of Songs That Are Explicit`,names_to="Feature") 

Feats_Data %>% 
  ggplot(aes(x=Playlist,y=value)) +
  geom_line(aes(color=Wrapped,group=Wrapped),size=1.5) +
  geom_point(aes(color=Wrapped),size=5) +
  scale_color_manual(values=c("#A9A9A9","#1DB954")) +
  facet_wrap(~Feature,scales = "free_y") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  labs(title="What kind of music <span style='color: #1DB954'>**you**</span> listened to versus what was <span style='color: #A9A9A9'>**popular**</span>") +
  theme(plot.title = element_markdown(size = rel(2)),
        axis.title = element_blank(),
        plot.title.position = "plot",
        strip.text = element_text(size=rel(1.3)),
        legend.position = "none")


# Genres ------------------------------------------------------------------
genres <- Full_Comparison %>% select(Wrapped, Playlist,`Percent Hip Hop`:`Percent Other`)

genres %>% pivot_longer(cols=`Percent Hip Hop`:`Percent Other`,names_to="Genre",values_to="Percent") %>% 
  ggplot(aes(x=Playlist,y=Percent,group=Wrapped)) +
  geom_line(aes(color=Wrapped),size=1.5) +
  geom_point(aes(color=Wrapped),size=5) +
  scale_color_manual(values=c("#A9A9A9","#1DB954")) +
  facet_wrap(~Genre,scales = "free_y") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title="Which genres of music <span style='color: #1DB954'>**you**</span> listened to versus which ones were <span style='color: #A9A9A9'>**popular**</span>") +
  theme(plot.title = element_markdown(size = rel(2)),
        axis.title = element_blank(),
        plot.title.position = "plot",
        strip.text = element_text(size=rel(1.5)),
        legend.position = "none")

# NRC Sentiments --------------------------------------------------------------

sentiments <- Full_Comparison %>% select(Wrapped,Playlist,`Percent of Words in Trust Category`:`Percent of Words in Anticipation Category`)
names(sentiments) <- names(sentiments) %>% str_replace("Percent of Words in ","") %>% str_replace(" Category","")


sentiments %>% 
  pivot_longer(cols=Trust:Anticipation,
               names_to="Genre",
               values_to="Percent") %>% 
  ggplot(aes(x=Playlist,y=Percent,group=Wrapped)) +
  geom_line(aes(color=Wrapped),size=1.5) +
  geom_point(aes(color=Wrapped),size=5) +
  scale_color_manual(values=c("#A9A9A9","#1DB954")) +
  facet_wrap(~Genre,scales = "free_y") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  labs(title="What percent of words in <span style='color: #1DB954'>**your**</span> music fell into given emotional cetageories versus words in <span style='color: #A9A9A9'>**popular**</span> music") +
  theme(plot.title = element_markdown(size = rel(2)),
        plot.subtitle = element_markdown(),
        axis.title = element_blank(),
        plot.title.position = "plot",
        strip.text = element_text(size=rel(1.5)),
        legend.position = "none")

# Overall Sentiment -------------------------------------------------
Full_Comparison %>% 
  select(Wrapped, Playlist, `Average Overall Sentiment`) %>% 
  ggplot(aes(x=Playlist,y=`Average Overall Sentiment`,color=Wrapped,group=Wrapped)) +
  geom_line(size=1.5) +
  geom_point(size=5) +
  geom_hline(yintercept = 0,linetype="dashed") +
  scale_color_manual(values = c("#1DB954","#A9A9A9")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  guides(color="none") +
  labs(title="What was the overall sentiment of <span style='color: #1DB954'>**your**</span> music versus <span style='color: #A9A9A9'>**popular**</span> music") +
  theme(plot.title = element_markdown(size = rel(2)),
        axis.title.x = element_blank(),
        plot.title.position = "plot")

# Top Artists -------------------------------------------------------------
Artists <- data %>% 
  count(Playlist,Artist,wt = Streams) %>% 
  arrange(desc(n)) %>% 
  group_by(Playlist) %>% 
  top_n(5, n) %>% 
  mutate(Artist=reorder_within(x = Artist,within = Playlist,by = n),
         billions_of_streams=n/1000000000)

Artists$Playlist <- factor(Artists$Playlist,
                           levels=c("2017","2018","2019","2020"),
                           labels=c("2017","2018","2019","2020"))

(p1 <- ggplot(Artists, aes(Artist, billions_of_streams, fill = Playlist)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "count") +
    facet_wrap(~Playlist, ncol = 2, scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +
    scale_fill_manual(values = c("#5BC680","#1DB954","#16873D","#1B3B26")) +
    labs(title="Top Artists by Year",
         y="Billions of Streams") +
    theme(plot.title = element_text(size = rel(2)),
          plot.title.position = "plot",
          axis.text.y = element_text(face="bold")))

#Number distinct artists
(p2 <- data %>% select(Playlist,Artist) %>% distinct(Playlist,Artist) %>% 
    group_by(Playlist) %>% 
    summarise(Distinct_Artists=n()) %>% 
    ggplot(aes(x=Playlist,y=Distinct_Artists,group=1)) +
    geom_line(color="#1DB954",size=1.5) +
    geom_point(color="#1DB954",size=5) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    labs(title="Number of Distinct Artists") +
    theme(plot.title = element_text(size = rel(2)),
          axis.title = element_blank(),
          plot.title.position = "plot"))

p1+p2 + plot_layout(ncol=1)


# Lyric Analysis ----------------------------------------------------------

# Top Clean Words ---------------------------------------------------------------

Lyrics <- data %>% select(Playlist,Streams,Id,Song,full_lyrics)

words <- Lyrics %>% 
  unnest_tokens(input = full_lyrics,output = "words",token="words",to_lower = F) %>% 
  filter(words!="NA") %>%  #Removing "NA" from the lyrics. This is an artifact of the genius API, which sometimes has the first line as missing
  mutate(words= tolower(words))


interesting_words <- words %>% 
  #Removing stop words
  anti_join(stop_words, by=c("words"="word")) %>% 
  #Removing racist and profane words
  anti_join(tibble(words=lexicon::profanity_racist)) %>% 
  anti_join(tibble(words=lexicon::profanity_alvarez)) %>% 
  #Removing more stop words
  filter(!(words %in% c("ya","yea","yeah","oh","ohh","ooh","ay","ayy","uh","gon"))) 

top_5_words <- interesting_words %>% 
  filter(!is.na(words)) %>% 
  count(Playlist,words,wt=Streams) %>% 
  arrange(desc(n)) %>% 
  group_by(Playlist) %>% 
  mutate(total=sum(n),
         percent=(n/total)) %>% 
  top_n(5, percent) %>% 
  mutate(words=str_to_title(words)) %>% 
  mutate(words=reorder_within(x = words,within = Playlist,by = percent))

top_5_words$Playlist <- factor(top_5_words$Playlist,
                               levels=c("2017","2018","2019","2020"),
                               labels=c("2017","2018","2019","2020"))


ggplot(top_5_words, aes(words, percent, fill = Playlist)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "percent") +
  facet_wrap(~Playlist, ncol = 2, scales = "free_y") +
  scale_y_continuous(expand = c(0,0),labels = scales::percent) +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_manual(values = c("#5BC680","#1DB954","#16873D","#1B3B26")) +
  labs(title="Top Words by Year",
       y="Percent") +
  theme(plot.title = element_text(size = rel(2)),
        plot.title.position = "plot",
        axis.text.y = element_text(face="bold"))

# Top Explicit Words ---------------------------------------------------------------
interesting_words_explicit <- words %>% 
  #Removing stop words
  anti_join(stop_words, by=c("words"="word")) %>% 
  #Removing more stop words
  filter(!(words %in% c("ya","yea","yeah","oh","ohh","ooh","ay","ayy","uh","gon"))) 

top_5_words_explicit <- interesting_words_explicit %>% 
  count(Playlist,words,wt=Streams) %>% 
  arrange(desc(n)) %>% 
  group_by(Playlist) %>% 
  mutate(total=sum(n),
         percent=(n/total)*100) %>% 
  top_n(5, percent) %>% 
  mutate(words_clean=toTitleCase(words),
         words_clean=case_when(
           words %in% lexicon::profanity_racist~str_replace_all(string = words_clean,pattern = "A|a|e|i|o|u",replacement = "*"),
           words %in% lexicon::profanity_alvarez~str_replace_all(string = words_clean,pattern = "A|a|e|i|o|u",replacement = "*"),
           TRUE~words_clean)) %>%
  mutate(words_clean=reorder_within(x = words_clean,within = Playlist,by = percent))

top_5_words_explicit$Playlist <- factor(top_5_words_explicit$Playlist,
                                        levels=c("2017","2018","2019","2020"),
                                        labels=c("2017","2018","2019","2020"))


ggplot(top_5_words_explicit, aes(words_clean, percent, fill = Playlist)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "percent") +
  facet_wrap(~Playlist, ncol = 2, scales = "free_y") +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_manual(values = c("#5BC680","#1DB954","#16873D","#1B3B26")) +
  labs(title="Top Words by Year",
       y="Percent") +
  theme(plot.title = element_text(size = rel(2)),
        plot.title.position = "plot",
        axis.text.y = element_text(face="bold"))


# Percent of Explicit Words -----------------------------------------------
Per_Explicit_Words <- words %>% 
  mutate(explicit=ifelse(words %in% lexicon::profanity_racist | words %in% lexicon::profanity_alvarez,1,0)) %>% 
  group_by(Playlist) %>% 
  summarise(Weighted_Percent_Explicit=weighted.mean(explicit,na.rm = T,w=Streams),
            Unweighted_Percent_Explicit=mean(explicit,na.rm=T))

Per_Explicit_Words %>% 
  pivot_longer(Weighted_Percent_Explicit:Unweighted_Percent_Explicit,names_to="Weighted",values_to="Percent_Explicit") %>% 
  ggplot(aes(x=Playlist,y=Percent_Explicit,group=Weighted)) +
  geom_line(aes(color=Weighted),size=1.5) +
  geom_point(aes(color=Weighted),size=5) +
  scale_color_manual(values=c("#A9A9A9","#1DB954")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  labs(title="<span style='color: #1DB954'>**Weighted**</span> versus <span style='color: #A9A9A9'>**Unweighted**</span> Percent of Words that are Explicit",
       subtitle="Weighted by stream") +
  theme(plot.title = element_markdown(size=rel(2)),
        axis.title = element_blank(),
        plot.title.position = "plot",
        strip.text = element_text(size=rel(1.5)),
        legend.position = "none")


# Most Unique Words -------------------------------------------------------
#source(here("Analysis/Getting_Relative_Importance.R"))
Unweighted_Relative_Importance <- read_rds(here("data/Top200_Relative_Importance.rds"))
Weighted_Relative_Importance <- read_rds(here("data/Weighted_Top200_Relative_Importance.rds"))


Top_10_Relative_Importance <- Weighted_Relative_Importance %>% 
  select(Playlist,word,difference) %>%
  distinct() %>% 
  group_by(Playlist) %>% 
  top_n(5, difference) %>%
  mutate(word_clean=toTitleCase(word),
         word_clean=case_when(
           word %in% lexicon::profanity_racist~str_replace_all(string = word_clean,pattern = "A|a|e|i|o|u",replacement = "*"),
           word %in% lexicon::profanity_alvarez~str_replace_all(string = word_clean,pattern = "A|a|e|i|o|u",replacement = "*"),
           TRUE~word_clean)) %>%
  mutate(word_clean=reorder_within(x = word_clean,within = Playlist,by = difference))



Top_10_Relative_Importance$Playlist <- factor(Top_10_Relative_Importance$Playlist,
                                              levels=c("2017","2018","2019","2020"),
                                              labels=c("2017","2018","2019","2020"))

Top_10_Relative_Importance %>%
  ggplot(aes(word_clean, difference, fill = Playlist)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "count") +
  facet_wrap(~Playlist, ncol = 2, scales = "free_y") +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_manual(values = c("#5BC680","#1DB954","#16873D","#1B3B26")) +
  labs(title="Most Uniquely Important Words for Each Year",
       subtitle = "Percent of words made up by word X in year Y minus percent of words made up of word X outside of year Y",
       y="Proportional Importance (Percent)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(size = rel(2)),
        plot.title.position = "plot",
        axis.text.y = element_text(face="bold")) 