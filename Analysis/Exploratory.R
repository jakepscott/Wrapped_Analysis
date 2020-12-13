# Loading Libraries and Data-------------------------------------------------------
library(readr)
library(tidyverse)
library(tidytext)
library(ggridges)
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

comparison_data %>% 
  ggplot(aes(x=Playlist,y=`Mean Danceability`,)) +
  geom_line()
