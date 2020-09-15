### September 14: Introduction
### Gov 1347: Election Analysis
### Alison Hu

### Pre-amble ###

# load necessary libraries
library(tidyverse)
library(ggplot2)
library(usmap)

# set working directory
setwd("~/Documents/OneDrive - Harvard University/Fall '20/Gov 1347/Section/Section 1")


### Read and inspect data ###

# read
popvote_df <- read_csv("popvote_1948-2016.csv")
popvote_df_state <- read_csv("popvote_bystate_1948-2016.csv")
electoralvote_df <- read_csv("electoralvote_1948-2016.csv")

# elections in which winner lost popular vote
popvote_df %>% 
  filter(winner == FALSE & pv2p > 50) %>% 
  select(party, candidate, pv2p)


### Visualize data: popular vs electoral votes ###

# create personalized theme for bar graph visualizations
blog_theme <- theme_bw(base_family = "Avenir") + 
  theme(panel.border = element_blank(),
        plot.title   = element_text(size = 15, hjust = 0.5), 
        axis.text.x  = element_text(angle = 0, hjust = 0.5),
        axis.text    = element_text(size = 12),
        strip.text   = element_text(size = 18),
        axis.line    = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 10),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# create personalized theme for map visualizations
blog_theme1 <- theme_bw(base_family = "Avenir") + 
  theme(panel.border = element_blank(),
        axis.line    = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# stacked bar graph of popular vote distribution
ggplot(popvote_df, aes(x = pv2p, y = year, fill = party)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#2096f3", "#ff5252")) + 
  xlab("2-party vote share") + 
  ylab("") +
  ggtitle("Popular Vote Distribution") + 
  scale_x_continuous(breaks = seq(from = 0, to = 100, by = 25), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(from = 1948, to = 2016, by = 4), expand = c(0, 1)) +
  geom_vline(xintercept = 50, linetype= "dotted") +
  blog_theme

ggsave("popularvote_1948-2016.png", height = 6, width = 7)

# stacked bar graph of electoral vote distribution
ggplot(electoralvote_df, aes(x = p2_percentage*100, y = year, fill = party)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#2096f3", "#ff5252")) + 
  xlab("2-party vote share") + 
  ylab("") +
  ggtitle("Electoral Vote Distribution") + 
  scale_x_continuous(breaks = seq(from = 0, to = 100, by = 25), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(from = 1948, to = 2016, by = 4), expand = c(0, 1)) +
  geom_vline(xintercept = 50, linetype="dotted") +
  blog_theme

ggsave("electoralvote_1948-2016.png", height = 6, width = 7)


### Visualize data: map ###

# read in population to electoral vote ratios by state
population_df <- read_csv("population_electoralvote.csv")

# shapefile of states from `usmap` library
states_map <- usmap::us_map()
unique(states_map$abbr)

# map of population per electoral vote
plot_usmap(data = population_df, regions = "states", values = "ratio") + 
  scale_fill_gradient(low = "#2096f3", high = "#ff5252", name = "Population per electoral vote") +
  xlab("") + 
  ylab("") +
  blog_theme1

ggsave("population_to_electoralvote.png", height = 6, width = 7)
