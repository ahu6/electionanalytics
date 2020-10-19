### October 19: Ground Game
### Gov 1347: Election Analysis
### Alison Hu

### PREAMBLE -----------------------------------

# load necessary libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(usmap)

fieldoffice_2012_county <- read.csv("fieldoffice_2012_bycounty.csv")
popvote_state <- read.csv("popvote_bystate_1948-2016.csv")

### OBAMA FIELD OFFICE PLACEMENT -----------------------------------

# subset data
fieldoffice_obama <- fieldoffice_2012_county %>%
  filter(obama12fo > 0) %>%
  drop_na(battle, swing, core_dem, core_rep)

# battleground states
fieldoffice_obama_battle <- fieldoffice_obama %>%
  count(battle) %>%
  mutate(perc = n / nrow(fieldoffice_obama))

o1 <- ggplot(fieldoffice_obama_battle, aes(x = as.factor(battle), y = perc)) + 
  geom_bar(stat = "identity", fill = "#2096f3") + 
  xlab("") +
  ylab("") +
  ggtitle("Battleground State") +
  blog_theme

# swing counties
fieldoffice_obama_swing <- fieldoffice_obama %>%
  count(swing) %>%
  mutate(perc = n / nrow(fieldoffice_obama))

o2 <- ggplot(fieldoffice_obama_swing, aes(x = as.factor(swing), y = perc)) + 
  geom_bar(stat = "identity", fill = "#2096f3") + 
  xlab("") +
  ylab("") +
  ggtitle("Swing County") +
  blog_theme

# core democratic counties
fieldoffice_obama_coredem <- fieldoffice_obama %>%
  count(core_dem) %>%
  mutate(perc = n / nrow(fieldoffice_obama))

o3 <- ggplot(fieldoffice_obama_coredem, aes(x = as.factor(core_dem), y = perc)) + 
  geom_bar(stat = "identity", fill = "#2096f3") + 
  xlab("") +
  ylab("") +
  ggtitle("Core Dem County") +
  blog_theme

# core republican counties
fieldoffice_obama_corerep <- fieldoffice_obama %>%
  count(core_rep) %>%
  mutate(perc = n / nrow(fieldoffice_obama))

o4 <- ggplot(fieldoffice_obama_corerep, aes(x = as.factor(core_rep), y = perc)) + 
  geom_bar(stat = "identity", fill = "#2096f3") + 
  xlab("") +
  ylab("") +
  ggtitle("Core Rep County") +
  blog_theme

# arrange plots together
figure1 <- ggarrange(o1, o2, o3, o4, ncol = 2, nrow = 2)
annotate_figure(figure1,
                top = text_grob("Obama 2012 Field Office Placement", 
                                face = "bold", size = 15))

ggsave("obama_field_office.png", height = 6, width = 6)


### ROMNEY FIELD OFFICE PLACEMENT -----------------------------------

# subset data
fieldoffice_romney <- fieldoffice_2012_county %>%
  filter(romney12fo > 0) %>%
  drop_na(battle, swing, core_dem, core_rep)

# battleground states
fieldoffice_romney_battle <- fieldoffice_romney %>%
  count(battle) %>%
  mutate(perc = n / nrow(fieldoffice_romney))

r1 <- ggplot(fieldoffice_romney_battle, aes(x = as.factor(battle), y = perc)) + 
  geom_bar(stat = "identity", fill = "#ff5252") + 
  xlab("") +
  ylab("") +
  ggtitle("Battleground State") +
  blog_theme

# swing counties
fieldoffice_romney_swing <- fieldoffice_romney %>%
  count(swing) %>%
  mutate(perc = n / nrow(fieldoffice_romney))

r2 <- ggplot(fieldoffice_romney_swing, aes(x = as.factor(swing), y = perc)) + 
  geom_bar(stat = "identity", fill = "#ff5252") + 
  xlab("") +
  ylab("") +
  ggtitle("Swing County") +
  blog_theme

# core democratic counties
fieldoffice_romney_coredem <- fieldoffice_romney %>%
  count(core_dem) %>%
  mutate(perc = n / nrow(fieldoffice_romney))

r3 <- ggplot(fieldoffice_romney_coredem, aes(x = as.factor(core_dem), y = perc)) + 
  geom_bar(stat = "identity", fill = "#ff5252") + 
  xlab("") +
  ylab("") +
  ggtitle("Core Dem County") +
  blog_theme

# core republican counties
fieldoffice_romney_corerep <- fieldoffice_romney %>%
  count(core_rep) %>%
  mutate(perc = n / nrow(fieldoffice_romney))

r4 <- ggplot(fieldoffice_romney_corerep, aes(x = as.factor(core_rep), y = perc)) + 
  geom_bar(stat = "identity", fill = "#ff5252") + 
  xlab("") +
  ylab("") +
  ggtitle("Core Rep County") +
  blog_theme

# arrange plots together
figure2 <- ggarrange(r1, r2, r3, r4, ncol = 2, nrow = 2)
annotate_figure(figure2,
                top = text_grob("Romney 2012 Field Office Placement", 
                                face = "bold", size = 15))

ggsave("romney_field_office.png", height = 6, width = 6)


### VISUALIZE DATA -----------------------------------

# personalized blog theme
blog_theme <- theme_bw(base_family = "Avenir") + 
  theme(panel.border = element_blank(),
        plot.title   = element_text(size = 12, hjust = 0.5), 
        axis.text.x  = element_text(angle = 0, hjust = 0.5),
        axis.text    = element_text(size = 12),
        strip.text   = element_text(size = 18),
        legend.position = "none",
        panel.grid.minor = element_blank()
  )

# personalized theme for map visualizations
blog_theme1 <- theme_bw(base_family = "Avenir") + 
  theme(panel.border = element_blank(),
        axis.line    = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# obama total offices by state
fieldoffice_state_obama <- fieldoffice_2012_county %>%
  select(state, obama12fo, battle, swing, core_dem, core_rep) %>%
  group_by(state) %>%
  summarise(count = sum(obama12fo))

# romney total offices by state
fieldoffice_state_romney <- fieldoffice_2012_county %>%
  select(state, romney12fo, battle, swing, core_dem, core_rep) %>%
  group_by(state) %>%
  summarise(count = sum(romney12fo))

# shapefile of states from `usmap` library
states_map <- usmap::us_map()
unique(states_map$abbr)

# map of obama locations
map1 <- plot_usmap(data = fieldoffice_state_obama, regions = "state", values = "count") + 
  scale_fill_gradient(low = "#ffffff", high = "#2096f3", name = "Obama Field Office Count") +
  xlab("") + 
  ylab("") +
  blog_theme1

# map of romney locations
map2 <- plot_usmap(data = fieldoffice_state_romney, regions = "state", values = "count") + 
  scale_fill_gradient(low = "#ffffff", high = "#ff5252", name = "Romney Field Office Count") +
  xlab("") + 
  ylab("") +
  blog_theme1

# arrange figures together
fieldoffice_map <- ggarrange(map1, map2, ncol = 1, nrow = 2)
annotate_figure(fieldoffice_map,
                top = text_grob("Field Office Location by State", 
                                face = "bold", size = 15))

ggsave("fieldoffice_map.png", height = 5, width = 6)


### REGRESSION OF FIELD OFFICE COUNTS AND VOTE SHARE -----------------------------------

# filter to 2012 election
popvote_state_2012 <- popvote_state %>%
  filter(year == 2012) 

# obama vote share data
obama_2012 <- popvote_state_2012 %>%
  select(state, D_pv2p) %>%
  right_join(fieldoffice_state_obama)

# obama linear regression model
lm(D_pv2p ~ count, obama_2012)

# romney vote share data
romney_2012 <- popvote_state_2012 %>%
  select(state, R_pv2p) %>%
  right_join(fieldoffice_state_romney)

# romney linear regression model
lm(R_pv2p ~ count, romney_2012)


# demographic variables: surges