### October 26: Shocks
### Gov 1347: Election Analysis
### Alison Hu

### PREAMBLE -----------------------------------

# load necessary libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(statebins)

### DEMOGRAPHIC VARIABLES: SURGES -----------------------------------

# read in data
demog <- read_csv("demographic_1990-2018.csv")
pvstate_df    <- read_csv("popvote_bystate_1948-2016.csv")
pollstate_df  <- read_csv("pollavg_bystate_1968-2016.csv")
pvstate_df$state <- state.abb[match(pvstate_df$state, state.name)]
pollstate_df$state <- state.abb[match(pollstate_df$state, state.name)]

# merge data
dat <- pvstate_df %>% 
  full_join(pollstate_df %>% 
              filter(weeks_left == 3) %>% 
              group_by(year,party,state) %>% 
              summarise(avg_poll=mean(avg_poll)),
            by = c("year" ,"state")) %>%
  left_join(demog %>%
              select(-c("total")),
            by = c("year" ,"state"))

dat$region <- state.division[match(dat$state, state.abb)]
demog$region <- state.division[match(demog$state, state.abb)]

# calculate change in each demographic group
dat_change <- dat %>%
  group_by(state) %>%
  mutate(Asian_change = Asian - lag(Asian, order_by = year),
         Black_change = Black - lag(Black, order_by = year),
         Hispanic_change = Hispanic - lag(Hispanic, order_by = year),
         Indigenous_change = Indigenous - lag(Indigenous, order_by = year),
         White_change = White - lag(White, order_by = year),
         Female_change = Female - lag(Female, order_by = year),
         Male_change = Male - lag(Male, order_by = year),
         age20_change = age20 - lag(age20, order_by = year),
         age3045_change = age3045 - lag(age3045, order_by = year),
         age4565_change = age4565 - lag(age4565, order_by = year),
         age65_change = age65 - lag(age65, order_by = year)
  )

# regression model
mod_demog_change <- lm(D_pv2p ~ Black_change + Hispanic_change + Asian_change +
                         Female_change +
                         age3045_change + age4565_change + age65_change +
                         as.factor(region), data = dat_change)

# new data for 2020
demog_2020 <- subset(demog, year == 2018)
demog_2020 <- as.data.frame(demog_2020)
rownames(demog_2020) <- demog_2020$state
demog_2020 <- demog_2020[state.abb, ]

# calculate change in each demographic group
demog_2020_change <- demog %>%
  filter(year %in% c(2016, 2018)) %>%
  group_by(state) %>%
  mutate(Asian_change = Asian - lag(Asian, order_by = year),
         Black_change = Black - lag(Black, order_by = year),
         Hispanic_change = Hispanic - lag(Hispanic, order_by = year),
         Indigenous_change = Indigenous - lag(Indigenous, order_by = year),
         White_change = White - lag(White, order_by = year),
         Female_change = Female - lag(Female, order_by = year),
         Male_change = Male - lag(Male, order_by = year),
         age20_change = age20 - lag(age20, order_by = year),
         age3045_change = age3045 - lag(age3045, order_by = year),
         age4565_change = age4565 - lag(age4565, order_by = year),
         age65_change = age65 - lag(age65, order_by = year)
  ) %>%
  filter(year == 2018)

demog_2020_change <- as.data.frame(demog_2020_change)
rownames(demog_2020_change) <- demog_2020_change$state
demog_2020_change <- demog_2020_change[state.abb, ]

# prediction
pred_original <- data.frame(pred = predict(mod_demog_change, newdata = demog_2020_change),
                           state = state.abb)
pred_new <- data.frame(pred = predict(mod_demog_change, newdata = demog_2020_change) +
                     (0.2)*demog_2020$Black,
                   state = state.abb)

# visualize
plot_original <- pred_original %>%
  mutate(state = as.character(state)) %>%
  ggplot(aes(state = state, fill = (pred >= 50))) +
  geom_statebins(dark_lbl = "white") +
  theme_statebins() +
  scale_fill_manual(values = c("#ff5252", "#2096f3")) +
  ggtitle("2016-2018 Demographic Changes") +
  blog_theme2

plot_1 <- pred_new %>% 
  mutate(state = as.character(state)) %>% ##`statebins` needs state to be character, not factor!
  ggplot(aes(state = state, fill = (pred >= 50))) +
  geom_statebins(dark_lbl = "white") +
  theme_statebins() +
  scale_fill_manual(values = c("#ff5252", "#2096f3")) +
  ggtitle("Hypothetical Black Surge") +
  blog_theme2

surge_map <- ggarrange(plot_original, plot_1, ncol = 2, nrow = 1)
annotate_figure(surge_map,
                top = text_grob("2020 Presidential Election Prediction", 
                                face = "bold", size = 15))
ggsave("demographic_surge_map1.png", width = 14, height = 6)


### COVID POLLING DESCRIPTIVE ANALYSIS -----

# read in data
covid_poll <- read_csv("covid-09_12.csv")

# personalized blog themes
blog_theme <- theme_bw(base_family = "Avenir") + 
  theme(panel.border = element_blank(),
        plot.title   = element_text(size = 12, hjust = 0.5), 
        axis.text.x  = element_text(angle = 0, hjust = 0.5),
        axis.text    = element_text(size = 12),
        strip.text   = element_text(size = 18),
        legend.position = "none",
        panel.grid.minor = element_blank()
  )

blog_theme1 <- theme_bw(base_family = "Avenir") + 
  theme(panel.border = element_blank(),
        plot.title   = element_text(size = 12, hjust = 0.5), 
        axis.text.x  = element_text(angle = 0, hjust = 0.5),
        axis.text    = element_text(size = 12),
        strip.text   = element_text(size = 18),
        legend.position = "top",
        panel.grid.minor = element_blank()
  )

blog_theme2 <- theme_bw(base_family = "Avenir") + 
  theme(panel.border = element_blank(),
        plot.title   = element_text(size = 12, hjust = 0.5), 
        legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

# poll question 1
covid_poll_Q1 <- covid_poll %>%
  filter(Q1 != "Skipped") %>%
  count(Q1) %>%
  mutate(perc = n / nrow(covid_poll))

g1 <- ggplot(covid_poll_Q1, aes(x = as.factor(Q1), y = perc)) + 
  geom_bar(stat = "identity", fill = "#ff5252") + 
  xlab("") +
  ylab("") +
  ggtitle("\"Do you approve or disapprove of the way Donald Trump\nis handling the response to the coronavirus (COVID-19)?\"") +
  blog_theme

# poll question 2
covid_poll_Q2 <- covid_poll %>%
  filter(Q2 != "Skipped") %>%
  count(Q2) %>%
  mutate(perc = n / nrow(covid_poll))

g2 <- ggplot(covid_poll_Q2, aes(x = as.factor(Q2), y = perc)) + 
  geom_bar(stat = "identity", fill = "#ff5252") + 
  xlab("") +
  ylab("") +
  ggtitle("\"In dealing with the coronavirus pandemic,\ndo you think Donald Trump acted\ntoo quickly, too slowly, or at about the right pace?\"") +
  blog_theme

f1 <- ggarrange(g1, g2, ncol = 1, nrow = 2)
ggsave("covid_poll1.png", width = 5, height = 6)

# poll question 3
covid_poll_Q3 <- covid_poll %>%
  filter(Q3 != "Skipped") %>%
  count(Q3) %>%
  mutate(perc = n / nrow(covid_poll))

g3 <- ggplot(covid_poll_Q3, aes(x = as.factor(Q3), y = perc)) + 
  geom_bar(stat = "identity", fill = "#ff5252") + 
  xlab("") +
  ylab("") +
  ggtitle("\"How much do you trust what Donald Trump\nsays about the coronavirus pandemic?\"") +
  blog_theme

# poll question 4
covid_poll_Q4 <- covid_poll %>%
  filter(Q4 != "Skipped") %>%
  count(Q4) %>%
  mutate(perc = n / nrow(covid_poll))

g4 <- ggplot(covid_poll_Q4, aes(x = as.factor(Q4), y = perc)) + 
  geom_bar(stat = "identity", fill = "#2096f3") + 
  xlab("") +
  ylab("") +
  ggtitle("\"How much do you trust what Joe Biden\nsays about the coronavirus pandemic?\"") +
  blog_theme

f2 <- ggarrange(g3, g4, ncol = 2, nrow = 1)
ggsave("covid_poll2.png", width = 11, height = 6)

# poll question 1 and 3
covid_poll %>% 
  filter(Q1 != "Skipped" & Q3 != "Skipped") %>%
  mutate(Q1 = as.factor(Q1)) %>% 
  ggplot(mapping = aes(x = Q3, fill = Q1)) +
  geom_bar() +
  scale_fill_manual("", values = c("Approve" = "#ff5252", "Disapprove" = "#2096f3")) +
  ggtitle("\"How much do you trust what Donald Trump\nsays about the coronavirus pandemic?\"") +
  xlab("") +
  blog_theme1

ggsave("covid_poll3.png", width = 6, height = 6)

