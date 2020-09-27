### September 28: Polling
### Gov 1347: Election Analysis
### Alison Hu

### PREAMBLE -----------------------------------

# load libraries
library(tidyverse)
library(ggplot2)
library(usmap)
library(formattable)

setwd("~/Documents/OneDrive - Harvard University/Fall '20/Gov 1347/Section/03-Polling")

# read in data
popvote_df <- read_csv("popvote_1948-2016.csv")
popvote_state_df <- read_csv("popvote_bystate_1948-2016.csv")
poll_state_df <- read_csv("pollavg_bystate_1968-2016.csv")
poll_2020_df <- read_csv("polls_2020.csv")
economy_df <- read_csv("econ.csv")

# standardize naming of Nebraska
poll_2020_df$state[((poll_2020_df$state == "Nebraska CD-1") | (poll_2020_df$state == "Nebraska CD-2"))] <- "Nebraska"

### SUBSET DATA ---------------------------------

# data with incumbency variable
incumbent_dat <- popvote_df %>%
  select(year, party, incumbent_party)

# full data with polling and economic data
dat <- popvote_state_df %>% 
  right_join(poll_state_df %>% 
               filter(days_left <= 60) %>% 
               group_by(year, state, party) %>% 
               summarise(avg_poll = mean(avg_poll))) %>%
  full_join(incumbent_dat) %>% 
  left_join(economy_df %>% 
              filter(quarter == 2))

### FIT MODELS AND PREDICT -----------------------------------
# prediction variables
days_left <- 37
pwt <- 1/sqrt(days_left); ewt <- 1-(1/sqrt(days_left));

states_list <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
                 "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
                 "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
                 "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
                 "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
                 "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
                 "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
                 "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

states_list_abr <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT",
                     "DE", "FL", "GA", "HI", "ID",
                     "IL", "IN", "IA", "KS", "KY", "LA", "ME", 
                     "MD", "MA", "MI", "MN", "MS", "MO", 
                     "MT", "NE", "NV", "NH", "NJ", "NM",
                     "NY", "NC", "ND", "OH", "OK", "OR", 
                     "PA", "RI", "SC", "SD", "TN",
                     "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

# create dataframe for predictions
state_predictions <- data.frame(states_list, states_list_abr)
namevector <- c("state_prediction", "poll_fit", "poll_lwr", "poll_upr")
state_predictions[ , namevector] <- NA

# function to find point prediction for state
predict.function <- function(state_name) {
  state_df <- dat %>%
    filter((state == state_name) & (party == "republican"))
  state_econ_model <- lm(R_pv2p ~ GDP_growth_qt, data = state_df) 
  state_poll_model <- lm(R_pv2p ~ avg_poll, data = state_df) 
  state_2020 <- poll_2020_df %>%
    filter((state == state_name) & (candidate_name == "Donald Trump"))
  state_2020_true <- data.frame(GDP_growth_qt = -0.0949, avg_poll = mean(state_2020$pct))
  state_prediction <- pwt*predict(state_poll_model, state_2020_true) + ewt*predict(state_econ_model, state_2020_true)
}

# function to find prediction interval for poll-only model
predict.interval.function <- function(state_name) {
  state_df <- dat %>%
    filter((state == state_name) & (party == "republican"))
  state_poll_model <- lm(R_pv2p ~ avg_poll, data = state_df) 
  state_2020 <- poll_2020_df %>%
    filter((state == state_name) & (candidate_name == "Donald Trump"))
  state_2020_true <- data.frame(GDP_growth_qt = -0.0949, avg_poll = mean(state_2020$pct))
  pred_poll <- predict(state_poll_model, state_2020_true, interval = "prediction", level = 0.95)
}

# add to dataframe
for (state in states_list) {
  state_prediction <- predict.function(state)
  state_prediction_fit <- predict.interval.function(state)
  state_predictions$state_prediction[state_predictions$states_list == state] <- state_prediction
  state_predictions$poll_fit[state_predictions$states_list == state] <- state_prediction_fit[, 1]
  state_predictions$poll_lwr[state_predictions$states_list == state] <- state_prediction_fit[, 2]
  state_predictions$poll_upr[state_predictions$states_list == state] <- state_prediction_fit[, 3]
}

### VISUALIZE -----------------------------------
# personalized themes
blog_theme <- theme_bw(base_family = "Avenir") + 
  theme(panel.border = element_blank(),
        plot.title   = element_text(size = 15, hjust = 0.5), 
        axis.text.x  = element_text(angle = 0, hjust = 0.5),
        axis.text    = element_text(size = 12),
        strip.text   = element_text(size = 18),
        legend.position = "none",
        panel.grid.minor = element_blank()
  )

blog_theme1 <- theme_bw(base_family = "Avenir") + 
  theme(panel.border = element_blank(),
        axis.line    = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

state_predictions_graph <- state_predictions[-c(13, 39, 41, 50), ]
state_predictions_graph <- state_predictions_graph %>% rename(state = states_list)

# visualize prediction intervals for polling model
ggplot(state_predictions_graph, aes(x = reorder(states_list_abr, poll_fit), y = poll_fit, color = as.integer(states_list_abr))) +
  geom_point(size = 2.5) + 
  geom_errorbar(aes(ymin = poll_lwr, ymax = poll_upr), width = .1) +
  scale_colour_gradient(low = "#2096f3", high = "#ff5252") +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 25)) +
  xlab("State") + 
  ylab("Predicted 2-Party Vote Share") +
  ggtitle("Incumbent Party 2-Party Vote Share\nBased on State Polling Data") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 50, linetype = "dotted") +
  geom_vline(xintercept = 0.5) +
  blog_theme

ggsave("poll_model_state.png", height = 4, width = 15)

# shapefile of states from `usmap` library
states_map <- usmap::us_map()
unique(states_map$abbr)

# map of population per electoral vote
plot_usmap(data = state_predictions_graph, regions = "states", values = "state_prediction") + 
  scale_fill_gradient(low = "#ffffff", high = "#ff5252", name = "Incumbent Party \n2-Party Vote Share") +
  xlab("") + 
  ylab("") +
  blog_theme1

ggsave("ensemble_model_state_map.png", height = 6, width = 7)

# state predictions in table
state_predictions_table <- state_predictions_graph %>%
  select(state, state_prediction)

formattable(state_predictions_table,
            align = c("l", "c"),
            list(
              `state` = formatter("span", style = ~ style(font.weight = "bold")),
              `state_prediction`= color_tile("#ffefef", "#ff5252")
            ))

