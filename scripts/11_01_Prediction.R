### November 1: Final Prediction
### Gov 1347: Election Analysis
### Alison Hu

### PREAMBLE -----------------------------------

# load libraries
library(tidyverse)
library(ggplot2)
library(statebins)
library(Metrics)
library(ggpubr)

# read in data
popvote_df <- read_csv("popvote_1948-2016.csv")
popvote_state_df <- read_csv("popvote_bystate_1948-2016.csv")
poll_state_df <- read_csv("pollavg_bystate_1968-2016.csv")
covid_state_df <- read_csv("United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")
poll_2020_df <- read_csv("presidential_poll_averages_2020.csv")
census_df <- read_csv("census_2019.csv")
demog_df <- read_csv("demographic_1990-2018.csv")
electoral_votes_df <- read_csv("electoralcollegevotes_1948-2020.csv")

# standardize naming of Nebraska and Maine
poll_2020_df$state[((poll_2020_df$state == "Nebraska CD-1") | (poll_2020_df$state == "Nebraska CD-2"))] <- "Nebraska"
poll_state_df$state[((poll_state_df$state == "NE-1") | (poll_state_df$state == "NE-2") | (poll_state_df$state == "NE-3"))] <- "Nebraska"
poll_2020_df$state[((poll_2020_df$state == "Maine CD-1") | (poll_2020_df$state == "Maine CD-2"))] <- "Maine"
poll_state_df$state[((poll_state_df$state == "ME-1") | (poll_state_df$state == "ME-2"))] <- "Maine"

### SUBSET AND MERGE DATA ---------------------------------

# data with incumbency variable
incumbent_dat <- popvote_df %>%
  select(year, party, incumbent_party)

# dataframe with state names and abbreviations
temp_df <- data.frame(states_list, states_list_abr)

# adjust date column
poll_2020_df <- poll_2020_df %>%
  mutate(modeldate = as.Date(modeldate, "%m/%d/%y"))

covid_state_df <- covid_state_df %>%
  mutate(submission_date = as.Date(submission_date, "%m/%d/%y"))

# historic polling and incumbent dataframe
dat <- popvote_state_df %>% 
  right_join(poll_state_df %>% 
               filter(days_left <= 56) %>%      # include polls up to 8 weeks before election
               group_by(year, state, party) %>% 
               summarise(avg_poll = mean(avg_poll))) %>%
  full_join(incumbent_dat)

# dataframe with 2019 population estimates by state
census_df <- census_df %>%
  select(NAME, POPESTIMATE2019)

# covid deaths and 2020 polling dataframe
covid_dat <- covid_state_df %>% 
  select(submission_date, state, tot_death) %>%
  rename(state_abr = state) %>%
  left_join(temp_df, by = c("state_abr" = "states_list_abr")) %>% 
    rename(state = states_list) %>%
  inner_join(poll_2020_df, by = c("submission_date" = "modeldate", "state" = "state")) %>%
  left_join(census_df, by = c("state" = "NAME")) %>%
  mutate(tot_death_adj = tot_death / POPESTIMATE2019)

# demographics and historic polling dataframe
demog_dat <- popvote_state_df %>% 
  full_join(poll_state_df %>% 
              filter(days_left <= 56) %>%      # include polls up to 8 weeks before election
              group_by(year, state, party) %>% 
              summarise(avg_poll = mean(avg_poll)),
            by = c("year" ,"state")) %>%
  left_join(temp_df, by = c("state" = "states_list")) %>%
  left_join(demog_df %>% select(-c("total")), by = c("year", "states_list_abr" = "state"))

# calculate change in each demographic group
demog_dat_change <- demog_dat %>%
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

# new data for 2020
demog_2020 <- subset(demog_df, year == 2018) %>%
  left_join(temp_df, by = c("state" = "states_list_abr"))

demog_2020_change <- demog_df %>%
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
         age65_change = age65 - lag(age65, order_by = year)) %>%
  filter(year == 2018) %>%
  left_join(temp_df, by = c("state" = "states_list_abr"))

# electoral votes dataframe
electoral_votes <- electoral_votes_df %>%
  select(X1, `2020`) %>%
  rename(state = X1, votes = `2020`)


### FIT MODELS AND PREDICT -----------------------------------

# prediction variables
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
namevector1 <- c("state_prediction")
namevector2 <- c("poll_model_coef")
namevector3 <- c("covid_model_coef")
namevector4a <- c("demog_model_coef1")
namevector4b <- c("demog_model_coef2")
namevector5 <- c("lwr")
namevector6 <- c("upr")
namevector7 <- c("poll_model_rmse")
namevector8 <- c("covid_model_rmse")
namevector9 <- c("demog_model_rmse")
namevector10 <- c("weighted_rmse")
state_predictions[ , c(namevector1, namevector2, namevector3, namevector4a, namevector4b, namevector5, 
                       namevector6, namevector7, namevector8, namevector9, namevector10)] <- NA

# function to find prediction for each state with weighted ensemble
predict.function <- function(state_name) {
  
  # polling model
  state_df <- dat %>%
    filter((state == state_name) & (party == "republican"))
  state_poll_model <- lm(R_pv2p ~ avg_poll, data = state_df)
  state_predictions$poll_model_coef[state_predictions$states_list == state] <<- state_poll_model$coef[2]
   state_2020 <- poll_2020_df %>%
    filter((state == state_name) & (candidate_name == "Donald Trump"))
  state_2020_true <- data.frame(avg_poll = mean(state_2020$pct_trend_adjusted))
  
  ## polling model rmse
  state_poll <- state_df %>%
    select(avg_poll)
  state_df$predicted_pv2p <- predict(state_poll_model, state_poll)
  state_predictions$poll_model_rmse[state_predictions$states_list == state] <<- rmse(state_df$R_pv2p, state_df$predicted_pv2p)
  
  # covid and polling model
  state_covid_df <- covid_dat %>%
    filter(state == state_name)
  state_covid_model <- lm(pct_trend_adjusted ~ tot_death_adj, data = state_covid_df)
  state_predictions$covid_model_coef[state_predictions$states_list == state] <<- state_covid_model$coef[2]
  state_covid <- covid_dat %>%
    filter((state == state_name) & (submission_date == "2020-10-27"))
  state_covid_true <- data.frame(tot_death_adj = mean(state_covid$tot_death_adj))
  
  ## covid and polling model rmse
  state_covid <- state_covid_df %>%
    select(tot_death_adj)
  state_covid_df$predicted_pv2p <- predict(state_covid_model, state_covid)
  state_predictions$covid_model_rmse[state_predictions$states_list == state] <<- rmse(state_covid_df$pct_trend_adjusted, state_covid_df$predicted_pv2p)
  
  # demographic and polling model
  state_demog_df <- demog_dat_change %>%
    filter(state == state_name) %>%
    drop_na()
  state_demog_model <- lm(R_pv2p ~ Female_change + Black_change, data = state_demog_df)
  state_predictions$demog_model_coef1[state_predictions$states_list == state] <<- state_demog_model$coef[2]
  state_predictions$demog_model_coef2[state_predictions$states_list == state] <<- state_demog_model$coef[3]
  state_demog <- demog_2020_change %>%
    filter(states_list == state_name)
  state_demog_true <- data.frame(Female_change = state_demog$Female_change, 
                                 Black_change = state_demog$Black_change)
  
  ## demographic and polling model rmse
  state_demog <- state_demog_df %>%
    select(Female_change, Black_change)
  state_demog_df$predicted_pv2p <- predict(state_demog_model, state_demog)
  state_predictions$demog_model_rmse[state_predictions$states_list == state] <<- rmse(state_demog_df$R_pv2p, state_demog_df$predicted_pv2p)

  # weighted rmse
  state_predictions$weighted_rmse[state_predictions$states_list == state] <<- 0.6*rmse(state_df$R_pv2p, state_df$predicted_pv2p) +
                                                                              0.2*rmse(state_covid_df$pct_trend_adjusted, state_covid_df$predicted_pv2p) +
                                                                              0.2*rmse(state_demog_df$R_pv2p, state_demog_df$predicted_pv2p)
  
  # prediction interval bounds
  state_poll_prediction <- predict(state_poll_model, state_2020_true, interval = "prediction", level = 0.95)
  state_covid_prediction <- predict(state_covid_model, state_covid_true, interval = "prediction", level = 0.95)
  state_demog_prediction <- predict(state_demog_model, state_demog_true, interval = "prediction", level = 0.95)
  
  state_predictions$lwr[state_predictions$states_list == state] <<- 0.6*state_poll_prediction[, 2] +
                                                                    0.2*state_covid_prediction[, 2] +
                                                                    0.2*state_demog_prediction[, 2]
  state_predictions$upr[state_predictions$states_list == state] <<- 0.6*state_poll_prediction[, 3] +
                                                                    0.2*state_covid_prediction[, 3] +
                                                                    0.2*state_demog_prediction[, 3]
  # point prediction
  state_prediction <- 0.6*predict(state_poll_model, state_2020_true) + 
                      0.2*predict(state_covid_model, state_covid_true) +
                      0.2*(predict(state_demog_model, state_demog_true))

  return(state_prediction)
  
  }

# add to dataframe
for (state in states_list) {
  state_prediction <- predict.function(state)
  state_predictions$state_prediction[state_predictions$states_list == state] <- state_prediction
}

# determine electoral vote count
state_predictions <- state_predictions %>%
  left_join(electoral_votes, by = c("states_list" = "state"))

trump_win <- state_predictions %>%
  filter(state_prediction >= 50)

sum(trump_win$votes)


### VISUALIZE -----------------------------------

# personalized themes
blog_theme <- theme_bw(base_family = "Avenir") + 
  theme(panel.border = element_blank(),
        plot.title   = element_text(size = 15, hjust = 0.5), 
        axis.text.x  = element_text(angle = 0, hjust = 0.5),
        axis.text    = element_text(size = 12),
        strip.text   = element_text(size = 18),
        legend.position = "none",
        panel.grid.minor = element_blank())

blog_theme1 <- theme_bw(base_family = "Avenir") + 
  theme(panel.border = element_blank(),
        plot.title   = element_text(size = 12, hjust = 0.5), 
        legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# map of predictions
state_predictions %>% 
  mutate(states_list_abr = as.character(states_list_abr)) %>%
  ggplot(aes(state = states_list_abr, fill = (state_prediction <= 50))) +
  geom_statebins(dark_lbl = "white") +
  theme_statebins() +
  scale_fill_manual(values = c("#ff5252", "#2096f3")) +
  ggtitle("2020 Presidential Election Prediction") +
  blog_theme1

ggsave("final_pred_map.png", height = 4, width = 6)

# visualize prediction intervals for polling model
state_predictions_graph <- state_predictions %>% rename(state = states_list)
state_predictions_graph$inc_win <- NA
state_predictions_graph$inc_win[state_predictions_graph$state_prediction >= 50] <- TRUE
state_predictions_graph$inc_win[state_predictions_graph$state_prediction < 50] <- FALSE

ggplot(state_predictions_graph, aes(x = states_list_abr, y = state_prediction, color = as.factor(inc_win))) +
  geom_point(size = 2.5) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .1) +
  scale_colour_manual(values = c("#2096f3", "#ff5252")) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 25)) +
  xlab("State") + 
  ylab("Predicted Incumbent Party\n2-Party Vote Share") +
  ggtitle("2020 Presidential Election Predictions\n95% Confidence Intervals") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 50, linetype = "dotted") +
  geom_vline(xintercept = 0.5) +
  blog_theme

ggsave("final_pred_state.png", height = 4, width = 15)

# histogram of coefficients
hist1 <- ggplot(state_predictions, aes(x = poll_model_coef)) + 
  geom_histogram(binwidth = 0.06, color = "white", fill = "#9074a3") +
  xlab("") +
  ylab("") +
  ggtitle("Polling Model") +
  blog_theme

hist2 <- ggplot(state_predictions, aes(x = covid_model_coef)) + 
  geom_histogram(binwidth = 3300, color = "white", fill = "#9074a3") + 
  xlab("") +
  ylab("") +
  ggtitle("COVID Model") +
  blog_theme

hist3 <- ggplot(state_predictions, aes(x = demog_model_coef2)) + 
  geom_histogram(binwidth = 8, color = "white", fill = "#9074a3") + 
  xlab("") +
  ylab("") +
  ggtitle("Demographics Model: Black Change") +
  blog_theme

hist4 <- ggplot(state_predictions, aes(x = demog_model_coef1)) + 
  geom_histogram(binwidth = 10, color = "white", fill = "#9074a3") + 
  xlab("") +
  ylab("") +
  ggtitle("Demographics Model: Female Change") +
  blog_theme

coef_hist <- ggarrange(hist1, hist2, hist3, hist4, ncol = 2, nrow = 2)
annotate_figure(coef_hist,
                top = text_grob("Regression Model Coefficients", face = "bold", size = 20))
ggsave("final_pred_coef.png", width = 10, height = 6)

# histogram of rmse
ggplot(state_predictions, aes(x = weighted_rmse)) + 
  geom_histogram(binwidth = 1, color = "white", fill = "#9074a3") + 
  xlab("") +
  ylab("") +
  ggtitle("RMSE") +
  blog_theme

ggsave("final_pred_rmse.png", width = 6, height = 4)

# sensitivity test
poll_weight <- c(0.6, 0.7, 0.8, 0.9, 1.0)
new_pred <- c(224, 259, 265, 269, 269)
sensitivity_df <- data.frame(poll_weight, new_pred)

ggplot(sensitivity_df, aes(x = poll_weight, y = new_pred)) +
  geom_point(size = 2.5, color = "#595959") +
  xlab("Poll Model Weight") +
  ylab("Predicted Incumbent Party\nElectoral Vote Count") +
  blog_theme

ggsave("sensitivity.png", width = 6, height = 4)

