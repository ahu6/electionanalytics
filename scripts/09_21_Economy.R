### September 21: Economic Indicators
### Gov 1347: Election Analysis
### Alison Hu

### PREAMBLE -----------------------------------

# load necessary libraries
library(tidyverse)
library(ggplot2)
library(Metrics)

# read in data
economy_df <- read_csv("econ.csv") 
popvote_df <- read_csv("popvote_1948-2016.csv") 

### SUBSET DATA ---------------------------------

# combined df with popular vote and economic indicators
full_df <- popvote_df %>% 
  filter(incumbent_party == TRUE) %>%
  select(year, winner, pv2p) %>%
  left_join(economy_df %>% filter(quarter == 2 | quarter == 3))

# df with GDP growth rate Q14-15 average
GDP_df <- full_df %>%
  select(year, pv2p, quarter, GDP_growth_qt) %>%
  spread(quarter, GDP_growth_qt) %>% 
  mutate(GDP_avg = (`2` + `3`) / 2)

# df with RDI growth rate Q14-15 average
RDI_df <- full_df %>% 
  filter(year >= 1960) %>%
  select(year, pv2p, quarter, RDI_growth) %>%
  spread(quarter, RDI_growth) %>% 
  mutate(RDI_avg = (`2` + `3`) / 2)

# df with unemployment rate Q14-15 average
unempl_df <- full_df %>%
  select(year, pv2p, quarter, unemployment) %>%
  spread(quarter, unemployment) %>% 
  mutate(unempl_avg = (`2` + `3`) / 2)

# df with inflation rate Q14-15 average
infl_df <- full_df %>%
  select(year, pv2p, quarter, inflation) %>%
  spread(quarter, inflation) %>% 
  mutate(infl_avg = (`2` + `3`) / 2)



### FIT MODELS -----------------------------------

#### linear model 1: GDP growth rate Q14-15 average ----
lm_GDP <- lm(pv2p ~ GDP_avg, data = GDP_df)
summary(lm_GDP)

# r-squared value
r2_GDP <- summary(lm_GDP)$r.squared

# in-sample error
GDP_all <- GDP_df %>%
  select(GDP_avg)
GDP_df$predicted_pv2p <- predict(lm_GDP, GDP_all)
rmse_GDP <- rmse(GDP_df$pv2p, GDP_df$predicted_pv2p)

hist(GDP_df$pv2p - GDP_df$predicted_pv2p,
     main="histogram of true Y - predicted Y")

# out-of-sample error: cross-validation (1000 runs)
outsamp_errors <- sapply(1:1000, function(i){
  years_outsamp <- sample(GDP_df$year, 8)
  outsamp_mod <- lm(pv2p ~ GDP_avg,
                    GDP_df[!(GDP_df$year %in% years_outsamp),])
  
  outsamp_pred <- predict(outsamp_mod,
                          newdata = GDP_df[GDP_df$year %in% years_outsamp,])
  outsamp_true <- GDP_df$pv2p[GDP_df$year %in% years_outsamp]
  mean(outsamp_pred - outsamp_true)
})

# distribution of out-of-sample errors
mean(abs(outsamp_errors))
hist(outsamp_errors,
     main="mean out-of-sample residual \n(1000 runs of cross-validation)")

# prediction for 2020
GDP_2020 <- economy_df %>%
  subset(year == 2020 & quarter == 2) %>%
  select(GDP_growth_qt) %>%
  rename(GDP_avg = GDP_growth_qt)

# predicted value with 95% confidence interval
predict_GDP <- predict(lm_GDP, GDP_2020, interval = "prediction")

#### linear model 2: RDI growth rate Q14-15 average ----
lm_RDI <- lm(pv2p ~ RDI_avg, data = RDI_df)
summary(lm_RDI)

# r-squared value
r2_RDI <- summary(lm_RDI)$r.squared

# in-sample error
RDI_all <- full_df %>%
  filter(year >= 1960) %>%
  select(RDI_growth) 
predicted3 <- predict(lm3, RDI_all)
rmse_RDI <- rmse(RDI_df$pv2p, RDI_df$predicted_pv2p)

# prediction for 2020
RDI_2020 <- economy_df %>%
  subset(year == 2020 & quarter == 2) %>%
  select(RDI_growth) %>%
  rename(RDI_avg = RDI_growth)

# predicted value with 95% confidence interval
predict_RDI <- predict(lm_RDI, RDI_2020, interval = "prediction")

#### linear model 3: unemployment rate Q14-15 average ----
lm_unempl <- lm(pv2p ~ unempl_avg, data = unempl_df)
summary(lm_unempl)

# r-squared value
r2_unempl <- summary(lm_unempl)$r.squared

# in-sample error
unempl_all <- unempl_df %>%
  select(unempl_avg)
unempl_df$predicted_pv2p <- predict(lm_unempl, unempl_all)
rmse_unempl <- rmse(unempl_df$pv2p, unempl_df$predicted_pv2p)

# prediction for 2020
unempl_2020 <- economy_df %>%
  subset(year == 2020 & quarter == 2) %>%
  select(unemployment) %>%
  rename(unempl_avg = unemployment)

# predicted value with 95% confidence interval
predict_unempl <- predict(lm_unempl, unempl_2020, interval = "prediction")

#### linear model 4: inflation rate Q14-15 average ----
lm_infl <- lm(pv2p ~ infl_avg, data = infl_df)
summary(lm_infl)

# r-squared value
r2_infl <- summary(lm_infl)$r.squared

# in-sample error
infl_all <- infl_df %>%
  select(infl_avg)
infl_df$predicted_pv2p <- predict(lm_infl, infl_all)
rmse_infl <- rmse(infl_df$pv2p, infl_df$predicted_pv2p)

# prediction for 2020
infl_2020 <- economy_df %>%
  subset(year == 2020 & quarter == 2) %>%
  select(inflation) %>%
  rename(infl_avg = inflation)

# predicted value with 95% confidence interval
predict_infl <- predict(lm_infl, infl_2020, interval = "prediction")

#### linear model 5: multivariate unemployment and inflation rates Q14-15 average ----

# check degree of colinearity
cor(unempl_df$unempl_avg, infl_df$infl_avg)

# combine df
multi_df <- unempl_df %>% 
  select(year, pv2p, unempl_avg) %>%
  left_join(infl_df) %>%
  select(year, pv2p, unempl_avg, infl_avg)

# fit model
lm_multi <- lm(pv2p ~ unempl_avg + infl_avg, data = multi_df)
summary(lm_multi)

# r-squared value
r2_multi <- summary(lm_multi)$r.squared

# in-sample error
multi_all <- multi_df %>%
  select(unempl_avg, infl_avg)
multi_df$predicted_pv2p <- predict(lm_multi, multi_all)
rmse_multi <- rmse(multi_df$pv2p, multi_df$predicted_pv2p)

# prediction for 2020
multi_2020 <- economy_df %>%
  subset(year == 2020 & quarter == 2) %>%
  select(unemployment, inflation) %>%
  rename(unempl_avg = unemployment, infl_avg = inflation)

# predicted value with 95% confidence interval
predict_multi <- predict(lm_multi, multi_2020, interval = "prediction")

### COMBINE MODEL PREDICTIONS -----------------------------------
indicator <- c("GDP", "RDI", "unemployment", "inflation", "multi")
r2 <- c(r2_GDP, r2_RDI, r2_unempl, r2_infl, r2_multi)
rmse <- c(rmse_GDP, rmse_RDI, rmse_unempl, rmse_infl, rmse_multi)
r2_rmse_df <- data.frame(indicator, r2, rmse)

# create new data frame with each model's prediction interval
fit <- c(predict_GDP[1], predict_RDI[1], predict_unempl[1], predict_infl[1], predict_multi[1])
lwr <- c(predict_GDP[2], predict_RDI[2], predict_unempl[2], predict_infl[2], predict_multi[2])
upr <- c(predict_GDP[3], predict_RDI[3], predict_unempl[3], predict_infl[3], predict_multi[3])
prediction_df <- data.frame(indicator, lwr, fit, upr)

### VISUALIZE PREDICTIONS -----------------------------------

# personalized theme
blog_theme <- theme_bw(base_family = "Avenir") + 
  theme(panel.border = element_blank(),
        plot.title   = element_text(size = 15, hjust = 0.5), 
        axis.text.x  = element_text(angle = 0, hjust = 0.5),
        axis.text    = element_text(size = 12),
        strip.text   = element_text(size = 18),
        legend.position = "none",
        panel.grid.minor = element_blank()
        )

# prediction intervals for 2020
ggplot(prediction_df, aes(x = reorder(indicator, fit), y = fit, color = indicator)) +
  geom_point(size = 2.5) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .1) +
  scale_color_manual(values = c("#2096f3", "#5885cb", "#9074a3", "#ff5252", "#c7637a")) + 
  scale_y_continuous(breaks = seq(from = -50, to = 150, by = 25)) +
  xlab("Indicator") + 
  ylab("Predicted 2-Party Vote Share") +
  ggtitle("Predictions Based on Economic Indicators") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 100, linetype = "dotted") +
  geom_vline(xintercept = 0.5) +
  blog_theme

ggsave("predictions.png", height = 4, width = 6)

# r2 
ggplot(r2_rmse_df, aes(x = reorder(indicator, -r2), y = r2, fill = indicator)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#5885cb", "#c7637a", "#9074a3", "#2096f3", "#ff5252")) +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = .1)) +
  xlab("Indicator") + 
  ylab("R2") +
  ggtitle("R2 Scores") +
  blog_theme

ggsave("r2.png", height = 4, width = 6)




#### STATE LEVEL ------
#### data processing ----
# read in data
local_df <- read_csv("local.csv") 
popvote_state_df <- read_csv("popvote_bystate_1948-2016.csv") 
load(file = "state_predictions.Rdata")

# select relevant columns
local_df_select <- local_df %>%
  rename(year = Year, state = `State and area`) %>%
  select(state, year, Month, Unemployed_prce)

# df with popular vote and unemployment
state_full_df <- popvote_state_df %>% 
  select(state, year, R_pv2p, D_pv2p) %>%
  left_join(local_df_select) %>% 
  filter(Month == "04" | Month == "05" | Month == "06" | 
         Month == "07" | Month == "08" | Month == "09") %>% # Apr-Sep = Q14-15
  spread(Month, Unemployed_prce) %>% 
  mutate(unemployed_avg = (`04` + `05` + `06` + `07` + `08` + `09`) / 6)

# select incumbent indicator from national popular vote data
incumbent_df <- popvote_df %>% 
  filter(incumbent_party == TRUE) %>%
  select(year, party, incumbent)

# merge incumbent indicator with state data
incumbent_state_df <- state_full_df %>%
  select(state, year, R_pv2p, D_pv2p, unemployed_avg) %>%
  left_join(incumbent_df)

# encode party of incumbent in column "party"
incumbent_state_df$party[(incumbent_state_df$party == "republican") & (incumbent_state_df$incumbent == FALSE)] <- "democrat"
incumbent_state_df$party[(incumbent_state_df$party == "democrat") & (incumbent_state_df$incumbent == FALSE)] <- "republican"

# encode 2-party vote share of incumbent in new column "pv2p"
incumbent_state_df$pv2p <- ifelse(incumbent_state_df$party == "republican", incumbent_state_df$R_pv2p, incumbent_state_df$D_pv2p)

# select relevant columns
state_unempl_df <- incumbent_state_df %>%
  select(state, year, unemployed_avg, pv2p)

#### fit model ----

# prediction for 2020
state_2020 <- local_df_select %>%
  subset(year == 2020 & (Month == "04" | Month == "05")) %>%
  spread(Month, Unemployed_prce) %>%
  mutate(unemployed_avg = (`04` + `05`) / 2) %>%
  select(unemployed_avg)

# list of states
states <- local_df_select  %>%
  subset(year == 2020 & (Month == "04" | Month == "05")) %>%
  spread(Month, Unemployed_prce) %>%
  mutate(unemployed_avg = (`04` + `05`) / 2)

##### functions to clean data ----
al_state_unempl_df <- state_unempl_df %>%
  filter(state == "Alabama")
lm_state_al <- lm(pv2p ~ unemployed_avg, data = al_state_unempl_df)
state_r2 <- summary(lm_state_al)$r.squared
state_predictions <- predict(lm_state_al, state_2020[1,], interval = "prediction")

predict.function <- function(state_name, state_num) {
  state_name_unempl_df <- state_unempl_df %>%
    filter(state == state_name)
  lm_state_name <- lm(pv2p ~ unemployed_avg, data = state_name_unempl_df)
  state_predictions_tmp <- predict(lm_state_name, state_2020[state_num,], interval = "prediction")
  state_predictions <- rbind(state_predictions, state_predictions_tmp)
}

r2.function <- function(state_name, state_num) {
  state_name_unempl_df <- state_unempl_df %>%
    filter(state == state_name)
  lm_state_name <- lm(pv2p ~ unemployed_avg, data = state_name_unempl_df)
  state_r2_tmp <- summary(lm_state_name)$r.squared
  state_r2 <- rbind(state_r2, state_r2_tmp)
}

state_r2 <- r2.function("Wyoming", 53)
state_predictions <- predict.function("Wyoming", 53)

states_list <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
               "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho",
               "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", 
               "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
               "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
               "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
               "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
               "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

state_r2 <- data.frame(states_list, state_r2)
state_predictions <- data.frame(states_list, state_predictions)

states_list_abr <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT",
                 "DE", "DC", "FL", "GA", "HI", "ID",
                 "IL", "IN", "IA", "KS", "KY", "LA", "ME", 
                 "MD", "MA", "MI", "MN", "MS", "MO", 
                 "MT", "NE", "NV", "NH", "NJ", "NM",
                 "NY", "NC", "ND", "OH", "OK", "OR", 
                 "PA", "RI", "SC", "SD", "TN",
                 "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

state_predictions <- data.frame(states_list_abr, state_predictions)

save(state_r2, file = "state_r2.Rdata")
save(state_predictions, file = "state_predictions.Rdata")


# prediction intervals for 2020 ----
# visualize
ggplot(state_predictions, aes(x = reorder(states_list_abr, fit), y = fit, color = as.integer(states_list_abr))) +
  geom_point(size = 2.5) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .1) +
  scale_colour_gradient(low = "#2096f3", high = "#ff5252") +
  scale_y_continuous(breaks = seq(from = -50, to = 150, by = 25)) +
  xlab("State") + 
  ylab("Predicted 2-Party Vote Share") +
  ggtitle("Predictions Based on State Unemployment Rate") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 100, linetype = "dotted") +
  geom_vline(xintercept = 0.5) +
  blog_theme

ggsave("predictions_state.png", height = 4, width = 15)

# mean of all 50 states' prediction intervals
summary(state_predictions)
