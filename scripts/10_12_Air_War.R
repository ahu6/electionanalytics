### October 12: Air War
### Gov 1347: Election Analysis
### Alison Hu

### PREAMBLE -----------------------------------

# load necessary libraries
library(tidyverse)
library(ggplot2)

# suppress scientific notation
options(scipen = 999)

# read in data
popvote_df <- read_csv("popvote_1948-2016.csv")
popvote_state_df <- read_csv("popvote_bystate_1948-2016.csv")
ad_2000_2012 <- read_csv("ad_campaigns_2000-2012.csv")
ad_2020 <- read_csv("ads_2020.csv")
poll_2020_df <- read_csv("polls_2020.csv")

### 2012 ELECTION AD SPENDING ANALYSIS ---------------------------------

# subset ad spending to year 2012
ad_2012 <- ad_2000_2012 %>%
  filter(cycle == 2012)

# subset popular vote share to year 2012
popvote_state_2012 <- popvote_state_df %>%
  filter(year == 2012)

# create dataframe with total ad costs by state for 2012 election
state <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
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

state_spending <- data.frame(state, states_list_abr)
namevector <- c("obama_ad_cost", "romney_ad_cost")
state_spending[ , namevector] <- NA

# functions to calculate total spending by candidate
cand1.cost.function <- function(state_name) {
  dat <- ad_2012 %>%
    filter((sponsor == "Barack Obama") & (state == state_name))
  cand1_cost <- sum(dat$total_cost)
}

cand2.cost.function <- function(state_name) {
  dat <- ad_2012 %>%
    filter((sponsor == "Mitt Romney") & (state == state_name))
  cand2_cost <- sum(dat$total_cost)
}

# add to dataframe
for (state in states_list_abr) {
  cand1_cost <- cand1.cost.function(state)
  cand2_cost <- cand2.cost.function(state)
  state_spending$obama_ad_cost[state_spending$states_list_abr == state] <- cand1_cost
  state_spending$romney_ad_cost[state_spending$states_list_abr == state] <- cand2_cost
}

# drop rows with 0 cost in either/both obama and romney ad cost and join with popular vote data
df_2012 <- state_spending %>%
  filter((obama_ad_cost > 0) & (romney_ad_cost > 0)) %>%
  left_join(popvote_state_2012)

# linear regression models
obama_spending_mod <- lm(D_pv2p ~ obama_ad_cost, data = df_2012)
romney_spending_mod <- lm(R_pv2p ~ romney_ad_cost, data = df_2012)


### VISUALIZE 2012 ELECTION -----------------------------------

# personalized theme for visualizations
blog_theme <- theme_bw(base_family = "Avenir") + 
  theme(panel.border = element_blank(),
        plot.title   = element_text(size = 15, hjust = 0.5), 
        axis.text.x  = element_text(angle = 0, hjust = 0.5),
        axis.text    = element_text(size = 12),
        strip.text   = element_text(size = 18),
        legend.position = "none",
        panel.grid.minor = element_blank()
  )

ggplot(df_2012, aes(x = obama_ad_cost, y = D_pv2p)) +
  geom_smooth(method = "lm", color = "#262626", fill = "#2096f3") +
  geom_text(aes(label = states_list_abr), hjust = 0, vjust = 0, color = "#262626") +
  xlab("Obama Ad Spending") + 
  ylab("Democratic Party 2-Party Vote Share") +
  ggtitle("Obama 2012 Advertisement Spending and Vote Share by State") +
  blog_theme

ggsave("obama_2012_ad_spending.png", height = 4, width = 8)

ggplot(df_2012, aes(x = romney_ad_cost, y = R_pv2p)) +
  geom_smooth(method = "lm", color = "#262626", fill = "#ff5252") +
  geom_text(aes(label = states_list_abr), hjust = 0, vjust = 0, color = "#262626") +
  xlab("Romney Ad Spending") + 
  ylab("Republican Party 2-Party Vote Share") +
  ggtitle("Romney 2012 Advertisement Spending and Vote Share by State") +
  blog_theme

ggsave("romney_2012_ad_spending.png", height = 4, width = 8)




### 2020 ELECTION AD COUNT ANALYSIS ---------------------------------

# subset data to longer time frame
ad_2020_04 <- ad_2020 %>%
  filter(period_startdate == "2020-04-09")

# standardize naming of Nebraska and Maine entries
poll_2020_df$state[((poll_2020_df$state == "Nebraska CD-1") | (poll_2020_df$state == "Nebraska CD-2"))] <- "Nebraska"
poll_2020_df$state[((poll_2020_df$state == "Maine CD-1") | (poll_2020_df$state == "Maine CD-2"))] <- "Maine"

# selected grades to include in model
grades <- c("A+", "A", "A-", "A/B", "B+", "B", "B-")

# select data from April 9, 2020 forward, aggregate polling data to average support
poll_2020_df_new <- poll_2020_df %>%
  slice(1:3224) %>%
  drop_na(state) %>%
  select(state, fte_grade, answer, pct) %>%
  filter(fte_grade %in% grades) %>%
  filter(answer == "Trump" | answer == "Biden") %>%
  group_by(state, answer) %>%
  summarise(avg_poll = mean(pct))

# airing count data
poll_2020_df_new$spending <- c(NA, NA, 36166, 29412, 114,
                               702, 128, 61, 7, 1,
                               NA, NA, 75931, 58425, 124,
                               27747, 868, 8435, NA, NA,
                               7, 226, 19, 467, 40,
                               3, 52657, 19777, 8804, 7173,
                               6, 401, 60, 9, 909,
                               1226, 8875, 6198, NA, NA,
                               NA, NA, 9, 1017, 7,
                               179, 36596, 43781, 4998, 9073,
                               65830, 53535, 1889, 4759, 451,
                               596, 3, 330, 51473, 41684)

# subset to biden data
ad_2020_biden <- poll_2020_df_new %>%
  drop_na(spending) %>%
  filter(answer == "Biden")

# subset to trump data
ad_2020_trump <- poll_2020_df_new %>%
  drop_na(spending) %>%
  filter(answer == "Trump")

# linear regression models
biden_spending_mod <- lm(avg_poll ~ spending, data = ad_2020_biden)
trump_spending_mod <- lm(avg_poll ~ spending, data = ad_2020_trump)

### VISUALIZE 2020 ELECTION -----------------------------------

# state abbreviations for visualization
ad_2020_biden$states_list_abr <- c("AZ", "CA", "CO", "CT",
                                   "FL", "GA", "IA", "KY", 
                                   "ME", "MA", "MI", "MN",
                                   "MS", "MT", "NE", "NV",
                                   "NM", "NY", "NC", "OH", 
                                   "PA", "SC", "TX", "WA", "WI")

# state abbreviations for visualization
ad_2020_trump$states_list_abr <- c("AZ", "CA", "CO", "CT",
                                   "FL", "GA", "IA", "KY", 
                                   "ME", "MA", "MI", "MN",
                                   "MS", "MT", "NE", "NV",
                                   "NM", "NY", "NC", "OH", 
                                   "PA", "SC", "TX", "WA", "WI")

# plots
ggplot(ad_2020_biden, aes(x = spending, y = avg_poll)) +
  geom_smooth(method = "lm", color = "#262626", fill = "#2096f3") +
  geom_text(aes(label = states_list_abr), hjust = 0, vjust = 0, color = "#262626") +
  xlab("Biden Advertisement Airings") + 
  ylab("Average Poll Support") +
  ggtitle("Biden 2020 Advertisement Airings and Poll Support by State") +
  blog_theme

ggsave("biden_2020_ad_spending.png", height = 4, width = 8)

ggplot(ad_2020_trump, aes(x = spending, y = avg_poll)) +
  geom_smooth(method = "lm", color = "#262626", fill = "#ff5252") +
  geom_text(aes(label = states_list_abr), hjust = 0, vjust = 0, color = "#262626") +
  xlab("Trump Advertisement Airings") + 
  ylab("Average Poll Support") +
  ggtitle("Trump 2020 Advertisement Spending and Poll Support by State") +
  blog_theme

ggsave("trump_2020_ad_spending.png", height = 4, width = 8)

