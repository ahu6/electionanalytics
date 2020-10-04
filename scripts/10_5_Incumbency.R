### October 5: Incumbency
### Gov 1347: Election Analysis
### Alison Hu

### PREAMBLE -----------------------------------

# load libraries
library(tidyverse)
library(ggplot2)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

# read in data
popvote_df    <- read_csv("popvote_1948-2016.csv")
economy_df    <- read_csv("econ.csv")
approval_df   <- read_csv("approval_gallup_1941-2020.csv")

### TIME-FOR-CHANGE MODEL -----------------------------------

election_years <- seq(1948, 2020, 4)

# create month column and filter to October data
approval_df_new <- approval_df %>%
  distinct() %>%
  mutate(poll_enddate_month = months(poll_enddate)) %>%
  mutate(net_approve = approve - disapprove) %>%
  filter(year %in% election_years) %>%
  filter(poll_enddate_month == "October") %>%
  select(year, incumbent_pres = president, net_approve, poll_enddate, poll_enddate_month)

# aggregate mean net approval for years with more than one entry
approval_df_new$net_approve_avg <- NA
approval_df_new$net_approve_avg[1] <- mean(approval_df_new$net_approve[approval_df_new$year == 2016])
approval_df_new$net_approve_avg[31] <- mean(approval_df_new$net_approve[approval_df_new$year == 2012])
approval_df_new$net_approve_avg[58] <- mean(approval_df_new$net_approve[approval_df_new$year == 2008])
approval_df_new$net_approve_avg[61] <- mean(approval_df_new$net_approve[approval_df_new$year == 2004])
approval_df_new$net_approve_avg[67] <- mean(approval_df_new$net_approve[approval_df_new$year == 2000])
approval_df_new$net_approve_avg[69] <- mean(approval_df_new$net_approve[approval_df_new$year == 1996])
approval_df_new$net_approve_avg[71] <- mean(approval_df_new$net_approve[approval_df_new$year == 1992])
approval_df_new$net_approve_avg[75] <- mean(approval_df_new$net_approve[approval_df_new$year == 1984])
approval_df_new$net_approve_avg[77] <- mean(approval_df_new$net_approve[approval_df_new$year == 1968])
approval_df_new$net_approve_avg[78] <- mean(approval_df_new$net_approve[approval_df_new$year == 1960])
approval_df_new$net_approve_avg[79] <- mean(approval_df_new$net_approve[approval_df_new$year == 1952])
                                                                  
# merge data
tfc_df <- popvote_df %>%
  filter(incumbent_party) %>%
  select(year, candidate, party, pv, pv2p, incumbent) %>%
  inner_join(
    approval_df_new %>% 
      group_by(year, incumbent_pres) %>% 
      slice(1) %>%
      select(year, incumbent_pres, net_approve_avg, poll_enddate),
    by = "year"
  ) %>%
  inner_join(
    economy_df %>%
      filter(quarter == 2) %>%
      select(GDP_growth_qt, RDI_growth, year),
    by = "year"
  )

tfc_df$incumbent <- as.factor(tfc_df$incumbent)

# data for rdi model
tfc_df_rdi <- tfc_df %>%
  filter(year > 1952)

# create models
tfc_gdp_mod <- lm(pv2p ~ net_approve_avg + GDP_growth_qt + incumbent, data = tfc_df)
tfc_rdi_mod <- lm(pv2p ~ net_approve_avg + RDI_growth + incumbent, data = tfc_df_rdi)

# summary table of models
tab_model(tfc_gdp_mod, tfc_rdi_mod, show.ci = FALSE,
          dv.labels = c("GDP Model", "RDI Model"),
          p.style = "stars")

### PREDICT USING MODELS -----------------------------------

dat_2020 <- data.frame(GDP_growth_qt = -9.49471586, RDI_growth = 0.0972422967, net_approve_avg = -15, incumbent = as.factor(TRUE))
dat_2020_adj <- data.frame(GDP_growth_qt = -3.02933574, RDI_growth = 0.0972422967, net_approve_avg = -15, incumbent = as.factor(TRUE))

# predicted value for 2020 with 95% confidence interval
predict_gdp_mod <- predict(tfc_gdp_mod, dat_2020, interval = "prediction")
predict_rdi_mod <- predict(tfc_rdi_mod, dat_2020, interval = "prediction")

# predicted value for 2020 adjusted with 95% confidence interval
predict_gdp_mod_adj <- predict(tfc_gdp_mod, dat_2020_adj, interval = "prediction")

# dataframe with model predictions
indicator <- c("GDP", "RDI", "GDP adj")
fit <- c(predict_gdp_mod[1], predict_rdi_mod[1], predict_gdp_mod_adj[1])
lwr <- c(predict_gdp_mod[2], predict_rdi_mod[2], predict_gdp_mod_adj[2])
upr <- c(predict_gdp_mod[3], predict_rdi_mod[3], predict_gdp_mod_adj[3])
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
ggplot(prediction_df, aes(x = indicator, y = fit, color = indicator)) +
  geom_point(size = 2.5) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .1) +
  scale_y_continuous(breaks = seq(from = -50, to = 150, by = 25)) +
  scale_color_manual(values = c("#2096f3", "#9074a3", "#ff5252")) +
  xlab("Economic Indicator Used") + 
  ylab("Predicted Incumbent Party \n2-Party Vote Share") +
  ggtitle("Variations of Time-for-Change Model") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 50, linetype = "dotted") +
  geom_vline(xintercept = 0.5) +
  blog_theme

ggsave("tfc_predictions.png", height = 4, width = 6)

