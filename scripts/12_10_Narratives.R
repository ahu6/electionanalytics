### December 10: Narratives
### Gov 1347: Election Analysis
### Alison Hu

### PREAMBLE -----------------------------------

# load libraries
library(quanteda)
library(tidyverse)
library(ggplot2)
library(ggpubr)

# read in data
speech_df <- read_csv("campaignspeech_2019-2020.csv")

biden_speech_df <- speech_df %>%
  filter(candidate == "Joe Biden")

trump_speech_df <- speech_df %>%
  filter(candidate == "Donald Trump")

biden_tweet_df <- read_csv("biden_tweets.csv")

### SPEECH ANALYSIS -----------------------------------

## BIDEN
# pre-process: make a `quanteda` corpus from dataframe
biden_speech_corpus <- corpus(biden_speech_df, text_field = "text", docid_field = "url")

# pre-process: tokenize and clean
biden_speech_toks <- tokens(biden_speech_corpus, 
                            remove_punct = TRUE,
                            remove_symbols = TRUE,
                            remove_numbers = TRUE,
                            remove_url = TRUE) %>% 
        tokens_tolower()

# pre-process: make doc-freq matrix
biden_speech_dfm <- dfm(biden_speech_toks, select = c("battle","*build", "better", "bipartisan",
                                          "character", "decen*", "love", "faith",
                                          "dignity", "respect", "soul", "truth", "humanity",
                                          "lead", "unite", "unity", "unify", "unification", "heal", "home", "restore",
                                          "integrity", "compassion*", "kind", "kindness", "empathy",
                                          "community", "tolerance", "generosity", "hope",
                                          "together", "believe", "care"
                                          ))
# word cloud
textplot_wordcloud(biden_speech_dfm, color = "#2096f3")

blog_theme <- theme_bw(base_family = "Avenir") + 
  theme(panel.border = element_blank(),
        plot.title   = element_text(size = 15, hjust = 0.5), 
        axis.text.x  = element_text(angle = 0, hjust = 0.5),
        axis.text    = element_text(size = 12),
        strip.text   = element_text(size = 18),
        legend.position = "none",
        panel.grid.minor = element_blank())

# summarize and visualize
biden_tstat_freq <- textstat_frequency(biden_speech_dfm)
head(biden_tstat_freq, n = 50)

biden_speech <- biden_speech_dfm %>% 
  textstat_frequency(n = 10) %>% 
  ggplot(aes(x = feature, y = frequency)) +
  geom_segment(aes(xend = feature, yend = 0), color = "#595959") +
  geom_point(size = 4, color = "#2096f3") +
  coord_flip() +
  ylim(0, 1100) +
  labs(x = NULL, y = NULL) +
  ggtitle("Biden") +
  blog_theme


## TRUMP
# pre-process: make a `quanteda` corpus from dataframe
trump_speech_corpus <- corpus(trump_speech_df, text_field = "text", docid_field = "url")

# pre-process: tokenize and clean
trump_speech_toks <- tokens(trump_speech_corpus, 
                            remove_punct = TRUE,
                            remove_symbols = TRUE,
                            remove_numbers = TRUE,
                            remove_url = TRUE) %>% 
  tokens_tolower()

# pre-process: make doc-freq matrix
trump_speech_dfm <- dfm(trump_speech_toks, select = c("better", "care", "together", 
                                                      "community", "home", "believe",
                                                      "build", "hope", "love", "kind"))

# summarize and visualize
trump_tstat_freq <- textstat_frequency(trump_speech_dfm)
head(trump_tstat_freq)

trump_speech <- trump_speech_dfm %>% 
  textstat_frequency(n = 10) %>% 
  ggplot(aes(x = feature, y = frequency)) +
  geom_segment(aes(xend = feature, yend = 0), color = "#595959") +
  geom_point(size = 4, color = "#ff5252") +
  coord_flip() +
  ylim(0, 1100) +
  labs(x = NULL, y = "Frequency") +
  ggtitle("Trump") +
  blog_theme

f1 <- ggarrange(biden_speech, trump_speech, ncol = 1, nrow = 2)
f1
ggsave("speech_analysis.png", width = 8, height = 8)

### SPEECH ANALYSIS: Time Series Analysis -----------------------------------

# dictionary of words
words <- list("battle","build", "better", "bipartisan",
          "character", "decency", "decent", "love", "faith",
          "dignity", "respect", "soul", "truth", "humanity",
          "lead", "unite", "unity", "unify", "unification", "heal", "home", "restore",
          "integrity", "compassion", "kind", "kindness", "empathy",
          "community", "tolerance", "generosity", "hope",
          "together", "believe", "care")

# calculate sum and average counts of words
biden_speech_time <- biden_speech_df %>%
  mutate(count = str_count(text, paste(words, collapse = '|'))) %>%
  group_by(approx_date) %>%
  mutate(tot_count = sum(count), avg_count = mean(count))

# visualize
ggplot(biden_speech_time, aes(x = approx_date, y = count)) +
  geom_point(color = "#2096f3") +
  geom_smooth(aes(y = avg_count), method = "loess", se = FALSE, color = "#595959") +
  xlab("") +
  ylab("Frequency") +
  ggtitle("Biden Campaign Message in Speeches") +
  geom_hline(yintercept = mean(biden_speech_time$count), linetype = "dashed", color = "gray") +
  geom_vline(xintercept = as.Date("2020-11-08"), linetype = "dashed", color = "#2096f3") +
  blog_theme

ggsave("biden_speech_time.png", width = 8, height = 6)


### TWITTER ANALYSIS -----------------------------------

# calculate counts of words
biden_tweet <- biden_tweet_df %>%
  select(created_at, text, favorite_count, retweet_count) %>%
  mutate(date = as.Date(created_at, "%m/%d/%Y")) %>%
  mutate(count = str_count(text, paste(words, collapse = '|'))) %>%
  group_by(date) %>%
  mutate(avg_count = mean(count))

# visualize
ggplot(biden_tweet, aes(x = date, y = count)) +
  geom_point(color = "#2096f3") +
  geom_smooth(aes(y = avg_count), method = "loess", se = FALSE, color = "#595959") +
  xlab("") +
  ylab("Frequency") +
  ggtitle("Biden Campaign Message in Tweets") +
  geom_hline(yintercept = mean(biden_tweet$count), linetype = "dashed", color = "gray") +
  geom_vline(xintercept = as.Date("2020-11-08"), linetype = "dashed", color = "#2096f3") +
  blog_theme

ggsave("biden_tweet_time.png", width = 10, height = 6)

