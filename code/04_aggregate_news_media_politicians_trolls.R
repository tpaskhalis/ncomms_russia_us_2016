#!/usr/bin/env Rscript
# coding: utf-8

#===============================================================================
#  Gregory Eady, 2020-09-16
#  Tom Paskhalis, 2020-10-12
#
#  Script for graphing the time series of tweets seen from news media,
#  politicians, and trolls
#
#===============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(cowplot)
library(lmtest)
library(MASS)
library(stargazer)

# setwd("~/GitHub/electoral_integrity/")
source("./code/helper_functions.R")
source("./code/ggplot_theme.R")

# YouGov respondents' friends
friends_2016 <- readr::read_csv(
  "./data/yougov_2016_processed/friends_2016.csv",
  col_types = readr::cols(.default = readr::col_character())
)

rogue_users <- readr::read_csv(
  "./data/twitter_elections/rogue_users.csv",
  col_types = readr::cols(.default = readr::col_character())
)
russian_user_ids <- unique(
  rogue_users$userid[rogue_users$country %in% c("ira", "russia")]
)

fls <- list.files("./data/yougov_2016_processed/selected_tweets/", full.names = TRUE)

troll_files <- grep("rogue", fls, value = TRUE)
politician_files <- grep("politician", fls, value = TRUE)
news_media_files <- grep("news", fls, value = TRUE)

# Exposures to troll tweets (load data and bind into a single data.frame)
T <- do.call(rbind, lapply(troll_files,
                           function(x) read_csv(x, col_type = c("user_id" = "c",
                                                                "text_user_id" = "c",
                                                                "quoted_user_id" = "c",
                                                                "tweet_id" = "c"))))


# Exposures to politician tweets (load data and bind into a single data.frame)
P <- do.call(rbind, lapply(politician_files,
                           function(x) read_csv(x, col_type = c("user_id" = "c",
                                                                "text_user_id" = "c",
                                                                "quoted_user_id" = "c",
                                                                "tweet_id" = "c"))))


# Exposures to news media tweets (load data and bind into a single data.frame)
N <- do.call(rbind, lapply(news_media_files,
                           function(x) read_csv(x, col_type = c("user_id" = "c",
                                                                "text_user_id" = "c",
                                                                "quoted_user_id" = "c",
                                                                "tweet_id" = "c"))))

T$tweet_created_at <- as.Date(T$tweet_created_at, format = "%a %b %d %H:%M:%S %z %Y")
P$tweet_created_at <- as.Date(P$tweet_created_at, format = "%a %b %d %H:%M:%S %z %Y")
N$tweet_created_at <- as.Date(N$tweet_created_at, format = "%a %b %d %H:%M:%S %z %Y")


# Merge in the users that YouGov respondents follow into the tweet dataset
# This will create duplicate rows for each tweet if more than one respondent
# follows the user who sent a tweet
T <- merge(T, friends_2016, by.x = "user_id", by.y = "id", all.y = TRUE); gc()
P <- merge(P, friends_2016, by.x = "user_id", by.y = "id", all.y = TRUE); gc()
N <- merge(N, friends_2016, by.x = "user_id", by.y = "id", all.y = TRUE); gc()

G_T <- T %>%
  filter(user_id %in% russian_user_ids |
           quoted_user_id %in% russian_user_ids |
           text_user_id %in% russian_user_ids) %>%
  group_by(smapp_original_user_id, tweet_created_at) %>%
  summarize(count = n())

G_P <- P %>%
  group_by(smapp_original_user_id, tweet_created_at) %>%
  summarize(count = n())

G_N <- N %>%
  group_by(smapp_original_user_id, tweet_created_at) %>%
  summarize(count = n())


# Filter the data.frame for all dates of interest
start_date <- ymd("2016-09-28") # First day of October
end_date <- ymd("2016-11-08") # Election day


# Create a data.frame that will include each unique YouGov ID for as many
# dates betweet start_date and end_date
G <- data.frame(smapp_original_user_id = rep(unique(T$smapp_original_user_id),
                                             each = length(seq(start_date, end_date, by = 1))),
                date = rep(seq(start_date, end_date, by = 1), length(unique(T$smapp_original_user_id))))
G$week <- floor_date(G$date, "weeks", week_start = 3)



# Merge in the daily counts of tweets received from trolls, politicians,
# and news media into the day-user_id data.frame.
# Any NA indicates that no tweets were match for a given YouGov respondent to
# a given day. These are thus assigned a count of zero on that day
G_T_by_day <- tibble(merge(G, G_T,
                           by.x = c("smapp_original_user_id", "date"),
                           by.y = c("smapp_original_user_id", "tweet_created_at"),
                           all.x = TRUE))
G_T_by_day$count[is.na(G_T_by_day$count)] <- 0

G_T_by_week <- G_T_by_day %>%
               group_by(smapp_original_user_id, week) %>%
               summarize(count = sum(count))


G_P_by_day <- tibble(merge(G, G_P,
                           by.x = c("smapp_original_user_id", "date"),
                           by.y = c("smapp_original_user_id", "tweet_created_at"),
                           all.x = TRUE))
G_P_by_day$count[is.na(G_P_by_day$count)] <- 0

G_P_by_week <- G_P_by_day %>%
               group_by(smapp_original_user_id, week) %>%
               summarize(count = sum(count))


G_N_by_day <- tibble(merge(G, G_N,
                           by.x = c("smapp_original_user_id", "date"),
                           by.y = c("smapp_original_user_id", "tweet_created_at"),
                           all.x = TRUE))
G_N_by_day$count[is.na(G_N_by_day$count)] <- 0

G_N_by_week <- G_N_by_day %>%
               group_by(smapp_original_user_id, week) %>%
               summarize(count = sum(count))


# Calculate the per day mean and median number of posts seen from trolls,
# politicians, and news media
G_Trolls_day <- G_T_by_day %>%
  filter(date >= ymd("2016-10-01")) %>%
  group_by(date) %>%
  summarize(mean_exposure = mean(count),
            median_exposure = median(count)) %>%
  mutate(type = "IRA accounts")

G_Trolls_week <- G_T_by_week %>%
  group_by(week) %>%
  summarize(mean_exposure = mean(count),
            median_exposure = median(count)) %>%
  mutate(type = "IRA accounts")


G_Politicians_day <- G_P_by_day %>%
  filter(date >= ymd("2016-10-01")) %>%
  group_by(date) %>%
  summarize(mean_exposure = mean(count),
            median_exposure = median(count)) %>%
  mutate(type = "Politicians")

G_Politicians_week <- G_P_by_week %>%
  group_by(week) %>%
  summarize(mean_exposure = mean(count),
            median_exposure = median(count)) %>%
  mutate(type = "Politicians")


G_News_Media_day <- G_N_by_day %>%
  filter(date >= ymd("2016-10-01")) %>%
  group_by(date) %>%
  summarize(mean_exposure = mean(count),
            median_exposure = median(count)) %>%
  mutate(type = "News media")

G_News_Media_week <- G_N_by_week %>%
  group_by(week) %>%
  summarize(mean_exposure = mean(count),
            median_exposure = median(count)) %>%
  mutate(type = "News media")





# Bind all three of the data.frame into one
G_day <- rbind(G_Trolls_day, G_Politicians_day, G_News_Media_day)
G_week <- rbind(G_Trolls_week, G_Politicians_week, G_News_Media_week)

# Bit of simple recoding for graphing
G_day <- G_day %>%
  pivot_longer(cols = c("mean_exposure", "median_exposure"),
               names_to = "average") %>%
  mutate(average = recode(average, mean_exposure = "Mean",
                                   median_exposure = "Median"))

G_week <- G_week %>%
  pivot_longer(cols = c("mean_exposure", "median_exposure"),
               names_to = "average") %>%
  mutate(average = recode(average, mean_exposure = "Mean",
                                   median_exposure = "Median"))

readr::write_csv(G_day, "./results/04_aggregate_news_media_politicians_trolls/G_day.csv")
readr::write_csv(G_week, "./results/04_aggregate_news_media_politicians_trolls/G_week.csv")
