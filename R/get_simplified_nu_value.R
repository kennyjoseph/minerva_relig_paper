DATA_DIR <- "raw_data"

library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(bit64)

remove_countries <- c("sudan","qatar")

inv_logis <- function(x){return(log(x/(1-x)))}

load("result_data/news_data.rdata")
load("result_data/full_news_data_all.rdata")
load("result_data/full_twitter_data.rdata")
load("result_data/twitter_data.rdata")

ALL_CATEGORIES <- intersect(ALL_TWITTER_CATEGORIES,ALL_NEWS_CATEGORIES)
ALL_TIMES <- intersect(ALL_TWITTER_TIMES,ALL_NEWS_TIMES)
ALL_COUNTRIES <- ALL_TWITTER_COUNTRIES

##note this isn't truly the total number of users, its just the total that
##used any of the categories
##determined by casos_total_n_users.py
TOTAL_N_TWITTER_USERS <- 2841825

N_NEWS_ARTICLES <- length(unique(ctm[ymd_date %in% ALL_TIMES]$unique_id))

v_w_data.twitter <- term_count[term_count$ymd_date %in% ALL_TIMES,
                               length(unique(uid))/TOTAL_N_TWITTER_USERS,by=c("category")]
v_w_data.twitter$simp_v_w <- inv_logis(v_w_data.twitter$V1)
v_w_data.twitter$V1 <- NULL
twitter_data <- merge(twitter_data, v_w_data.twitter, by="category")
twitter_data$type <- "TWITTER"

v_w_data.news <- news_df[ymd_date %in% ALL_TIMES,
                         length(unique(unique_id))/N_NEWS_ARTICLES,
                         by=c("category")]
v_w_data.news$simp_v_w <- inv_logis(v_w_data.news$V1)
v_w_data.news$V1 <- NULL
news_data <- merge(news_data, v_w_data.news, by="category")
news_data$type <- "NEWS"


final_data <- rbind(news_data,twitter_data)
final_data <- final_data[datetime%in% ALL_TIMES & category %in% ALL_CATEGORIES]
save(final_data,file="result_data/final_data.rdata")
save(ALL_CATEGORIES,ALL_TIMES,ALL_COUNTRIES,TOTAL_N_TWITTER_USERS,N_NEWS_ARTICLES,file="all_stuff.rdata")



