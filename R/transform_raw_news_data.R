DATA_DIR <- "raw_data"

library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(bit64)
library(stringr)

remove_countries <- c("sudan","qatar")
TERMS_TO_REMOVE <- c("im","saudi","pkk","dr")

inv_logis <- function(x){return(log(x/(1-x)))}

concept_to_category <- fread(file.path(DATA_DIR,"goldstone_to_category.tsv"),header=F)
setnames(concept_to_category,c("concept","category"))
concept_to_category$concept <- tolower(concept_to_category$concept)



ctm <- fread(file.path(DATA_DIR,"country_to_articles.csv"))
news_df <- fread(file.path(DATA_DIR,"news_term_counts.tsv"))
setnames(news_df, c("date","articleNumber","concept","raw"))

news_df <- merge(news_df, ctm, by=c("date","articleNumber"),allow.cartesian=T)

##ck bad; looks okay
news_df <- news_df[! (raw %in% TERMS_TO_REMOVE)]
ck_bad_raw <- news_df[,length(unique(paste0(date,articleNumber))),by=c("raw","concept")]%>% arrange(-V1)

##cleaning more
news_df$datetime <- ymd(news_df$date)
news_df$concept <- tolower(news_df$concept)
news_df <- merge(news_df,concept_to_category,by="concept",all.x=T)
news_df <- news_df[!is.na(category)] #things we clean post-hoc from concept -> category list

news_df$unique_id <- with(news_df, paste0(articleNumber,date))
##See if there are any concepts we can just ignore because they are so sparse
ck_lame_cat <- news_df[,length(unique(unique_id)),by=c("category")] %>% arrange(-V1)
##Lets toss the low-lying categories - < 10000
news_df <- news_df[category %in% ck_lame_cat[V1 > 10000,]$category]

news_df$country[news_df$country =="saudi arabia"] <- "saudi_arabia"
news_df$country[news_df$country =="united arab emirates"] <- "uae"

news_df$md <- paste(year(news_df$datetime),month(news_df$datetime),sep="-")

##number of users per category per country per month - in other words, c_r,w,t
articles_per_cat <- news_df[,length(unique(unique_id)),by=c("category","country","md")]
setnames(articles_per_cat,"V1","c_wrt")

ctm$md <- paste(year(ctm$date),month(ctm$date),sep="-")
articles_per_month <- ctm[,length(articleNumber),by=c("md","country")]
articles_per_month$country[articles_per_month$country =="saudi arabia"] <- "saudi_arabia"
articles_per_month$country[articles_per_month$country =="united arab emirates"] <- "uae"
setnames(articles_per_month,"V1","s_wrt")

###Final clean and merge
full_news_data <- merge(articles_per_cat,articles_per_month,by=c("country","md"))
full_news_data$datetime <- ymd(paste(full_news_data$md,"-01",sep=""))
full_news_data$md <- NULL
##only use counts from the full dataset
ALL_NEWS_CATEGORIES <- unique(full_news_data$category)
ALL_NEWS_COUNTRIES <- unique(full_news_data$country)
ALL_NEWS_TIMES <- unique(full_news_data$datetime)
full_factorial_data <- data.table(expand.grid(country=ALL_NEWS_COUNTRIES,
                                              datetime=ALL_NEWS_TIMES,
                                              category=ALL_NEWS_CATEGORIES,
                                              stringsAsFactors=F))
full_factorial_data <- merge(full_factorial_data,full_news_data,by=c("country","datetime","category"),all.x=T)
full_factorial_data[is.na(c_wrt)]$c_wrt <- 0
full_factorial_data[is.na(s_wrt)]$s_wrt <- 0
news_data <- full_factorial_data

ctm$ymd_date <- ymd(paste(ctm$md,"-01",sep=""))
news_df$ymd_date <- ymd(paste(news_df$md,"-01",sep=""))

ctm$unique_id <- paste0(ctm$articleNumber,ctm$date)


save(news_data,ALL_NEWS_CATEGORIES,ALL_NEWS_COUNTRIES,ALL_NEWS_TIMES, file="result_data/news_data.rdata")
save(news_df,ctm, file="result_data/full_news_data_all.rdata")