##all this data was determined via casos_get_twitter_term_nets.py
DATA_DIR <- "raw_data"

library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(bit64)

remove_countries <- c("sudan","qatar")
TERMS_TO_REMOVE <- c("im","saudi","pkk","dr")

inv_logis <- function(x){return(log(x/(1-x)))}

concept_to_category <- fread(file.path(DATA_DIR,"goldstone_to_category.tsv"),header=F)
setnames(concept_to_category,c("concept","category"))
concept_to_category$concept <- tolower(concept_to_category$concept)

##########################################
##############TWITTER DATA###############
##########################################

###USER COUNT STUFF
#the number of users per country per month that tweeted at least once.
##loc_type is how we determined country (full == both, geo, text)
user_count <- fread(file.path(DATA_DIR,"twitter_usercounts.tsv"))
user_count <- user_count[,sum(V3),by=c("V1","V2","V4")]
setnames(user_count, c("country","date","loc_type","count"))
user_count <- user_count[!country %in% remove_countries]
user_count$datetime <- ymd(paste(user_count$date,"-01",sep=""))
user_count$date <- NULL

####TERM COUNT STUFF
term_count <- fread(file.path(DATA_DIR, "twitter_termcounts.tsv"))
setnames(term_count, c("tw_id","datetime","uid","concept","raw","country","loc_type"))

###see if anything weird showed up... definitely "im", "pkk" and "saudi", which is used to capture tweets. 
#Get rid of them
ck_bad_raw <- term_count[,length(unique(uid)),by=c("raw","concept")]%>% arrange(-V1)
term_count <- term_count[!raw %in% TERMS_TO_REMOVE,]
term_count$datetime <- ymd(term_count$datetime)

####***only using 2011 -> beginning of 2013 for sampling reasons
term_count <- term_count[datetime > ymd("2011-01-31") & datetime < ymd("2013-01-01"),]
term_count$md <- paste(year(term_count$datetime),month(term_count$datetime),sep="-")
term_count$loc_type <- tolower(term_count$loc_type)
term_count$concept <- tolower(term_count$concept)
term_count <- merge(term_count,concept_to_category,by="concept",all.x=T)

##See if there are any concepts we can just ignore because they are so sparse
ck_lame_cat <- term_count[,length(unique(uid)),by=c("category")] %>% arrange(-V1)
term_count <- term_count[!is.na(category)] #things we clean post-hoc from concept -> category list
##Lets toss the low-lying categories - < 5000 users out of 800K is pretty uninteresting, at least for right now
term_count <- term_count[category %in% ck_lame_cat[V1 > 10000,]$category]

##number of users per category per country per month - in other words, c_r,w,t
user_per_cat <- term_count[,length(unique(uid)),by=c("category","country","md")]
user_per_cat$loc_type <- "full"
user_per_cat$datetime <- ymd(paste(user_per_cat$md,"-01",sep=""))
setnames(user_per_cat,"V1","c_wrt")
user_per_cat$md <- NULL

###MERGE
setnames(user_count,"count","s_wrt")
full_data <- merge(user_per_cat,user_count,by=c("country","loc_type","datetime"))
##only use counts from the full dataset
full_data <- full_data[loc_type=="full"]
full_data$loc_type <- NULL

ALL_TWITTER_CATEGORIES <- unique(full_data$category)
ALL_TWITTER_COUNTRIES <- unique(full_data$country)
ALL_TWITTER_TIMES <- unique(full_data$datetime)


full_factorial_data <- data.table(expand.grid(ALL_TWITTER_COUNTRIES,
                                              ALL_TWITTER_TIMES,
                                              ALL_TWITTER_CATEGORIES,
                                              stringsAsFactors=F))

setnames(full_factorial_data, names(full_data)[1:3])
full_factorial_data <- merge(full_factorial_data,full_data,by=c("country","datetime","category"),all.x=T)
full_factorial_data[is.na(c_wrt)]$c_wrt <- 0
full_factorial_data[is.na(s_wrt)]$s_wrt <- 0
twitter_data <- full_factorial_data

term_count$ymd_date <- ymd(paste(term_count$md,"-01",sep=""))

save(twitter_data, ALL_TWITTER_COUNTRIES,ALL_TWITTER_TIMES,ALL_TWITTER_CATEGORIES, file="result_data/twitter_data.rdata")
save(term_count, file="result_data/full_twitter_data.rdata")

