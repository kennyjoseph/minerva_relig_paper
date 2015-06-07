library(readxl)
library(dplyr)
library(reshape)
library(lubridate)
library(tidyr)
library(boot)
library(Scales)
####################
##########GET THE DATA

theme_set(theme_bw(16))
load("result_data/final_data_w_mu_v.rdata")

eta_samples_list <- list() 
for(f in Sys.glob("result_data/*_eta_samples.rdata")){ 
  load(f)
  eta_samples_list <- c(eta_samples_list,per_cat_eta_samples_list) 
}

results <- data.frame()
for(f in Sys.glob("result_data/*_results.rdata")){
  load(f)
  results <- rbind(results,df)
}
results <- data.table(results)
results <- results[paste(type,country,category,sep="_") %in% names(eta_samples_list)]
eta_res <- results[,as.list(apply(eta_samples_list[[paste(type,country,category,sep="_")]],1,mean)),by=c("type","country","category","a","sigma","iter")]

load("all_stuff.rdata")
load("result_data/news_data.rdata")
time_df <- data.frame(variable=paste0("V",1:23), 
                      date=sort(ALL_NEWS_TIMES[ALL_NEWS_TIMES %in% ALL_TIMES]))
eta_res <- melt(eta_res, id.vars=c("type","country","category","a","sigma"))
eta_res <- merge(eta_res, time_df,by="variable")
eta_res$variable <- NULL


eta_res <- data.table(eta_res )
#not enough Twitter data for political, morocco/oman/algeria
eta_res <- eta_res[category != "political"]
eta_res <- eta_res[! country %in% c("morocco","oman","algeria")]

eta_res$country <- factor(eta_res$country,
                          levels=c("tunisia","egypt","libya","yemen","syria",
                                   "jordan","lebanon","bahrain",
                                   "saudi_arabia","kuwait","uae",
                                    "iran","iraq"),
                          labels=c("Tunisia","Egypt","Libya","Yemen","Syria",
                                   "Jordan","Lebanon", "Bahrain",
                                   "Saudi Arabia","Kuwait","UAE",
                                   "Iran","Iraq"))
eta_res$category <- factor(eta_res$category,
                           levels=c("adaptation","youth","profession",
                                    "nationality","tribe","terrorist_org",
                                    "protest","violence","war","stop4",
                                    "stop5","stop6"),
                           labels=c("Adaptation","Youth","Professions",
                                    "Nationalities","Ethnic Groups",
                                    "Terrorist Orgs",
                                    "Protest","Violence","War","Stopwords 1",
                                    "Stopwords 2","Stopwords 3"))

eta_res <- eta_res[category != "Professions"]




### Confirm sigma for news vs twitter
p3 <- ggplot(unique(eta_res[,c("type","country","category","sigma"),with=F]), aes(sigma,fill=type)) + geom_density(alpha=.6) + xlab(expression(sigma^2)) + ylab("density") + guides(fill = guide_legend(title="Medium",override.aes = list(colour = NULL)))
ggsave("../arab_spring_paper/imgs/diff_sigma.pdf",p3,width=8,height=3.5)


##############################

lagged_vals <- eta_res[,list(date=arrange(.SD,date)$date[2:nrow(.SD)], value=diff(arrange(.SD,date)$value),sigma=sigma[2:nrow(.SD)]),by=c("category","country","type"),]

##take a look at the correlations.
lv <- lagged_vals
lv$sigma <- NULL
spread(lv,type,value)
lv <- spread(lv,type,value)
lv$cat_is_stop <- with(lv,category=="Stopwords 1"|
                         category=="Stopwords 2"|
                         category=="Stopwords 3")
ggplot(lv, aes(NEWS,TWITTER)) + geom_point() + facet_wrap(~cat_is_stop)



f <- function(d, i){
  return(cor(d[i,]$TWITTER, d[i,]$NEWS))
}
boot_approx <- function(data,times_to_run=10000){
  boot_out <- boot(data, f, R=times_to_run)
  return(c(boot_out$t0,boot.ci(boot_out,type="basic",conf=.99)$basic[4:5]))
}

##test for independence -> rel is approx linear, so pearson is fine
boot_approx(lv[cat_is_stop == F])
##some stop word correlation but pretty slight
boot_approx(lv[cat_is_stop==T ])

lv$corr_cat <- lv$category
lv[cat_is_stop == T]$corr_cat <- "stop"
d2 <- lv[,as.list(boot_approx(.SD)),by="corr_cat"]
d2[corr_cat =="stop"]$corr_cat <- "All Stopword Categories"

p4 <- ggplot(d2, aes(reorder(corr_cat,-V1),V1, ymin=V2,ymax=V3,color=corr_cat!="All Stopword Categories" )) + geom_pointrange(size=1) + coord_flip() + geom_hline(y=0,color='red') + ylab("Correlation") + xlab("Category") + theme(legend.position="none")
ggsave("../arab_spring_paper/imgs/cat_corr.pdf",p4,width=8,height=4.5)

library(cocor)
##check difference between stopowrds, terrorist is sig.
cor1 <- with(lv[corr_cat == "Professions"], cor(NEWS,TWITTER))
cor2 <- with(lv[corr_cat == "stop"], cor(NEWS,TWITTER))
cocor.indep.groups(cor1,cor2,
                   nrow(lv[corr_cat == "Professions"]), 
                   nrow(lv[corr_cat == "stop"]), 
                   data.name=c("group1", "group2"),
                   var.labels=c("news","twitter","news","twitter"))

d <- lv[cat_is_stop==F,as.list(boot_approx(.SD)),by="country"]
p5 <- ggplot(d, aes(reorder(country,-V1),V1, ymin=V2,ymax=V3 )) + geom_pointrange(size=1) + coord_flip() + geom_hline(y=0,color='red') + ylab("Correlation") + xlab("Country")
ggsave("../arab_spring_paper/imgs/country_corr.pdf",p5,width=8,height=4.5)


##########################################
### SURPRISING EVENTS
xtable <- function(x, ...) {
  for (i in which(sapply(x, function(y) !all(is.na(match(c("POSIXt","Date"),class(y))))))) x[[i]] <- as.character(x[[i]])
  xtable::xtable(x, ...)
}

xtable(arrange(lagged_vals[!category %in% c("Stopwords 1","Stopwords 2","Stopwords 3")],-abs(value)/sigma)[1:10])




##############################
## INTRO/TWITTER V NEWS
by_cat <- eta_res[!(category %in% c("Stopwords 1","Stopwords 2","Stopwords 3")),mean(value),by=c("type","country","category")]
by_date <- eta_res[!(category %in% c("Stopwords 1","Stopwords 2","Stopwords 3")),mean(value),by=c("type","date","category")]

theme_set(theme_bw(22))
p1 <- ggplot(by_cat[type=="NEWS"], 
             aes(country,category,fill=V1)) 
p1 <- p1 + geom_tile() + facet_wrap(~type)
p1 <- p1 + theme(axis.text.x=element_text(angle=45,hjust=1))
p1 <- p1 + ylab("") + xlab("") 
p1 <- p1 + guides(fill=guide_colourbar(title=expression(bar(eta[wrt]))))
p1 <- p1 + scale_fill_gradientn(
  colours=c("red","white","steelblue"),
  rescaler = function(x, ...) x,values=c(min(by_cat[type=="NEWS"]$V1-.1)
                                         ,0,max(by_cat[type=="NEWS"]$V1)+.1),
  oob = identity,name="value") 
ggsave("../arab_spring_paper/imgs/activity_topics_news.pdf",p1,
       width=8,height=5.5)
ggsave("../arab_spring_paper/imgs/activity_topics_twitter.pdf",
       p1 %+% by_cat[type=="TWITTER" ] + 
         scale_fill_gradientn(colours=c("red","white","steelblue"),
                              rescaler = function(x, ...) x,
                              values=c(min(by_cat[type=="TWITTER"]$V1-.1),
                                       0,max(by_cat[type=="TWITTER"]$V1)+.1),
                              oob = identity,name="value"), 
       width=8,height=5.5)


p_lib <- ggplot(eta_res[category %in% c("War","Protest") & country=="Libya"], aes(date,value,color=category)) + geom_point(size=3) + geom_smooth(size=3,alpha=.3) + facet_wrap(~type,scales="free_y") + geom_hline(y=0) + geom_vline(x=as.numeric(ymd("2011-08-01")))  + geom_vline(x=as.numeric(ymd("2012-10-01"))) + theme(axis.text.x=element_text(angle=45,hjust=1),legend.position="bottom") + scale_x_datetime(breaks="3 months")  + ylab(expression(eta[wrt])) + scale_colour_discrete("Theme")

ggsave("../arab_spring_paper/imgs/lib_protest_viol.pdf",w=8,h=5)


##check agg countries
news <- spread(eta_res[!(category %in% c("Stopwords 1","Stopwords 2","Stopwords 3")) & type=="NEWS",mean(value),by=c("type","country","category")],category,V1)
twitter <- spread(eta_res[!(category %in% c("Stopwords 1","Stopwords 2","Stopwords 3")) & type=="TWITTER",mean(value),by=c("type","country","category")],category,V1)
comb <- merge(news,twitter,by="country")
comb$type.x <- NULL
comb$type.y <- NULL

dist_mat <-as.matrix(comb[,2:ncol(comb),with=F])
row.names(dist_mat) <- comb$country

hc <- hclust(dist(dist_mat))
library(ggdendro)
p2 <- ggdendrogram(hc)
ggsave("../arab_spring_paper/imgs/country_dendro.pdf",p2,width=6,height=4)



matt_plot <-  ggplot(eta_res[category %in% c("Protest") & country%in% c("Libya","Bahrain","Tunisia","Yemen","Syria","Egypt")], aes(date,value,color=type)) + geom_point(size=3) + geom_smooth(size=3,alpha=.3) + facet_wrap(~country,scales="free_y") + geom_hline(y=0) +  theme(axis.text.x=element_text(angle=45,hjust=1),legend.position="bottom") + scale_x_datetime(breaks="6 months")  + ylab(expression(eta[wrt])) + scale_colour_discrete("Theme")

ggsave("../arab_spring_paper/imgs/cottle.pdf",matt_plot,width=8,height=7)

nat_v_eth <- eta_res[category %in% c("Ethnic Groups","Nationalities") & country %in% c("Egypt","Libya","Syria","Yemen","Bahrain","Tunisia","Iraq")]
diff_eth <- nat_v_eth[,.SD[category=="Nationalities"]$value-.SD[category=="Ethnic Groups"]$value,by=c("type","country","date")]

ggplot(diff_eth, aes(date,V1,color=type,fill=type)) + geom_line() + geom_point() + geom_hline(y=0) + facet_wrap(~country) + theme(axis.text.x=element_text(angle=45,hjust=1)) + geom_smooth()
