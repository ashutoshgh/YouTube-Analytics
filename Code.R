rm(list=ls())

setwd("D:/Social Media Analytics/YouTube/")

Sys.which("make")

#devtools::install_github("soodoku/tuber", build_vignettes = TRUE)

library('tuber')
library(magrittr) # Pipes %>%, %T>% and equals(), extract().
library(tidyverse) # all tidyverse packages
library(purrr) # package for iterating/extracting data
library(lubridate)
library(stringi)
library(wordcloud2)
library(gridExtra)
library(radarchart)
library(chartjs)
library(tidyverse)
library(rtweet)
library(qdapRegex)
library(plyr)
library(ggplot2)
library(SnowballC)
library(tm)
library(wordcloud)
library(syuzhet)
library(stringr)
library(lubridate)
library(plotly)
library(RColorBrewer)
library(wordcloud2)
library(rpart)

#install.packages('gridExtra')

yt_oauth(app_id="xxxx", 
         app_secret="xxx", token = "")

#get_stats(video_id = "N708P-A45D0")

mkbhd_stats <- get_channel_stats("UCBJycsmduvYEL83R_U4JriQ")


# Enter playlist id here
p <- "PLBsP89CPrMeNm71T5gYC6jebm9vPbLBiP"

# REVIEWS PLAYLIST
mkbhd_reviews = get_playlist_items(filter = c(playlist_id = "PLBsP89CPrMeNm71T5gYC6jebm9vPbLBiP"), max_results = 200)
View(mkbhd_reviews)
write.csv(mkbhd_reviews, 'review_dates.csv')
mkbhd_reviews <- read.csv('review_dates.csv')

mkbhd_reviews1 <- mkbhd_reviews[1:96,]
View(mkbhd_reviews1)

comments = lapply(as.character(mkbhd_reviews$contentDetails.videoId), function(x){
  get_comment_threads(c(video_id = x), max_results = 100)
})
View(comments)
review_comments <- do.call("rbind", comments)
View(mkbhd_comments1)
write.csv(mkbhd_comments1, 'mkbhd_comments1.csv')

mkbhd_reviews2 <- mkbhd_reviews[97:191,]
View(mkbhd_reviews2)

comments = lapply(as.character(mkbhd_reviews2$contentDetails.videoId), function(x){
  get_comment_threads(c(video_id = x), max_results = 150)
})
View(comments)
mkbhd_comments2 <- do.call("rbind", comments)
View(mkbhd_comments2)
write.csv(mkbhd_comments2, 'mkbhd_comments2.csv')

reviews_comments <- read.csv('comments.csv')
View(reviews_comments)
tab1 <- table(reviews_comments$authorDisplayName)
View(tab1)

# Video ids
vid_ids <- as.vector(mkbhd_reviews$contentDetails.videoId)
# Function to scrape stats for all vids
get_all_stats <- function(id) {get_stats(id)} 
# Get stats and convert results to data frame 
res <- lapply(vid_ids, get_all_stats)
mkbhd_reviews_df <- do.call(rbind, lapply(res, data.frame))
head(mkbhd_reviews_df)
view(mkbhd_reviews_df)
write.csv(mkbhd_reviews_df,'reviews.csv')

reviews_stats <- read.csv('reviews.csv')
View(reviews_stats)



# = videostats Plot = #
p1 = ggplot(data = reviews_stats[-1, ]) + geom_point(aes(x = viewCount, y = likeCount))
p2 = ggplot(data = reviews_stats[-1, ]) + geom_point(aes(x = viewCount, y = dislikeCount))
p3 = ggplot(data = reviews_stats[-1, ]) + geom_point(aes(x = viewCount, y = commentCount))
grid.arrange(p1, p2, p3, ncol = 2)

reviews_comments$publishedAt <- as.Date(reviews_comments$publishedAt)
comments_ts <- reviews_comments$publishedAt
View(comments_ts)

comments_ts = tibble(date = as.Date(Reduce(c, comments_ts))) %>%
  group_by(date) %>% count()

ggplot(data = comments_ts) + geom_line(aes(x = date, y = n)) +
  geom_smooth(aes(x = date, y = n), se = FALSE) + ggtitle("Comments by day")+
  geom_vline(xintercept = as.numeric(as.Date("2020-09-25")), linetype = 2,color = "red")+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-28")), linetype = 2,color = "red")


#remotes::install_github("r-hub/rhub")


# EXPLAINED PLAYLIST
# playlist id = PL6566A39B68523E18
explained <- get_playlist_items(filter = c(playlist_id = "PL6566A39B68523E18"), max_results = 150)
View(explained)
write.csv(explained, 'exp_dates.csv')

comments_exp = lapply(as.character(explained$contentDetails.videoId), function(x){
  get_comment_threads(c(video_id = x), max_results = 150)
})
View(comments_exp)
comments_exp <- do.call("rbind", comments_exp)
View(comments_exp)

write.csv(comments_exp, 'bexp_comments.csv')
write.csv(comments_exp, 'comments_exp.csv')
comments_exp <- read.csv('exp_comments.csv')
View(comments_exp)
tab <- table(comments_exp$authorDisplayName)
View(tab)


# Video ids
vid_ids_exp <- as.vector(explained$contentDetails.videoId)
# Function to scrape stats for all vids
get_all_stats <- function(id) {get_stats(id)} 
# Get stats and convert results to data frame 
res_exp <- lapply(vid_ids_exp, get_all_stats)

explained_reviews <- do.call(rbind, lapply(res_exp, data.frame))
head(explained_reviews)
View(explained_reviews)
write.csv(explained_reviews,'exp_reviews.csv')

exp_reviews_stats <- read.csv('exp_reviews.csv')
View(exp_reviews_stats)

# = videostats Plot = #
p1 = ggplot(data = exp_reviews_stats[-1, ]) + geom_point(aes(x = viewCount, y = likeCount))
p2 = ggplot(data = exp_reviews_stats[-1, ]) + geom_point(aes(x = viewCount, y = dislikeCount))
p3 = ggplot(data = exp_reviews_stats[-1, ]) + geom_point(aes(x = viewCount, y = commentCount))
grid.arrange(p1, p2, p3, ncol = 2)

comments_exp$publishedAt <- as.Date(comments_exp$publishedAt)
comments_ts_exp <- comments_exp$publishedAt
View(comments_ts_exp)

comments_ts_exp = tibble(date = as.Date(Reduce(c, comments_ts_exp))) %>%
  group_by(date) %>% count()
ggplot(data = comments_ts_exp) + geom_line(aes(x = date, y = n)) +
  geom_smooth(aes(x = date, y = n), se = FALSE) + ggtitle("Comments by day")+
  geom_vline(xintercept = as.numeric(as.Date("2020-09-25")), linetype = 2,color = "red")+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-28")), linetype = 2,color = "red")




# Top 5 PLAYLIST
top_5 = get_playlist_items(filter = c(playlist_id = "PLBsP89CPrMeOA6Ypzvu4hfZigZIRBclY-"), max_results = 97)
View(top_5)
write_csv(top_5, 'top5_dates.csv')

comments = lapply(as.character(top_5$contentDetails.videoId), function(x){
  get_comment_threads(c(video_id = x), max_results = 150)
})
View(comments)
top5_comments <- do.call("rbind", comments)
View(top5_comments)
write_csv(top5_comments, 'btop5_comments.csv')
write.csv(top5_comments, 'top5_comments.csv')
top5_comments <- read.csv('top5_comments.csv')
View(top5_comments)
tabtop <- table(top5_comments$authorDisplayName)
View(tabtop)

# Video ids
vid_ids_top <- as.vector(top_5$contentDetails.videoId)
# Function to scrape stats for all vids
get_all_stats <- function(id) {get_stats(id)} 
# Get stats and convert results to data frame 
res <- lapply(vid_ids_top, get_all_stats)
top5_reviews <- do.call(rbind, lapply(res, data.frame))
head(top5_reviews)
View(top5_reviews)
write.csv(top5_reviews,'top5_reviews.csv')

top5_reviews <- read.csv('top5_reviews.csv')
View(top5_reviews)

# = videostats Plot = #
p1 = ggplot(data = top5_reviews[-1, ]) + geom_point(aes(x = viewCount, y = likeCount))
p2 = ggplot(data = top5_reviews[-1, ]) + geom_point(aes(x = viewCount, y = dislikeCount))
p3 = ggplot(data = top5_reviews[-1, ]) + geom_point(aes(x = viewCount, y = commentCount))
grid.arrange(p1, p2, p3, ncol = 2)

top5_comments$publishedAt <- as.Date(top5_comments$publishedAt)
comments_ts_top5 <- top5_comments$publishedAt
View(comments_ts_top5)

comments_ts_top5 = tibble(date = as.Date(Reduce(c, comments_ts_top5))) %>%
  group_by(date) %>% count()

ggplot(data = comments_ts_top5) + geom_line(aes(x = date, y = n)) +
  geom_smooth(aes(x = date, y = n), se = FALSE) + ggtitle("Comments by day")+
  geom_vline(xintercept = as.numeric(as.Date("2020-09-25")), linetype = 2,color = "red")+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-28")), linetype = 2,color = "red")


#############################COMPARE DIFFERENT PLAYLISTS##########################

#Dope Tech

dope_tech <- get_playlist_items(filter = c(playlist_id = "PLBsP89CPrMePWBCMIp0naluIz67UwRX9B"))
View(dope_tech)
write_csv(dope_tech, 'dt_dates.csv')

comments = lapply(as.character(dope_tech$contentDetails.videoId), function(x){
  get_comment_threads(c(video_id = x), max_results = 150)
})
View(comments)
dt_comments <- do.call("rbind", comments)
View(dt_comments)
write_csv(dt_comments, 'bdt_comments.csv')
write.csv(dt_comments, 'dt_comments.csv')
dt_comments <- read.csv('dt_comments.csv')
View(dt_comments)
tabdt <- table(dt_comments$authorDisplayName)
View(tabdt)

# Video ids
vid_ids_dt <- as.vector(dope_tech$contentDetails.videoId)
# Function to scrape stats for all vids
get_all_stats <- function(id) {get_stats(id)} 
# Get stats and convert results to data frame 
res <- lapply(vid_ids_dt, get_all_stats)
dt_reviews <- do.call(rbind, lapply(res, data.frame))
head(dt_reviews)
View(dt_reviews)
write.csv(dt_reviews,'dt_reviews.csv')

dt_reviews <- read.csv('dt_reviews.csv')
View(dt_reviews)

# = videostats Plot = #
p1 = ggplot(data = dt_reviews[-1, ]) + geom_point(aes(x = viewCount, y = likeCount))
p2 = ggplot(data = dt_reviews[-1, ]) + geom_point(aes(x = viewCount, y = dislikeCount))
p3 = ggplot(data = dt_reviews[-1, ]) + geom_point(aes(x = viewCount, y = commentCount))
grid.arrange(p1, p2, p3, ncol = 2)

dt_comments$publishedAt <- as.Date(dt_comments$publishedAt)
comments_ts_dt <- dt_comments$publishedAt
View(comments_ts_dt)

comments_ts_dt = tibble(date = as.Date(Reduce(c, comments_ts_dt))) %>%
  group_by(date) %>% count()

ggplot(data = comments_ts_dt) + geom_line(aes(x = date, y = n)) +
  geom_smooth(aes(x = date, y = n), se = FALSE) + ggtitle("Comments by day")+
  geom_vline(xintercept = as.numeric(as.Date("2020-09-25")), linetype = 2,color = "red")+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-28")), linetype = 2,color = "red")



#####################################FROM HERE#################################

## + Find comments of all the videos in the channel
# + Sentiment analysis of entire channel
# + Word cloud of entire channel
# + Which videos are most liked
# + Word cloud
# + Which videos are most disliked
# + Word cloud
# + Top 10 active users for the entire channel
# + During which time of the day, channel uploads the video
# + Analysis on views based on published time for entire channel
# + Any other analysis which you think will be useful for your company.

# PUBLSIHED DATE DATASETS (USED TO FIND ACTIVE USERS AND WHEN THE VIDEOS WERE PUBLISHED)

setwd("D:/SCIT/SEM 3/Social Media Analytics/YouTube/")
rm(list=ls())

dt_dates <- read.csv('dt_dates.csv')
top5_dates <- read.csv('top5_dates.csv')
reviews_dates <- read.csv('review_dates.csv')
exp_dates <- read.csv('exp_dates.csv')
exp_dates <- exp_dates[-1]
reviews_dates <- reviews_dates[-1]
total_dates <- rbind(dt_dates, top5_dates, reviews_dates, exp_dates)
write.csv(total_dates, 'total_dates.csv')
View(reviews_dates)



# COMMENTS OF ALL THE PLAYLISTS 


dt_comments <- read.csv('dt_comments.csv')
dt_stats <- read.csv('dt_stats.csv')

dt_comments$textOriginal <- gsub("&amp", "", dt_comments$textOriginal)
dt_comments$textOriginal <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", dt_comments$textOriginal)
dt_comments$textOriginal <- gsub("@\\w+", "", dt_comments$textOriginal)
dt_comments$textOriginal <- gsub("[[:punct:]]", "", dt_comments$textOriginal)
dt_comments$textOriginal <- gsub("[[:digit:]]", "", dt_comments$textOriginal)
dt_comments$textOriginal <- gsub("http\\w+", "", dt_comments$textOriginal)
dt_comments$textOriginal <- gsub("[ \t]{2,}", "", dt_comments$textOriginal)
dt_comments$textOriginal <- gsub("^\\s+|\\s+$", "", dt_comments$textOriginal)
dt_comments$textOriginal <- iconv(dt_comments$textOriginal, "UTF-8", "ASCII", sub="")
#dt_comments$text <- gsub("dt_commentsid", "", dt_comments$text)

#Total number of unique tweets = 13846
dt_comments <- dt_comments %>% distinct(textOriginal, .keep_all= TRUE)
length(dt_comments$textOriginal)
View(dt_comments)

write.csv(dt_comments, 'dt_comments.csv')

#number of unique users who's tweets have been downloaded = 9601
uniqueusers<-length(unique(dt_comments$authorDisplayName))
uniqueusers

summary(dt_comments$is_quote)
View(dt_comments)

#number of people who liked the tweets in total = 41187
sum(dt_stats$likeCount)

#number of people who retweeted the tweets = 8360
sum(dt_stats$commentCount)

#Sentiment Analysis
emotions <- get_nrc_sentiment(dt_comments$textOriginal)
head(emotions)
View(emotions)

dt_comments_sentiment <- get_sentiment(dt_comments$textOriginal)

emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion type for tweets related to Dope Tech Playlist")
p


#create corpus
docs <- Corpus(VectorSource(dt_comments$textOriginal))

#text cleaning
docs = tm_map(docs, tolower)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeWords, c(stopwords("english")))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs, removeWords, c("dt_commentsid"))
#docs = tm_map(docs, stemDocument)

# create document term matrix
dtm = TermDocumentMatrix(docs)
m = as.matrix(dtm)
v = sort(rowSums(m),decreasing=TRUE)
word_frequency = data.frame(word = names(v),freq=v)
head(word_frequency, 10)
p = head(word_frequency, 300)

#word frequencies
barplot(word_frequency[1:10,]$freq, las = 2, names.arg = word_frequency[1:10,]$word,
        col ="purple", main ="Most frequent words",
        ylab = "Word frequencies")

#wordcloud
set.seed(69)

wordcloud2(data = p, color = 'random-light', backgroundColor = 'black')



# TOP 5 COMMENTS (PLAYLIST NAME)

top5_comments <- read.csv('top5_comments.csv')
top5_stats <- read.csv('top5_stats.csv')

top5_comments$textOriginal <- gsub("&amp", "", top5_comments$textOriginal)
top5_comments$textOriginal <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", top5_comments$textOriginal)
top5_comments$textOriginal <- gsub("@\\w+", "", top5_comments$textOriginal)
top5_comments$textOriginal <- gsub("[[:punct:]]", "", top5_comments$textOriginal)
top5_comments$textOriginal <- gsub("[[:digit:]]", "", top5_comments$textOriginal)
top5_comments$textOriginal <- gsub("http\\w+", "", top5_comments$textOriginal)
top5_comments$textOriginal <- gsub("[ \t]{2,}", "", top5_comments$textOriginal)
top5_comments$textOriginal <- gsub("^\\s+|\\s+$", "", top5_comments$textOriginal)
top5_comments$textOriginal <- iconv(top5_comments$textOriginal, "UTF-8", "ASCII", sub="")
#top5_comments$text <- gsub("top5_commentsid", "", dt_comments$text)

#Total number of unique tweets = 13846
top5_comments <- top5_comments %>% distinct(textOriginal, .keep_all= TRUE)
length(top5_comments$textOriginal)
View(top5_comments)

write.csv(top5_comments, 'top5_comments.csv')

#number of unique users who's tweets have been downloaded = 9601
uniqueusers<-length(unique(top5_comments$authorDisplayName))
uniqueusers

summary(top5_comments$is_quote)
View(top5_comments)

#number of people who liked the tweets in total = 41187
sum(top5_stats$likeCount)

#number of people who retweeted the tweets = 8360
sum(top5_stats$commentCount)

#Sentiment Analysis
emotions <- get_nrc_sentiment(top5_comments$textOriginal)
head(emotions)
View(emotions)

top5_comments_sentiment <- get_sentiment(top5_comments$textOriginal)

emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion type for tweets related to Top 5 Playlist")
p


#create corpus
docs <- Corpus(VectorSource(top5_comments$textOriginal))

#text cleaning
docs = tm_map(docs, tolower)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeWords, c(stopwords("english")))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs, removeWords, c("bhajpa"))
#docs = tm_map(docs, stemDocument)

# create document term matrix
dtm = TermDocumentMatrix(docs)
m = as.matrix(dtm)
v = sort(rowSums(m),decreasing=TRUE)
word_frequency = data.frame(word = names(v),freq=v)
head(word_frequency, 10)
p = head(word_frequency, 300)

#word frequencies
barplot(word_frequency[1:10,]$freq, las = 2, names.arg = word_frequency[1:10,]$word,
        col ="purple", main ="Most frequent words",
        ylab = "Word frequencies")

#wordcloud
set.seed(69)

wordcloud2(data = p, color = 'random-light', backgroundColor = 'black')



# REVIEWS PLAYLIST

reviews_comments <- read.csv('reviews_comments.csv')
reviews_stats <- read.csv('reviews_stats.csv')

reviews_comments$textOriginal <- gsub("&amp", "", reviews_comments$textOriginal)
reviews_comments$textOriginal <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", reviews_comments$textOriginal)
reviews_comments$textOriginal <- gsub("@\\w+", "", reviews_comments$textOriginal)
reviews_comments$textOriginal <- gsub("[[:punct:]]", "", reviews_comments$textOriginal)
reviews_comments$textOriginal <- gsub("[[:digit:]]", "", reviews_comments$textOriginal)
reviews_comments$textOriginal <- gsub("http\\w+", "", reviews_comments$textOriginal)
reviews_comments$textOriginal <- gsub("[ \t]{2,}", "", reviews_comments$textOriginal)
reviews_comments$textOriginal <- gsub("^\\s+|\\s+$", "", reviews_comments$textOriginal)
reviews_comments$textOriginal <- iconv(reviews_comments$textOriginal, "UTF-8", "ASCII", sub="")
#reviews_comments$text <- gsub("reviews_commentsid", "", dt_comments$text)

#Total number of unique tweets = 13846
reviews_comments <- reviews_comments %>% distinct(textOriginal, .keep_all= TRUE)
length(reviews_comments$textOriginal)
View(reviews_comments)

write.csv(reviews_comments, 'reviews_comments.csv')

#number of unique users who's tweets have been downloaded = 9601
uniqueusers<-length(unique(reviews_comments$authorDisplayName))
uniqueusers

summary(reviews_comments$is_quote)
View(reviews_comments)

#number of people who liked the tweets in total = 41187
sum(reviews_stats$likeCount)

#number of people who retweeted the tweets = 8360
sum(reviews_stats$commentCount)

#Sentiment Analysis
emotions <- get_nrc_sentiment(reviews_comments$textOriginal)
head(emotions)
View(emotions)

reviews_comments_sentiment <- get_sentiment(reviews_comments$textOriginal)

emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion type for tweets related to Reviews Playlist")
p


#create corpus
docs <- Corpus(VectorSource(reviews_comments$textOriginal))

#text cleaning
docs = tm_map(docs, tolower)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeWords, c(stopwords("english")))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs, removeWords, c("bhajpa"))
#docs = tm_map(docs, stemDocument)

# create document term matrix
dtm = TermDocumentMatrix(docs)
m = as.matrix(dtm)
v = sort(rowSums(m),decreasing=TRUE)
word_frequency = data.frame(word = names(v),freq=v)
head(word_frequency, 10)
p = head(word_frequency, 300)

#word frequencies
barplot(word_frequency[1:10,]$freq, las = 2, names.arg = word_frequency[1:10,]$word,
        col ="purple", main ="Most frequent words",
        ylab = "Word frequencies")

#wordcloud
set.seed(69)

wordcloud2(data = p, color = 'random-light', backgroundColor = 'black')


# Explained Palylist

exp_comments <- read.csv('exp_comments.csv')
exp_stats <- read.csv('exp_stats.csv')

exp_comments$textOriginal <- gsub("&amp", "", exp_comments$textOriginal)
exp_comments$textOriginal <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", exp_comments$textOriginal)
exp_comments$textOriginal <- gsub("@\\w+", "", exp_comments$textOriginal)
exp_comments$textOriginal <- gsub("[[:punct:]]", "", exp_comments$textOriginal)
exp_comments$textOriginal <- gsub("[[:digit:]]", "", exp_comments$textOriginal)
exp_comments$textOriginal <- gsub("http\\w+", "", exp_comments$textOriginal)
exp_comments$textOriginal <- gsub("[ \t]{2,}", "", exp_comments$textOriginal)
exp_comments$textOriginal <- gsub("^\\s+|\\s+$", "", exp_comments$textOriginal)
exp_comments$textOriginal <- iconv(exp_comments$textOriginal, "UTF-8", "ASCII", sub="")
#exp_comments$text <- gsub("exp_commentsid", "", dt_comments$text)

#Total number of unique tweets = 13846
exp_comments <- exp_comments %>% distinct(textOriginal, .keep_all= TRUE)
length(exp_comments$textOriginal)
View(exp_comments)

write.csv(exp_comments, 'exp_comments.csv')

#number of unique users who's tweets have been downloaded = 9601
uniqueusers<-length(unique(exp_comments$authorDisplayName))
uniqueusers

#number of people who liked the tweets in total = 41187
sum(exp_stats$likeCount)

#number of people who retweeted the tweets = 8360
sum(exp_stats$commentCount)

#Sentiment Analysis
emotions <- get_nrc_sentiment(exp_comments$textOriginal)
head(emotions)
View(emotions)

exp_comments_sentiment <- get_sentiment(exp_comments$textOriginal)

emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion type for tweets related to Explained Playlist")
p


#create corpus
docs <- Corpus(VectorSource(exp_comments$textOriginal))

#text cleaning
docs = tm_map(docs, tolower)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeWords, c(stopwords("english")))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs, removeWords, c("bhajpa"))
#docs = tm_map(docs, stemDocument)

# create document term matrix
dtm = TermDocumentMatrix(docs)
m = as.matrix(dtm)
v = sort(rowSums(m),decreasing=TRUE)
word_frequency = data.frame(word = names(v),freq=v)
head(word_frequency, 10)
p = head(word_frequency, 300)

#word frequencies
barplot(word_frequency[1:10,]$freq, las = 2, names.arg = word_frequency[1:10,]$word,
        col ="purple", main ="Most frequent words",
        ylab = "Word frequencies")

#wordcloud
set.seed(69)

wordcloud2(data = p, color = 'random-light', backgroundColor = 'black')



labs <- c("Negative", "Positive", "Anger", "Anticipation", "Disgust", "Fear", 
          "Joy", "Sadness", "Surprise", "Trust")
c <- grDevices::col2rgb(c("green", "blue"))
scores <- list(
  "dt_commentsid" = c(6006, 9149, 2742, 5285, 1967, 3220, 3426, 2906, 2158, 5222),
  "iPhone" = c(6370, 8814, 3008, 5124, 2081, 3541, 3615, 3282, 2529, 4927 ))

chartJSRadar(scores=scores, labs=labs, colMatrix = c)






total_comments <- rbind(dt_comments, top5_comments)
total_comments <- rbind(total_comments,  reviews_comments)
total_comments <- rbind(total_comments, exp_comments)
write_csv(total_comments, 'total_comments')



# HUGE ASS COMMENTS DATABASE OF ALL THE SAME PLAYLISTS
bdt_comments <- read.csv('bdt_comments.csv')
btop5_comments <- read.csv('btop5_comments.csv')
bexp_comments <- read.csv('bexp_comments.csv')
bexp_comments <- bexp_comments[-1]
breviews_comments1 <- read.csv('breview_comments1.csv')
breviews_comments1 <- breviews_comments1[-1]
breviews_comments2 <- read.csv('breview_comments2.csv')
breviews_comments2 <- breviews_comments2[-1]
btotal_comments <- do.call("rbind", list(bdt_comments, btop5_comments, bexp_comments))
write_csv(btotal_comments, 'btotal_comments.csv')
View(btotal_comments)
btotal_comments <- read.csv('btotal_comments.csv')
bcomb_comments <- rbind(breviews_comments1, breviews_comments2)  
View(bcomb_comments)


btotal_comments <- read.csv('total_comments.csv')
View(btotal_comments)

btotal_comments$textOriginal <- gsub("&amp", "", btotal_comments$textOriginal)
btotal_comments$textOriginal <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", btotal_comments$textOriginal)
btotal_comments$textOriginal <- gsub("@\\w+", "", btotal_comments$textOriginal)
btotal_comments$textOriginal <- gsub("[[:punct:]]", "", btotal_comments$textOriginal)
btotal_comments$textOriginal <- gsub("[[:digit:]]", "", btotal_comments$textOriginal)
btotal_comments$textOriginal <- gsub("http\\w+", "", btotal_comments$textOriginal)
btotal_comments$textOriginal <- gsub("[ \t]{2,}", "", btotal_comments$textOriginal)
btotal_comments$textOriginal <- gsub("^\\s+|\\s+$", "", btotal_comments$textOriginal)
btotal_comments$textOriginal <- iconv(btotal_comments$textOriginal, "UTF-8", "ASCII", sub="")
btotal_comments$textOriginal = gsub("\n", "", btotal_comments$textOriginal)

write_csv(btotal_comments, 'clean_comments.csv')
write_csv(btotal_comments, 'clean_comments_small.csv')

btotal_comments <- btotal_comments %>% distinct(textOriginal, .keep_all= TRUE)
length(btotal_comments$textOriginal)
View(btotal_comments)

#number of unique users who's tweets have been downloaded = 9601
uniqueusers<-length(unique(btotal_comments$authorDisplayName))
uniqueusers

write_csv(btotal_comments, 'distinct_comments.csv')

dist_comments <- read.csv('distinct_comments.csv')


emotions <- get_nrc_sentiment(dist_comments$textOriginal)
head(emotions)
View(emotions)

dist_comments_sentiment <- get_sentiment(dist_comments$textOriginal)

emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion type for comments related to MKBHD's channel")
p


docs <- Corpus(VectorSource(dist_comments$textOriginal))
View(wc)


#text cleaning
docs = tm_map(docs, tolower)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeWords, c(stopwords("english")))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, stripWhitespace)
#docs = tm_map(docs, removeWords, c("btotal_commentsid"))

#docs = tm_map(docs, stemDocument)

# create document term matrix
dtm = TermDocumentMatrix(docs)
View(dtm)
m = as.matrix(dtm)
v = sort(rowSums(dtm),decreasing=TRUE)
word_frequency = data.frame(word = names(v),freq=v)
head(word_frequency, 10)
p = head(word_frequency, 300)



# STATS OF ALL THE PLAYLISTS
# STATS IS BELOW DONT RUN THIS CODE
#dt_stats <- read.csv('dt_stats.csv')
#View(dt_stats)
# top5_stats <- read.csv('top5_stats.csv')
# reviews_stats <- read.csv('reviews_stats.csv')
# exp_stats <- read.csv('exp_stats.csv')
# total_stats <- do.call("rbind", list(dt_stats, top5_stats, reviews_stats, exp_stats))
# write_csv(total_stats, 'total_stats.csv')


total_stats <- read.csv('total_stats.csv')
View(total_stats)

most_liked <- total_stats[order(-total_stats$likeCount),]
most_liked <- most_liked[1:5,1:5]
View(most_liked)
most_disliked <- total_stats[order(-total_stats$dislikeCount),]
most_disliked <- most_disliked[1:5,]
View(most_disliked)

distinct_comments <- read.csv('distinct_comments.csv')
View(distinct_comments)
dl <- subset(distinct_comments, videoId == "vtqtyyGZvXM" | videoId == "sVdIWxa4dpA" | videoId == "mBHY_Qaw5AI" 
             | videoId == "_CTUs_2hq6Y" | videoId == "aXfiyuUziY0")

View(dl)
write.csv(dl, 'disliked_videos.csv')
dl <- read.csv("disliked_videos.csv")
View(dl)
dl$textOriginal <- gsub("&amp", "", dl$textOriginal)
dl$textOriginal <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", dl$textOriginal)
dl$textOriginal <- gsub("@\\w+", "", dl$textOriginal)
dl$textOriginal <- gsub("[[:punct:]]", "", dl$textOriginal)
dl$textOriginal <- gsub("[[:digit:]]", "", dl$textOriginal)
dl$textOriginal <- gsub("http\\w+", "", dl$textOriginal)
dl$textOriginal <- gsub("[ \t]{2,}", "", dl$textOriginal)
dl$textOriginal <- gsub("^\\s+|\\s+$", "", dl$textOriginal)
dl$textOriginal <- iconv(dl$textOriginal, "UTF-8", "ASCII", sub="")
#dl$text <- gsub("dlid", "", dl$text)

#Total number of unique tweets = 13846
dl <- dl %>% distinct(textOriginal, .keep_all= TRUE)
length(dl$textOriginal)
View(dl)

# Exporting to Excel

write.csv(dl, "disliked_clean.csv")
View(dl)
dl <- read.csv("disliked_clean.csv")

emotions <- get_nrc_sentiment(dl$textOriginal)
head(emotions)
View(emotions)

dl_sentiment <- get_sentiment(dl$textOriginal)

emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion type for tweets related to dlid")
p


#create corpus
docs <- Corpus(VectorSource(dl$textOriginal))

#text cleaning
docs = tm_map(docs, tolower)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeWords, c(stopwords("english")))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs, removeWords, c("dlid"))
#docs = tm_map(docs, stemDocument)

# create document term matrix
dtm = TermDocumentMatrix(docs)
m = as.matrix(dtm)
v = sort(rowSums(m),decreasing=TRUE)
word_frequency = data.frame(word = names(v),freq=v)
head(word_frequency, 10)
p = head(word_frequency, 300)

#word frequencies
barplot(word_frequency[1:10,]$freq, las = 2, names.arg = word_frequency[1:10,]$word,
        col ="purple", main ="Most frequent words",
        ylab = "Word frequencies")

#wordcloud
set.seed(69)

wordcloud2(data = p, color = rep_len(c(red1, red2, red3), 400), backgroundColor = "#f5eded")
wordcloud2(fontFamily = )


#LIKED VIDEOS

l <- subset(distinct_comments, videoId == "vtqtyyGZvXM" | videoId == "UIwdCN4dV6w" | videoId == "KB4_WIPE7vo" 
            | videoId == "_CTUs_2hq6Y" | videoId == "aXfiyuUziY0")
View(l)
write.csv(l, 'liked_videos.csv')
l <- read.csv('liked_videos.csv')

l$textOriginal <- gsub("&amp", "", l$textOriginal)
l$textOriginal <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", l$textOriginal)
l$textOriginal <- gsub("@\\w+", "", l$textOriginal)
l$textOriginal <- gsub("[[:punct:]]", "", l$textOriginal)
l$textOriginal <- gsub("[[:digit:]]", "", l$textOriginal)
l$textOriginal <- gsub("http\\w+", "", l$textOriginal)
l$textOriginal <- gsub("[ \t]{2,}", "", l$textOriginal)
l$textOriginal <- gsub("^\\s+|\\s+$", "", l$textOriginal)
l$textOriginal <- iconv(l$textOriginal, "UTF-8", "ASCII", sub="")
#dl$text <- gsub("dlid", "", dl$text)

#Total number of unique tweets = 13846
l <- l %>% distinct(textOriginal, .keep_all= TRUE)
length(l$textOriginal)
View(l)

# Exporting to Excel

write.csv(l, "liked_clean.csv")
View(l)
l <- read.csv('liked_clean.csv')

emotions <- get_nrc_sentiment(l$textOriginal)
head(emotions)
View(emotions)

l_sentiment <- get_sentiment(l$textOriginal)

emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

#create corpus
docs <- Corpus(VectorSource(l$textOriginal))

#text cleaning
docs = tm_map(docs, tolower)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeWords, c(stopwords("english")))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs, removeWords, c("bhajpa"))
#docs = tm_map(docs, stemDocument)

# create document term matrix
dtm = TermDocumentMatrix(docs)
m = as.matrix(dtm)
v = sort(rowSums(m),decreasing=TRUE)
word_frequency = data.frame(word = names(v),freq=v)
head(word_frequency, 10)
p = head(word_frequency, 300)

#wordcloud
set.seed(69)
colorvec = rep(c('red1', 'red2', 'red3'), nrow(word_frequency))
red1 <- "#d72323"
red2 <- "#3e3636"
red3 <- "#000000"

wordcloud2(data = p, color = rep_len(c(red1, red2, red3), 400), backgroundColor = "#f5eded")


labs <- c("Negative", "Positive", "Anger", "Anticipation", "Disgust", "Fear", 
          "Joy", "Sadness", "Surprise", "Trust")
c <- grDevices::col2rgb(c("green", "red"))
scores <- list(
  "Liked" = c(243, 291, 123, 154, 82, 134, 125, 125, 68, 170),
  "Disliked" = c(243, 253, 118, 140, 88, 113, 100, 105, 68, 159 ))

chartJSRadar(scores=scores, labs=labs, colMatrix = c)


# total_stats <- total_stats[-1]
final_stats <- read.csv('final_stats.csv')
View(final_stats)

# first impressions id <- PLBsP89CPrMeM2MmF4suOeT0vsic9nEC2Y

dope_tech <- get_playlist_items(filter = c(playlist_id = "PLBsP89CPrMeM2MmF4suOeT0vsic9nEC2Y"))
videos = yt_search(term="", type="video", channel_id = "UCBJycsmduvYEL83R_U4JriQ")
View(videos)



# Video ids
vid_ids <- as.vector(videos$video_id)
# Function to scrape stats for all vids
get_all_stats <- function(id) {get_stats(id)} 
# Get stats and convert results to data frame 
res <- lapply(vid_ids, get_all_stats)
View(res)
res <- res[-c(365,400,404,403,381)]
res[[365]] <- NULL
res[[400]] <- NULL
res[[404]] <- NULL
res[[401]] <- NULL
res[[403]] <- NULL
res[[380]] <- NULL
res[[398]] <- NULL

total_stats <- do.call(rbind, lapply(res, data.frame))
view(total_stats)
all_stats <- total_stats
View(all_stats)
x <- rbind(all_stats, total_stats)
View(x)

x = x[order(x[,'id'],-x[,'viewCount']),]
x = x[!duplicated(x$id),]

library(data.table)
y <- unique(setDT(x)[order(id, -viewCount)], by = "id")
View(y)
write_csv(y, 'final_stats')
sum(table(y$id)-1)


yt_searc
videos2 = yt_search(term="", type="video", channel_id = "UCBJycsmduvYEL83R_U4JriQ", published_before = "2016-10-01T00:00:00Z")

comments = lapply(as.character(videos$video_id), function(x){
  get_comment_threads(c(video_id = x), max_results = 100)
})
View(comments)
