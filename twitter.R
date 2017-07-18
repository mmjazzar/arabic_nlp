#Following packages are being used so install these if not installed
library(twitteR)
library(tm) #text minning 
library(ggplot2)
library(lubridate)
library(tidytext)
library(stringr)
library(dplyr)
library(tidyr)
library(scales)


########


#Set authentication with Twitter. Copy keys generated in step-1
api_key <- "q19KDEViDMNsBh1wjUjD3TNvF"
api_secret <- "W14UMa3DU1H2HIcv96dpA7gOQOkJYtYPL1fywNkJMl5rnMwT7v"
access_token <- "115216526-hphcEh4FwJZ9x1wuhRfe1Osl7d5i6aFVCuhkPlQc"
access_token_secret <- "iMw6aJbBpJGQZm72CsCZiAOrNQUAtBMW2XqKdmBzauhgZ"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

tweets <- searchTwitter("#",n=100000)
#tweets <- searchTwitter('TrumpinSaudi ', since='2017-05-24', until='2017-05-27', n=10000)

tweets <- searchTwitter('#', since='2017-06-02', until='2017-06-05', n=50000)

df <- twListToDF(tweets)


#saving arabic data
RHCJO <- twListToDF(some_tweets)


RHCJO$text <- as.character(RHCJO$text)
write.xlsx(RHCJO, file = "RHCJO.xlsx")


df <- df[, order(names(df))]
df$created <- strftime(df$created, '%Y-%m-%d')
if (file.exists(paste('RHCJO', '_stack.csv'))==FALSE) write.csv(df, file=paste('TrumpinSaudi', '_stack.csv'), row.names=F)

#merge last access with cumulative file and remove duplicates
stack <- read.csv(file=paste(searchterm, '_stack.csv'))
stack <- rbind(stack, df)
stack <- subset(stack, !duplicated(stack$text))
write.csv(stack, file=paste(searchterm, '_stack.csv'), row.names=F)

#convert tweets to a data frame
tweets.df <- twListToDF(tweets)
backup <- twListToDF(tweets)

# using dates
tweets.df$timestamp <- as.Date(tweets.df$created)



#Tweets by Year, Month, and Day
ggplot(data = tweets.df, aes(x = timestamp)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")


#####################################year

ggplot(data = tweets.df, aes(x = year(timestamp))) +
  geom_histogram(breaks = seq(2009.5, 2019, by =1), aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

#########################################   day
ggplot(data = tweets.df, aes(x = wday(timestamp, label = TRUE))) +
  geom_histogram(breaks = seq(0.5, 7.5, by =1), aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Day of the Week") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

#############time only

tweets.df$time= as.POSIXct(tweets.df$created, format="%y/%m/%d %H:%M:%S")  

ggplot(data = tweets.df, aes(x = time)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")


# with hashtag or not 
ggplot(tweets.df, aes(factor(grepl("#", tweets.df$text)))) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Number of tweets") + 
  ggtitle("Tweets with Hashtags") +
  scale_x_discrete(labels=c("No hashtags", "Tweets with hashtags"))


# with specific word  
ggplot(tweets.df, aes(factor(grepl("Israel", tweets.df$text)))) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Number of tweets") + 
  ggtitle("Tweets with Hashtags") +
  scale_x_discrete(labels=c("not found", "found"))


######################## filter retweeted tweets
######################## favoriteCount, retweetCount

tweets.df$time= as.POSIXct(tweets.df$created, format="%y/%m/%d %H:%M:%S")  


###################################################
#Specify the condition k = 17
k <- subset(tweets.df, tweets.df$isRetweet ==FALSE )
nrow(k)
k1 <- k[order((k$retweetCount), decreasing = TRUE), ]

# get fav tweets
f <- subset(tweets.df, tweets.df$favoriteCount > 1 )
nrow(f)
f1 <- f[order((f$favoriteCount), decreasing = TRUE), ]


f1$screenName[1:20]
f1$favoriteCount[1:20]

# plot users of high favouirte tweets 
p<-ggplot(data=f1[1:5,], aes(x=screenName, y= favoriteCount, fill = screenName)) +
  geom_bar(stat="identity") +
  theme_minimal()
p

# plot users of high retweeted tweets 
p<-ggplot(data=k1[1:5,], aes(x=screenName, y= retweetCount, fill = screenName)) +
  geom_bar(stat="identity") +
  theme_minimal()
p

# present high retweeted tweets
k1$text[1:5]
f1$text[1:5]

############################### add score
tweets.df$score <- tweets.df$favoriteCount + tweets.df$retweetCount * 2

s <- tweets.df[order((tweets.df$score), decreasing = TRUE), ]


# Most influnce users score
p<-ggplot(data=s[1:10,], aes(x=screenName, y= score, fill = score)) +
  geom_bar(stat="identity") +
  theme_minimal()
p

library(plyr)

### some table
tweets_by_author <-  tweets.df %>% 
  ddply(~ tweets.df$screenName, function(x) {
    data.frame(num_tweets = nrow(x),
               avg_favorites = mean(x$favoriteCount) %>% round(digits = 1),
               avg_retweets = mean(x$retweetCount) %>% round(digits = 1),
               avg_score = mean(x$score) %>% round(digits = 1)
    )}) %>%  arrange(desc(avg_score)) 


tweets_by_author %>% arrange(desc(num_tweets)) %>% head(10)


# get top users tweets
top_authors <- tweets_by_author %>% arrange(desc(avg_score)) %>%  head(10)

tweets_top_authors <- tweets.df %>% filter()

tweets_top_authors <- subset(tweets.df, tweets.df$screenName %in% top_authors$`tweets.df$screenName`)

tweets_top_authors$text

# score of top users
ggplot(tweets_top_authors) +
  geom_point(aes(score, retweetCount, fill = screenName), size = 5, shape = 21) +
  theme_bw(15) +
  scale_fill_brewer("Author", type = "qual", palette = 3) +
  ggtitle("Score of top users") +
  xlab("# score") + ylab("# retweets")

# And let's see them again, in perspective to all tweets
ggplot(tweets.df) +
  geom_jitter(aes(score, retweetCount), size = 3, shape = 21, fill = "#444444", alpha=0.4) +
  theme_bw(15) +
  geom_point(data = tweets_top_authors,
             aes(score, retweetCount, fill = screenName), size = 6, shape = 21) +
  scale_fill_brewer("Author", type = "qual", palette = 3) +
  xlab("# favorites") + ylab("# retweets") +
  ggtitle("Score of them to all")

#########################################################################
#######################################################  save dataset
searchterm <- "messi"
# export tweets into CSV 
tweets <- searchTwitter(searchterm,n= 100, lang="en") 
tweets.df <- twListToDF(tweets)
df <- twListToDF(tweets)


df <- df[, order(names(df))]
df$created <- strftime(df$created, '%Y-%m-%d')
if (file.exists(paste(searchterm, '_stack.csv'))==FALSE) write.csv(df, file=paste(searchterm, '_stack.csv'), row.names=F)
#merge last access with cumulative file and remove duplicates
stack <- read.csv(file=paste(searchterm, '_stack.csv'))
stack <- rbind(stack, df)
stack <- subset(stack, !duplicated(stack$text))
write.csv(stack, file=paste(searchterm, '_stack.csv'), row.names=F)

#####
library(stringi); library(dplyr); library(SciencesPo)

tweets <- searchTwitter(searchterm,n= 10, lang="en") 
tweets.df <- twListToDF(tweets)
txts = sapply(tweets, function(x) x$getText())

tweets.df$n <- vapply(tweets.df$tweets, function(x) sum(stri_count_fixed(x, filter)), 1L)


corp <- Corpus(VectorSource(txts))
term.matrix <- TermDocumentMatrix(corp)
term.matrix <- as.matrix(term.matrix)
colnames(term.matrix) <- names(txts)
term.matrix <- as.data.frame(term.matrix)

ggplot(term.matrix, 
       aes_string(x=names(txts)[1], 
                  y=names(txts)[2], 
                  label="rownames(term.matrix)")) + 
  geom_text()



#################################


############################################################
# word frequency


replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

# comparing two hashtags in the beginning 

jordan2 <- searchTwitter("#jordan", n=50000)

KingAbdullahII  <- searchTwitter("KingAbdullahII", n=10000)

QueenRania<- searchTwitter("QueenRania", n=10000)
# To DF
palestine <- twListToDF(palestine)
israel <- twListToDF(israel)

# adding time
palestine$timestamp <- as.Date(palestine$created)
israel$timestamp <- as.Date(israel$created)



# bining
tweets2 <- bind_rows(palestine %>% 
                      mutate(person = "palestine"),
                    israel %>% 
                      mutate(person = "israel"))


# Selecting retweet only
tidy_tweets <- tweets2 %>%   subset(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word,str_detect(word, "[a-z]"))

  
frequency <- tidy_tweets %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_tweets %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)


#
frequency <- frequency %>% 
  select(person, word, freq) %>% 
  spread(person, freq) 

# replacing na with 0
frequency[is.na(frequency)] <- 0



ggplot(frequency, aes(palestine, israel)) +
  geom_jitter(alpha = 0.1, size = 3.5, width = 0.35, height = 0.35) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")


ggplot(frequency, aes(palestine, israel)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")


#Comparing word usage

word_ratios <- tidy_tweets %>%
  subset(!str_detect(word, "^@")) %>%
  count(word, person) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(person, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log(palestine / israel)) %>%
  arrange(desc(logratio))


word_ratios %>% 
  arrange(abs(logratio))


word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col() +
  coord_flip() +
  ylab("log odds ratio (palestine/israel)") +
  scale_fill_discrete(name = "", labels = c("palestine", "israel"))

#############################
### words by time



words_by_time <- tidy_tweets %>%
  subset(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(timestamp, unit = "1 day")) %>%
  count(time_floor, person, word) %>%
  ungroup() %>%
  group_by(person, time_floor) %>%
  mutate(time_total = sum(n))%>%
  group_by(word) %>%
  mutate(word_total = sum(n))%>%
  ungroup() %>%
  rename(count = n) %>%
  subset(word_total > 10)


words_by_time



nested_data <- words_by_time %>%
  nest(-word, -person) 

nested_data


library(purrr)

nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, ., 
                                  family = "binomial")))

nested_models

library(broom)

slopes <- nested_models %>%
  unnest(map(models, tidy)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))

top_slopes <- slopes %>% 
  subset(adjusted.p.value > 0.1)

top_slopes



words_by_time %>%
  inner_join(nested_models, by = c("word", "person")) %>%
  filter(person == "Jordan") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency")

# 7.5 Favorites and retweets

totals <- tidy_tweets %>% 
  group_by(person, id) %>% 
  summarise(rts = sum(retweetCount)) %>% 
  group_by(person) %>% 
  summarise(total_rts = sum(rts))


word_by_rts <- tidy_tweets %>% 
  group_by(id, word, person) %>% 
  summarise(rts = first(retweetCount)) %>% 
  group_by(person, word) %>% 
  summarise(retweets = median(rts), uses = n()) %>%
  left_join(totals) %>%
  filter(retweets != 0) %>%
  ungroup()

word_by_rts %>% 
  filter(uses >= 5) %>%
  arrange(desc(retweets))


word_by_rts %>%
  filter(uses >= 5) %>%
  group_by(person) %>%
  top_n(10, retweets) %>%
  arrange(retweets) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup() %>%
  ggplot(aes(word, retweets, fill = person)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ person, scales = "free", ncol = 2) +
  coord_flip() +
  labs(x = NULL, 
       y = "Median # of retweets for tweets containing each word")


# for fav
totals <- tidy_tweets %>% 
  group_by(person, id) %>% 
  summarise(favs = sum(favoriteCount)) %>% 
  group_by(person) %>% 
  summarise(total_favs = sum(favs))

word_by_favs <- tidy_tweets %>% 
  group_by(id, word, person) %>% 
  summarise(favs = first(favoriteCount)) %>% 
  group_by(person, word) %>% 
  summarise(favorites = median(favs), uses = n()) %>%
  left_join(totals) %>%
  filter(favorites != 0) %>%
  ungroup()

word_by_favs %>%
  filter(uses >= 5) %>%
  group_by(person) %>%
  top_n(10, favorites) %>%
  arrange(favorites) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup() %>%
  ggplot(aes(word, favorites, fill = person)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ person, scales = "free", ncol = 2) +
  coord_flip() +
  labs(x = NULL, 
       y = "Median # of favorites for tweets containing each word")



# looking for special word in tweets 
word_by_favs %>%
  filter(uses >= 5) %>%
  group_by(person) %>%
  top_n(10, favorites) %>%
  arrange(favorites) %>%
  ungroup() %>%
  mutate(word = factor(word, "gaza")) %>%
  ungroup() %>%
  ggplot(aes(word, favorites, fill = person)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ person, scales = "free", ncol = 2) +
  coord_flip() +
  labs(x = NULL, 
       y = "Median # of favorites for tweets containing each word")

###############

library("rtweet")
library("dplyr")
library("magick")
library("httr")
library("stringr")
library("purrr")
#getting users
users <- search_users(q= '@RHCJO',
                      n = 1000,
                      parse = TRUE)
  users <- unique(users)

