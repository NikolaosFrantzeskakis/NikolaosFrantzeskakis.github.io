
# install.packages("")
library(rtweet)
library(ggplot2)
library(tidytext)
library(stringr)
library(lubridate)
library(dplyr)
library(tm)
library(dplyr)
library(wordcloud)
library(syuzhet)

api_key <- "Rw6NeoRInBUE5T2h4zkyrDbHN"
api_secret_key <- "DhF1ee7G3Zn27KUql4zsMxJD6bg86sCnJVheEdMxyNoFmTKLeg"

access_token <- "1168680501302976512-uKTilMtLTbr9JFgVGObKJNOWYiTEpB"
access_token_secret <- "IbX17sv4xnEdSPrlFz9zLJ93wKY2YnasHSh8z4AVMjRSx"

# authenticate with web browser
token <- create_token(
  app = "r_ssda",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

token

# check status look up
rate_limit("followers")
cat(rep("-",53))
# look all limits up
rate_limit()

trends <- trends_available()
trends

us_trends <- get_trends(woeid = 23424977) # US
detroit_trends <- get_trends(woeid = 2391585) # Detroit

us_trends[1:15,]

detroit_trends[1:15,]

friends <- get_friends("calebjlucas",
                       n = 100,                # how many friends do you want to retrieve?
                       retryonratelimit = TRUE # retry if you go over the API limits?
                      )

head(friends)

my_accounts <- c("calebjlucas", "MSU_SocSci")
friends <- get_friends(my_accounts,             # multiple accounts at once
                       n = 100,                 # how many friends do you want to retrieve?
                       retryonratelimit = TRUE) # retry if you go over the API limits?

friends[c(1:5, (nrow(friends)-5):nrow(friends)), ]


followers <- get_followers("calebjlucas",
                       n = 100, # how many friends do you want to retrieve?
                       retryonratelimit = TRUE #retry if you go over the API limits?
                       )

cat("People that follow caleb:")
head(followers)

followers_lim <- get_followers("BrendanNyhan", n = 999999, retryonratelimit = TRUE)

# rate limiting at this point
rate_limit("followers")

# sleep for 10 seconds and notice the time going down
Sys.sleep(10)
rate_limit("followers")

user_info <- lookup_users(followers$user_id[1:5])
head(user_info)


colnames(user_info)

user_info[1:5, c("name","description","location")]

# random 1% of real-time tweets
stream <- stream_tweets(q = "", timeout = 60) #default timeout is 30 seconds
head(stream)

cat("Number of streamed tweets in 60 seconds:", nrow(stream), "\n")
cat("Number of tweets in 24 hours at same rate:", nrow(stream) * 1440) # 1440 minutes in a day

format(object.size(stream), units = "auto")



stream %>%
  ts_plot("1 second") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = "Time", y = "Number of Tweets",
    title = "Twitter Stream"
  )













tweets <- search_tweets(q = "msu", n = 5)
tweets

tweets <- search_tweets(q = '"michigan state"', n = 5) # looks for exact phrase (See quotes)
tweets

tweets <- search_tweets(q = "msu AND uk", n = 5)
tweets

tweets <- search_tweets(q = "msu OR uk", n = 5)
tweets

tweets <- search_tweets(q = "Liban", lang = "fr", n = 5) # search for Lebanon in French
tweets

tweets <- search_tweets(q = "", n = 5, geocode = "42.73,-84.48,15mi") # east lansing
tweets

tweets <- search_tweets(q = '"michigan state"', lang = "en", n = 5, include_rts = FALSE)
head(tweets)

# search for hashtag
trump_hashtag <- search_tweets("#impeachtrump", n = 5, include_rts = FALSE)
trump_hashtag

trump_tweets <- search_tweets("trump", n = 18000, include_rts = FALSE)

# select last (oldest) status ID from previous search
last_status_id <- trump_tweets$status_id[nrow(trump_tweets)]

# pass last_status_id to max_id and run search again.
trump_tweets_contd <- search_tweets(
    "trump",
    n = 18000, 
    include_rts = FALSE,
    max_id = last_status_id)

lots_of_trump <- search_tweets("trump", n = 999999, retryonratelimit = TRUE, include_rts = FALSE)

caleb_twitter <- get_timeline("calebjlucas", n = 3200)

head(caleb_twitter)

colnames(caleb_twitter)

caleb_twitter %>%
  ts_plot("2 days") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = "Time", y = "Number of Tweets",
    title = ".",
    subtitle = "."
  )

# post_message("Hey Caleb!!!", user = "calebjlucas")

caleb_profile <- search_users("calebjlucas")
# function matched multiple users, subset to include only my profile
caleb_profile <- subset(caleb_profile, screen_name == "calebjlucas")

caleb_profile

for(i in 1:ncol(caleb_profile)){
    cat(colnames(caleb_profile)[i],"\n")
}

caleb_profile$name

caleb_profile$description

caleb_profile$location

caleb_profile$account_created_at

cnn <- get_timeline("cnn", n = 3200)
head(cnn)

cnn <- plain_tweets(cnn)

cnn_words <- cnn %>%
  dplyr::select(text) %>%
  tidytext::unnest_tokens(word, text)

cnn_words[1:10,]
cnn$text[1]

cnn_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
      labs(x = "Count",
      y = "Unique words",
      title = "Count of unique words found in tweets")

cnn_words[1:20, ]

data(stop_words)

cnn_words <- cnn_words %>%
  anti_join(stop_words)



head(cnn_words)





# to lowercase - this is a really important step
cnn_words$word <- tolower(cnn_words$word)

# unnecessary spaces
cnn_words$word <- str_squish(cnn_words$word)

# drop urls
cnn_words$word <- str_replace_all(cnn_words$word,"\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"," ") 
cnn_words$word <- str_replace_all(cnn_words$word,"t.co"," ") 

# drop nums
cnn_words$word <- str_replace_all(cnn_words$word,"[[:digit:]]"," ") 

# drop rid of hashtags
cnn_words$word <- str_replace_all(cnn_words$word,"#[a-z,A-Z]*"," ")

# drop references to other screennames
cnn_words$word <- str_replace_all(cnn_words$word,"@[a-z,A-Z]*"," ") 

# drop punc
cnn_words$word <- str_replace_all(cnn_words$word,"[[:punct:]]"," ") 

# drop where 's existed (we dropped punctuation so there will be things
# like this in the corpus : [trump s] where trump's used to be)
cnn_words$word <- str_replace_all(cnn_words$word,"\\ss"," ") 

# unnecessary spaces
cnn_words$word <- str_squish(cnn_words$word)
cnn_words$word <- str_trim(cnn_words$word, side = "both")


cnn_words[1:20, ]

cnn_words <- cnn_words[!(cnn_words$word == ""), ]
cnn_words %>%
  count(word, sort = TRUE) %>%
  top_n(25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(title = "Count of Unique Words")



trump <- get_timeline("realDonaldTrump", 
                      n = 10000, 
                      retryonratelimit = TRUE)



trump$sent <- get_sentiment(trump$text, method = "bing")

head(trump$created_at)

trump$created_at_day <- as.Date(ymd_hms(trump$created_at))
head(trump$created_at_day)

trump_agg <- trump %>% 
  group_by(created_at_day) %>%
  summarize(avg_sent = mean(sent))
head(trump_agg)

ggplot(data = trump_agg, aes(x = created_at_day, y = avg_sent)) + 
  geom_line() +
  ylim(-1,1) + 
  theme_minimal()







nrc_sentiment <- get_nrc_sentiment(trump$text) 

nrc_sentiment <- data.frame(colSums(nrc_sentiment))

colnames(nrc_sentiment) <- "Score"

sentimentscores <- cbind("sentiment" = rownames(nrc_sentiment), nrc_sentiment)

ggplot(data = sentimentscores,aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("sentiment") + 
  ylab("score") +
  ggtitle("Total Sentiment of Tweets") +
  theme_minimal()

install.packages("igraph")

library(igraph)

search_acc <- c("calebjlucas", "MSU_poli_sci", "shaylafolson", "SchellintheC",
                "ErikaaVallejo", "maxwelch_pls")
friends <- get_friends(search_acc)

frq_table <- table(fds$user_id)
# change the 0 to a higher threshold if you would like
friends_sub <- subset(friends, user_id %in% names(frq_table[frq_table > 0]))

mat <- as.matrix(friends_sub)

mat_graph <- igraph::graph_from_edgelist(mat)

plot(mat_graph,
     edge.arrow.size = .4,
     vertex.size = 5,
     vertex.label = ifelse(V(mat_graph)$name %in% search_acc, V(mat_graph)$name, NA),
     main = "MSU Political Science Network"
    )




