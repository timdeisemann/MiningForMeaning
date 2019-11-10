# 14.06.2019
# Tim Deisemann

# Libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(ROAuth)
library(rtweet)
#library(twitteR) archived
library(NLP)
library(tm) # tm_map
library(syuzhet) # sentiment analysis
library(httpuv)

print(sessionInfo())

# Inputs ------------------------------------------------------------------

#select_matrix <- party elite / senate

n_tweets <- 50 # second analysis <- 250

app_name <- 'ToBeReplaced'          # insert your application name
consumer_key <- 'ToBeReplaced'      # insert your consumer key
consumer_secret <- 'ToBeReplaced'   # insert your consumer secret
access_token <- 'ToBeReplaced'      # insert your access token
access_secret <- 'ToBeReplaced'     # insert your access secret

token <- create_token(
  app_name, 
  consumer_key = consumer_key, 
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret
)
token
path_to_token <- file.path(path.expand("~"), ".twitter_token.rds")
saveRDS(token, path_to_token)
env_var <- paste0("TWITTER_PAT=", path_to_token)
cat(env_var, file = file.path(path.expand("~"), ".Renviron"), fill = TRUE, append = TRUE)
readRenviron("~/.Renviron")

names(rt)

# Party Elite ------------------------------------------------------------------

# Chairperson: Ronna McDaniel - @GOPChairwoman
# Spokesperson: Kayleigh McEnany - @kayleighmcenany
# U.S. President: Donald Trump - @realDonaldTrump
# Senate Majority Leader: Mitch McConnell - @senatemajldr
# House Minority Leader: Kevin McCarthy - @GOPLeader

# Chairperson: Tom Perez - @tomperez
# Secretary: Jason Rae - @JasonRRae
# Speaker of the House: Nancy Pelosi - @SpeakerPelosi
# House Majority Leader: Steny Hoyer - @LeaderHoyer
# Senate Minority Leader: Chuck Schumer - @SenSchumer

# # Full Senate -----------------------------------------------------------

# politicians <- matrix(c(
#   '@SenShelby', '@lisamurkowski', '@SenDanSullivan', '@SenMcSallyAZ', '@JohnBoozman', '@TomCottonAR', '@SenCoryGardner', '@marcorubio', '@SenRickScott', '@SenatorIsakson', '@sendavidperdue', '@MikeCrapo', '@SenatorRisch', '@SenToddYoung', '@braun4indiana', '@ChuckGrassley', '@joniernst', '@PatRoberts', '@JerryMoran', '@senatemajldr', '@RandPaul', '@BillCassidy', '@SenJohnKennedy', '@SenatorCollins', '@RogerWicker', '@cindyhydesmith', '@RoyBlunt', '@HawleyMO', '@SteveDaines', '@SenatorFischer', '@SenSasse', '@SenatorBurr', '@SenThomTillis', '@SenJohnHoeven', '@SenKevinCramer', '@senrobportman', '@JimInhofe', '@SenatorLankford', '@SenToomey', '@LindseyGrahamSC', '@SenatorTimScott', '@SenJohnThune', '@SenatorRounds', '@SenAlexander', '@MarshaBlackburn', '@JohnCornyn', '@SenTedCruz', '@SenMikeLee', '@MittRomney', '@SenCapito', '@SenRonJohnson', '@SenatorEnzi', '@SenJohnBarrasso', # Republicans
#   '@DougJones', '@SenatorSinema', '@SenFeinstein', '@KamalaHarris', '@SenatorBennet', '@SenBlumenthal', '@ChrisMurphyCT', '@SenatorCarper', '@ChrisCoons', '@SenBrianSchatz', '@maziehirono', '@SenatorDurbin', '@SenDuckworth', '@SenatorCardin', '@ChrisVanHollen', '@SenWarren', '@SenMarkey', '@SenStabenow', '@GaryPeters', '@amyklobuchar', '@TinaSmithMN', '@SenatorTester', '@CatherineForNV', '@SenJackyRosen', '@SenatorShaheen', '@SenatorHassan', 'SenatorMenendez', '@CoryBooker', '@SenatorTomUdall', '@MartinHeinrich', '@SenSchumer', '@SenGillibrand', '@SenSherrodBrown', '@RonWyden', '@JeffMerkley', '@BobCasey', '@SenJackReed', '@SenWhitehouse', '@SenatorLeahy', '@MarkWarner', '@timkaine', '@PattyMurray', '@SenatorCantwell', '@Sen_JoeManchin', '@SenatorBaldwin'  #Democrats
# ), nrow = 2, byrow = TRUE)

politicians <- matrix(c(
  '@GOPChairwoman', '@kayleighmcenany', '@realDonaldTrump', '@senatemajldr', '@GOPLeader', # Republicans
  '@tomperez', '@JasonRRae', '@SpeakerPelosi', '@LeaderHoyer', '@SenSchumer' # Democrats
), nrow = 2, byrow = TRUE)


# Functions ---------------------------------------------------------------

clean_tweets <- function(tweets) {
  #convert all text to lower case
  tweets<- tolower(tweets)
  # Replace blank space (“rt”)
  #tweets <- gsub("rt", "", tweets)
  # Replace @UserName
  tweets <- gsub("@\\w+", "", tweets)
  # Remove punctuation
  tweets <- gsub("[[:punct:]]", "", tweets)
  # Remove links
  tweets <- gsub("http\\w+", "", tweets)
  # Remove non ASCII chars
  tweets <- gsub("[^\x01-\x7F]", "", tweets)
  # Remove tabs
  tweets <- gsub("[ |\t]{2,}", "", tweets)
  # Remove spaces, enters
  tweets <- gsub("[\n\r]", "", tweets)
  # Remove blank spaces at the beginning
  tweets <- gsub("^ ", "", tweets)
  # Remove blank spaces at the end
  tweets <- gsub(" $", "", tweets)
  
  return(tweets)
}

# Fetch Tweets ------------------------------------------------------------

#setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets <- data.frame()

for (i in 1:dim(politicians)[1]) {
  party_i <- i
  for (j in 1:dim(politicians)[2]) {
    politician_i <- j
    politician <- politicians[party_i, politician_i]
    
    print(paste(politician, party_i))
    
    tweets_politician <- get_timeline(
      user = politician, 
      n = n_tweets, 
      include_rts = FALSE
    ) %>% filter(
      is_retweet == FALSE
    ) %>% select(
      text
    ) %>% mutate(
      politician = politician,
      party = party_i
    )
    
    #if (nrow(tweets_politician) < 0.05 * n_tweets) {
    #  print(paste("Error: API Issue. Only ", nrow(tweets_politician), " tweets fetched for", politician))
    #  stop()
    #}
    
    tweets <- rbind(tweets , tweets_politician)
  }
}
tweets$politician <- as.factor(tweets$politician)
tweets$party <- as.factor(tweets$party)

# basic cleaning
tweets$text_raw <- tweets$text
tweets$text <- clean_tweets(tweets$text) 
tweets <- tweets %>% filter(
  text != '',
  
)



# Group Summarise ---------------------------------------------------------

tweets_n_par <- tweets %>% group_by(
  party
) %>% summarise(
  n_tweets = n()
)

tweets_n_pol <- tweets %>% group_by(
  party,
  politician
) %>% summarise(
  n_tweets = n()
)
tweets_n_pol

# Sentiment Analysis ------------------------------------------------------

#summary(tweets)
#head(tweets)

tweets_sentiment <- get_nrc_sentiment(
  tweets$text,
  language = "english",
  cl = NULL
)

tweets <- cbind(tweets, tweets_sentiment)

#tweets$anger <- ifelse(tweets$anger >= 1, 1, 0)

tweets_politicians_sum <-  tweets %>% group_by(
  party
  ,politician
) %>% summarise_at(
  vars(anger:positive),
  mean,
  na.rm = TRUE
)

tweets_party_sum <-  tweets %>% group_by(
  party
) %>% summarise_at(
  vars(anger:positive),  
  mean,
  na.rm = TRUE
)


ggplot(
  data = tweets_party_sum %>% select(
    -anger,
    -anticipation,
    -fear) 
  %>% melt(
    id = 'party'
  ),
  aes(x = variable, y = value, col = party, group = party)
) + geom_point(
) + geom_line() +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("red", "blue"), labels = c('Republican Party', 'Democratic Party')) +
  labs(color = '', x = 'Selected Assessed Sentiments', y = 'Mean Sentiment Level per Tweet') +
  ggtitle("Party Sentiment Analysis ", subtitle = "50 Tweets per United States Party Leader Account")

