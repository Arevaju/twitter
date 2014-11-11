library(tm)
library(streamR)
library(maps)
library(ggplot2)
setwd('')
consumer_key <- "#########################"
consumer_secret <- "#########################"
access_token <- "#########################"
access_secret <- "#########################"
twitCred <- setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
save(list="twitCred", file="twitteR_credentials")
###Loading credentials
load('twitteR_credentials')
####
tweets <- filterStream(file.name='jobseuropeos.json', 
                       locations = c(-10.45, 35.94, 3.27, 62.74),
                       track = c("job", "vacancy","job vacancy"), 
                    #   follow = c("91109236", "38261757", "38261757","38720614"),
                       timeout=345600, oauth=twitCred) 

tweets.df <- parseTweets("jobseuropeos.json", verbose = FALSE)



countries <- c('Austria','België','Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Danmark', 'Estonia',
            'Finland','France','Deutschland',  'Greece',  'Hungary',  'Ireland','Italy', 'Latvia',     
              'Lithuania', 'Luxembourg','Malta',  'Polska',  'Portugal', 'Romania', 'Slovakia','Slovenia',  
             'España',  'Sweden', 'The Nederland',  'United Kingdom')
  
data <- tweets.df[grep (paste(countries, collapse="|"), tweets.df$country),]


### Mapping Tweets####
map.data <- map_data("world")
#map.data <- subset (map.data, map.data$region == "Europe")
points <- data.frame(x = as.numeric(data$place_lon), y = as.numeric(data$place_lat))

ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "black", 
                            color = "grey50", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) +
  scale_x_continuous(limits=c(-10.106505,25.508008)) + scale_y_continuous(limits=c(39.27528,58.73945)) +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_rect( fill='black', color='black'), panel.border = element_blank(), 
        panel.grid.major = element_line(color='black'), panel.grid.minor = element_line(color='black'),
        plot.background = element_rect( fill='black', color='black'), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
                                                                                 aes(x = x, y = y), size = 1, alpha = 1/5, color = "white")

###Make graph###############
counts <- table(as.character(data$country))
barplot(counts, main="Jobs distribution based on number of tweets", 
        xlab="Countries", ylab="Number of tweets",
        space=c(0.2,0.8),  
        col = 'blue', 
        cex.names = 0.7)


### Write csv file ##########################
write.csv(tweets.df, file="tweetjobeurope.csv", row.names=FALSE)

