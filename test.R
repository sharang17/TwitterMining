# required pakacges
library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)


access_token<-"417969461-AvBi9h9Hwclo99tvU67pzZrFD1AZa5noGQKSOC4S"
access_secret<-"KTJLlZqw5NOPmUQS30o5PCSx8qU8BTt5vpmJW0Z8bnHtX"
consumer_key<-"qjDp9xo9oXFdZoesGwLtltrxf"
consumer_secret<-"x0CzssloeeY4RusbSRN0KKNDBnJyxkgD0CZ2CD9N6OFXyuJ0dx"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

some_txt<-searchTwitter("#donaldtrump",n=100)

# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)

# classify emotion
class_emo = classify_emotion(some_txt, algorithm="voter")
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
