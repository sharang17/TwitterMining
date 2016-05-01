#GitHub-sharang17 SHA_256
#Commmit 34

library(twitteR)
library(plyr)
library(stringr)
library(ggvis)
library(ggplot2)
library(memoise)
library(gridExtra)
library(wordcloud)
library(sentiment)
options(shiny.trace=TRUE)

n_tweets <- 500
n_summary <- 10

access_token<-"417969461-AvBi9h9Hwclo99tvU67pzZrFD1AZa5noGQKSOC4S"
access_secret<-"KTJLlZqw5NOPmUQS30o5PCSx8qU8BTt5vpmJW0Z8bnHtX"
consumer_key<-"qjDp9xo9oXFdZoesGwLtltrxf"
consumer_secret<-"x0CzssloeeY4RusbSRN0KKNDBnJyxkgD0CZ2CD9N6OFXyuJ0dx"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

shinyServer(function(input, output, session) {
  # Define a reactive expression for the document term matrix
  
  

  tryTolower = function(x){
    # create missing value
    # this is where the returned value will be
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error = function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }

  score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
  {
    scores = laply(sentences, function(sentence, pos.words, neg.words) {

      # clean up sentences with R's regex-driven global substitute, gsub():
      sentence = gsub('[[:punct:]]', '', sentence)
      sentence = gsub('[[:cntrl:]]', '', sentence)
      sentence = gsub('\\d+', '', sentence)
      # and convert to lower case:
      #sentence = tolower(sentence)

      # split into words. str_split is in the stringr package
      word.list = str_split(sentence, '\\s+')
      # sometimes a list() is one level of hierarchy too much
      words = unlist(word.list)

      # compare our words to the dictionaries of positive & negative terms
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)

      # match() returns the position of the matched term or NA
      # we just want a TRUE/FALSE:
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)

      # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
      score = sum(pos.matches) - sum(neg.matches)

      return(score)
    }, pos.words, neg.words, .progress=.progress )

    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
  }

  cleanFun <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
  }

  get_source <- function(x){
    X <- cleanFun(x[["statusSource"]])
    X
    }

  tweets_df <- reactive({
    # Change when the "update" button is pressed...
    input$plot_feel
    
    isolate({
      withProgress({
        setProgress(message = "Processing sentiment...")

        if(input$lang=="All")
        tweets <- searchTwitter(input$source1, n=n_tweets)
        else
        tweets <- searchTwitter(input$source1, n=n_tweets, lang=input$lang)
        tweets1.df<<-tweets
        tweets <- strip_retweets(tweets, strip_manual=TRUE, strip_mt=TRUE)

         df <- twListToDF(tweets)

         df$Search <- input$source1

         if( (input$show_source2 == TRUE) && (input$source2 != ""))
         {
           if(input$lang=="All")
             tweets2 <- searchTwitter(input$source2, n=n_tweets)
           else
           tweets2 <- searchTwitter(input$source2, n=n_tweets, lang=input$lang)
           #tweets2.df<<-tweets2  
           tweets2 <- strip_retweets(tweets2, strip_manual=TRUE, strip_mt=TRUE)
           df2 <- twListToDF(tweets2)
           df2$Search <- input$source2
           df <- rbind(df, df2)
           tweets <- c(tweets, tweets2)
           #create word cloud if compare is enabled
           output$wctext2<-renderText({input$source2})
           output$wcplot2<-renderPlot({
             wordcloudentity(tweets2)})
           #emo and pol classification if compare is enabled 
           
 #          output$emotext2<-renderText({input$source2})
  #         output$emoplot2<-renderPlot({
   #          emo(tweets2)
          
           #})
         }


  df$Date <- format(df$created,'%m/%d/%Y %H:%I:%S')
  df$Source <-  apply(df, 1, get_source)

  sentences <- sapply(df$text, function(x) tryTolower(x))

  scores <- score.sentiment(sentences, pos.words, neg.words)
  df <- cbind(df, scores)

  df <- df[, c("id", "text", "Source", "Date", "Search", "created", "score")]
  names(df) <- c("id", "Post", "Source", "Date", "Search", "created", "score")



  df
      })
    })
  })
#----------------------------------------------------------------------------------------


cleantweets<-function(tweets)
{
  tweets<-tweets$text
  
  tweets<-gsub("[ \t]{2,}", "", tweets)
  tweets<-gsub("^\\s+|\\s+$", "", tweets)   
  # Get rid of URLs
  tweets<-gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", tweets)
  # Take out retweet header, there is only one
  tweets <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets)
  # Get rid of hashtags
  tweets <-gsub(" #\\S*","",tweets)
  # Get rid of references to other screennames
  tweets <- gsub("@\\w+", "", tweets)
  return(tweets)
}

#Create wordcloud
wordcloudentity<-function(tweets)
{
  tweets<-twListToDF(tweets)
  tweets<-cleantweets(tweets)
  tweets<-gsub("[^[:alnum:]]"," ",tweets)
  tweetCorpus<-Corpus(VectorSource(tweets))
  tweetTDM<-TermDocumentMatrix(tweetCorpus,control=list(removePunctuation=TRUE,
                                                        stopwords=c(stopwords('english')),
                                                        removeNumbers=TRUE,tolower=TRUE))
  tdMatrix <- as.matrix(tweetTDM) # creating a data matrix
  sortedMatrix<-sort(rowSums(tdMatrix),decreasing=TRUE) # calculate row sum of each term and sort in descending order (high freq to low)
  cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)#extracting names from named list in prev command and binding together into a dataframe with frequencies - called cloudFrame, names in separate columns
  
  wcloudentity<-wordcloud(cloudFrame$word,cloudFrame$freq,max.words=Inf, colors=brewer.pal(8,"Dark2"),scale=c(5,0.0002), random.order=TRUE)
  print(wcloudentity)
}

output$wctext1<-renderText({input$source1})
output$wcplot1<-renderPlot({
  wordcloudentity(tweets1.df)
  }
  )

  
#emotion classification
emo<-function(tweets){
  tweets<-twListToDF(tweets)
  tweets<-tweets$text
  # remove retweet entities
  tweets<-gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets)
  # remove at people
  tweets<- gsub("@\\w+", "", tweets)
  # remove punctuation
  tweets<-gsub("[[:punct:]]", "", tweets)
  # remove numbers
  tweets<-gsub("[[:digit:]]", "", tweets)
  # remove html links
  tweets<-gsub("http\\w+", "", tweets)
  # remove unnecessary spaces
  tweets<-gsub("[ \t]{2,}", "", tweets)
  tweets<-gsub("^\\s+|\\s+$", "", tweets)
  
  # classify emotion
  class_emo = classify_emotion(tweets, algorithm="bayes")
  #class_emo<-as.data.frame(class_emo)
  # get emotion best fit
  emotion = class_emo[,7]
  # substitute NA's by "unknown"
  emotion[is.na(emotion)] = "unknown"
  
  
  # classify polarity
  class_pol = classify_polarity(tweets, algorithm="voter")
  #class_pol<-as.data.frame(class_pol)
  # get polarity best fit
  polarity = class_pol[,4]
  
  sent_df<-within(sent_df,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
  

  ggplot(sent_df, aes(x=emotion)) +
    geom_bar(aes(y=..count.., fill=emotion)) +
    scale_fill_brewer(palette="Dark2") +
    labs(x="Emotion/Polarity categories", y="number of tweets") 
  
  
  
  # plot distribution of polarity
  ggplot(sent_df, aes(x=polarity)) +
    geom_bar(aes(y=..count.., fill=polarity)) +
    scale_fill_brewer(palette="RdGy") +
    labs(x="Emotion/Polarity categories", y="number of tweets") 
     
}

output$Img1<-renderImage({
  outfile <- tempfile(fileext='.png')
  png(outfile, width=400, height=300)
  
  list(src = "plot.png",
       contentType = 'image/png',
       width = 600,
       height = 350,
       alt = "This is alternate text")
},deleteFile=FALSE)

output$Img2<-renderImage({
  outfile <- tempfile(fileext='.png')
  png(outfile, width=400, height=300)
  
  list(src = "plot2.png",
       contentType = 'image/png',
       width = 600,
       height = 350,
       alt = "This is alternate text")
},deleteFile = FALSE)

output$emotext1<-renderText({input$source1})

#output$emoplot1<-renderPlot({
 # emo(tweets1.df)
#})










#----------------------------------------------------------------------------------------

  output$plot <- renderPlot({
    df <- tweets_df()
    sources <- df$Source
    sources <- sapply(sources, function(x) ifelse(length(x) > 1, x[2], x[1]))
    source_table <- table(sources)
    s_t <- source_table[source_table > 10]
    pie(s_t, col = rainbow(length(s_t)))

  })

  output$trends <- renderPlot({
    df <- tweets_df()

    source1 <- df[df$Search==input$source1,]
   p1 <- ggplot(source1, aes(x=created, y=score)) + geom_point(shape=1, size=0)+geom_smooth(se=F)+labs(title=input$source1, x = "Date /Time", y = "Popularity") + ylim(-5, 5)

    if( (input$show_source2 == TRUE) && (input$source2 != ""))
    {
      source2 <- df[df$Search==input$source2,]

      p2 <- ggplot(source2, aes(x=created, y=score)) + geom_point(shape=1, size=0)+geom_smooth(se=F)+labs(title=input$source2, x = "Date /Time", y = "Popularity") + ylim(-5, 5)
      grid.arrange(p1, p2, nrow=1, ncol=2)
    }
    else
      print(p1)

  })

  output$twitter_view <- renderPrint({
    if( (input$show_source2 == TRUE) && (input$source2 != ""))
    cat(paste(input$source1, " vs. ", input$source2))
   else
    cat(input$source1)
  })

  output$view <- renderTable({
    df <- tweets_df()
    df <- df[df$Search==input$source1,]
    head(df, n = n_summary, addrownums=F)
  })

  output$vs_view <- renderTable({

    if( (input$show_source2 == TRUE) && (input$source2 != ""))
    {
    df <- tweets_df()
    df <- df[df$Search==input$source2,]
    head(df, n = n_summary, addrownums=F)
    }
  })


  # Function for generating tooltip text
  movie_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$id)) return(NULL)

    all_tweets <- isolate(tweets_df())
    tweet <- all_tweets[all_tweets$id == x$id, ]

    paste0("<b>", tweet$Post, "</b><br><em><small>from ", tweet$Source, " (", tweet$Date, ")</small></em>")
  }


  # A reactive expression with the ggvis plot
  vis2 <- reactive({

    df <- tweets_df()

   df[df$Search==input$source2,] %>%  ggvis(~created, ~score) %>% layer_points(fill = ~Search, key := ~id)  %>% layer_lines(stroke=~Search) %>% add_legend(c("fill", "stroke"), orient="left") %>% add_axis("x", title = "Date Time") %>% add_axis("y", title = "Popularity") %>% set_options(width = 800, height = 300) %>% add_tooltip(movie_tooltip, "click")

   if( (input$show_source2 != TRUE) || (input$source2 == "") )
     invisible()
  })

  vis <- reactive({
    legend_val <- c(input$source1)
    if( (input$show_source2 == TRUE) && (input$source2 != ""))
      legend_val <- c(input$source1, input$source2)

    df <- tweets_df()

    df %>%  ggvis(~created, ~score) %>% layer_points(fill = ~Search, key := ~id)  %>% layer_lines(stroke=~Search) %>% add_legend(c("fill", "stroke"), orient="left") %>% add_axis("x", title = "Date Time") %>% add_axis("y", title = "Popularity") %>% set_options(width = 800, height = 300) %>% add_tooltip(movie_tooltip, "click")
  })

 vis %>% bind_shiny("plot1")

})
