#...... Logic of Sentiment Analysis tab .........

#...... Function to check if there are any invalid multibyte string in a character vector ......

has.invalid.multibyte.string  <- function(x,return.elements=F)
{
  # determine if "invalid multibyte string" error will be triggered
  # if return.elements=T, then output is logical along x, otherwise single logical
  if (is.null(x))
    return(F)
  if (return.elements)
  {
    n <- length(x)
    out <- rep(F,n)
    for (i in 1:n)
      out[i] <- is.error(try(toupper(x[i]),silent = T))
  }
  else
    out <- is.error(try(toupper(x),silent = T))
  return(out)
}

is.error <- function(x)
{
  # test output of try()
  return(class(x)[1]=="try-error")
}

#........ Function to clean text .........

clean.text <- function(some_txt)
{  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)

# Remove the text which start from "@"
some_txt = gsub("@\\w+", "", some_txt)

# Remove punctuations
some_txt = gsub("[[:punct:]]", "", some_txt)

#Remove Digits
some_txt = gsub("[[:digit:]]", "", some_txt)

#Remove links
some_txt = gsub("http\\w+", "", some_txt)

# remove extra white spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)


# Remove non-english characters
some_txt = gsub("[^\x20-\x7E]", "", some_txt)

# define "tolower error handling" function
try.tolower = function(x)
{  y = NA
try_error = tryCatch(tolower(x), error=function(e) e)
if (!inherits(try_error, "error"))
  y = tolower(x)
return(y)
}

some_txt = sapply(some_txt, try.tolower)
some_txt = some_txt[some_txt != ""]
names(some_txt) = NULL
return(some_txt)}

#.......... Function for finding sentiment score ........

score.sentiment = function(sentences, pos.words, neg.words,negation.words, .progress='none')
{
  library(plyr)
  library(stringr)
  
  
  nscores = laply(sentences, function(sentence, pos.words, neg.words, negation.words) {
    
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # compare our words with list of negation words
    negation=match(words,negation.words)
    
    
    # match() returns the position of the matched term or NA
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    negation=!is.na(negation)
    
    # calculation of score
    score= sum(pos.matches)-sum(neg.matches)
    
    # calculation for negation handling
    score1=score
    if(score<0)
      nscore=score1+sum(negation)
    else 
      nscore=score1-sum(negation)
    return(nscore)
  }, pos.words, neg.words,negation.words, .progress=.progress )
  
  scores.df = data.frame(score=nscores, text=sentences)
  return(scores.df)
}


#...... Calculation .........

Sentiment_Output = function( twitter_query, number_of_tweets_to_fetch, from_date, to_date ){
  
 # lat_long = unlist( geocode( location_value ) )
  
#  geocode_formation = paste( lat_long[2], lat_long[1], '1000mi', sep = ',' )

  tweets_list = searchTwitter( twitter_query, n = number_of_tweets_to_fetch, lang = 'en',
                               
                               since = from_date, until = to_date )

  tweets_df = twListToDF( tweets_list )


  #....... Row indexes which has invalid multibyte stings .......

  invalid_row_indexes = which( has.invalid.multibyte.string( tweets_df$text, return.elements = T ) )
  
  if( length( invalid_row_indexes ) > 0 ){
    
    final_tweets_df = tweets_df[ -invalid_row_indexes, ]
    
  } else{ final_tweets_df = tweets_df }

  # tweets = final_tweets_df$text

  # duplicated_tweets_removed = tweets[ !duplicated( tweets ) ]

  # tweets_cleaned = clean.text( duplicated_tweets_removed )

  # tweets_cleaned_duplicates_removed = tweets_cleaned[ !duplicated( tweets_cleaned ) ]

  

  positive_words = lexicon$word[ lexicon$polarity == 'positive' ]

  negative_words = lexicon$word[ lexicon$polarity == 'negative' ]

  negation_words = NULL

  sentiment_score_df_0 = score.sentiment( final_tweets_df$text, positive_words, negative_words, negation_words )
  
  sentiment_score_df_1 = inner_join( sentiment_score_df_0, final_tweets_df, by = 'text' )
  
  sentiment_score_df_2 = sentiment_score_df_1[ c( 'created', 'screenName', 'text', 'score' ) ]
  
  sentiment_score_df = sentiment_score_df_2[ !duplicated( sentiment_score_df_2$text ), ]

  total_rows = nrow( sentiment_score_df )
  
  positive_score_percent = round( 100*length( which( sentiment_score_df$score > 0 ) )/total_rows, 2 )
  
  neutral_score_percent = round( 100*length( which( sentiment_score_df$score == 0 ) )/total_rows, 2 )
  
  negative_score_percent = round( 100*length( which( sentiment_score_df$score < 0 ) )/total_rows, 2 )
  
  sentiment_distribution = data.frame( values = c( positive_score_percent, neutral_score_percent, negative_score_percent ),
                                       
                                       sentiment = c( 'Positive', 'Neutral', 'Negative' ) )
  
  return( list( 'sentiment_score_df' = sentiment_score_df, 'sentiment_distribution' = sentiment_distribution ) )
  
}


#...... Tweets and Sentiment DF for output .........

Fetched_Tweets_Sentiment_DF = function( sentiment_score_df ){
  
  tweet_sentiment = ifelse( sentiment_score_df$score > 0, 'Positive', ifelse( sentiment_score_df$score == 0, 'Neutral', 'Negative' ) )
  
  sentiment_df = data.frame( 'Date_Created' = sentiment_score_df$created, 'Tweeted_By' = sentiment_score_df$screenName,
                             
                             'Tweets' = sentiment_score_df$text, 'Tweet_Sentiment' = tweet_sentiment )
  
  return( sentiment_df )
  
}


#...... Pie chart ........

#....... Function for donut and pie together ........

#' x      numeric vector for each slice
#' group  vector identifying the group for each slice
#' labels vector of labels for individual slices
#' col    colors for each group
#' radius radius for inner and outer pie (usually in [0,1])
#' plot_title title over plot

donuts <- function(x, group = 1, labels = NA, col = NULL, radius = c( 2.5, 3 ), title = NA ) {
  group <- rep_len(group, length(x))
  ug  <- unique(group)
  tbl <- table(group)[order(ug)]
  
  col <- if (is.null(col))
    seq_along(ug) else rep_len(col, length(ug))
  col.main <- Map(rep, col[seq_along(tbl)], tbl)
  col.sub  <- lapply(col.main, function(x) {
    al <- head(seq(0, 1, length.out = length(x) + 2L)[-1L], -1L)
    Vectorize(adjustcolor)(x, alpha.f = al)
  })
  
  plot.new()
  
  par(new = TRUE)
  pie(x, border = NA, radius = radius[2L],
      col = unlist(col.sub), labels = labels, main = title )
  
  par(new = TRUE)
  pie(x, border = NA, radius = radius[1L],
      col = unlist(col.main), labels = NA)
}

Tweet_Sentiment_Pie_Chart = function( sentiment_distribution, twitter_query ){
  
  plot_title = paste0( 'Pie Chart of Sentiment Analysis for ', twitter_query )
  
  return(
    
    with( sentiment_distribution, donuts( values, group = sentiment, labels = sentiment, radius = c( 0.7, 1 ),
                                          
                                          col = c( 'green', 'Blue', 'grey' ), title = plot_title ) )
    
  )
  
}


#....... Bar Plot ........

Tweet_Sentiment_Bar_Plot = function( sentiment_distribution, twitter_query ){
  
  plot_title = paste0( 'Bar Diagram of Sentiment Analysis for ', twitter_query )
  
  bar_plot_output = ggplot( sentiment_distribution, aes( x = sentiment, y = values, fill = sentiment ) ) +
    
    geom_bar( stat = 'identity', width = 0.6 ) + scale_fill_manual( values = c( "firebrick1", "orange", "green" ) ) + guides( fill = F ) +
    
    xlab( 'Sentiment' ) + ylab( 'Percentage' ) + ggtitle( plot_title ) +
    
    theme( plot.title = element_text( size = 12, colour = "firebrick", face = 'bold' ), axis.text = element_text( colour = "firebrick", face = 'bold' ), 
           
           axis.title.y = element_text( size = 12, face = 'bold' ), axis.title.x = element_text(size = 12, face = 'bold') ) # +
    
  #  theme_solarized_2( light = F ) + scale_colour_solarized( "black" )
  
  return( bar_plot_output )
  
}


#....... Treemap ........

Tweet_Sentiment_Treemap = function( sentiment_distribution, twitter_query ){
  
  plot_title = paste0( 'Treemap of Sentiment Analysis for ', twitter_query )
  
  return(
    
    treemap( sentiment_distribution, index = 'sentiment', vSize = 'values', vColor = 'values',
             
             type = 'value', title = plot_title )
    
  )
  
}



#...... user_informtion #..........
user_table=function(twitter_query, number_of_tweets_to_fetch, from_date, to_date){
  searchResults= searchTwitter( twitter_query, n = number_of_tweets_to_fetch, lang = 'en', since = from_date, until = to_date)
  tweetFrame <- twListToDF(searchResults)  # Convert to a nice dF
  
  userInfo <- lookupUsers(tweetFrame$screenName)  # Batch lookup of user info
  userFrame <- twListToDF(userInfo)  # Convert to a nice dF
  
  return(userFrame)
  
}



#........... world_map #......
world_map=function(twitter_query, number_of_tweets_to_fetch, from_date, to_date){
  searchResults= searchTwitter( twitter_query, n = number_of_tweets_to_fetch, lang = 'en', since = from_date, until = to_date)
  tweetFrame <- twListToDF(searchResults)  # Convert to a nice dF
  
  userInfo <- lookupUsers(tweetFrame$screenName)  # Batch lookup of user info
  userFrame <- twListToDF(userInfo)  # Convert to a nice dF
  
  locatedUsers=userFrame[!(userFrame$location==""),]
  
  clean.text <- function(some_txt)
  {  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  
  # Remove the text which start from "@"
  some_txt = gsub("@\\w+", "", some_txt)
  
  # Remove punctuations
  some_txt = gsub("[[:punct:]]", "", some_txt)
  
  #Remove Digits
  some_txt = gsub("[[:digit:]]", "", some_txt)
  
  #Remove links
  some_txt = gsub("http\\w+", "", some_txt)
  
  # remove extra white spaces
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  
  
  # Remove non-english characters
  some_txt = gsub("[^\x20-\x7E]", "", some_txt)
  
  # define "tolower error handling" function
  
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)}
  
  
  
  locatedUsers=clean.text(locatedUsers$location)
  return(locatedUsers)
  
  
}


##..... word_cloud #..........


Sentiment_Output_wc = function( twitter_query, number_of_tweets_to_fetch, from_date, to_date ){
  
  # lat_long = unlist( geocode( location_value ) )
  
  #  geocode_formation = paste( lat_long[2], lat_long[1], '1000mi', sep = ',' )
  
  tweets_list = searchTwitter( twitter_query, n = number_of_tweets_to_fetch, lang = 'en',
                               
                               since = from_date, until = to_date )
  
  tweets_df = twListToDF( tweets_list )
  
  
  #....... Row indexes which has invalid multibyte stings .......
  
  invalid_row_indexes = which( has.invalid.multibyte.string( tweets_df$text, return.elements = T ) )
  
  if( length( invalid_row_indexes ) != 0 ){
    
    final_tweets_df = tweets_df[ -invalid_row_indexes, ]
    
  } else{ final_tweets_df = tweets_df }
  
  tweets = final_tweets_df$text
  
  duplicated_tweets_removed = tweets[ !duplicated( tweets ) ]
  
  tweets_cleaned = clean.text( duplicated_tweets_removed )
  
  tweets_cleaned_duplicates_removed = tweets_cleaned[ !duplicated( tweets_cleaned ) ]
  
  
  
  positive_words = lexicon$word[ lexicon$polarity == 'positive' ]
  
  negative_words = lexicon$word[ lexicon$polarity == 'negative' ]
  
  negation_words = NULL
  
  sentiment_score_df = as.data.frame(score.sentiment( tweets_cleaned_duplicates_removed, positive_words, negative_words, negation_words ))
  
  return(sentiment_score_df)
  #return( data.frame( 'sentiment_score_df' = sentiment_score_df, 'sentiment_distribution' = sentiment_distribution ) )
  
}

Consumer_insights_sentiment <- function( ) {
  
  i = 1 ; final_df = list()
  
  for( i in 1:22 ){
    
    #Specifying the url for desired website to be scrapped
    
    url = paste0( 'https://www.consumeraffairs.com/finance/bbt_bank.html?page=', i, '#sort=recent&filter=none' )
    
    #Reading the HTML code from the website
    webpage <- read_html(url)
    
    
    #Using CSS selectors to scrap the description section
    description_data_html <- html_nodes(webpage,'.rvw-aut')
    
    #Converting the description data to text
    reviewer_details <- html_text(description_data_html)
    
    #Using CSS selectors to scrap the date section
    description_data_html1 <- html_nodes(webpage,'.ca-txt-cpt.ca-txt--clr-gray')
    
    #Converting the date data to text
    description_data1 <- html_text(description_data_html1)
    
    
    date_created<-substr(description_data1,18,nchar(description_data1))
    
    #Using CSS selectors to scrap the review section
    description_data_html2 <- html_nodes(webpage,'.rvw-bd.ca-txt-bd-2')
    
    #Converting the review data to text
    description_data2 <- html_text(description_data_html2)
    
    review<-substr(description_data2,31,nchar(description_data2))
    
    effective_length = min( length( reviewer_details ), length( date_created ), length( review ) )
    
    reviewer_details = reviewer_details[ 1:effective_length ]
    
    date_created = date_created[ 1:effective_length ]
    
    review = review[ 1:effective_length ]
    
    reviewdat<-cbind(reviewer_details,date_created,review)
    
    final_df[[i]] <-as.data.frame(reviewdat)
    
  }
  
  all_reviews = bind_rows( final_df )
  
  # clean text
  sentdata_clean = clean.text(all_reviews$review)
  
  
  # remove duplicated text after cleaning
  #sentdata_clean=as.data.frame(sentdata_clean[!duplicated(sentdata_clean)])
  names(sentdata_clean)=c("review")
  
  
  #Sentiment Analysis code starts from here
  
  
  # run model
  pbi_class_emo = classify_emotion(sentdata_clean, algorithm="bayes", prior=1.0)
  
  # Fetch emotion category best_fit for our analysis purposes, visitors to this tutorials are encouraged to play around with other classifications as well.
  emotion = pbi_class_emo[,7]
  
  # Replace NA's (if any, generated during classification process) by word "unknown"
  emotion[is.na(emotion)] = "unknown"
  
  # Polarity Classification
  pbi_class_pol = classify_polarity(sentdata_clean, algorithm="bayes")
  
  # we will fetch polarity category best_fit for our analysis purposes, and as usual, visitors to this tutorials are encouraged to play around with other classifications as well
  polarity = pbi_class_pol[,4]
  
  # Let us now create a data frame with the above results obtained and rearrange data for plotting purposes
  # creating data frame using emotion category and polarity results earlier obtained
  
  sentiment_dataframe = data.frame(text=sentdata_clean, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)
  
  # rearrange data inside the frame by sorting it
  sentiment_dataframe = within(sentiment_dataframe, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
  
  
  ## Final dataframe to show
  final_dataframe_draft = cbind(all_reviews,sentiment_dataframe)
  final_dataframe_0 = final_dataframe_draft[,-3]
  final_dataframe = final_dataframe_0[ !duplicated(final_dataframe_0$text), ]
  
  return( list( 'final_dataframe' = final_dataframe, 'sentiment_dataframe' = sentiment_dataframe ) )
  
}

classify_polarity = function (textColumns, algorithm = "bayes", pstrong = 0.5, pweak = 1, 
                              prior = 1, verbose = FALSE, ...) 
{
  matrix <- create_matrix(textColumns, ...)
  lexicon <- read.csv(system.file("data/subjectivity.csv.gz", 
                                  package = "sentiment"), header = FALSE)
  counts <- list(positive = length(which(lexicon[, 3] == "positive")), 
                 negative = length(which(lexicon[, 3] == "negative")), 
                 total = nrow(lexicon))
  documents <- c()
  for (i in 1:nrow(matrix)) {
    if (verbose) 
      print(paste("DOCUMENT", i))
    scores <- list(positive = 0, negative = 0)
    doc <- matrix[i, ]
    words <- findFreqTerms(doc, lowfreq = 1)
    for (word in words) {
      index <- pmatch(word, lexicon[, 1], nomatch = 0)
      if (index > 0) {
        entry <- lexicon[index, ]
        polarity <- as.character(entry[[2]])
        category <- as.character(entry[[3]])
        count <- counts[[category]]
        score <- pweak
        if (polarity == "strongsubj") 
          score <- pstrong
        if (algorithm == "bayes") 
          score <- abs(log(score * prior/count))
        if (verbose) {
          print(paste("WORD:", word, "CAT:", category, 
                      "POL:", polarity, "SCORE:", score))
        }
        scores[[category]] <- scores[[category]] + score
      }
    }
    if (algorithm == "bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]] + score
      }
    }
    else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]] + 1e-06
      }
    }
    best_fit <- names(scores)[which.max(unlist(scores))]
    ratio <- as.integer(abs(scores$positive/scores$negative))
    if (ratio == 1) 
      best_fit <- "neutral"
    documents <- rbind(documents, c(scores$positive, scores$negative, 
                                    abs(scores$positive/scores$negative), best_fit))
    if (verbose) {
      print(paste("POS:", scores$positive, "NEG:", scores$negative, 
                  "RATIO:", abs(scores$positive/scores$negative)))
      cat("\n")
    }
  }
  colnames(documents) <- c("POS", "NEG", "POS/NEG", "BEST_FIT")
  return(documents)
}

classify_emotion = function (textColumns, algorithm = "bayes", prior = 1, verbose = FALSE, 
                             ...) 
{
  matrix <- create_matrix(textColumns, ...)
  lexicon <- read.csv(system.file("data/emotions.csv.gz", package = "sentiment"), 
                      header = FALSE)
  counts <- list(anger = length(which(lexicon[, 2] == "anger")), 
                 disgust = length(which(lexicon[, 2] == "disgust")), fear = length(which(lexicon[, 
                                                                                                 2] == "fear")), joy = length(which(lexicon[, 2] == 
                                                                                                                                      "joy")), sadness = length(which(lexicon[, 2] == "sadness")), 
                 surprise = length(which(lexicon[, 2] == "surprise")), 
                 total = nrow(lexicon))
  documents <- c()
  for (i in 1:nrow(matrix)) {
    if (verbose) 
      print(paste("DOCUMENT", i))
    scores <- list(anger = 0, disgust = 0, fear = 0, joy = 0, 
                   sadness = 0, surprise = 0)
    doc <- matrix[i, ]
    words <- findFreqTerms(doc, lowfreq = 1)
    for (word in words) {
      for (key in names(scores)) {
        emotions <- lexicon[which(lexicon[, 2] == key), 
                            ]
        index <- pmatch(word, emotions[, 1], nomatch = 0)
        if (index > 0) {
          entry <- emotions[index, ]
          category <- as.character(entry[[2]])
          count <- counts[[category]]
          score <- 1
          if (algorithm == "bayes") 
            score <- abs(log(score * prior/count))
          if (verbose) {
            print(paste("WORD:", word, "CAT:", category, 
                        "SCORE:", score))
          }
          scores[[category]] <- scores[[category]] + 
            score
        }
      }
    }
    if (algorithm == "bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]] + score
      }
    }
    else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]] + 1e-06
      }
    }
    best_fit <- names(scores)[which.max(unlist(scores))]
    if (best_fit == "disgust" && as.numeric(unlist(scores[2])) - 
        3.09234 < 0.01) 
      best_fit <- NA
    documents <- rbind(documents, c(scores$anger, scores$disgust, 
                                    scores$fear, scores$joy, scores$sadness, scores$surprise, 
                                    best_fit))
  }
  colnames(documents) <- c("ANGER", "DISGUST", "FEAR", "JOY", 
                           "SADNESS", "SURPRISE", "BEST_FIT")
  return(documents)
}

create_matrix = function (textColumns, language = "english", minDocFreq = 1, 
                          minWordLength = 3, removeNumbers = TRUE, removePunctuation = TRUE, 
                          removeSparseTerms = 0, removeStopwords = TRUE, stemWords = FALSE, 
                          stripWhitespace = TRUE, toLower = TRUE, weighting = weightTf) 
{
  stem_words <- function(x) {
    split <- strsplit(x, " ")
    return(wordStem(split[[1]], language = language))
  }
  control <- list(language = language, tolower = toLower, removeNumbers = removeNumbers, 
                  removePunctuation = removePunctuation, stripWhitespace = stripWhitespace, 
                  minWordLength = minWordLength, stopwords = removeStopwords, 
                  minDocFreq = minDocFreq, weighting = weighting)
  if (stemWords == TRUE) 
    control <- append(control, list(stemming = stem_words), 
                      after = 6)
  trainingColumn <- apply(as.matrix(textColumns), 1, paste, 
                          collapse = " ")
  trainingColumn <- sapply(as.vector(trainingColumn, mode = "character"), 
                           iconv, to = "UTF8", sub = "byte")
  corpus <- Corpus(VectorSource(trainingColumn), readerControl = list(language = language))
  matrix <- DocumentTermMatrix(corpus, control = control)
  if (removeSparseTerms > 0) 
    matrix <- removeSparseTerms(matrix, removeSparseTerms)
  gc()
  return(matrix)
}

