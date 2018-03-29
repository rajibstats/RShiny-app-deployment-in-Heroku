#....... Packages ......

library( twitteR ) ; library( RCurl ) ; library( RJSONIO ) ; library( stringr ) ; library( ggplot2 ) ; library( treemap )

library( ggthemes ) ; library( ggmap ) ; library( plotly ) ; library( tm ) ; library( wordcloud ) ; library( RColorBrewer )

library( rvest ) ; library( dplyr ) ; library( plyr ) ; library( stringr ) 



#....... Loading lexicon of positive and negative words (from Neal Caren) .......

lexicon = read.csv( 'lexicon.csv' , stringsAsFactors = F )

sentiment_dataframe = read.csv( 'sentiment_dataframe.csv', stringsAsFactors = F )

final_dataframe = read.csv( 'final_dataframe.csv', stringsAsFactors = F )

fb_sentiment_df = read.csv( 'fb_sentiment_dataframe.csv', stringsAsFactors = F )

fb_sentiment_distribution = read.csv( 'fb_sentiment_distribution.csv', stringsAsFactors = F )

#....... Scripts defined by us .......

source( 'Twitter_Authentication.R' )       #....... twitter authentication

source( 'logic_Sentiment_Analysis.R' )     #....... url related functions


