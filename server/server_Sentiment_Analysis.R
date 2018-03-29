#..... Server logic for Sentiment Analysis tab .....

#.......... Analysis ............
  
dataInput = eventReactive( input$tab1_submit_button , {
  
  twitter_query = '@BBT' # input$tab1_query
  
  number_of_tweets_to_fetch = input$tab1_no_of_tweets_to_fetch
  
  from_date = as.character( input$tab1_from_date )
  
  to_date = as.character( input$tab1_to_date )
  
 # location_value = input$tab1_location
  
  sentiment_output = Sentiment_Output( twitter_query, number_of_tweets_to_fetch, from_date, to_date )    #... contains "sentiment_score_df" and "sentiment_distribution"
  
  sentiment_info = c( sentiment_output, list( 'twitter_query' = twitter_query, 'number_of_tweets_to_fetch' = number_of_tweets_to_fetch ) )
  
  return( sentiment_info )
  
})


#...... Output filtered by comment type ......

output$tab1_fetched_tweets_df = renderDataTable({
  
  sentiment_score_df = dataInput()$sentiment_score_df
  
  Fetched_Tweets_Sentiment_DF( sentiment_score_df )
  
})


#...... Pie Chart .......

output$tab1_pie_chart = renderPlot({
  
  sentiment_distribution = dataInput()$sentiment_distribution
  
  twitter_query = dataInput()$twitter_query
  
  Tweet_Sentiment_Pie_Chart( sentiment_distribution, twitter_query )
  
})


#...... Bar Chart ......

output$tab1_bar_plot = renderPlot({
  
  sentiment_distribution = dataInput()$sentiment_distribution
  
  twitter_query = dataInput()$twitter_query
  
  Tweet_Sentiment_Bar_Plot( sentiment_distribution, twitter_query )
  
})


#...... Tree Map ......

output$tab1_treemap = renderPlot({
  
  sentiment_distribution = dataInput()$sentiment_distribution
  
  twitter_query = dataInput()$twitter_query
  
  Tweet_Sentiment_Treemap( sentiment_distribution, twitter_query )
  
})




#.... user_info .....

secnd_tab = eventReactive( input$world_map_submit_button, {
  
  twitter_query = '@BBT' # input$tab1_query
  
  number_of_tweets_to_fetch = input$tab1_no_of_tweets_to_fetch
  
  from_date = as.character( input$tab1_from_date )
  
  to_date = as.character( input$tab1_to_date )
  
  user_info=as.data.frame(user_table(twitter_query,number_of_tweets_to_fetch,from_date,to_date))
  
  user_info_extract = user_info[,c("name","location","followersCount")]
 
  user_info_extract
  
})

######## world_map ########

secnd_tab_map = eventReactive( input$world_map_submit_button, {
  
  twitter_query = '@BBT' # input$tab1_query
  
  number_of_tweets_to_fetch = input$tab1_no_of_tweets_to_fetch
  
  from_date = as.character( input$tab1_from_date )
  
  to_date = as.character( input$tab1_to_date )
  
  wmap=(world_map(twitter_query,number_of_tweets_to_fetch,from_date,to_date))

  locations <- geocode(wmap) 
  #with(locations, plot(lon, lat))
  
  worldMap <- map_data("usa")  # Easiest way to grab a world map shapefile
  
  zp1 <- ggplot(worldMap)
  zp1 <- zp1 + geom_path(aes(x = long, y = lat, group = group),  # Draw map
                         colour = gray(2/3), lwd = 1/3)
  zp1 <- zp1 + geom_point(data = locations,  # Add points indicating users
                          aes(x = lon, y = lat),
                          colour = "RED", alpha = 1/2, size = 1)
  zp1 <- zp1 + coord_equal()  # Better projections are left for a future post
  zp1 <- zp1 + theme_minimal()  # Drop background annotations
  #print(zp1)
  ggplotly(zp1)
  
})



###### world_map #######


output$world_map=renderPlotly({
  secnd_tab_map()  
})



###### user_info ######
output$tab2_fetched_user_info_df1=renderDataTable({
  secnd_tab()
})



###### word cloud #######

output$pos_wc=renderPlot({
  
  set.seed(1234)
  
  twitter_query = '@BBT' # input$tab1_query
  
  number_of_tweets_to_fetch = input$tab1_no_of_tweets_to_fetch
  
  from_date = as.character( input$tab1_from_date )
  
  to_date = as.character( input$tab1_to_date )
  
sentiment_output = (Sentiment_Output_wc( twitter_query, number_of_tweets_to_fetch, from_date, to_date ))    #... contains "sentiment_score_df" and "sentiment_distribution"

pos=filter(sentiment_output, score>0)
neg=filter(sentiment_output, score < 0)
neu=filter(sentiment_output, score==0)

if(input$obs_senttyp=="Positive_Sentiment"){
text=Corpus(VectorSource(pos$text))
#inspect(text)[1:10]


text_data<-tm_map(text,removeWords, stopwords("english"))

tdm_text_data<-TermDocumentMatrix (text_data) #Creates a TDM

TDM1<-as.matrix(tdm_text_data) #Convert this into a matrix format

v = sort(rowSums(TDM1), decreasing = TRUE) #Gives you the frequencies for every word

# wordcloud (text_data, scale=c(5,0.5), max.words=50, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

d <- data.frame(word = names(v),freq=v)

d1=head(d, n=100)

wordcloud(words = d1$word, freq = d1$freq, min.freq = 1,
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

}else if(input$obs_senttyp=="Negative_Sentiment"){
  text=Corpus(VectorSource(neg$text))
  
  text_data<-tm_map(text,removeWords, stopwords("english"))
  
  tdm_text_data<-TermDocumentMatrix (text_data) #Creates a TDM
  
  TDM1<-as.matrix(tdm_text_data) #Convert this into a matrix format
  
  v = sort(rowSums(TDM1), decreasing = TRUE) #Gives you the frequencies for every word
  
  #wordcloud (text_data, scale=c(5,0.5), max.words=10, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

  d <- data.frame(word = names(v),freq=v)
  
  d1=head(d, n=100)
  #wordcloud (text_data, scale=c(5,0.5), max.words=50, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
  wordcloud(words = d1$word, freq = d1$freq, min.freq = 1,
            random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
}else if(input$obs_senttyp=="Neutral_Sentiment"){
  text=Corpus(VectorSource(neu$text))
  #inspect(text)[1:10]
  
  
  text_data<-tm_map(text,removeWords, stopwords("english"))
  
  tdm_text_data<-TermDocumentMatrix (text_data) #Creates a TDM
  
  TDM1<-as.matrix(tdm_text_data) #Convert this into a matrix format
  
  v = sort(rowSums(TDM1), decreasing = TRUE) #Gives you the frequencies for every word
  
  #wordcloud (text_data, scale=c(5,0.5), max.words=50, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

  d <- data.frame(word = names(v),freq=v)
  
  d1=head(d, n=100)
  #wordcloud (text_data, scale=c(5,0.5), max.words=50, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
  
  wordcloud(words = d1$word, freq = d1$freq, min.freq = 1,
            random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))

}
})

#..... consumer insights .....
if( F ){
Consumer_insights_info = eventReactive( input$consumer_insights_submit_button, {
  
  # page_count = input$consumer_insights_page_count
  
  result = Consumer_insights_sentiment()
  
  result
  
})
}

output$consumer_insights_df = renderDataTable({
  
  # Consumer_insights_info()$final_dataframe
  
  final_dataframe
  
})


#..... consumer insights emotion graph .....

output$consumer_insights_emotion = renderPlot({
  
  ggplot(final_dataframe, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
    scale_fill_brewer(palette="Dark2") + ggtitle('Sentiment Analysis of BB&T reviews') +
    theme(legend.position='right') + ylab('Number of Tweets') + xlab('Emotion Categories')
  
  
})


#..... consumer insights polarity graph .....

output$consumer_insights_polarity = renderPlot({
  
  ggplot(final_dataframe, aes(x=polarity))+geom_bar(aes(y=..count.., fill=polarity)) +
    scale_fill_brewer(palette="Paired") + ggtitle('Sentiment Analysis of BB&T reviews') +
    theme(legend.position='right') + ylab('Number of Tweets') + xlab('Polarity Categories') 
  
})



#.... Facebook Insights ....

output$fb_insights_df = renderDataTable({
  
  fb_sentiment_df
  
})


#..... fb insights emotion graph .....

output$fb_insights_emotion = renderPlot({
  
  ggplot(fb_sentiment_df, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
    scale_fill_brewer(palette="Dark2") + ggtitle('Sentiment Analysis of BBTBank') +
    theme(legend.position='right') + ylab('Number of Comments') + xlab('Emotion Categories')
  
  
})


#..... fb insights polarity graph .....

output$fb_insights_polarity = renderPlot({
  
  ggplot(fb_sentiment_df, aes(x=sentiment))+geom_bar(aes(y=..count.., fill=sentiment)) +
    scale_fill_brewer(palette="Spectral") + ggtitle('Sentiment Analysis of BBTBank') +
    theme(legend.position='right') + ylab('Number of Posts') + xlab('Polarity Categories') 
  
})


#...... Pie Chart fb .......

output$fb_pie_chart = renderPlot({
  
  Tweet_Sentiment_Pie_Chart( fb_sentiment_distribution, 'BBTBank' )
  
})


#...... Tree Map ......

output$fb_treemap = renderPlot({
 
  Tweet_Sentiment_Treemap( fb_sentiment_distribution, 'BBTBank' )
  
})
