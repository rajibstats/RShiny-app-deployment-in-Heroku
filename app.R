library( shiny )

source( 'SourceOfSource.R' )

ui = navbarPage(
  
  theme = 'bootstrap_slate.css',
  
  title = 'Sentiment Analytics',
  
  #..... Include ui for each tab .....
  
  source( file.path( "ui", "ui_Sentiment_Analysis.R" ),  local = T )$value    #..... ui for sentiment analysis tab

)

server = function( input, output, session ){
  
  #..... Include server logic for each tab .....
  
  source( file.path( 'server', 'server_Sentiment_Analysis.R' ), local = T )$value     #...... server logic for sentiment analysis tab
  
}

shinyApp( ui = ui, server = server )

