#...... ui for Sentiment Analysis tab .....
tabsetPanel(
tabPanel(
  
  'Sentiment Analysis',
  
  fluidRow(
    
  #  column( 3, textInput( 'tab1_query', label = 'Topic', value = 'Machine Learning' ) ),
    
    column( 3, numericInput( 'tab1_no_of_tweets_to_fetch', label = 'Max. Number of Tweets', value = 100 ) ),
    
    column( 3, dateInput( 'tab1_from_date', label = 'From Date', value = Sys.Date() - 365 ) ),
    
    column( 3, dateInput( 'tab1_to_date', label = 'To Date', value = Sys.Date() ) ),
    
  #  column( 3, textInput( 'tab1_location', label = 'Country', value = 'USA' ) ),
    
    br(), br(), br(), br(),
    
    actionButton( 'tab1_submit_button', 'Submit', class = 'fa fa-flag fa-2x pull-right',
                  
                  lib = 'font-awesome', width = '10%' )
    
  ),
 
 br(),
  
  fluidRow(
    
    column( 6, plotOutput( 'tab1_pie_chart', click = 'plot_click', hover = 'plot_hover', brush = 'plot_brush' ) ),
    
    column( 6, plotOutput( 'tab1_bar_plot', click = 'plot_click', hover = 'plot_hover', brush = 'plot_brush' ) )
    
  ),
  
  br(),
  
  fluidRow(
    
    plotOutput( 'tab1_treemap', click = 'plot_click', hover = 'plot_hover', brush = 'plot_brush' )
    
  ),
  
  br(),
  
  fluidRow(
    
    dataTableOutput( 'tab1_fetched_tweets_df' )
    
  )
  
),

tabPanel(
  
  'Word Cloud',
  
  selectInput("obs_senttyp", "Choose sent type:",
              c("Positive_Sentiment",
                "Negative_Sentiment",
                "Neutral_Sentiment")),
  
  column( 12, plotOutput( 'pos_wc' ) )
  
),

tabPanel(
  
  'US Map',
  
  actionButton( 'world_map_submit_button', 'Submit', class = 'fa fa-flag fa-2x pull-right',
                
                lib = 'font-awesome', width = '10%' ),
  br(),
  br(),
  
  
  fluidRow(
    
    plotlyOutput( 'world_map' )
    
  ),
  
  br(),
  
  fluidRow(
    
    dataTableOutput( 'tab2_fetched_user_info_df1' )
    
  )
  
 ),

tabPanel(
  
  'Web Review Insights',
  
  fluidRow(
    
    # column( 3, numericInput( 'consumer_insights_page_count', label = 'Max. Pages to Fetch', value = 1 ) ),
    
    actionButton( 'consumer_insights_submit_button', 'Submit', class = 'fa fa-flag fa-2x pull-right',
                  
                  lib = 'font-awesome', width = '10%' )
    
  ),

  
  br(),
  
  fluidRow(
    
    column( 6, plotOutput( 'consumer_insights_emotion', click = 'plot_click', hover = 'plot_hover', brush = 'plot_brush' ) ),
    
    column( 6, plotOutput( 'consumer_insights_polarity', click = 'plot_click', hover = 'plot_hover', brush = 'plot_brush' ) )
    
  ),
  
  br(),
  
  fluidRow(
    
    dataTableOutput( 'consumer_insights_df' )
    
  )
  
),

tabPanel(
  
  'Facebook Insights',
  
  fluidRow(
    
    # column( 3, numericInput( 'consumer_insights_page_count', label = 'Max. Pages to Fetch', value = 1 ) ),
    
    actionButton( 'fb_insights_submit_button', 'Submit', class = 'fa fa-flag fa-2x pull-right',
                  
                  lib = 'font-awesome', width = '10%' )
    
  ),
  
  
  br(),
  
  fluidRow(
    
    column( 6, plotOutput( 'fb_pie_chart', click = 'plot_click', hover = 'plot_hover', brush = 'plot_brush' ) ),
    
    column( 6, plotOutput( 'fb_insights_polarity', click = 'plot_click', hover = 'plot_hover', brush = 'plot_brush' ) )
    
  ),
  
  br(),
  
  fluidRow(
    
    plotOutput( 'fb_treemap', click = 'plot_click', hover = 'plot_hover', brush = 'plot_brush' )
    
  ),
  
  br(),
  
  fluidRow(
    
    dataTableOutput( 'fb_insights_df' )
    
  )
  
)

)

