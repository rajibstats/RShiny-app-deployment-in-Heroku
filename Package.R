rm( list = ls() )

options( warn = -1 )    #..... To suppress all warning messages globally

required_packages = list( 'twitteR', 'RCurl', 'RJSONIO', 'stringr', 'ggplot2', 'treemap', 'ggthemes' )

dummy = lapply( required_packages, function( x ){
  
  if( !( x %in% installed.packages() ) ) { install.packages( x, dependencies = T ) }
  
} )
