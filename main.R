#.... Output of R code .......

to_stop_output = if( file.exists('.Rhistory') ) file.remove('.Rhistory')  #..... removing history files

rm( list = ls() )

setwd( 'G:/Zreyas Technology Project/watchdog' )

source( 'SourceOfSource.R' )

#....... JSON processing .......

warning_info = NULL ; error_info = NULL ; general_info = NULL

json0 = suppressWarnings( lapply( readLines( 'current_json.json', n=1L ), fromJSON ) )     #............ JSON input of data

if( length( json0 ) > 0 ){
  
  error_info = c( error_info, paste0( "JSON is blank" ) )
  
  input_json = json0[[1]]
  
} else { input_json = NULL }

#........ Calcuation ....

subassembly_instance_list = names( input_json[['subassemblies']] )

#...... Checking whether json is null or not .....

if( length( subassembly_instance_list ) == 0 ) { error_info = c( error_info, paste0( "Subassembly instances missing" ) ) }

i = 1

for( i in 1:length( subassembly_instance_list ) ){
  
  subassembly_instance = subassembly_instance_list[i]
  
  collector_names = names( input_json[["subassemblies"]][[subassembly_instance]][["collectors"]] )
  
  general_info = c( general_info, paste0( subassembly_instance, '-------- collectors present --------', paste( collector_names, collapse = ',') ) )
  
}

