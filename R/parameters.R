message( paste0( rep( '-', 100 ), collapse = '' ) )
message( '\tSetting parameters' )

options( stringsAsFactors = FALSE )

par <- new.env()
par$results <- 'results/'
par$RData <- 'RData/'
par$data <- 'data/'

par$src_info <- '/home/aju/Development/COVID-19/csse_covid_19_data/csse_covid_19_time_series/'

# Folder for RData's
if ( !dir.exists( par$RData ) ) {
  dir.create( par$RData )
}

# Folder for results
if ( !dir.exists( par$results ) ) {
  dir.create( par$results )
}

message( paste0( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'par' ) ) ] )
gc()