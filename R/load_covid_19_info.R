# COVID-19 -----------------------------------------------------------------------------------------
message( paste0( rep( '-', 100 ), collapse = '' ) )
message( '\tLoad COVID-19 info' )

# CSSE data ----------------------------------------------------------------------------------------
message( '\tLoading CSSE information' )
fold <- par$src_csse_info

# Confirmed
conf <- read.csv( file = paste0( fold, 'time_series_covid19_confirmed_global.csv' ) )
conf <- as.data.table( conf )

setnames( conf, tolower( names( conf ) ) )
setnames( conf, gsub( '[.]', '_', names( conf ) ) )
conf <- melt.data.table( data = conf, variable.name = 'date', value.name = 'i',
                         id.vars = c( 'province_state', 'country_region', 'lat', 'long' ) )
conf[ , date := gsub( 'x', '', date ) ]
conf[ , date := mdy( date ) ]

# Deaths
dead <- read.csv( file = paste0( fold, 'time_series_covid19_deaths_global.csv' ) )
dead <- as.data.table( dead )

setnames( dead, tolower( names( dead ) ) )
setnames( dead, gsub( '[.]', '_', names( dead ) ) )
dead <- melt.data.table( data = dead, variable.name = 'date', value.name = 'd',
                         id.vars = c( 'province_state', 'country_region', 'lat', 'long' ) )
dead[ , date := gsub( 'x', '', date ) ]
dead[ , date := mdy( date ) ]

# Recovered
reco <- read.csv( file = paste0( fold, 'time_series_covid19_recovered_global.csv' ) )
reco <- as.data.table( reco )

setnames( reco, tolower( names( reco ) ) )
setnames( reco, gsub( '[.]', '_', names( reco ) ) )
reco <- melt.data.table( data = reco, variable.name = 'date', value.name = 'r',
                         id.vars = c( 'province_state', 'country_region', 'lat', 'long' ) )
reco[ , date := gsub( 'x', '', date ) ]
reco[ , date := mdy( date ) ]

save( conf, dead, reco, 
      file = paste0( par$RData, 'covid_19_csse_information.RData' ) )

# WHO data ----------------------------------------------------------------------------------------
message( '\tLoading WHO information' )
fold <- par$src_who_info

who_ts <- read.csv( file = paste0( fold, 'who_covid_19_sit_rep_time_series.csv' ) )
who_ts <- as.data.table( who_ts )

setnames( who_ts, tolower( names( who_ts ) ) )
setnames( who_ts, gsub( '[.]', '_', names( who_ts ) ) )

cols <- names( who_ts )
cols <- cols[ !( cols %in% c( 'province_states', 'country_region', 'who_region' ) ) ]
for ( c in cols ) {
  eval( expr = parse( text = paste0( 'who_ts[ , ', c, ' := as.numeric( ', c, ' ) ]' ) ) )
}

who_ts <- melt.data.table( data = who_ts, variable.name = 'date', value.name = 'n', 
                           value.factor = FALSE,
                           id.vars = c( 'province_states', 'country_region', 'who_region' ) )


who_ts[ , date := gsub( 'x', '', date ) ]
who_ts[ , date := gsub( '_', '/', date ) ]
who_ts[ , date := mdy( date ) ]

save( who_ts, 
      file = paste0( par$RData, 'covid_19_who_information.RData' ) )

message( paste0( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'par' ) ) ] )
gc()
