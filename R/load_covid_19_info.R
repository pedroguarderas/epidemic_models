# COVID-19 -----------------------------------------------------------------------------------------
message( paste0( rep( '-', 100 ), collapse = '' ) )
message( '\tLoad COVID-19 info' )
fold <- '/home/aju/Development/COVID-19/csse_covid_19_data/csse_covid_19_time_series/'

# Creando carpeta para guardar los resultados
if ( !dir.exists( 'RData' ) ) {
  dir.create( 'RData' )
}

conf <- read.csv( file = paste0( fold, 'time_series_19-covid-Confirmed.csv' ) )
conf <- as.data.table( conf )
dead <- read.csv( file = paste0( fold, 'time_series_19-covid-Deaths.csv' ) )
dead <- as.data.table( dead )
reco <- read.csv( file = paste0( fold, 'time_series_19-covid-Recovered.csv' ) )
reco <- as.data.table( reco )

setnames( conf, tolower( names( conf ) ) )
setnames( conf, gsub( '[.]', '_', names( conf ) ) )
conf <- melt.data.table( data = conf, variable.name = 'date', value.name = 'i',
                         id.vars = c( 'province_state', 'country_region', 'lat', 'long' ) )
conf[ , date := gsub( 'x', '', date ) ]
conf[ , date := mdy( date ) ]

setnames( dead, tolower( names( dead ) ) )
setnames( dead, gsub( '[.]', '_', names( dead ) ) )
dead <- melt.data.table( data = dead, variable.name = 'date', value.name = 'd',
                         id.vars = c( 'province_state', 'country_region', 'lat', 'long' ) )
dead[ , date := gsub( 'x', '', date ) ]
dead[ , date := mdy( date ) ]

setnames( reco, tolower( names( reco ) ) )
setnames( reco, gsub( '[.]', '_', names( reco ) ) )
reco <- melt.data.table( data = reco, variable.name = 'date', value.name = 'r',
                         id.vars = c( 'province_state', 'country_region', 'lat', 'long' ) )
reco[ , date := gsub( 'x', '', date ) ]
reco[ , date := mdy( date ) ]

save( conf, dead, reco, file = 'RData/covid_19_information.RData' )

message( paste0( rep( '-', 100 ), collapse = '' ) )
rm( list = ls() )
gc()
