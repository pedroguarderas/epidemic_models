message( paste0( rep( '-', 100 ), collapse = '' ) )
message( '\tEstimation COVID-19 info' )

covid <- merge( conf, dead, 
                by = c( 'province_state', 'country_region', 'lat', 'long', 'date' ), 
                all.x = TRUE )

covid <- merge( covid, reco, 
                by = c( 'province_state', 'country_region', 'lat', 'long', 'date' ), 
                all.x = TRUE )

covid_ts <- covid[ , list( i = sum( i, na.rm = TRUE ), 
                           d = sum( d, na.rm = TRUE ), 
                           r = sum( r, na.rm = TRUE ) ), 
                   by = list( date ) ]
setorder( covid_ts, date )
covid_ts[ , beta := r / ( i + d + r ) ]
covid_ts[ , eta := d / ( i + d + r ) ]

covid_geo <- covid[ , list( i = sum( i, na.rm = TRUE ), 
                            d = sum( d, na.rm = TRUE ), 
                            r = sum( r, na.rm = TRUE ) ), 
                    by = list( lat, long ) ]

save( covid, covid_ts, covid_geo, reco, file = 'RData/covid_19_estimation.RData' )

message( paste0( rep( '-', 100 ), collapse = '' ) )
rm( list = ls() )
gc()
