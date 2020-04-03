message( paste0( rep( '-', 100 ), collapse = '' ) )
message( '\tStatistics of COVID-19' )
load( paste0( par$RData, 'covid_19_csse_information.RData' ) )

covid <- merge( conf, dead, 
                by = c( 'province_state', 'country_region', 'lat', 'long', 'date' ), 
                all.x = TRUE )

covid <- merge( covid, reco, 
                by = c( 'province_state', 'country_region', 'lat', 'long', 'date' ), 
                all.x = TRUE )

covid <- covid[ i > 0 ]
covid[ , t := as.POSIXct( date ) ]
covid[ , tmin := min( t ), by = list( country_region ) ]
covid[ , t := interval( tmin, t ) / ddays( 1 ) + 1 ]

# Global time series
covid_ts <- covid[ , list( i = sum( i, na.rm = TRUE ), 
                           d = sum( d, na.rm = TRUE ), 
                           r = sum( r, na.rm = TRUE ) ), 
                   by = list( date ) ]
setorder( covid_ts, date )

# covid_ts[ , t := as.POSIXct( date ) ]
# covid_ts[ , t := interval( min( t ), t ) / ddays( 1 ) + 1 ]

covid_dy <- covid[ , list( i = sum( i, na.rm = TRUE ), 
                           d = sum( d, na.rm = TRUE ), 
                           r = sum( r, na.rm = TRUE ) ), 
                   by = list( t ) ]
setorder( covid_dy, t )

covid_dy[ , ri := shift( i, fill = 0 ) ]
covid_dy[ , ri := i / ri - 1 ]
covid_dy[ !is.finite( ri ), ri := 0 ]
covid_dy[ , rd := shift( d, fill = 0 ) ]
covid_dy[ , rd := d / rd - 1 ]
covid_dy[ !is.finite( rd ), rd := 0 ]
covid_dy[ , rr := shift( r, fill = 0 ) ]
covid_dy[ , rr := r / rr - 1 ]
covid_dy[ !is.finite( rr ), rr := 0 ]
covid_dy[ , beta := 0 ]
covid_dy[ i + d + r > 0, beta := r / ( i + d + r ) ]
covid_dy[ , eta := 0 ]
covid_dy[ i + d + r > 0, eta := d / ( i + d + r ) ]


# Time series by country or region
covid_reg_dy <- covid[ , list( i = sum( i, na.rm = TRUE ), 
                               d = sum( d, na.rm = TRUE ), 
                               r = sum( r, na.rm = TRUE ) ), 
                       by = list( country_region, t ) ]
# covid_reg_dy <- covid_reg_dy[ i > 0 ]
setorder( covid_reg_dy, country_region, t )

# covid_reg_dy[ , t := as.POSIXct( date ) ]
# covid_reg_dy[ , tmin := min( t ), by = list( country_region ) ]
# covid_reg_dy[ , t := interval( tmin, t ) / ddays( 1 ) + 1 ]

covid_reg_dy[ , ri := shift( i, fill = 0 ), by = country_region ]
covid_reg_dy[ , ri := i / ri - 1 ]
covid_reg_dy[ !is.finite( ri ), ri := 0 ]
covid_reg_dy[ , rd := shift( d, fill = 0 ), by = country_region ]
covid_reg_dy[ , rd := d / rd - 1 ]
covid_reg_dy[ !is.finite( rd ), rd := 0 ]
covid_reg_dy[ , rr := shift( r, fill = 0 ), by = country_region ]
covid_reg_dy[ , rr := r / rr - 1 ]
covid_reg_dy[ !is.finite( rr ), rr := 0 ]
covid_reg_dy[ , beta := 0 ]
covid_reg_dy[ i + d + r > 0, beta := r / ( i + d + r ) ]
covid_reg_dy[ , eta := 0 ]
covid_reg_dy[ i + d + r > 0, eta := d / ( i + d + r ) ]

# By geographic position
covid_geo <- covid[ , list( i = sum( i, na.rm = TRUE ), 
                            d = sum( d, na.rm = TRUE ), 
                            r = sum( r, na.rm = TRUE ) ), 
                    by = list( lat, long ) ]

save( covid, covid_geo, covid_ts, covid_reg_dy, covid_dy,
      file = paste0( par$RData, 'covid_19_statistics.RData' ) )


message( paste0( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'par' ) ) ] )
gc()
