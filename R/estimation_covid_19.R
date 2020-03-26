message( paste0( rep( '-', 100 ), collapse = '' ) )
message( '\tEstimation COVID-19 info' )
load( paste0( par$RData, 'covid_19_information.RData' ) )

covid <- merge( conf, dead, 
                by = c( 'province_state', 'country_region', 'lat', 'long', 'date' ), 
                all.x = TRUE )

covid <- merge( covid, reco, 
                by = c( 'province_state', 'country_region', 'lat', 'long', 'date' ), 
                all.x = TRUE )

# By geographic position
covid_geo <- covid[ , list( i = sum( i, na.rm = TRUE ), 
                            d = sum( d, na.rm = TRUE ), 
                            r = sum( r, na.rm = TRUE ) ), 
                    by = list( lat, long ) ]

# Global time series
covid_ts <- covid[ , list( i = sum( i, na.rm = TRUE ), 
                           d = sum( d, na.rm = TRUE ), 
                           r = sum( r, na.rm = TRUE ) ), 
                   by = list( date ) ]
setorder( covid_ts, date )
covid_ts[ , ri := shift( i, fill = 0 ) ]
covid_ts[ , ri := i / ri - 1 ]
covid_ts[ !is.finite( ri ), ri := 0 ]
covid_ts[ , rd := shift( d, fill = 0 ) ]
covid_ts[ , rd := d / rd - 1 ]
covid_ts[ !is.finite( rd ), rd := 0 ]
covid_ts[ , rr := shift( r, fill = 0 ) ]
covid_ts[ , rr := r / rr - 1 ]
covid_ts[ !is.finite( rr ), rr := 0 ]
covid_ts[ , beta := 0 ]
covid_ts[ i + d + r > 0, beta := r / ( i + d + r ) ]
covid_ts[ , eta := 0 ]
covid_ts[ i + d + r > 0, eta := d / ( i + d + r ) ]
covid_ts[ , t := as.POSIXct( date ) ]
covid_ts[ , t := interval( min( t ), t ) / ddays( 1 ) + 1 ]

# Time series by country or region
covid_reg_ts <- covid[ , list( i = sum( i, na.rm = TRUE ), 
                               d = sum( d, na.rm = TRUE ), 
                               r = sum( r, na.rm = TRUE ) ), 
                       by = list( country_region, date ) ]
covid_reg_ts <- covid_reg_ts[ i > 0 ]
setorder( covid_reg_ts, country_region, date )
covid_reg_ts[ , ri := shift( i, fill = 0 ), by = country_region ]
covid_reg_ts[ , ri := i / ri - 1 ]
covid_reg_ts[ !is.finite( ri ), ri := 0 ]
covid_reg_ts[ , rd := shift( d, fill = 0 ), by = country_region ]
covid_reg_ts[ , rd := d / rd - 1 ]
covid_reg_ts[ !is.finite( rd ), rd := 0 ]
covid_reg_ts[ , rr := shift( r, fill = 0 ), by = country_region ]
covid_reg_ts[ , rr := r / rr - 1 ]
covid_reg_ts[ !is.finite( rr ), rr := 0 ]
covid_reg_ts[ , beta := 0 ]
covid_reg_ts[ i + d + r > 0, beta := r / ( i + d + r ) ]
covid_reg_ts[ , eta := 0 ]
covid_reg_ts[ i + d + r > 0, eta := d / ( i + d + r ) ]
covid_reg_ts[ , t := as.POSIXct( date ) ]
covid_reg_ts[ , tmin := min( t ), by = list( country_region ) ]
covid_reg_ts[ , t := interval( tmin, t ) / ddays( 1 ) + 1 ]

save( covid, covid_geo, covid_ts, covid_reg_ts,
      file = paste0( par$RData, 'covid_19_estimation.RData' ) )


# Statistics ---------------------------------------------------------------------------------------
# load( paste0( par$RData, 'covid_19_estimation.RData' ) )
covid_ecu_ts <- covid_reg_ts[ country_region == 'Ecuador' ]
covid_chn_ts <- covid_reg_ts[ country_region == 'China' ]
covid_ita_ts <- covid_reg_ts[ country_region == 'Italy' ]

x_brk <- seq( 1, max( covid_ts$t ), 6 )
y_brk <- seq( 0, 1, length.out = 11 )

plt_beta <- ggplot() +
  geom_line( data = covid_ts, aes( x = t, y = beta ), color = 'dodgerblue4', size = 0.5 ) +
  geom_line( data = covid_ecu_ts, aes( x = t, y = beta ), color = 'orange', size = 0.5 ) +
  geom_line( data = covid_chn_ts, aes( x = t, y = beta ), color = 'red3', size = 0.5 ) +
  scale_x_continuous( breaks = x_brk, limits = range( x_brk ) ) +
  scale_y_continuous( breaks = y_brk, limits = range( y_brk ) ) +
  xlab( 'days' ) +
  ylab( 'beta' ) +
  theme_bw() +
  theme( legend.position = 'none',
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank() )


plt_beta


x_brk <- seq( 1, max( covid_ts$t ), 6 )
y_brk <- seq( 0, 0.1, length.out = 11 )

plt_eta <- ggplot() +
  geom_line( data = covid_ts, aes( x = t, y = eta ), color = 'dodgerblue4', size = 0.5 ) +
  geom_line( data = covid_ecu_ts, aes( x = t, y = eta ), color = 'orange', size = 0.5 ) +
  geom_line( data = covid_chn_ts, aes( x = t, y = eta ), color = 'red3', size = 0.5 ) +
  scale_x_continuous( breaks = x_brk, limits = range( x_brk ) ) +
  scale_y_continuous( breaks = y_brk, limits = range( y_brk ) ) +
  xlab( 'days' ) +
  ylab( 'eta' ) +
  theme_bw() +
  theme( legend.position = 'none',
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank() )


plt_eta


x_brk <- seq( 1, max( covid_ts$t ), 6 )
y_brk <- seq( 0, 1, length.out = 11 )

plt_ri <- ggplot() +
  geom_line( data = covid_ts, aes( x = t, y = ri ), color = 'dodgerblue4', size = 0.5 ) +
  geom_line( data = covid_ecu_ts, aes( x = t, y = ri ), color = 'orange', size = 0.5 ) +
  geom_line( data = covid_chn_ts, aes( x = t, y = ri ), color = 'red3', size = 0.5 ) +
  scale_x_continuous( breaks = x_brk, limits = range( x_brk ) ) +
  scale_y_continuous( breaks = y_brk, limits = range( y_brk ) ) +
  xlab( 'days' ) +
  ylab( 'infected' ) +
  theme_bw() +
  theme( legend.position = 'none',
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank() )


plt_ri

x_brk <- seq( 1, max( covid_ts$t ), 6 )
y_brk <- seq( 0, 1.1, length.out = 11 )

plt_rd <- ggplot() +
  geom_line( data = covid_ts, aes( x = t, y = rd ), color = 'dodgerblue4', size = 0.5 ) +
  geom_line( data = covid_ecu_ts, aes( x = t, y = rd ), color = 'orange', size = 0.5 ) +
  geom_line( data = covid_chn_ts, aes( x = t, y = rd ), color = 'red3', size = 0.5 ) +
  # geom_line( data = covid_ita_ts, aes( x = t, y = rd ), color = 'green3', size = 0.5 ) +
  scale_x_continuous( breaks = x_brk, limits = range( x_brk ) ) +
  scale_y_continuous( breaks = y_brk, limits = range( y_brk ) ) +
  xlab( 'days' ) +
  ylab( 'death' ) +
  theme_bw() +
  theme( legend.position = 'none',
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank() )


plt_rd

x_brk <- seq( 1, max( covid_ts$t ), 6 )
y_brk <- seq( 0, 1, length.out = 11 )

plt_rr <- ggplot() +
  geom_line( data = covid_ts, aes( x = t, y = rr ), color = 'dodgerblue4', size = 0.5 ) +
  geom_line( data = covid_ecu_ts, aes( x = t, y = rr ), color = 'orange', size = 0.5 ) +
  geom_line( data = covid_chn_ts, aes( x = t, y = rr ), color = 'red3', size = 0.5 ) +
  # geom_line( data = covid_ita_ts, aes( x = t, y = rr ), color = 'green3', size = 0.5 ) +
  scale_x_continuous( breaks = x_brk, limits = range( x_brk ) ) +
  scale_y_continuous( breaks = y_brk, limits = range( y_brk ) ) +
  xlab( 'days' ) +
  ylab( 'recovery' ) +
  theme_bw() +
  theme( legend.position = 'none',
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank() )


plt_rr

message( paste0( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'par' ) ) ] )
gc()
