message( paste0( rep( '-', 100 ), collapse = '' ) )
message( '\tGraphics of statistics' )

load( paste0( par$RData, 'covid_19_estimation.RData' ) )

# Statistics ---------------------------------------------------------------------------------------
covid_ecu_ts <- covid_reg_ts[ country_region == 'Ecuador' ]
covid_chn_ts <- covid_reg_ts[ country_region == 'China' ]

# Rate of infection --------------------------------------------------------------------------------
x_brk <- seq( 1, max( covid_ts$t ), 6 )
# x_brk <- seq( min( covid_ts$date ), max( covid_ts$date ), by = '7 days' )
y_brk <- seq( round( 1.2 * min( c( covid_ts$ri, covid_ecu_ts$ri ) ), 2 ), 
              round( 1.2 * max( c( covid_ts$ri, covid_ecu_ts$ri ) ), 2 ), length.out = 11 )

plt_ri <- ggplot() +
  geom_line( data = covid_ts, aes( x = t, y = ri ), color = 'dodgerblue4', size = 0.5 ) +
  geom_line( data = covid_ecu_ts, aes( x = t, y = ri ), color = 'orange', size = 0.5 ) +
  # geom_line( data = covid_chn_ts, aes( x = t, y = ri ), color = 'red3', size = 0.5 ) +
  scale_x_continuous( breaks = x_brk, limits = range( x_brk ) ) +
  scale_y_continuous( breaks = y_brk, limits = range( y_brk ) ) +
  xlab( 'days' ) +
  ylab( 'infected' ) +
  theme_bw() +
  theme( legend.position = 'none',
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank() )

ggsave( plot = plt_ri, 
        filename = paste0(  par$results, 'graf_sta_rate_infe_codvid_19.pdf' ), 
        width = 12, height = 12, dpi = 300, units = 'cm' )

# Rate of deaths -----------------------------------------------------------------------------------
x_brk <- seq( 1, max( covid_ts$t ), 6 )
# x_brk <- seq( min( covid_ts$date ), max( covid_ts$date ), by = '7 days' )
y_brk <- seq( round( 1.2 * min( c( covid_ts$rd, covid_ecu_ts$rd ) ), 2 ), 
              round( 1.2 * max( c( covid_ts$rd, covid_ecu_ts$rd ) ), 2 ), length.out = 11 )

plt_rd <- ggplot() +
  geom_line( data = covid_ts, aes( x = t, y = rd ), color = 'dodgerblue4', size = 0.5 ) +
  geom_line( data = covid_ecu_ts, aes( x = t, y = rd ), color = 'orange', size = 0.5 ) +
  # geom_line( data = covid_chn_ts, aes( x = t, y = rd ), color = 'red3', size = 0.5 ) +
  # geom_line( data = covid_ita_ts, aes( x = t, y = rd ), color = 'green3', size = 0.5 ) +
  scale_x_continuous( breaks = x_brk, limits = range( x_brk ) ) +
  scale_y_continuous( breaks = y_brk, limits = range( y_brk ) ) +
  xlab( 'days' ) +
  ylab( 'death' ) +
  theme_bw() +
  theme( legend.position = 'none',
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank() )

ggsave( plot = plt_rd, 
        filename = paste0(  par$results, 'graf_sta_rate_death_codvid_19.pdf' ), 
        width = 12, height = 12, dpi = 300, units = 'cm' )

# Rate of recuperation -----------------------------------------------------------------------------
x_brk <- seq( 1, max( covid_ts$t ), 6 )
# x_brk <- seq( min( covid_ts$date ), max( covid_ts$date ), by = '7 days' )
y_brk <- seq( round( 1.2 * min( c( covid_ts$rr, covid_ecu_ts$rr ) ), 2 ), 
                round( 1.2 * 1.5, 2 ), length.out = 11 )

plt_rr <- ggplot() +
  geom_line( data = covid_ts, aes( x = t, y = rr ), color = 'dodgerblue4', size = 0.5 ) +
  geom_line( data = covid_ecu_ts, aes( x = t, y = rr ), color = 'orange', size = 0.5 ) +
  # geom_line( data = covid_chn_ts, aes( x = t, y = rr ), color = 'red3', size = 0.5 ) +
  scale_x_continuous( breaks = x_brk, limits = range( x_brk ) ) +
  scale_y_continuous( breaks = y_brk, limits = range( y_brk ) ) +
  xlab( 'days' ) +
  ylab( 'recovery' ) +
  theme_bw() +
  theme( legend.position = 'none',
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank() )

ggsave( plot = plt_rr, 
        filename = paste0(  par$results, 'graf_sta_rate_recu_codvid_19.pdf' ), 
        width = 12, height = 12, dpi = 300, units = 'cm' )

# beta ---------------------------------------------------------------------------------------------
x_brk <- seq( 1, max( covid_ts$t ), 6 )
# x_brk <- seq( min( covid_ts$date ), max( covid_ts$date ), by = '7 days' )
y_brk <- seq( 0, round( 1.2 * max( c( covid_ts$beta, covid_ecu_ts$beta ) ), 2 ), length.out = 11 )

plt_beta <- ggplot() +
  geom_path( data = covid_ts, aes( x = t, y = beta ), color = 'dodgerblue4', size = 0.5 ) +
  geom_line( data = covid_ecu_ts, aes( x = t, y = beta ), color = 'orange', size = 0.5 ) +
  # geom_line( data = covid_chn_ts, aes( x = t, y = beta ), color = 'red3', size = 0.5 ) +
  scale_x_continuous( breaks = x_brk, limits = range( x_brk ) ) +
  scale_y_continuous( breaks = y_brk, limits = range( y_brk ) ) +
  xlab( 'days' ) +
  ylab( 'beta' ) +
  theme_bw() +
  theme( legend.position = 'none',
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank() )

ggsave( plot = plt_beta, 
        filename = paste0(  par$results, 'graf_sta_beta_codvid_19.pdf' ), 
        width = 12, height = 12, dpi = 300, units = 'cm' )

# eta ----------------------------------------------------------------------------------------------
x_brk <- seq( 1, max( covid_ts$t ), 6 )
# x_brk <- seq( min( covid_ts$date ), max( covid_ts$date ), by = '7 days' )
y_brk <- seq( 0, round( 1.2 * max( c( covid_ts$eta, covid_ecu_ts$eta ) ), 2 ), length.out = 11 )

plt_eta <- ggplot() +
  geom_line( data = covid_ts, aes( x = t, y = eta ), color = 'dodgerblue4', size = 0.5 ) +
  geom_line( data = covid_ecu_ts, aes( x = t, y = eta ), color = 'orange', size = 0.5 ) +
  # geom_line( data = covid_chn_ts, aes( x = t, y = eta ), color = 'red3', size = 0.5 ) +
  scale_x_continuous( breaks = x_brk, limits = range( x_brk ) ) +
  scale_y_continuous( breaks = y_brk, limits = range( y_brk ) ) +
  xlab( 'days' ) +
  ylab( 'eta' ) +
  theme_bw() +
  theme( legend.position = 'none',
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank() )

ggsave( plot = plt_eta, 
        filename = paste0(  par$results, 'graf_sta_eta_codvid_19.pdf' ), 
        width = 12, height = 12, dpi = 300, units = 'cm' )

message( paste0( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'par' ) ) ] )
gc()

