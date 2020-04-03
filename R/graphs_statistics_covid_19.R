message( paste0( rep( '-', 100 ), collapse = '' ) )
message( '\tGraphics of statistics' )

load( paste0( par$RData, 'covid_19_statistics.RData' ) )

# Statistics ---------------------------------------------------------------------------------------
covid_ecu_dy <- covid_reg_dy[ country_region == 'Ecuador' ]
covid_chn_dy <- covid_reg_dy[ country_region == 'China' ]

# Number of infected -------------------------------------------------------------------------------
x_brk <- seq( min( covid_ts$date ), max( covid_ts$date ), by = '7 days' )
y_brk <- seq( 0, 1.05 * round( max( covid_ts$i / 1e6 ), 1 ) * 1e6, length.out = 11 )

plt_i <- ggplot() +
  geom_line( data = covid_ts, aes( x = date, y = i ), color = 'dodgerblue4', size = 0.5 ) +
  geom_line( data = covid_ts, aes( x = date, y = r ), color = 'orange', size = 0.5 ) +
  geom_line( data = covid_ts, aes( x = date, y = d ), color = 'red3', size = 0.5 ) +
  scale_x_date( breaks = x_brk, limits = range( x_brk ) ) +
  scale_y_continuous( breaks = y_brk, limits = range( y_brk ) ) +
  xlab( 'Fecha' ) +
  ylab( 'Conteo de casos' ) +
  theme_bw() +
  theme( legend.position = 'none',
         axis.text.x = element_text( angle = 90 ),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank() )

ggsave( plot = plt_i, 
        filename = paste0(  par$results, 'graf_num_codvid_19.pdf' ), 
        width = 17, height = 12, dpi = 300, units = 'cm' )

# Rate of infection --------------------------------------------------------------------------------
x_brk <- seq( 1, max( covid_dy$t ), 6 )
# x_brk <- seq( min( covid_dy$date ), max( covid_dy$date ), by = '7 days' )
y_brk <- seq( round( 1.2 * min( c( covid_dy$ri, covid_ecu_dy$ri ) ), 2 ), 
              round( 1.2 * max( c( covid_dy$ri, covid_ecu_dy$ri ) ), 2 ), length.out = 11 )

plt_ri <- ggplot() +
  geom_line( data = covid_dy, aes( x = t, y = ri ), color = 'dodgerblue4', size = 0.5 ) +
  geom_line( data = covid_ecu_dy, aes( x = t, y = ri ), color = 'orange', size = 0.5 ) +
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
x_brk <- seq( 1, max( covid_dy$t ), 6 )
# x_brk <- seq( min( covid_dy$date ), max( covid_dy$date ), by = '7 days' )
y_brk <- seq( round( 1.2 * min( c( covid_dy$rd, covid_ecu_dy$rd ) ), 2 ), 
              round( 1.2 * max( c( covid_dy$rd, covid_ecu_dy$rd ) ), 2 ), length.out = 11 )

plt_rd <- ggplot() +
  geom_line( data = covid_dy, aes( x = t, y = rd ), color = 'dodgerblue4', size = 0.5 ) +
  geom_line( data = covid_ecu_dy, aes( x = t, y = rd ), color = 'orange', size = 0.5 ) +
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
x_brk <- seq( 1, max( covid_dy$t ), 6 )
# x_brk <- seq( min( covid_dy$date ), max( covid_dy$date ), by = '7 days' )
y_brk <- seq( round( 1.2 * min( c( covid_dy$rr, covid_ecu_dy$rr ) ), 2 ), 
                round( 1.2 * 1.5, 2 ), length.out = 11 )

plt_rr <- ggplot() +
  geom_line( data = covid_dy, aes( x = t, y = rr ), color = 'dodgerblue4', size = 0.5 ) +
  geom_line( data = covid_ecu_dy, aes( x = t, y = rr ), color = 'orange', size = 0.5 ) +
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
x_brk <- seq( 1, max( covid_dy$t ), 6 )
# x_brk <- seq( min( covid_dy$date ), max( covid_dy$date ), by = '7 days' )
y_brk <- seq( 0, round( 1.2 * max( c( covid_dy$beta, covid_ecu_dy$beta ) ), 2 ), length.out = 11 )

plt_beta <- ggplot() +
  geom_path( data = covid_dy, aes( x = t, y = beta ), color = 'dodgerblue4', size = 0.5 ) +
  geom_line( data = covid_ecu_dy, aes( x = t, y = beta ), color = 'orange', size = 0.5 ) +
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
x_brk <- seq( 1, max( covid_dy$t ), 6 )
# x_brk <- seq( min( covid_dy$date ), max( covid_dy$date ), by = '7 days' )
y_brk <- seq( 0, round( 1.2 * max( c( covid_dy$eta, covid_ecu_dy$eta ) ), 2 ), length.out = 11 )

plt_eta <- ggplot() +
  geom_line( data = covid_dy, aes( x = t, y = eta ), color = 'dodgerblue4', size = 0.5 ) +
  geom_line( data = covid_ecu_dy, aes( x = t, y = eta ), color = 'orange', size = 0.5 ) +
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

