# COVID-19 -----------------------------------------------------------------------------------------
fold <- '/home/aju/Development/COVID-19/csse_covid_19_data/csse_covid_19_time_series/'
conf <- read.csv( file = paste0( fold, 'time_series_19-covid-Confirmed.csv' ) )
conf <- as.data.table( conf )
dead <- read.csv( file = paste0( fold, 'time_series_19-covid-Deaths.csv' ) )
dead <- as.data.table( dead )
reco <- read.csv( file = paste0( fold, 'time_series_19-covid-Recovered.csv' ) )
reco <- as.data.table( reco )

setnames( conf, tolower( names( conf ) ) )
conf <- melt.data.table( data = conf, variable.name = 'date', value.name = 'i',
                         id.vars = c( 'province.state', 'country.region', 'lat', 'long' ) )
conf[ , date := gsub( 'x', '', date ) ]
conf[ , date := mdy( date ) ]

setnames( dead, tolower( names( dead ) ) )
dead <- melt.data.table( data = dead, variable.name = 'date', value.name = 'd',
                         id.vars = c( 'province.state', 'country.region', 'lat', 'long' ) )
dead[ , date := gsub( 'x', '', date ) ]
dead[ , date := mdy( date ) ]

setnames( reco, tolower( names( reco ) ) )
reco <- melt.data.table( data = reco, variable.name = 'date', value.name = 'r',
                         id.vars = c( 'province.state', 'country.region', 'lat', 'long' ) )
reco[ , date := gsub( 'x', '', date ) ]
reco[ , date := mdy( date ) ]

covid <- merge( conf, dead, 
                by = c( 'province.state', 'country.region', 'lat', 'long', 'date' ), 
                all.x = TRUE )

covid <- merge( covid, reco, 
                by = c( 'province.state', 'country.region', 'lat', 'long', 'date' ), 
                all.x = TRUE )

covid_ts <- covid[ , list( i = sum( i ), d = sum( d ), r = sum( r ) ), by = list( date ) ]
setorder( covid_ts, date )
covid_ts[ , beta := r / ( i + d + r ) ]
covid_ts[ , eta := d / ( i + d + r ) ]

# plot( covid_ts$date, covid_ts$beta, type = 'l', col = 'purple' )
# plot( covid_ts$date, covid_ts$eta, type = 'l', col = 'purple' )
# 
# plot( covid_ts$date, covid_ts$i, type = 'l', col = 'orange' )
# points( covid_ts$date, covid_ts$d, type = 'l', col = 'gold' )
# points( covid_ts$date, covid_ts$r, type = 'l', col = 'darkgreen' )

covid_geo <- covid[ , list( i = sum( i ), d = sum( d ), r = sum( r ) ), by = list( lat, long ) ]
# plot( covid_geo$lat, covid_geo$long )
# points( covid[ country.region == 'Ecuador' ]$lat, covid[ country.region == 'Ecuador' ]$long, col = 'red' )

# Modelo SIR 1 -------------------------------------------------------------------------------------
source( 'R/solvers.R', encoding = 'UTF-8', echo = FALSE )

N <- 300
I0 <- 10 / N
S0 <- 1 - I0
R0 <- 0

# alpha <- 0.75
beta <- covid_ts[ date == max( date ) ]$beta
eta <- covid_ts[ date == max( date ) ]$eta
mu <- 0.0001

# rho <- ( beta + mu ) / alpha

rho <- 1 / 6
alpha <- ( beta + mu ) / rho

e <- c( ( beta + mu ) / alpha , 
        mu * ( alpha - beta - mu ) / ( alpha * ( mu + ( 1 - eta ) * beta ) ),
        ( 1 - eta ) * beta * ( alpha - beta - mu ) / ( alpha * ( mu + ( 1 - eta ) * beta ) ) )

n <- 1500
t <- seq( 0, 30, length.out = n )

s <- euler_solver_sir_mor( t, alpha, beta, eta, mu, S0, I0, R0 )  
sol <- data.table( t = t, S = s$S, I = s$I, R = s$R )
rm( s )

x_brk <- seq( 0, 1, length.out = 11 )
y_brk <- seq( 0, 1, length.out = 11 )

plt_phase <- ggplot( data = sol ) +
  geom_path( aes( x = S, y = I ), color = 'dodgerblue4', size = 0.5 ) + 
  geom_abline( intercept = 1, slope = -1, color = 'red4', linetype = 'dashed' ) +
  # geom_point( data = sol[ t == t[n] ], aes( x = S, y = I ), size = 2, col = 'red' ) +
  geom_point( aes( x = e[1], y = e[2] ), size = 1, col = 'red' ) +
  geom_vline( xintercept = rho, color = 'orange', linetype = 'dashed' ) +
  scale_x_continuous( breaks = x_brk, limits = c( 0, 1 ) ) +
  scale_y_continuous( breaks = y_brk, limits = c( 0, 1 ) ) +
  xlab( 'S' ) +
  ylab( 'I' ) +
  theme_bw() +
  theme( panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank() )

# ggsave( plot = plt_phase, filename = 'slides/graf_phase_sir_mor.pdf', width = 12, height = 12, 
#         dpi = 300, units = 'cm' )

x_brk <- seq( 0, max( t ), length.out = 11 )
y_brk <- seq( 0, 1, length.out = 11 )

plt_solv <- ggplot( data = sol ) +
  geom_path( aes( x = t, y = S ), color = 'dodgerblue4' ) + 
  geom_line( aes( x = t, y = I ), color = 'red4' ) +
  geom_line( aes( x = t, y = R ), color = 'orange' ) +
  scale_x_continuous( breaks = x_brk, limits = c( 0, max( t ) ) ) +
  scale_y_continuous( breaks = y_brk, limits = c( 0, 1 ) ) +
  xlab( 't' ) +
  ylab( 'S, I, R' ) +
  # legend( labels = c( 'S', 'I', 'R' ) ) +
  # scale_color_discrete( name = 'Variables', 
  #                       breaks = c( 'S', 'I', 'R' ),
  #                       labels = c( 'S', 'I', 'R' ) ) +
  theme_bw() +
  theme( legend.position = 'right', 
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank() )

ggsave( plot = plt_solv, filename = 'slides/graf_sol_covid_1.pdf', width = 12, height = 12,
        dpi = 300, units = 'cm' )

# Modelo SIR 2 -------------------------------------------------------------------------------------
N <- 150
I0 <- 10 / N
S0 <- 1 - I0
R0 <- 0

# alpha <- 0.75
beta <- covid_ts[ date == max( date ) ]$beta
eta <- covid_ts[ date == max( date ) ]$eta
mu <- 0.0001

# rho <- ( beta + mu ) / alpha
rho <- 1 / 2.3
alpha <- ( beta + mu ) / rho

e <- c( ( beta + mu ) / alpha , 
        mu * ( alpha - beta - mu ) / ( alpha * ( mu + ( 1 - eta ) * beta ) ),
        ( 1 - eta ) * beta * ( alpha - beta - mu ) / ( alpha * ( mu + ( 1 - eta ) * beta ) ) )

n <- 1500
t <- seq( 0, 60, length.out = n )

s <- euler_solver_sir_mor( t, alpha, beta, eta, mu, S0, I0, R0 )  
sol <- data.table( t = t, S = s$S, I = s$I, R = s$R )
rm( s )

x_brk <- seq( 0, 1, length.out = 11 )
y_brk <- seq( 0, 1, length.out = 11 )

plt_phase <- ggplot( data = sol ) +
  geom_path( aes( x = S, y = I ), color = 'dodgerblue4', size = 0.5 ) + 
  geom_abline( intercept = 1, slope = -1, color = 'red4', linetype = 'dashed' ) +
  # geom_point( data = sol[ t == t[n] ], aes( x = S, y = I ), size = 2, col = 'red' ) +
  geom_point( aes( x = e[1], y = e[2] ), size = 1, col = 'red' ) +
  geom_vline( xintercept = rho, color = 'orange', linetype = 'dashed' ) +
  scale_x_continuous( breaks = x_brk, limits = c( 0, 1 ) ) +
  scale_y_continuous( breaks = y_brk, limits = c( 0, 1 ) ) +
  xlab( 'S' ) +
  ylab( 'I' ) +
  theme_bw() +
  theme( panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank() )

# ggsave( plot = plt_phase, filename = 'slides/graf_phase_sir_mor.pdf', width = 12, height = 12, 
#         dpi = 300, units = 'cm' )

x_brk <- seq( 0, max( t ), length.out = 11 )
y_brk <- seq( 0, 1, length.out = 11 )

plt_solv <- ggplot( data = sol ) +
  geom_path( aes( x = t, y = S ), color = 'dodgerblue4' ) + 
  geom_line( aes( x = t, y = I ), color = 'red4' ) +
  geom_line( aes( x = t, y = R ), color = 'orange' ) +
  scale_x_continuous( breaks = x_brk, limits = c( 0, max( t ) ) ) +
  scale_y_continuous( breaks = y_brk, limits = c( 0, 1 ) ) +
  xlab( 't' ) +
  ylab( 'S, I, R' ) +
  # legend( labels = c( 'S', 'I', 'R' ) ) +
  # scale_color_discrete( name = 'Variables', 
  #                       breaks = c( 'S', 'I', 'R' ),
  #                       labels = c( 'S', 'I', 'R' ) ) +
  theme_bw() +
  theme( legend.position = 'right', 
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank() )

ggsave( plot = plt_solv, filename = 'slides/graf_sol_covid_2.pdf', width = 12, height = 12,
        dpi = 300, units = 'cm' )

rm( list = ls() )
gc()