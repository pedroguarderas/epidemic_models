source( 'R/solvers.R', encoding = 'UTF-8', echo = FALSE )

# Modelo SIR ---------------------------------------------------------------------------------------
S0 <- 0.8
I0 <- 0.2
R0 <- 0

alpha <- 0.52
beta <- 0.2

n <- 5e2
t <- seq( 0, 30, length.out = n )

sol <- euler_solver_sir( t, alpha, beta, S0, I0, R0 )

plt_solv <- ggplot() +
  geom_line( aes( x = t, y = sol$S ), color = 'purple' ) + 
  geom_line( aes( x = t, y = sol$I ), color = 'orange' ) +
  geom_line( aes( x = t, y = sol$R ), color = 'darkgreen' ) +
  scale_y_continuous( limits = c( 0, 1 ) ) +
  theme_bw()

# Modelo SIR sin mortalidad y diagramas de fase ----------------------------------------------------
m <- 11

I0 <- seq( 0.01, 1.0, length.out = m )
S0 <- 1 - I0
R0 <- rep( 0, m )

alpha <- 0.5
beta <- 0.15
rho <- beta / alpha

n <- 1e3
t <- seq( 0, 50, length.out = n )

sol <- NULL
for ( k in 1:m ) {
  s <- euler_solver_sir( t, alpha, beta, S0[ k ], I0[ k ], R0[ k ] )  
  sol <- rbind( sol, data.table( m = k, S = s$S, I = s$I, R = s$R ) )
}

x_brk <- seq( 0, 1, length.out = 11 )
y_brk <- seq( 0, 1, length.out = 11 )

plt_phase <- ggplot( data = sol ) +
  geom_path( aes( x = S, y = I, group = m ), color = 'dodgerblue4', size = 0.5 ) + 
  geom_abline( intercept = 1, slope = -1, color = 'red4', linetype = 'dashed' ) +
  geom_vline( xintercept = rho, color = 'orange', linetype = 'dashed' ) +
  scale_x_continuous( breaks = x_brk, limits = c( 0, 1 ) ) +
  scale_y_continuous( breaks = y_brk, limits = c( 0, 1 ) ) +
  xlab( 'S' ) +
  ylab( 'I' ) +
  theme_bw() +
  theme( panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank() )

ggsave( plot = plt_phase, 
        filename = paste0(  par$results, 'graf_model_phase_sir.pdf' ),
        width = 12, height = 12, 
        dpi = 300, units = 'cm' )

# Modelo SIR con mortalidad y diagramas de fase ----------------------------------------------------
m <- 15

I0 <- seq( 0.01, 0.99, length.out = m )
S0 <- 1 - I0
R0 <- 0

alpha <- 0.5
beta <- 0.15
eta <- 0.12
mu <- 0.01

# alpha <- 0.8
# beta <- 0.32
# eta <- 0.022
# mu <- 0.001

rho <- ( beta + mu ) / alpha

e <- c( ( beta + mu ) / alpha , 
        mu * ( alpha - beta - mu ) / ( alpha * ( mu + ( 1 - eta ) * beta ) ),
        ( 1 - eta ) * beta * ( alpha - beta - mu ) / ( alpha * ( mu + ( 1 - eta ) * beta ) ) )

n <- 1500
t <- seq( 0, 240, length.out = n )

sol <- NULL
for ( k in 1:length( I0 ) ) {
  s <- euler_solver_sir_mor( t, alpha, beta, eta, mu, S0[ k ], I0[ k ], R0 )  
  sol <- rbind( sol, data.table( m = k, t = t, S = s$S, I = s$I, R = s$R ) )
}

x_brk <- seq( 0, 1, length.out = 11 )
y_brk <- seq( 0, 1, length.out = 11 )

plt_phase <- ggplot( data = sol ) +
  geom_path( aes( x = S, y = I, group = m ), color = 'dodgerblue4', size = 0.5 ) + 
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

plot( plt_phase )
ggsave( plot = plt_phase, 
        filename = paste0(  par$results, 'graf_phase_sir_mor.pdf' ),
        width = 12, height = 12, dpi = 300, units = 'cm' )

x_brk <- seq( 0, max( t ), length.out = 11 )
y_brk <- seq( 0, 1, length.out = 11 )

plt_solv <- ggplot( data = sol ) +
  geom_path( aes( x = t, y = S, group = m ), color = 'dodgerblue4' ) + 
  geom_line( aes( x = t, y = I, group = m ), color = 'red4' ) +
  geom_line( aes( x = t, y = R, group = m ), color = 'orange' ) +
  scale_x_continuous( breaks = x_brk, limits = c( 0, max( t ) ) ) +
  scale_y_continuous( breaks = y_brk, limits = c( 0, 1 ) ) +
  xlab( 't' ) +
  ylab( 'S, I, R' ) +
  theme_bw() +
  theme( legend.position = 'right', 
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank() )

ggsave( plot = plt_solv, 
        filename = paste0(  par$results, 'graf_solu_sir_mor.pdf' ),
        width = 12, height = 12, dpi = 300, units = 'cm' )

rm( list = ls()[ !( ls() %in% c( 'par' ) ) ] )
gc()
