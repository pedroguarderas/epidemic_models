source( 'R/solvers.R', encoding = 'UTF-8', echo = FALSE )

# Modelo SEIR ---------------------------------------------------------------------------------------
S0 <- 0.9
E0 <- 0.09
I0 <- 0.01
R0 <- 0.0

alpha <- 0.9
beta <- 0.2
sigma <- 0.5
mu <- 0.0001
nu <- 0.001

n <- 1e3
t <- seq( 0, 60, length.out = n )

sol <- euler_solver_seir( t, alpha, beta, sigma, mu, nu, S0, E0, I0, R0 )

xbrk <- seq( min( t ), max( t ), length.out = 11 )
ybrk <- seq( 0, 1, length.out = 11 )

plt_solv <- ggplot() +
  geom_line( aes( x = t, y = sol$S ), color = 'purple' ) + 
  geom_line( aes( x = t, y = sol$E ), color = 'dodgerblue3' ) + 
  geom_line( aes( x = t, y = sol$I ), color = 'orange' ) +
  geom_line( aes( x = t, y = sol$R ), color = 'darkgreen' ) +
  scale_x_continuous( breaks = xbrk, limits = range( xbrk ) ) +
  scale_y_continuous( breaks = ybrk, limits = range( ybrk ) ) +
  xlab( 'days' ) +
  ylab( 'S,E,I,R' ) +
  theme_bw()

plot( plt_solv )
