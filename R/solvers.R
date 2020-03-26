# Solver for the SIR model -------------------------------------------------------------------------
# Death is not considered in the model
euler_solver_sir <- function( t, alpha, beta, S0, I0, R0 ) {
  n <- length( t )
  
  S <- array( S0, dim = n )
  I <- array( I0, dim = n )
  R <- array( R0, dim = n )
  
  for ( i in 1:( n - 1 ) ) {
    dt <- t[ i + 1 ] - t[ i ]
    
    S[ i + 1 ] = S[ i ] - dt * alpha * S[ i ] * I[ i ]
    
    I[ i + 1 ] = I[ i ] + dt * ( alpha * S[ i ] * I[ i ] - beta * I[ i ] )
    
    R[ i + 1 ] = R[ i ] + dt * beta * I[ i ]
    
  } 
  
  return( list( 'S' = S, 'I' = I, 'R' = R ) )
}

# Solver for SIR model considering death -----------------------------------------------------------
# The solver is implemented with the 4th order Runge-Kutta method
euler_solver_sir_mor <- function( t, alpha, beta, eta, mu, S0, I0, R0 ) {
  n <- length( t )
  
  S <- array( S0, dim = n )
  I <- array( I0, dim = n )
  R <- array( R0, dim = n )
  
  for ( i in 1:( n - 1 ) ) {
    dt <- t[ i + 1 ] - t[ i ]
    
    Si <- S[i]
    Ii <- I[i]
    Ri <- R[i]
    Sk1 <- mu * ( 1 - Si ) - alpha * Si * Ii + eta * beta * Ii
    Ik1 <- alpha * Si * Ii - ( beta + mu ) * Ii
    Rk1 <- ( 1 - eta ) * beta * Ii - mu * Ri
    
    Si <- S[i] + 0.5 * dt * Sk1
    Ii <- I[i] + 0.5 * dt * Ik1
    Ri <- R[i] + 0.5 * dt * Rk1
    Sk2 <- mu * ( 1 - Si ) - alpha * Si * Ii + eta * beta * Ii
    Ik2 <- alpha * Si * Ii - ( beta + mu ) * Ii 
    Rk2 <- ( 1 - eta ) * beta * Ii - mu * Ri
    
    Si <- S[i] + 0.5 * dt * Sk2
    Ii <- I[i] + 0.5 * dt * Ik2
    Ri <- R[i] + 0.5 * dt * Rk2
    Sk3 <- mu * ( 1 - Si ) - alpha * Si * Ii + eta * beta * Ii
    Ik3 <- alpha * Si * Ii - ( beta + mu ) * Ii
    Rk3 <- ( 1 - eta ) * beta * Ii - mu * Ri
    
    Si <- S[i] + dt * Sk3
    Ii <- I[i] + dt * Ik3
    Ri <- R[i] + dt * Rk3
    Sk4 <- mu * ( 1 - Si ) - alpha * Si * Ii + eta * beta * Ii
    Ik4 <- alpha * Si * Ii - ( beta + mu ) * Ii
    Rk4 <- ( 1 - eta ) * beta * Ii - mu * Ri
    
    S[ i + 1 ] = S[ i ] + ( 1 / 6 ) * dt * ( Sk1 + 2 * Sk2 + 2 * Sk3 + Sk4 )
    I[ i + 1 ] = I[ i ] + ( 1 / 6 ) * dt * ( Ik1 + 2 * Ik2 + 2 * Ik3 + Ik4 )
    R[ i + 1 ] = R[ i ] + ( 1 / 6 ) * dt * ( Rk1 + 2 * Rk2 + 2 * Rk3 + Rk4 )
    
  } 
  
  return( list( 'S' = S, 'I' = I, 'R' = R ) )
}

# Solver for SEIR model considering death ----------------------------------------------------------s
# The solver is implemented with the 4th order Runge-Kutta method
euler_solver_seir <- function( t, alpha, beta, sigma, mu, S0, E0, I0, R0 ) {
  n <- length( t )
  
  S <- array( S0, dim = n )
  E <- array( E0, dim = n )
  I <- array( I0, dim = n )
  R <- array( R0, dim = n )
  
  for ( i in 1:( n - 1 ) ) {
    dt <- t[ i + 1 ] - t[ i ]
    
    Si <- S[i]
    Ei <- S[i]
    Ii <- I[i]
    Ri <- R[i]
    Sk1 <- mu * ( 1 - Si ) - alpha * Si * Ii
    Ek1 <- alpha * Si * Ii - ( mu + sigma ) * Ei
    Ik1 <- sigma * Ei - ( beta + mu ) * Ii
    Rk1 <- beta * Ii - mu * Ri
    
    Si <- S[i] + 0.5 * dt * Sk1
    Ei <- E[i] + 0.5 * dt * Ek1
    Ii <- I[i] + 0.5 * dt * Ik1
    Ri <- R[i] + 0.5 * dt * Rk1
    Sk2 <- mu * ( 1 - Si ) - alpha * Si * Ii
    Ek2 <- alpha * Si * Ii - ( mu + sigma ) * Ei
    Ik2 <- sigma * Ei - ( beta + mu ) * Ii
    Rk2 <- beta * Ii - mu * Ri
    
    Si <- S[i] + 0.5 * dt * Sk2
    Ei <- E[i] + 0.5 * dt * Ek2
    Ii <- I[i] + 0.5 * dt * Ik2
    Ri <- R[i] + 0.5 * dt * Rk2
    Sk3 <- mu * ( 1 - Si ) - alpha * Si * Ii
    Ek3 <- alpha * Si * Ii - ( mu + sigma ) * Ei
    Ik3 <- sigma * Ei - ( beta + mu ) * Ii
    Rk3 <- beta * Ii - mu * Ri
    
    Si <- S[i] + dt * Sk3
    Ei <- E[i] + dt * Ek3
    Ii <- I[i] + dt * Ik3
    Ri <- R[i] + dt * Rk3
    Sk4 <- mu * ( 1 - Si ) - alpha * Si * Ii
    Ek4 <- alpha * Si * Ii - ( mu + sigma ) * Ei
    Ik4 <- sigma * Ei - ( beta + mu ) * Ii
    Rk4 <- beta * Ii - mu * Ri
    
    S[ i + 1 ] = S[ i ] + ( 1 / 6 ) * dt * ( Sk1 + 2 * Sk2 + 2 * Sk3 + Sk4 )
    E[ i + 1 ] = E[ i ] + ( 1 / 6 ) * dt * ( Ek1 + 2 * Ek2 + 2 * Ek3 + Ek4 )
    I[ i + 1 ] = I[ i ] + ( 1 / 6 ) * dt * ( Ik1 + 2 * Ik2 + 2 * Ik3 + Ik4 )
    R[ i + 1 ] = R[ i ] + ( 1 / 6 ) * dt * ( Rk1 + 2 * Rk2 + 2 * Rk3 + Rk4 )
    
  } 
  
  return( list( 'S' = S, 'E' = E, 'I' = I, 'R' = R ) )
}
