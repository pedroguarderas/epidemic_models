# Solver empleando el esque de Euler para el modelo SIR --------------------------------------------
# No se considera muerte por causas naturales o enfermedad
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

# Solver empleando el esque de Euler para el modelo SIR con muerte ---------------------------------
# Se considera muerte por enfermedad y causas naturales
# Implementado con solver de orden cuarto de Runge-Kutta
euler_solver_sir_mor <- function( t, alpha, beta, theta, mu, S0, I0, R0 ) {
  n <- length( t )
  
  S <- array( S0, dim = n )
  I <- array( I0, dim = n )
  R <- array( R0, dim = n )
  
  for ( i in 1:( n - 1 ) ) {
    dt <- t[ i + 1 ] - t[ i ]
    
    Si <- S[i]
    Ii <- I[i]
    Ri <- R[i]
    Sk1 <- dt * ( mu * ( 1 - Si ) - alpha * Si * Ii + theta * beta * Ii )
    Ik1 <- dt * ( alpha * Si * Ii - ( beta + mu ) * Ii )
    Rk1 <- dt * ( ( 1 - theta ) * beta * Ii - mu * Ri )
    
    Si <- S[i] + 0.5 * Sk1
    Ii <- I[i] + 0.5 * Ik1
    Ri <- R[i] + 0.5 * Rk1
    Sk2 <- dt * ( mu * ( 1 - Si ) - alpha * Si * Ii + theta * beta * Ii )
    Ik2 <- dt * ( alpha * Si * Ii - ( beta + mu ) * Ii )
    Rk2 <- dt * ( ( 1 - theta ) * beta * Ii - mu * Ri )
    
    Si <- S[i] + 0.5 * Sk2
    Ii <- I[i] + 0.5 * Ik2
    Ri <- R[i] + 0.5 * Rk2
    Sk3 <- dt * ( mu * ( 1 - Si ) - alpha * Si * Ii + theta * beta * Ii )
    Ik3 <- dt * ( alpha * Si * Ii - ( beta + mu ) * Ii )
    Rk3 <- dt * ( ( 1 - theta ) * beta * Ii - mu * Ri )
    
    Si <- S[i] + Sk3
    Ii <- I[i] + Ik3
    Ri <- R[i] + Rk3
    Sk4 <- dt * ( mu * ( 1 - Si ) - alpha * Si * Ii + theta * beta * Ii )
    Ik4 <- dt * ( alpha * Si * Ii - ( beta + mu ) * Ii )
    Rk4 <- dt * ( ( 1 - theta ) * beta * Ii - mu * Ri )
    
    S[ i + 1 ] = S[ i ] + ( 1 / 6 ) * ( Sk1 + 2 * Sk2 + 2 * Sk3 + Sk4 )
    I[ i + 1 ] = I[ i ] + ( 1 / 6 ) * ( Ik1 + 2 * Ik2 + 2 * Ik3 + Ik4 )
    R[ i + 1 ] = R[ i ] + ( 1 / 6 ) * ( Rk1 + 2 * Rk2 + 2 * Rk3 + Rk4 )
    
  } 
  
  return( list( 'S' = S, 'I' = I, 'R' = R ) )
}
