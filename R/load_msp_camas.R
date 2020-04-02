# COVID-19 -----------------------------------------------------------------------------------------
message( paste0( rep( '-', 100 ), collapse = '' ) )
message( '\tCargando información de camas' )

# Camas año 2016 -----------------------------------------------------------------------------------
camas_2016 <- read.csv( file = paste0( par$data, 'MSP_camas_hospitales/camas_2016.csv' ), 
                        header = TRUE, sep = ';', fileEncoding = 'Latin1' )

camas_2016 <- as.data.table( camas_2016 )
camas_2016[ , y := 2016 ]

# Camas año 2017 -----------------------------------------------------------------------------------
camas_2017 <- read.csv( file = paste0( par$data, 'MSP_camas_hospitales/camas_2017.csv' ), 
                        header = TRUE, sep = ';', fileEncoding = 'Latin1' )

camas_2017 <- as.data.table( camas_2017 )
camas_2017[ , y := 2017 ]

# Recursos salud año 2017 --------------------------------------------------------------------------
ras_2017 <- read.csv( file = paste0( par$data, 'MSP_recursos_salud/RAS_2017.csv' ), 
                        header = TRUE, sep = ';', fileEncoding = 'Latin1' )

ras_2017 <- as.data.table( ras_2017 )
ras_2017[ , y := 2017 ]

# Guardando resultados -----------------------------------------------------------------------------
save( camas_2016, camas_2017, file = paste0( par$RData, 'inec_camas_hosp_ecu.RData' ) )

message( paste0( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'par' ) ) ] )
gc()
