# COVID-19 -----------------------------------------------------------------------------------------
message( paste0( rep( '-', 100 ), collapse = '' ) )
message( '\tCargando información de camas' )

# Procesando año 2017 ------------------------------------------------------------------------------
camas_2017 <- read.csv( file = paste0( par$data, 'MSP_camas_hospitales/camas_2017.csv' ), 
                        header = TRUE, sep = ';', fileEncoding = 'Latin1' )

camas_2017 <- as.data.table( camas_2017 )

camas_ecu <- copy( camas_2017 )

# Guardando resultados -----------------------------------------------------------------------------
save( camas_ecu, file = paste0( par$RData, 'inec_camas_hosp_ecu.RData' ) )

message( paste0( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'par' ) ) ] )
gc()
