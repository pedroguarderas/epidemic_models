message( paste0( rep( '-', 100 ), collapse = '' ) )

load( paste0( par$RData, 'inec_beds_hospital_ecu.RData' ) )

# Beds by province ---------------------------------------------------------------------------------
bed_prov <- camas_2018[ , list( dotemerg = sum( dotemerg, na.rm = TRUE ),
                                dotcinte = sum( dotcinte, na.rm = TRUE ),
                                dotcintrm = sum( dotcintrm, na.rm = TRUE ),
                                dototrapo = sum( dototrapo, na.rm = TRUE ) ), 
                        by = list( prov_ubi ) ]

# Beds by province ---------------------------------------------------------------------------------
bed_cant <- camas_2018[ , list( dotemerg = sum( dotemerg, na.rm = TRUE ),
                                dotcinte = sum( dotcinte, na.rm = TRUE ),
                                dotcintrm = sum( dotcintrm, na.rm = TRUE ),
                                dototrapo = sum( dototrapo, na.rm = TRUE ) ), 
                        by = list( prov_ubi, cant_ubi ) ]


save( bed_cant, bed_prov,
      file = paste0( par$RData, 'inec_beds_hospital_statistics.RData' ) )


message( paste0( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'par' ) ) ] )
gc()
