message( paste0( rep( '-', 100 ), collapse = '' ) )

load( paste0( par$RData, 'inec_beds_hospital_ecu.RData' ) )

# Beds by province ---------------------------------------------------------------------------------
bed_prov <- camas_2018[ , list( dotneumo = sum( dotneumo, na.rm = TRUE ),
                                disneumo = sum( disneumo, na.rm = TRUE ),
                                camas_dnor = sum( camas_dnor, na.rm = TRUE ),
                                camas_disp = sum( camas_disp, na.rm = TRUE ),
                                dotemerg = sum( dotemerg, na.rm = TRUE ),
                                dotcinte = sum( dotcinte, na.rm = TRUE ),
                                dotcintrm = sum( dotcintrm, na.rm = TRUE ),
                                dototrapo = sum( dototrapo, na.rm = TRUE ) ), 
                        by = list( prov_ubi ) ]
setorder( bed_prov, prov_ubi )

# Beds by province and type ------------------------------------------------------------------------
bed_prov_tip <- camas_2018[ , list( dotneumo = sum( dotneumo, na.rm = TRUE ),
                                    disneumo = sum( disneumo, na.rm = TRUE ),
                                    camas_dnor = sum( camas_dnor, na.rm = TRUE ),
                                    camas_disp = sum( camas_disp, na.rm = TRUE ),
                                    dotemerg = sum( dotemerg, na.rm = TRUE ),
                                    dotcinte = sum( dotcinte, na.rm = TRUE ),
                                    dotcintrm = sum( dotcintrm, na.rm = TRUE ),
                                    dototrapo = sum( dototrapo, na.rm = TRUE ) ), 
                            by = list( prov_ubi, tipo ) ]
setorder( bed_prov_tip, prov_ubi, tipo )

# Beds by province ---------------------------------------------------------------------------------
bed_cant <- camas_2018[ , list( dotneumo = sum( dotneumo, na.rm = TRUE ),
                                disneumo = sum( disneumo, na.rm = TRUE ),
                                camas_dnor = sum( camas_dnor, na.rm = TRUE ),
                                camas_disp = sum( camas_disp, na.rm = TRUE ),
                                dotemerg = sum( dotemerg, na.rm = TRUE ),
                                dotcinte = sum( dotcinte, na.rm = TRUE ),
                                dotcintrm = sum( dotcintrm, na.rm = TRUE ),
                                dototrapo = sum( dototrapo, na.rm = TRUE ) ), 
                        by = list( prov_ubi, cant_ubi ) ]
setorder( bed_cant, prov_ubi, cant_ubi )


save( bed_cant, bed_prov, bed_prov_tip,
      file = paste0( par$RData, 'inec_beds_hospital_statistics.RData' ) )

message( paste0( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'par' ) ) ] )
gc()
