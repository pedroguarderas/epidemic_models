message( paste0( rep( '-', 100 ), collapse = '' ) )

# Configuration
source( 'R/load_packages.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/parameters.R', encoding = 'UTF-8', echo = FALSE )

# Statistics
source( 'R/load_covid_19_info.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/estimation_covid_19.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/graphs_statistics_covid_19.R', encoding = 'UTF-8', echo = FALSE )

# Modelling
source( 'R/SIR_model_ecu_covid_19.R', encoding = 'UTF-8', echo = FALSE )

# Reporting

message( paste0( rep( '-', 100 ), collapse = '' ) )
gc()
