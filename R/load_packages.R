message( paste0( rep( '-', 100 ), collapse = '' ) )
message( '\tLoading packages' )

suppressPackageStartupMessages( library( data.table ) )
suppressPackageStartupMessages( library( lubridate ) )
suppressPackageStartupMessages( library( ggplot2 ) )
suppressPackageStartupMessages( library( knitr ) )
suppressPackageStartupMessages( library( extrafont ) )

suppressMessages( loadfonts() )

message( paste0( rep( '-', 100 ), collapse = '' ) )
