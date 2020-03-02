# Script para generar presentación
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tCompilación de presentación' )

#---------------------------------------------------------------------------------------------------
message( '\tCargando data frames' )
configuration <- new.env()
configuration$work_dir <- getwd()

options( scipen = 99 )
setNumericRounding( 0 )

setwd( configuration$work_dir )

#---------------------------------------------------------------------------------------------------
message( '\tHaciendo tablas y gráficos' )
# source( 'R/21_grafico_ajuste_tasas.R' )

#---------------------------------------------------------------------------------------------------
message( '\tGuardando paths' )
sld_work_dir<-paste0( configuration$work_dir, '/slides/' )
setwd( sld_work_dir )

# load( '/home/leovelez/Development/decrement_models/test/GPF_sri.RData' )

message( '\tEstableciendo opciones para Sweave' )
sld_dbg<-FALSE
sld_quiet<-TRUE

#---------------------------------------------------------------------------------------------------
message('\tParámetros para nota técnica')

#---------------------------------------------------------------------------------------------------
message('\tEstableciendo archivos a ser copiados')
# fls_latex_org<-c( )
# fls_latex_des<-c( )

#---------------------------------------------------------------------------------------------------
message( '\tCopiando archivos LaTeX' )
# file.copy( fls_latex_org, fls_latex_des, overwrite = TRUE )

#---------------------------------------------------------------------------------------------------
message('\tInicio Compilación LaTeX' )
sld_file<-'slides.tex'
knit( input = "slides.Rnw", output = sld_file, quiet = sld_quiet, encoding = 'utf8' )
tools::texi2pdf( sld_file, quiet = sld_quiet, clean = TRUE )
gc()
message( '\tFin compilación LaTeX' )

message( '\tBorrando archivos auxiliares' )
# file.remove( fls_latex_des, recursive = TRUE )

setwd( configuration$work_dir )
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ls()!='configuration'] )
gc()
