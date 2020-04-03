message( paste0( rep( '-', 100 ), collapse = '' ) )
message( '\tSetting parameters' )

options( stringsAsFactors = FALSE )

par <- new.env()
par$results <- 'results/'
par$RData <- 'RData/'
par$data <- 'data/'

par$use_git_or_https <- FALSE
par$update_git_covid_19 <- TRUE
if ( par$use_git_or_https ) {
  par$jh_git_data <- 'git@github.com:CSSEGISandData/COVID-19.git'
} else {
  par$jh_git_data <- 'https://github.com/CSSEGISandData/COVID-19.git'  
}

par$src_covid_19 <- 'COVID-19/'
if ( !dir.exists( par$src_covid_19 ) ) {
  message( '\tCloning COVID-19 project of John Hopkins' )
  system( paste0( 'git clone ', par$jh_git_data ), intern = FALSE )
} else {
  if ( par$update_git_covid_19 ) {
    message( '\tUpdating COVID-19 project of John Hopkins' )
    system( 'cd COVID-19; git pull', intern = FALSE )
  }
}

par$src_csse_info <- paste0( par$src_covid_19, 'csse_covid_19_data/csse_covid_19_time_series/' )
par$src_who_info <- paste0( par$src_covid_19, 'who_covid_19_situation_reports/who_covid_19_sit_rep_time_series/' )

# Folder for RData's
if ( !dir.exists( par$RData ) ) {
  dir.create( par$RData )
}

# Folder for results
if ( !dir.exists( par$results ) ) {
  dir.create( par$results )
}

message( paste0( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'par' ) ) ] )
gc()