#' Start the dropR Shiny App
#' 
#' Starts the interactive web application to use dropR in your web browser. 
#' Make sure to use Google Chrome or Firefox for best experience.
#' 
#' @export
start_app <- function(){
  shiny::runApp(system.file('app', package='dropR'),launch.browser = T)  
}
