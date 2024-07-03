#' Start the dropR Shiny App
#' 
#' Starts the interactive web application to use dropR in your web browser. 
#' Make sure to use Google Chrome or Firefox for best experience.
#' 
#' The app will give less experienced R users or statisticians a good overview of
#' how to conduct dropout analysis. For more experienced analysts, it can still be very helpful 
#' in guiding how to use the package as there are some steps that should be taken in order,
#' which is outlined in the app (as well as function documentation).
#' 
#' @returns No return value; starts the shiny app as a helper to get started with dropout analysis. All app procedures are available as functions.
#' 
#' @export
start_app <- function(){
  shiny::runApp(system.file('app', package='dropR'), launch.browser = TRUE)  
}
