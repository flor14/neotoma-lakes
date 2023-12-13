#' Launches the app
#'
#' @return either run the app as a side effect or return a shiny.appobj object
#'
#' @importFrom shiny shinyAppDir
#'
#' @export
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
run_app <- function() {

  shiny::runApp(appDir = system.file("app",
                                   package = "neotomalakes"))
}
