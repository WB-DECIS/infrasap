#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(...) {
  golem::with_golem_options(
    app = shinyApp(ui = app_ui,
                   server = app_server,
                   options = list('launch.browswer' = !(grepl('aws', tolower(Sys.info()['release']))))),
    golem_opts = list(...)
  )
}