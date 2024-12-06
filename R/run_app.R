#' Run the human cluedo app
#'
#' @export
run_app <- function() {

  pkg_file("inst", "app") |>
    shiny::shinyAppDir()

}
l <- run_app
