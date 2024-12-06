#' Run the human cluedo app
#'
#' @export
run_app <- function() {

  if (interactive()) {

    runApp(appDir = pkg_file("inst", "app"))

  } else {

    shinyAppDir(appDir = pkg_file("inst", "app"),
                options = list(host = "0.0.0.0",
                               port = 3838))

  }

}
l <- run_app
