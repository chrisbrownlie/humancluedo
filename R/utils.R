#' Get a file from the human cluedo app
#'
#' @param ... passed onto fs::path_package
#'
#' @export
pkg_file_strict <- function(...) {
  fs::path_package(
    package = "humancluedo",
    ...
  )
}
pkg_file <- purrr::possibly(
  pkg_file_strict,
  otherwise = fs::path()
)

#' Convert a comma-separated list to a vector
#'
#' @param csl a comma-separated list as a single-length character vector
#'
#' @return a multi-element character vector, split by comma
#' @export
csl_to_vec <- function(csl) {
  csl |>
    strsplit(",") |>
    purrr::pluck(1) |>
    trimws()
}


display_vec <- function(vec, label, add_to_first = NULL) {
  tags$li(
    tags$b(paste0(length(vec), " ", label, ": ")),
    tags$ul(
      purrr::imap(vec, \(x, i) {
        if (i != 1) return(tags$li(x))
        tags$li(x, add_to_first)
        }
        )
      )
  )
}
