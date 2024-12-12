#' Get a file from the human cluedo app
#'
#' @param ... passed onto fs::path_package
#'
#' @export
pkg_file <- function(...) {
  fs::path_package(
    package = "humancluedo",
    ...
  )
}

#' Convert a comma-separated list to a vector
#'
#' @param csl a comma-separated list as a single-length character vector
#'
#' @return a multi-element character vector, split by comma
#' @export
csl_to_vec <- function(csl, remove_empty = TRUE) {
  v <- csl |>
    strsplit(",") |>
    purrr::pluck(1) |>
    trimws()
  if (!remove_empty) return(v)
  v[v != ""]
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
