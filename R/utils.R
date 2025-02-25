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



pretty_timestamp <- function(datetime, title = FALSE) {
  et <- datetime
  if (!lubridate::is.POSIXct(datetime)) {
    et <- datetime |>
      lubridate::ymd_hms()
  }
  days_ago <- lubridate::today()-lubridate::date(et)

  if (days_ago == 0) {
    ex_date <- ifelse(title, "Today", " today!")
  } else if (days_ago == 1) {
    ex_date <- ifelse(title, "Yesterday", " yesterday!")
  } else {
    ex_date <- ifelse(title,
                      format(et, format="%A %e %b"),
                      paste0(" on ", format(et, format="%A %e %B %Y!")))
  }
  if (title) {
    title_ts <- paste(
      ex_date,
      format(et, format = "%R %Z")
    )
    return(title_ts)
  }
  paste0(
    " at ",
    format(et, format = "%R %Z"),
    ex_date
  )
}



to_ordinal <- function(nums) {

  suffix <- case_when(
    nums%%10 == 1 & nums%%100 != 11 ~ "st",
    nums%%10 == 2 & nums%%100 != 12 ~ "nd",
    nums%%10 == 3 & nums%%100 != 13 ~ "rd",
    .default = "th"
  )
  paste0(nums, suffix)
}
