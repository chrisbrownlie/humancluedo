#' Create sqlite database connection
#'
#' @return a connection object
#' @export
db_conn <- function() {
  dbpath <- fs::path("data-store", "hc-db.sqlite")
  if (!shiny::isRunning()) {
    dbpath <- fs::path("inst", "app", "data-store", "hc-db.sqlite")
  }
  DBI::dbConnect(
    RSQLite::SQLite(),
    dbpath
  )
}

#' Create database tables if they don't already exist
#'
#' @param conn a connection to the database
#'
#' @return NULL, invisibly
#' @export
create_database <- function(conn = db_conn()) {

  if (!dbExistsTable(conn, "games")) {
    tibble::tibble(
      game_id = character(),
      name = character(),
      active = logical(),
      win_condition = character(),
      population_method = character(),
      started_at = lubridate::POSIXct(),
      deadline = lubridate::POSIXct()
    ) |>
      dbWriteTable(
        conn = conn,
        name = "games"
      )
    cli::cli_alert_success("Created [games]")
  } else {
    cli::cli_alert_info("[games] already exists")
  }

  if (!dbExistsTable(conn, "players")) {
    tibble::tibble(
      game_id = character(),
      player = character(),
      identifier = character(),
      alive = logical(),
      is_admin = logical()
    ) |>
      dbWriteTable(
        conn = conn,
        name = "players"
      )
    cli::cli_alert_success("Created [players]")
  } else {
    cli::cli_alert_info("[players] already exists")
  }

  if (!dbExistsTable(conn, "items")) {
    tibble::tibble(
      game_id = character(),
      item = character(),
      generated_by = character()
    ) |>
      dbWriteTable(
        conn = conn,
        name = "items"
      )
    cli::cli_alert_success("Created [items]")
  } else {
    cli::cli_alert_info("[items] already exists")
  }

  if (!dbExistsTable(conn, "locations")) {
    tibble::tibble(
      game_id = character(),
      location = character(),
      generated_by = character()
    ) |>
      dbWriteTable(
        conn = conn,
        name = "locations"
      )
    cli::cli_alert_success("Created [locations]")
  } else {
    cli::cli_alert_info("[locations] already exists")
  }

  if (!dbExistsTable(conn, "contracts")) {
    tibble::tibble(
      game_id = character(),
      player = character(),
      item = character(),
      location = character(),
      target = character(),
      active = logical(),
      execution_time = lubridate::POSIXct(),
      execution_notes = character()
    ) |>
      dbWriteTable(
        conn = conn,
        name = "contracts"
      )
    cli::cli_alert_success("Created [contracts]")
  } else {
    cli::cli_alert_info("[contracts] already exists")
  }

}
