#' Create duckdb database connection
#'
#' @return a connection object
#' @export
db_conn <- function() {
  DBI::dbConnect(duckdb::duckdb(),
                 dbdir = pkg_file("hc-db.duckdb"))
}

#' Create database tables if they don't already exist
#'
#' @param conn a connection to the database
#'
#' @return NULL, invisibly
#' @export
create_database <- function(conn = db_conn()) {

  if (!DBI::dbExistsTable(conn, "games")) {
    tibble::tibble(
      game_id = character(),
      name = character(),
      active = logical(),
      win_condition = character(),
      population_method = character()
    ) |>
      DBI::dbWriteTable(
        conn = conn,
        name = "games"
      )
    cli::cli_alert_success("Created [games]")
  } else {
    cli::cli_alert_info("[games] already exists")
  }

  if (!DBI::dbExistsTable(conn, "players")) {
    tibble::tibble(
      game_id = character(),
      player = character(),
      identifier = character(),
      alive = logical(),
      is_admin = logical()
    ) |>
      DBI::dbWriteTable(
        conn = conn,
        name = "players"
      )
    cli::cli_alert_success("Created [players]")
  } else {
    cli::cli_alert_info("[players] already exists")
  }

  if (!DBI::dbExistsTable(conn, "items")) {
    tibble::tibble(
      game_id = character(),
      item = character(),
      generated_by = character()
    ) |>
      DBI::dbWriteTable(
        conn = conn,
        name = "items"
      )
    cli::cli_alert_success("Created [items]")
  } else {
    cli::cli_alert_info("[items] already exists")
  }

  if (!DBI::dbExistsTable(conn, "locations")) {
    tibble::tibble(
      game_id = character(),
      location = character(),
      generated_by = character()
    ) |>
      DBI::dbWriteTable(
        conn = conn,
        name = "locations"
      )
    cli::cli_alert_success("Created [locations]")
  } else {
    cli::cli_alert_info("[locations] already exists")
  }

  if (!DBI::dbExistsTable(conn, "contracts")) {
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
      DBI::dbWriteTable(
        conn = conn,
        name = "contracts"
      )
    cli::cli_alert_success("Created [contracts]")
  } else {
    cli::cli_alert_info("[contracts] already exists")
  }

}
