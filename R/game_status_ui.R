get_active_contract <- function(state) {
  if (state$get_game_status() == "finished") return(NULL)

  performance <- state$get_performance()
  layout_column_wrap(
    id = "activecontract",
    width = 1,
    heights_equal = "row",
    fill = FALSE,
    hr(),
    h3("Player status"),
    layout_column_wrap(
      width = 1/3,
      value_box(
        title = "Player",
        value = tags$span(class = "fs-1", state$active_player_name),
        showcase = bsicons::bs_icon("incognito"),
        theme = "info"
      ),
      value_box(
        title = "Status",
        value = tags$span(class = "fs-1", ifelse(state$is_alive(), "Alive", "Dead")),
        showcase = icon("heartbeat"),
        theme = ifelse(state$is_alive(), "success", "danger")
      ),
      value_box(
        title = "Kills",
        value = tags$span(class = "fs-1", NROW(state$get_performance()$kills)),
        showcase = icon("skull"),
        theme = "info"
      )
    ),
    hr(),
    if (state$is_alive()) {
      tags$div(
        h3("Current contract"),
        layout_column_wrap(
          width = 1/3,
          value_box(
            title = "Target",
            value = tags$span(class = "fs-1", state$get_target()),
            showcase = bsicons::bs_icon("crosshair"),
            theme = "purple"
          ),
          value_box(
            title = "Item",
            value = tags$span(class = "fs-1", state$get_item()),
            showcase = icon("wrench"),
            theme = "purple"
          ),
          value_box(
            title = "Location",
            value = tags$span(class = "fs-1", state$get_location()),
            showcase = bsicons::bs_icon("geo-alt"),
            theme = "purple"
          )
        )
      )
    } else {
      tags$div(
        h3("Manner of death:"),
        layout_column_wrap(
          width = 1/3,
          value_box(
            title = "Murdered by",
            value = tags$span(class = "fs-1", performance$killed_by$player),
            showcase = bsicons::bs_icon("crosshair"),
            theme = "warning"
          ),
          value_box(
            title = "Weapon",
            value = tags$span(class = "fs-1", performance$killed_by$item),
            showcase = icon("wrench"),
            theme = "warning"
          ),
          value_box(
            title = "Location",
            value = tags$span(class = "fs-1", performance$killed_by$location),
            showcase = bsicons::bs_icon("geo-alt"),
            theme = "warning"
          )
        )
      )
    }
  )
}

get_game_timeline <- function(state) {

  start <- pull(state$details, started_at)
  kills <- state$contracts |>
    collect() |>
    filter(active == 0, !is.na(execution_time)) |>
    arrange(execution_time)
  tags$div(
    br(),
    hr(),
    br(),
    h3("Timeline"),
    br(),
    tags$ul(
      tags$li(
        tags$div(class = "startdate", pretty_timestamp(as.POSIXct(start), title = TRUE)),
        tags$div(class = "title", "Game start")
      ),
      if (NROW(kills) == 0) {
        tags$li(
          tags$div(class = "descr", "(No kills yet)")
        )
      } else {
        purrr::pmap(
          kills,
          \(...) {
            kill <- list(...)
            tags$li(
              tags$div(class = "deathdate",
                       pretty_timestamp(
                         kill$execution_time,
                         title = TRUE)),
              tags$div(class = "title",
                       p(tags$b(kill$player),
                         "killed",
                         tags$b(kill$target))),
              tags$div(class = "descr",
                       tags$p(
                         kill$player,
                         death_verb(adverb = TRUE),
                         tags$b(kill$target),
                         "with",
                         tags$b(kill$item),
                         "while they were",
                         tags$b(kill$location),
                         pretty_timestamp(kill$execution_time)
                       ))
            )
          }
        )
      }
    )
  )


}

get_player_leaderboard <- function(state) {

  completed_contracts <- state$contracts |>
    filter(!is.na(execution_time)) |>
    collect() |>
    mutate(
      execution_time = lubridate::ymd_hms(execution_time),
      execution_order = min_rank(execution_time)
    )
  death_order <- select(completed_contracts, target,execution_time, execution_order)
  kills <- count(completed_contracts, player) |>
    mutate(n = if_else(is.na(n), 0, n))
  indirect_kills <- completed_contracts |>
    left_join(kills,
              by = c("target" = "player")) |>
    group_by(player) |>
    summarise(n = sum(n)) |>
    mutate(n = if_else(is.na(n), 0, n))
  players <- state$players |>
    collect() |>
    left_join(death_order,
              by = c("player" = "target")) |>
    left_join(kills,
              by = c("player")) |>
    left_join(indirect_kills,
              by = c("player"),
              suffix = c("", "_indirect")) |>
    mutate(
      execution_order = coalesce(execution_order, Inf),
      n = coalesce(n, 0),
      n_indirect = coalesce(n_indirect, 0),

      # Get rank of each metric
      across(
        c(execution_order, n, n_indirect),
        \(x) min_rank(desc(x)),
        .names = "{col}_rank"
      )
    )

  game_type <- pull(state$details, win_condition)
  if (game_type == "duel") {
    player_order <- players |>
      mutate(position = min_rank(
        pick(execution_order_rank, n_rank, n_indirect_rank)
        )
      )
  } else {
    player_order <- players |>
      mutate(position = min_rank(
        pick(n_rank, n_indirect_rank, execution_order_rank)
        )
      )
  }

  start_time <- pull(state$details, started_at) |>
    as.POSIXct()

  tbl_data <- player_order |>
    transmute(
      position = to_ordinal(position),
      player,
      kills = n,
      indirect_kills = n_indirect,
      alive,
      time_survived = if_else(
        is.na(execution_time),
        "",
        lubridate::as.duration(execution_time-start_time) |>
          stringr::str_extract(pattern = "(?<=\\(~).*(?=\\))")
      )
    ) |>
    arrange(position)
  tags$div(
    id = ""
  )
  tbl_data |>
    reactable::reactable(
      style = list(backgroundColor = "rgba(255, 255, 255, 0.4)"),
      pagination = FALSE,
      sortable = FALSE,
      width = "100%",
      rowStyle = function(index) {
        if (tbl_data$position[index] == "1st") {
          list(background = "rgba(255, 215, 0, 1)")
        } else if (tbl_data$position[index] == "2nd") {
          list(background = "rgba(192, 192, 192, 1)")
        } else if (tbl_data$position[index] == "3rd") {
          list(background = "rgba(205, 127, 50, 1)")
        }
      },
      columns = list(
        position = reactable::colDef(
          name = "Pos"
        ),
        player = reactable::colDef(
          name = "Player"
        ),
        kills = reactable::colDef(
          name = "Kills"
        ),
        indirect_kills = reactable::colDef(
          name = "Kills (indirect)"
        ),
        alive = reactable::colDef(
          name = "Status",
          html = TRUE,
          cell = function(value) {
            if (value == 1) {
              "&#128154;"
            } else {
              "&#128128"
            }
          }
        ),
        time_survived = reactable::colDef(
          name = "Time Survived"
        )
      )
    )
}
