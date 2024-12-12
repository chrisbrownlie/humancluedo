#' Update the game status ui based on
update_game_status_ui <- function(state) {
  # Remove existing status info and insert new state
  update_game_details(state)
  update_player_status(state)
  update_game_link(state)

  return(invisible(NULL))
}

#' Update basic game details like the name and the active player
update_game_details <- function(state) {
  shiny::removeUI("#gamename")
  shiny::removeUI("#activeplayer")
  shiny::insertUI(
    "#confirm_kill",
    card_title("Human Cluedo -", tags$b(state$active_game_name), id = "gamename"),
    where = "beforeBegin"
  )
  shiny::insertUI(
    "#confirm_kill",
    p("You are:", tags$b(state$active_player_name), id = "activeplayer"),
    where = "beforeBegin"
  )
}

#' Update the current players status
update_player_status <- function(state) {
  shiny::removeUI("#activecontract")
  shiny::removeUI("#playerstatus")
  shiny::removeUI("#game_performance")
  shiny::removeUI("#playerstatus_start")
  players <- collect(state$players)
  if (state$get_game_status() == "awaiting") {
    shiny::insertUI(
      "#confirm_kill",
      div(
        id = "activecontract",
        p("The game has not yet started, once all players have joined, you will",
          "see your contract appear here."),
        p("These are the other players in your game:"),
        tags$ul(
          purrr::map2(
            players$player[players$player != state$active_player_name],
            players$identifier[players$player != state$active_player_name],
            \(name, id) {
              tags$li(
                p(name, if_else(is.na(id), "(yet to join)", "(joined)")),
                class = if_else(is.na(id), "text-red", "text-green")
              )
            }
          )
        )
      ),
      where = "beforeBegin"

    )
    shinyjs::hide("confirm_kill")
    return(invisible(NULL))
  }
  shiny::insertUI(
    "#game_status",
    p("Current player status:", id = "playerstatus_start"),
    where = "beforeEnd"
  )
  performance <- state$get_performance()

  if (isTRUE(performance$is_alive)) {
    shiny::insertUI(
      "#confirm_kill",
      p("You are trying to kill",
        tags$b(state$get_target()),
        "with",
        tags$b(state$get_item()),
        "-",
        tags$b(state$get_location()),
        id = "activecontract"),
      where = "beforeBegin"
    )
  } else {
    shiny::insertUI(
      "#confirm_kill",
      p("You were killed by",
        tags$b(performance$killed_by$player),
        "with",
        tags$b(performance$killed_by$item),
        "-",
        tags$b(performance$killed_by$location),
        "at",
        format(performance$killed_by$execution_time, "%R"),
        "on",
        format(performance$killed_by$execution_time, "%d-%m-%Y"),
        id = "activecontract"),
      where = "beforeBegin"
    )
  }
  shiny::insertUI(
    "#confirm_kill",
    tags$div(
      id = "game_performance",
      performance$html
    ),
    where = "beforeBegin"
  )
  shiny::insertUI(
    "#playerstatus_start",
    tags$div(
      id = "playerstatus",
      state$get_player_status(as_html = TRUE)
    ),
    where = "afterEnd"
  )

  shinyjs::toggleState("confirm_kill",
                       condition = state$target_is_alive())
}

#' Update the game link
update_game_link <- function(state) {

  shiny::removeUI("#game_link")

  shiny::insertUI(
    "#playerstatus",
    tags$div(
      id = "game_link",
      actionButton("game_link",
                   label = tagList(
                     bsicons::bs_icon("share"),
                     "Get game link"
                   ),
                   icon = NULL)
    ),
    where = "afterEnd"
  )
}

show_invalid_game_id <- function() {
  shinyjs::hide("game_status")
  shinyjs::show("invalid_game", anim =TRUE, time = 1)
  shinyjs::show("create_game")
}


#' Show a game view in the app
#'
#' @param game_state a GameState object to use to populate game information
#' @param game_id optional game ID. If left NULL, will use the currently active
#' game in the supplied game_state
launch_a_game <- function(game_state, existing_cookie, game_id = NULL) {
  shinyjs::hide("create_game")
  shinyjs::hide("invalid_game")
  shinyjs::show("game_status")

  # Initialise game
  if (!length(game_id)) {
    game_id <- game_state$active_game
  }
  game_exists <- game_state$initialise_game(game_id)
  game_state$set_active_player(existing_cookie)

  # If link is invalid, show message and exit observer
  if (!game_exists) {
    show_invalid_game_id()
    return(NULL)
  }

  # If new player, show popup to select name and maybe enter items
  if (length(game_state$active_player_name) == 0) {
    showModal(player_join_modal(game_state))
  } else {
    # If waiting for players, check if this is the last player to join.
    # If so, set the contracts so the game can begin
    if (game_state$get_game_status() == "awaiting" &&
        sum(is.na(pull(game_state$players, identifier))) == 0) {
      game_state$set_contracts()
    }
    update_game_status_ui(game_state)
  }
}
