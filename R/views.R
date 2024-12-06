#' Update the game status ui based on
update_game_status_ui <- function(state) {
  # Remove existing status info and insert new state
  shiny::removeUI("#gamename")
  shiny::removeUI("#activeplayer")
  shiny::removeUI("#activecontract")
  shiny::removeUI("#playerstatus")
  shiny::removeUI("#game_performance")
  shiny::removeUI("#game_link")
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

  shinyjs::toggleState("confirm_kill",
                       condition = state$target_is_alive())
  return(invisible(NULL))
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
launch_a_game <- function(game_state, game_id = NULL) {
  shinyjs::hide("create_game")
  shinyjs::hide("invalid_game")
  shinyjs::show("game_status")

  # Initialise game
  if (!length(game_id)) {
    game_id <- game_state$active_game
  }
  game_exists <- game_state$initialise_game(game_id)

  # If link is invalid, show message and exit observer
  if (!game_exists) {
    show_invalid_game_id()
    return(NULL)
  }

  game_state$set_player(
    cookies::get_cookie(cookie_name = "player_id",
                        missing = NULL)
  )

  # If new player, show popup to select name
  if (length(game_state$active_player_name) == 0) {
    players <- collect(game_state$players)
    showModal(
      modalDialog(
        title = stringr::str_c("Welcome to ",
                               game_state$active_game_name),
        p("Select your name below to begin."),
        p("The following players have already joined the game: ",
          stringr::str_flatten_comma(
            players$player[!is.na(players$identifier)],
            last = " and ")
        ),
        radioButtons(
          "selected_player",
          "Your name is:",
          choices = players$player[is.na(players$identifier)]
        ),
        footer = actionButton("confirm", "Confirm")
      )
    )
  } else {
    update_game_status_ui(game_state)
  }
}
