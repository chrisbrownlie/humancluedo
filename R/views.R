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
  shiny::removeUI("#gamename", multiple = TRUE)
  shiny::insertUI(
    "#confirm_kill",
    tags$div(
      tags$span(class = "h5 text-info", "Human Cluedo"),
      br(),
      tags$b(class = "h1", state$active_game_name),
      id = "gamename"),
    where = "beforeBegin"
  )
}

#' Update the current players status
update_player_status <- function(state) {
  shiny::removeUI("#activecontract")
  shiny::removeUI("#game_performance")
  shiny::removeUI("#playerstatus")
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

  # If game is over, hide confirm kill. Otherwise show status & active contract
  if (state$get_game_status() == "finished") {
    winner <- state$players |>
      filter(alive) |>
      pull(player) |>
      stringr::str_flatten_comma()
    shiny::insertUI(
      "#confirm_kill",
      h2(paste0("Game over - congratulations ", winner, "! 🎉🎉🎉"),
         class = "text-warning"),
      where = "beforeBegin"
    )
    shinyjs::hide("confirm_kill")
  } else {
    # Insert player status and active contract
    shiny::insertUI(
      "#confirm_kill",
      get_active_contract(state),
      where = "beforeBegin"
    )
  }

  # Insert game timeline
  shiny::insertUI(
    "#confirm_kill",
    tags$div(
      id = "game_performance",
      class = "timeline text-center",
      get_game_timeline(state)
    ),
    where = "afterEnd"
  )

  # Insert leaderboard/player status
  shiny::insertUI(
    "#game_performance",
    tags$div(
      id = "playerstatus",
      hr(),
      h3("Leaderboard"),
      get_player_leaderboard(state)
    ),
    where = "afterEnd"
  )

  shinyjs::toggleState("confirm_kill",
                       condition = state$get_game_status() == "in progress")
}

#' Update the game link
update_game_link <- function(state) {

  shiny::removeUI("#whatsapp_link")

  # Can hide share link when game has started (no new players can join)
  if (state$get_game_status() == "in progress") return(invisible(NULL))

  link_text <- paste0(
    "Join my game of Human Cluedo - '",
    state$active_game_name, "' here: ",
    "http://apps.chrisbrownlie.com/app/humancluedo/?game=",
    state$active_game
  ) |>
    URLencode()

  shiny::insertUI(
    "#game_status",
    where = "beforeEnd",
    tags$div(
      p("Share the URL to invite people to this game, or use the",
        "button below to share on Whatsapp.",
        "Note that anyone who has the link can join!",
        class = "p-2"),
      tags$a(
        bsicons::bs_icon("whatsapp",
                         class = "fs-1 mb-3 ms-3 text-green"),
        href = paste0(
          "https://api.whatsapp.com/send?text=",
          link_text
        )
      ),
      id = "whatsapp_link",
    )
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

  # Update query string
  updateQueryString(paste0("?game=", game_id), mode = "replace")
}
