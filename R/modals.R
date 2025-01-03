create_game_modal <- function() {
  modalDialog(
    size = "l",
    easyClose = TRUE,
    tags$div(
      class = "text-center",
      tags$i("Create a game of human cluedo"),
      p("Names of players separated by a comma e.g. 'John, Jack, Sarah'. Note that the first player entered will be the admin."),
      textInput("players", "Players:"),
      radioButtons("win_condition", "Select the win condition rules:",
                   choices = c("Duel" = "duel", "Spree" = "spree"),
                   inline = TRUE),
      radioButtons("obj_pop_method", "Select how items and locations should be populated:",
                   choices = c("Players" = "players", "Auto" = "auto", "Admin" = "admin"),
                   inline = TRUE),
      div(
        id = "admin_entry_objs",
        p("Items for contracts. Should make sense following the word 'with', e.g. 'a spoon, an apple, a colander'"),
        textInput("items", "Items:"),
        p("Locations for contracts. Should make sense following an item or player, e.g. 'in the bedroom, next to the bbq, while touching a door'"),
        textInput("locations", "Locations:"),
        uiOutput("validation_ui")
      ) |>
        shinyjs::hidden(),
      textInput("game_name", "Optional game name (leave blank to randomly generate):"),
      actionButton("generate_random", "Generate random name")
    ),
    footer = actionButton("create_new",
                          "Confirm") |>
      shinyjs::disabled()
  )
}

player_join_modal <- function(game_state) {
  player_tbl <- collect(game_state$players)
  players <- player_tbl$player
  already_joined <- !is.na(player_tbl$identifier)
  modalDialog(
    title = stringr::str_c("Welcome to ",
                           game_state$active_game_name),
    p("Select your name below to begin."),
    if (any(already_joined)) {
      p("The following players have already joined the game: ",
        stringr::str_flatten_comma(
          players[already_joined],
          last = " and ")
      )
    } else {
      p("You are the first player to join the game!")
    },
    radioButtons(
      "selected_player",
      "Your name is:",
      choices = players[!already_joined]
    ),
    if (collect(game_state$details)$population_method == "players") {
      tagList(
        p("You must enter an item and a location to be used in contracts."),
        p("Item for a contract - should make sense following the word 'with', e.g. 'a spoon', 'an apple', 'a colander'"),
        textInput("item_obj_pop", "Item:"),
        p("Location for a contracts - should make sense following an item or player, e.g. 'in the bedroom', 'next to the bbq', 'while touching a door'"),
        textInput("location_obj_pop", "Location:")
      )
    },
    footer = actionButton("confirm_join", "Confirm")
  )
}
