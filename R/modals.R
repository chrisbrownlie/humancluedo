create_game_modal <- function() {
  modalDialog(
    tags$i("Create a game of human cluedo"),
    p("Enter a comma-separated list for all the below"),
    p("Names of players e.g. 'John, Jack, Sarah'. Note that the first player entered will be the admin."),
    textInput("players", "Players:"),
    p("Select the win condition i.e. how the game proceeds when down to the final two players"),
    tags$ul(
      tags$li(tags$b("Duel:"), "the winner is decided by which of the final two players manages to kill the other.",
              "In the event that the game ends before this, the player who has the most kills", tags$i("and is still alive"),
              "wins."),
      tags$li(tags$b("Spree:"), "the winner is decided by the person with the most kills at the end of the game.")
    ),
    radioButtons("win_condition", "Which win condition?",
                 choices = c("Duel" = "duel", "Spree" = "spree"),
                 inline = TRUE),
    p("Select how the items and locations should be populated:"),
    tags$ul(
      tags$li(tags$b("Players:"), "when each player joins the game they will be asked to enter an item and a location.",
              "Once all players have joined, you (as the admin) can start the game by randomly assigning contracts using",
              "the items and locations that players entered."),
      tags$li(tags$b("Auto:"), "automatically generate common househould items and locations to use. Note that this may",
              "result in items or locations that are not possible for your game."),
      tags$li(tags$b("Admin:"), "enter the necessary number of items and locations yourself.",
              "This will mean you have an advantage over the other players, but allows you to generate contracts before",
              "all players have joined, so that players can see their own contract as soon as they join.")
    ),
    radioButtons("obj_pop_method", "Item/Location population method:",
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
    textInput("game_name", "Game name:"),
    actionButton("generate_random", "Generate random name"),
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
