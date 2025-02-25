create_game_modal <- function() {
  modalDialog(
    size = "l",
    easyClose = TRUE,
    tags$div(
      class = "text-center",
      tags$b(class = "h4 text-green", "New game"),
      p("Names of players separated by a comma e.g. 'John, Jack, Sarah'. Note that the first player entered will be the admin."),
      textInput("players", "Players:") |> htmltools::tagAppendAttributes(class = "w-100"),
      hr(),
      radioButtons("win_condition", tags$span("Game type:", class = "h5 text-red"),
                   choices = c("Survival" = "duel", "Kills" = "spree"),
                   inline = TRUE),
      p(
        id = "win_condition_desc",
        tags$b("Survival:"),
        "the final standings are determined by the order in which players are killed. ",
        "In the event that multiple players are alive at the end of the game, ",
        "number of kills will be used to break ties."),
      hr(),
      radioButtons("obj_pop_method", tags$span("Contract method:", class = "h5 text-red"),
                   choices = c("Players" = "players", "Auto" = "auto", "Admin" = "admin"),
                   inline = TRUE),
      p(
        id = "obj_pop_method_desc",
        tags$b("Players:"),
        "when each player joins the game they will be asked to enter an item and a location.",
        "Once all players have joined, the game will start by randomly assigning contracts using",
        "the items and locations that players entered."),
      div(
        id = "admin_entry_objs",
        p("Items for contracts. Should make sense following the word 'with', e.g. 'a spoon, an apple, a colander'"),
        textInput("items", "Items:") |> htmltools::tagAppendAttributes(class = "w-100"),
        p("Locations for contracts. Should make sense following an item or player, e.g. 'in the bedroom, next to the bbq, while touching a door'"),
        textInput("locations", "Locations:") |> htmltools::tagAppendAttributes(class = "w-100"),
        uiOutput("validation_ui")
      ) |>
        shinyjs::hidden(),
      hr(),
      tags$b("Game Deadline", class = "h5 text-red"),
      p("After the deadline, the game will be finished and kills can no longer",
        "be recorded."),
      shinyDatetimePickers::datetimeMaterialPickerInput(
        inputId = "game_deadline",
        label = "",
        value = lubridate::now() + lubridate::days(14),
        disablePast = TRUE,
        style = "width:100%;"
      ),
      hr(),
      textInput("game_name", tags$span("Game name:", class = "h5 text-red")) |>
        htmltools::tagAppendAttributes(class = "w-100"),
      tags$small("(Leave blank to generate a random name)")
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
