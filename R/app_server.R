#' App server
#' @export
app_server <- function(input, output, session) {

  game_state <- GameState$new()

  # Existing game
  observe({

    if (game_state$is_initialised()) return(NULL)

    game_id <- parseQueryString(session$clientData$url_search)[["game"]]

    # If existing game, switch to existing game mode
    if (!is.null(game_id)) {
      game_state <- launch_a_game(game_state, game_id)
    }
  })

  # Once player selection has been confirmed
  observe({
    print("SETTING COOKIE")
    cookies::set_cookie(
      "player_id",
      session$token
    )
    set_player_id(id = session$token,
                  player = input$selected_player,
                  game_id = game_state$active_game,
                  conn = game_state$conn)
    # Refresh tables
    game_state$initialise_game(game_state$active_game)
    game_state$set_player(session$token)

    # update UI
    update_game_status_ui(game_state)
    cli::cli_alert_success("Added identifier for {input$selected_player}")
    removeModal()
  }) |>
    bindEvent(input$confirm)

  # Kill confirmation
  observe({
    shinyalert::shinyalert(
      "Are you sure you want to confirm the kill? This can't be undone!",
      showCancelButton = TRUE,
      cancelButtonText = "Cancel",
      showConfirmButton = TRUE,
      confirmButtonText = "Confirm kill",
      callbackR = function(confirmed) {
        if (confirmed) {
          game_state$confirm_kill()
          update_game_status_ui(game_state)
        }
      }
    )
  }) |>
    bindEvent(input$confirm_kill)

  # Creating new game
  observe({
    showModal(
      modalDialog(
        tags$i("Create a game of human cluedo"),
        p("Enter a comma-separated list for all the below"),
        p("Names of players e.g. 'John, Jack, Sarah'. Note that the first player entered will be the admin."),
        textInput("players", "Players:"),
        p("Items for contracts. Should make sense following the word 'with', e.g. 'a spoon, an apple, a colander'"),
        textInput("items", "Items:"),
        p("Locations for contracts. Should make sense following an item or player, e.g. 'in the bedroom, next to the bbq, while touching a door'"),
        textInput("locations", "Locations:"),
        uiOutput("validation_ui"),
        textInput("game_name", "Game name:"),
        actionButton("generate_random", "Generate random name"),
        footer = actionButton("create_new",
                               "Confirm") |>
          shinyjs::disabled()
      )
    )
  }) |>
    bindEvent(input$create)

  observeEvent(input$generate_random, {
    updateTextInput(inputId = "game_name", value = generate_game_name(length(csl_to_vec(input$players))))
  })

  output$validation_ui <- renderUI({
    req(input$players)
    req(input$items)
    req(input$locations)
    players <- csl_to_vec(input$players)
    items <- csl_to_vec(input$items)
    locations <- csl_to_vec(input$locations)

    if (length(players) != length(locations) | length(players) != length(items)) {
      shinyjs::disable("create_new")
      tl <- tagList(
        p(tags$b("Please enter the same number of items in each of the above boxes",
                 class = "text-danger")),
        tags$ul(
          display_vec(players, "players", tags$i("(admin)", class = "text-info")),
          display_vec(items, "items"),
          display_vec(locations, "locations")
        )
      )
    } else {
      shinyjs::enable("create_new")
      tl <- tagList(
        p("Your game will be created as follows:",
          class = "text-success"),
        tags$ul(
          display_vec(players, "players", tags$i("(admin)", class = "text-info")),
          display_vec(items, "items"),
          display_vec(locations, "locations")
        )
      )
    }

    tl
  })

  observe({
    # Create the game (in the DB)
    removeModal()
    # Clean inputs
    player_vec <- csl_to_vec(input$players)
    game_state <- create_a_game(game_state = game_state,
                                players = player_vec,
                                items = csl_to_vec(input$items),
                                locations = csl_to_vec(input$locations),
                                admin = first(player_vec),
                                game_name = input$name)
    launch_a_game(game_state)
  }) |>
    bindEvent(input$create_new)

  observeEvent(input$game_link, {
    showModal(
      shiny::urlModal(url = paste0("http://127.0.0.1:7048/?game=", game_state$active_game),
                      title = "See game link")
    )
  })

}
