#' App server
#'
#' @param input app input
#' @param output app output
#' @param session app session
#'
#' @export
app_server <- function(input, output, session) {

  # Create new game state
  game_state <- GameState$new()

  # Existing game
  observe({

    if (game_state$is_initialised()) return(NULL)

    game_id <- parseQueryString(session$clientData$url_search)[["game"]]

    # If existing game, switch to existing game mode
    if (!is.null(game_id)) {
      game_state <- launch_a_game(
        game_state = game_state,
        game_id = game_id,
        existing_cookie = cookies::get_cookie("player_id",
                                              missing = session$token)
      )
    }
  })

  # Once player selection has been confirmed
  observe({
    print("SETTING COOKIE")
    # Set cookie and active player identifier using session ID
    cookies::set_cookie(
      "player_id",
      session$token
    )
    game_state$set_active_player(
      session$token,
      player = input$selected_player
    )

    # Add item and location if necessary
    if (pull(game_state$details, population_method) == "players") {
      game_state$add_location(input$location_obj_pop,
                              generated_by = input$selected_player)
      game_state$add_item(input$item_obj_pop,
                          generated_by = input$selected_player)
    }

    # If waiting for players, check if this is the last player to join.
    # If so, set the contracts so the game can begin
    if (game_state$get_game_status() == "awaiting" &&
        sum(is.na(pull(game_state$players, identifier))) == 0) {
      game_state$set_contracts()
    }

    # update UI
    update_game_status_ui(game_state)
    cli::cli_alert_success("Added identifier for {input$selected_player}")
    removeModal()
  }) |>
    bindEvent(input$confirm_join)

  # If join game modal is showing item/location population, don't allow
  # confirming until the items are entered.
  observe({
    req(!is.null(input$item_obj_pop))
    if (input$item_obj_pop == "" | input$location_obj_pop == "") {
      shinyjs::disable("confirm_join")
    } else{
      shinyjs::enable("confirm_join")
    }
  })

  # Kill confirmation
  observe({
    shinyalert::shinyalert(
      "Are you sure you want to confirm the kill?",
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
      create_game_modal()
    )
  }) |>
    bindEvent(input$create)

  observeEvent(input$generate_random, {
    updateTextInput(inputId = "game_name", value = generate_game_name(length(csl_to_vec(input$players))))
  })

  # Validation for creating new game
  observe({
    req(input$obj_pop_method)
    # Show admin entry options if selected
    shinyjs::toggle("admin_entry_objs",
                    condition = input$obj_pop_method == "admin")
    if (input$obj_pop_method != "admin") {
      shinyjs::toggleState("create_new",
                           condition = input$players != "")
    }
  })

  # Details for creating new game
  observe({
    removeUI("#win_condition_desc")
    if (input$win_condition == "duel") {
      desc <- p(
        id = "win_condition_desc",
        tags$b("Duel:"),
        "the winner is decided by which of the final two players manages to kill the other.",
        "In the event that the game ends before this, the player who has the most kills", tags$i("and is still alive"),
        "wins.")
    } else if (input$win_condition == "spree") {
      desc <- p(
        id = "win_condition_desc",
        tags$b("Spree:"), "the winner is decided by the person with the most kills at the end of the game."
      )
    }
    insertUI(
      "#win_condition",
      where = "afterEnd",
      desc
    )
  }) |>
    bindEvent(input$win_condition)
  observe({
    removeUI("#obj_pop_method_desc")
    if (input$obj_pop_method == "players") {
      desc <- p(
        id = "obj_pop_method_desc",
        tags$b("Players:"),
        "when each player joins the game they will be asked to enter an item and a location.",
        "Once all players have joined, you (as the admin) can start the game by randomly assigning contracts using",
        "the items and locations that players entered.")
    } else if (input$obj_pop_method == "auto") {
      desc <- p(
        id = "obj_pop_method_desc",
        tags$b("Auto:"), "automatically generate common househould items and locations to use. Note that this may",
        "result in items or locations that are not possible for your game."
      )
    } else if (input$obj_pop_method == "admin") {
      desc <- p(
        id = "obj_pop_method_desc",
        tags$b("Admin:"), "enter the necessary number of items and locations yourself.",
        "This will mean you have an advantage over the other players, but allows you to generate contracts before",
        "all players have joined, so that players can see their own contract as soon as they join."
      )
    }
    insertUI(
      "#obj_pop_method",
      where = "afterEnd",
      desc
    )
  }) |>
    bindEvent(input$win_condition)

  # Validation UI for admin entry population method
  output$validation_ui <- renderUI({
    req(input$obj_pop_method == "admin")
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
    game_state$create_game(players = player_vec,
                           game_name = input$name,
                           win_condition = input$win_condition,
                           obj_pop_method = input$obj_pop_method)

    if (input$obj_pop_method == "auto") {
      # Auto generate
      items <- generate_items(n = length(player_vec))
      for (item in items) game_state$add_item(item, generated_by = "admin")

      locations <- generate_locations(n = length(player_vec))
      for (location in locations) game_state$add_location(location, generated_by = "admin")

      game_state$set_contracts()
    } else if (input$obj_pop_method == "admin") {
      # Use admin supplied values
      items <- csl_to_vec(input$items)
      for (item in items) game_state$add_item(item, generated_by = "admin")

      locations <- csl_to_vec(input$locations)
      for (location in locations) game_state$add_location(location, generated_by = "admin")

      game_state$set_contracts()
    }

    launch_a_game(game_state, existing_cookie = cookies::get_cookie("player_id", missing = session$token))
  }) |>
    bindEvent(input$create_new)

  observeEvent(input$game_link, {
    showModal(
      shiny::urlModal(url = paste0("http://apps.chrisbrownlie.com/app/humancluedo/?game=", game_state$active_game),
                      title = "Copy game link")
    )
  })


  # Testing
  output$url_parsing <- renderUI({
    req(session$clientData$url_search)
    query <- parseQueryString(session$clientData$url_search)
    #p(paste(names(query), query, sep = "=", collapse=", "))
    p(session$clientData$url_hash)
  })

  output$db_location <- renderUI({
    req(game_state)
    div(
      p("Tables: ", paste(dbListTables(game_state$conn), collapse = ", "))
    )
  })

}
