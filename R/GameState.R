#' Custom class for holding state of current game
GameState <- R6::R6Class(
  classname = "GameState",
  public = list(

    #' @field conn the database connection
    conn = "connection",

    #' @field active_game the ID of the active game this object instance represents
    active_game = character(),

    #' @field active_game_name the name of the active game this object instance represents
    active_game_name = character(),

    #' @field active_player the identifier of the currently active player,
    #' in the active game
    active_player = character(),

    #' @field active_player_name the name of the currently active player,
    #' in the active game
    active_player_name = character(),

    #' @field players a lazy tibble indicating game players and status
    players = tibble::tibble(),

    #' @field items a lazy tibble indicating items in the game
    items = tibble::tibble(),

    #' @field locations a lazy tibble indicating locations in the game
    locations = tibble::tibble(),

    #' @field contracts a lazy tibble indicating game contracts
    contracts = tibble::tibble(),

    #' @field details a one-row lazy tibble indicating the metadata for
    #' the game, including the win condition (duel or spree) and object
    #' population method (auto, admin, players)
    details = tibble::tibble(),

    #' @description
    #' Create a new game state
    #'
    #' @param game_id optional game ID
    #' @param conn database connection object, defaults to using db_conn()
    #'
    #' @return a new object of class GameState
    initialize = function(game_id = NULL,
                          conn = db_conn()) {

      self$conn <- conn

      # Create database if it doesn't exist
      create_database(conn = conn)

      if (!is.null(game_id)) {
        self$initialise_game(game_id)
      }

      invisible(self)
    },

    #' Populate game state using the database
    #'
    #' @param game_id the ID of the game to use to populate the object
    initialise_game = function(game_id) {

      self$details <- self$conn |>
        dplyr::tbl("games") |>
        filter(.data$game_id == .env$game_id,
               active)

      game <- collect(self$details)

      if (NROW(game) == 0) {
        cli::cli_alert_warning("Game {.val {game_id}} not found")
        return(FALSE)
      }
      if (NROW(game) != 1) {
        cli::cli_alert_warning("Game {.val {game_id}} matches multiple games")
        return(FALSE)
      }

      self$active_game <- game_id
      self$active_game_name <- game$name

      self$populate_object_tables()

      # Get player name
      if (is.null(self$active_player_name) && !is.null(self$active_player)) {
        self$active_player_name <- self$players |>
          filter(identifier == self$active_player) |?
          pull(player)
      }

      TRUE
    },

    #' Create a new game from a tibble of contracts
    #'
    #' @param players a character vector of unique player names. The first of these
    #' will be the admin.
    #' @param game_name a name for the new game
    #' @param win_condition the win condition for the game (duel or spree)
    #' @param obj_pop_method the object population method for the game (auto,
    #' admin or players)
    create_game = function(players,
                           game_name,
                           win_condition = c("duel", "spree"),
                           obj_pop_method = c("players", "auto", "admin")) {

      win_condition <- rlang::arg_match(win_condition)
      obj_pop_method <- rlang::arg_match(obj_pop_method)

      if (!is.character(players) || length(players) < 2) {
        cli::cli_abort("Must have at least two players")
      }

      new_game_id <- sqids::encode(as.numeric(Sys.time()) + runif(1,1,1000000), options=sqids::sqids_options(min_length = 8))
      self$active_game <- new_game_id

      # If no name, generate
      generated_name <- generate_game_name(n = length(players))
      if (missing(game_name)) {
        game_name <- generated_name
        cli::cli_alert_info("No game name supplied, generated as {.val {game_name}}")
      }
      if (length(game_name) == 0) {
        game_name <- generated_name
        cli::cli_alert_info("No game name supplied, generated as {.val {game_name}}")
      }
      if (game_name == "") {
        game_name <- generated_name
        cli::cli_alert_info("No game name supplied, generated as {.val {game_name}}")
      }

      # Add game
      tibble::tibble(
        game_id = new_game_id,
        name = game_name,
        active = TRUE,
        win_condition = win_condition,
        population_method = obj_pop_method
      ) |>
        dbAppendTable(conn = self$conn,
                              name = "games",
                              value = _)

      # Add players
      admin_col <- players==players[1]
      tibble::tibble(player = players) |>
        mutate(game_id = new_game_id,
               alive = TRUE,
               is_admin = admin_col) |>
        select(game_id, player, alive, is_admin) |>
        dbAppendTable(conn = self$conn,
                              name = "players",
                              value = _)
      cli::cli_alert_success("Players added for new game {.val {new_game_id}}")

      self$populate_object_tables()

      invisible(self)
    },

    #' Ensure that the object tables contain lazy copies of the relevant tables from the database
    populate_object_tables = function() {
      if (!self$is_initialised()) return(FALSE)
      self$players <- self$conn |>
        dplyr::tbl("players") |>
        filter(.data$game_id == !!self$active_game)

      self$items <- self$conn |>
        dplyr::tbl("items") |>
        filter(.data$game_id == !!self$active_game)

      self$locations <- self$conn |>
        dplyr::tbl("locations") |>
        filter(.data$game_id == !!self$active_game)

      self$contracts <- self$conn |>
        dplyr::tbl("contracts") |>
        filter(.data$game_id == !!self$active_game)

      invisible(self)
    },

    #' Add an item to the current game
    #' @param item the name of the item
    #' @param generated_by who or what the item was generated by
    add_item = function(item, generated_by) {
      if (!self$is_initialised()) cli::cli_abort("Initialise or create game first")
      tibble::tibble(
        game_id = self$active_game,
        item = item,
        generated_by = generated_by
      ) |>
        dbAppendTable(
          conn = self$conn,
          name = "items"
        )
    },
    #' Remove an item from the current game
    #' @param item the item to remove
    remove_item = function(item) {
      if (!self$is_initialised()) cli::cli_abort("Initialise or create game first")
      self$conn |>
        dbSendQuery(
          glue::glue_sql(
            "DELETE FROM items WHERE game_id = {self$active_game} AND item = {item}",
            .con = self$conn
          )
        )
    },

    #' Add a location to the current game
    #' @param location the name of the location
    #' @param generated_by who or what the location was generated by
    add_location = function(location, generated_by) {
      if (!self$is_initialised()) cli::cli_abort("Initialise or create game first")
      tibble::tibble(
        game_id = self$active_game,
        location = location,
        generated_by = generated_by
      ) |>
        dbAppendTable(
          conn = self$conn,
          name = "locations"
        )
    },
    #' Remove a location from the current game
    #' @param location the location to remove
    remove_location = function(location) {
      if (!self$is_initialised()) cli::cli_abort("Initialise or create game first")
      self$conn |>
        dbSendQuery(
          glue::glue_sql(
            "DELETE FROM locations WHERE game_id = {self$active_game} AND location = {location}",
            .con = self$conn
          )
        )
    },

    #' @description
    #' Use the games players, items and locations to set kill contracts.
    #' Done in such a way that you won't kill someone who is trying to kill you
    #' until the end of the game.
    set_contracts = function() {
      if (!self$is_initialised()) cli::cli_abort("Initialise or create game first")
      if (self$is_contracted()) {
        cli::cli_warn("Contracts already set, overwriting")
      }

      players <- self$players |>
        pull(player)
      items <- self$items |>
        pull(item)
      locations <- collect(self$locations) |>
        pull(location)

      if (length(items) != length(players)) {
        cli::cli_abort("There are {length(players)} players but {length(items)} items, there must be the same number of each.")
      }
      if (length(locations) != length(players)) {
        cli::cli_abort("There are {length(players)} players but {length(locations)} locations, there must be the same number of each.")
      }

      # Invalidate any existing contracts
      existing_contracts <- self$contracts |>
        filter(active) |>
        tally() |>
        pull(n)
      if (existing_contracts > 0) {
        cli::cli_alert_warning("Invalidating {existing_contracts} existing active contracts")
        dbSendQuery(
          self$conn,
          glue::glue_sql(
            "
            UPDATE contracts
            SET active = {FALSE}, execution_notes = 'Overwritten by setting new contracts'
            WHERE game_id = {self$active_game} AND active = {TRUE}
            ",
            .con = self$conn
          )
        )
      }

      # Generate random contracts
      players <- sample(players, length(players))
      items <- sample(items, length(items))
      locations <- sample(locations, length(locations))
      targets <- c(players[length(players)], players[1:(length(players)-1)])

      contracts <- tibble::tibble(
        player = players,
        item = items,
        location = locations,
        target = targets
      )

      # Add contracts
      contracts |>
        mutate(game_id = self$active_game,
               active = TRUE) |>
        dbAppendTable(conn = self$conn,
                              name = "contracts",
                              value = _)
      cli::cli_alert_success("New contracts set for game {.val {self$active_game}}")

    },

    #' @description Has a game been initialised?
    is_initialised = function() {
      length(self$active_game)>0
    },
    #' @description Have contracts been set?
    is_contracted = function() {
      pull(tally(self$contracts), n) > 0
    },

    #' @description Get my contract target
    get_target = function() {
      if (!self$is_initialised()) return("")
      self$contracts |>
        filter(player == self$active_player_name,
               active) |>
        pull(target)
    },

    #' @description Get my contract item
    get_item = function() {
      if (!self$is_initialised()) return("")
      self$contracts |>
        filter(player == self$active_player_name,
               active) |>
        pull(item)
    },

    #' @description Get my contract location
    get_location = function() {
      if (!self$is_initialised()) return("")
      self$contracts |>
        filter(player == self$active_player_name,
               active) |>
        pull(location)
    },

    #' @description Return TRUE if the current player is alive, FALSE otherwise
    is_alive = function() {
      if (!self$is_initialised()) return(FALSE)
      self$players |>
        filter(player == self$active_player_name) |>
        pull(alive) |>
        isTruthy()
    },

    #' @description Is my contract target still alive?
    target_is_alive = function() {
      if (!self$is_alive() | !self$is_contracted()) return(FALSE)
      self$players |>
        filter(player == !!self$get_target()) |>
        pull(alive) |>
        isTruthy()
    },

    #' @description Confirm a kill
    #' @param execution_time the time of execution
    #' @param notes any notes to add to the contract after completing
    confirm_kill = function(execution_time = lubridate::now(),
                            notes = "") {

      target <- self$get_target()
      # Mark player as dead
      dbSendQuery(
        self$conn,
        glue::glue_sql(
          "
        UPDATE players
        SET alive = {FALSE}
        WHERE player = {target}
        AND game_id = {self$active_game}
        ",
          .con = self$conn
        )
      )
      cli::cli_alert_success("Player {target} marked as dead")

      # Get target's contract
      target_contract <- self$contracts |>
        filter(player == .env$target,
               active)

      if (pull(count((target_contract))) != 1) cli::cli_abort("Something went wrong adding new contract")

      if (pull(target_contract, target) == self$active_player_name) {
        # Win condition
        cli::cli_abort("Deal with this issue")
      }

      cli::cli_alert_success("New contract added - killing {pull(target_contract, target)}")

      # Add to contracts table
      target_contract |>
        mutate(player = self$active_player_name,
               active = TRUE) |>
        collect() |>
        dbAppendTable(
          conn = self$conn,
          name = "contracts",
          value = _
        )

      # Mark both previous contracts as no longer active
      dbSendQuery(
        self$conn,
        glue::glue_sql(
          "
        UPDATE contracts
        SET active = {FALSE}, execution_notes = 'Contract incomplete'
        WHERE player = {target}
        AND game_id = {self$active_game}
        ",
          .con = self$conn
        )
      )
      cli::cli_alert_success("{target}'s contract marked as inactive")
      dbSendQuery(
        self$conn,
        glue::glue_sql(
          "
        UPDATE contracts
        SET active = {FALSE}, execution_time = {execution_time}, execution_notes = {notes}
        WHERE target = {target}
        AND game_id = {self$active_game}
        ",
          .con = self$conn
        )
      )
      cli::cli_alert_success("Contract on {target} marked as complete")

      # TODO: Handling for end of game


    },

    #' @description Get the status of all other players
    #' @param as_html if TRUE, will return a HTML list of player statuses,
    #' if FALSE (the default) will return a two column tibble of player name
    #' and status
    get_player_status = function(as_html = FALSE) {
      if (!self$is_initialised()) return("")
      ps <- self$players |>
        filter(player != self$active_player_name) |>
        select(player, alive) |>
        collect()

      if (!as_html) return(ps)

      tags$ul(
        purrr::map2(ps$player,
                    ps$alive,
                    \(player, state) {
                      tags$li(
                        tags$b(player),
                        "is",
                        tags$b(ifelse(state, "alive", "dead"), class = ifelse(state, "text-green", "text-red"))
                      )
                    })
      )
    },

    #' @description Get performance in the current game
    get_performance = function() {
      kills <- self$contracts |>
        filter(player == self$active_player_name,
               !is.na(execution_time)) |>
        collect()
      is_alive <- self$players |>
        filter(player == self$active_player_name) |>
        pull(alive)
      killed_by <- self$contracts |>
        filter(target == self$active_player_name,
               !active) |>
        collect()

      html <- tagList(
        p(
          "You",
          ifelse(is_alive, "have", "had"),
          tags$b(NROW(kills), "successful kills")
        ),
        if (NROW(kills)) {
          tags$ul(
            purrr::pmap(kills,
                        \(...) {
                          kill <- list(...)
                          et <- kill$execution_time |>
                            lubridate::ymd_hms()
                          days_ago <- lubridate::today()-lubridate::date(et)
                          if (days_ago == 0) {
                            ex_date <- "today!"
                          } else if (days_ago == 1) {
                            ex_date <- "yesterday!"
                          } else {
                            ex_date <- paste0("on ", format(et, format="%A%e %b %Y!"))
                          }
                          tags$li(
                            "Killed", tags$b(kill$target), "with", tags$b(kill$item), "-", tags$b(kill$location), " at ",
                            format(et, format = "%R %Z"), ex_date
                          )
                        })
          )
        }
      )

      return(
        list(
          is_alive = is_alive,
          kills = kills,
          killed_by = killed_by,
          html = html
        )
      )

    },

    #' Set the active player
    #' @param id the identifier of the current player
    #' @param player the name of the current player. If missing, it
    #' is assumed that id will match to an existing player and in that
    #' case this method will set the active player name and ID.
    set_active_player = function(id, player) {

      if (!self$is_initialised()) return(invisible(NULL))
      if (is.null(id)) return(invisible(NULL))

      self$active_player <- id

      # If just setting the active player name (i.e. it
      # is an existing game/player), don't alter the table
      if (missing(player)) {
        self$active_player_name <- self$players |>
          filter(identifier == id) |>
          pull(player)
        return(invisible(self))
      }

      self$active_player_name <- player

      # If setting the ID for a player, update the table
      dbSendQuery(
        self$conn,
        glue::glue_sql(
          "
          UPDATE players
          SET identifier = {id}
          WHERE player = {player}
          AND game_id = {self$active_game}
          ",
          .con = self$conn
        )
      )
    },

    #' @description Helper to determine whether the game is waiting for players to join or not
    get_game_status = function() {
      if (!self$is_initialised()) return("uninitialised")
      if ((pull(self$details, population_method) == "players") &&
          NROW(collect(self$contracts)) == 0) {
        return("awaiting")
      }
      return("in progress")
    }

  )
)
