#' App UI
#' @export
app_ui <- function() {

  page_fillable(
    title = "Human Cluedo",
    theme = bs_theme(version = 5),
    tags$header(shinyjs::useShinyjs()),
    # Created new game card
    card(
      fill = FALSE,
      id = "post_game_creation",
      card_title("Game created!", id = "gamename"),
      p("You can see your assignment below!"),
      p("Use this link to invite people to this game - note that anyone
        who has the link can join!"),
      tags$a("", id = "game_link_created")
    ) |>
      shinyjs::hidden(),

    # Existing game card
    card(
      fill = FALSE,
      id = "game_status",
      card_title("Game Status", id = "gamename"),
      p("You are: [active player]", id = "activeplayer"),
      p("You are trying to kill [target player] with [item]", id = "activecontract"),
      p(id = "game_performance"),
      actionButton("confirm_kill", "Confirm kill!"),
      p("Current player status:", id = "playerstatus_start")
    ) |>
      shinyjs::hidden(),

    card(
      fill = FALSE,
      id = "invalid_game",
      card_header("Invalid Link", class = "bg-warning"),
      p("Sorry, it looks like the link you've used isn't for a valid game."),
      p("Make sure you have copied the link correctly, or use the button
        below to create a new game!")
    ) |>
      shinyjs::hidden(),

    card(
      fill = FALSE,
      min_height = "300px",
      id = "create_game",
      card_title("Human cluedo"),
      card_body(
        actionButton("create", "Create new game!")
      )
    )
  ) |>
    cookies::add_cookie_handlers()
}
