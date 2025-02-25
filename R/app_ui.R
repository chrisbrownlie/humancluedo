#' App UI
#' @export
app_ui <- function() {

  page_fillable(
    title = "Human Cluedo",
    theme = bs_theme(version = 5,
                     heading_font = bslib::font_google("Jersey 15")),
    class = "bg-secondary-subtle",
    gap = "10px",
    padding = 10,
    tags$header(shinyjs::useShinyjs(),

                tags$link(href = "timeline.css", rel = "stylesheet", type = "text/css")),
    # Created new game card
    card(
      fill = FALSE,
      id = "post_game_creation",
      card_title("Game created!", id = "gamename"),
      p("You can see your assignment below!")
    ) |>
      shinyjs::hidden(),

    # Existing game card
    card(
      fill = FALSE,
      id = "game_status",
      class = "text-center",
      p(id = "game_performance"),
      actionButton("confirm_kill", "Confirm kill!",
                   class = "w-50 align-self-center"),
      tags$a(bsicons::bs_icon("whatsapp"),
             id = "whatsapp_link")
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
      min_height = "200px",
      id = "create_game",
      card_title("Human cluedo", class = "text-center display-3 text-cyan"),
      card_body(
        class = "d-flex",
        p("Read the rules below, or click the button to start a new game!",
          class = "w-50 align-self-center text-center"),
        actionButton("create", "Create new game!",
                     class = "rounded-3 w-50 align-self-center")
      )
    ),


    card(
      fill = FALSE,
      min_height = "300px",
      id = "rules",
      card_title("How to play", container = htmltools::h3, class = "text-center display-5 text-green"),
      card_body(
        class = "d-flex align-items-center",
        p(class = "text-center", "Human Cluedo is a live-action party game of strategy, persuasion, and stealth. Each player receives a contract at the start of the game, detailing:"),
          tags$ul(
          tags$li("A", tags$b("target"), "(another player)"),
          tags$li("A specific", tags$b("item"), "to give them, and"),
          tags$li("A designated", tags$b("location"), "where the exchange must occur.")
          ),
          p(class = "text-center", "To eliminate a target, you must hand them the specified item in the assigned location.",
          "If they accept the item, they are “killed” and eliminated from the game.",
          "Once successful, you take over their contract and continue playing.",
          "The goal is to eliminate as many targets as possible while avoiding elimination yourself."),
        h4("Key points", class = "display-5 text-green"),
        tags$ul(
          tags$li("Players cannot force a target to accept an item (e.g. by throwing it at them). Use strategy and don't be suspicious!"),
          tags$li("Once eliminated, players must stop pursuing their own contracts and cannot share any information."),
          tags$li("The game ends when only one player remains or the game deadline is reached. The winner is the last player standing or the one with the most completed contracts, depending on the selected game type.")
        ),
        p("Stay sharp, be creative, and trust no one!", class = "lead text-center text-cyan"),
        br()
      )
    )
  ) |>
    cookies::add_cookie_handlers()
}
