update_game_url <- function(state) {

  query_to_append <- paste0("humancluedo/?game=", state$active_game)

  # Update query string for running locally
  #shinyjs::runjs(paste0("window.history.replaceState(null, null, '",
  #                    query_to_append, "')"))

  # Update string for when running in shinyproxy iframe
  shinyjs::runjs(paste0("parent.document.defaultView.history.replaceState(null, null, '",
                 query_to_append, "')"))

  # Update game share link
  removeUI("#game_share_link")
  insertUI("#share_link_text",
           where = "afterEnd",
           ui = tags$a(
             paste0("apps.chrisbrownlie.com/app/", query_to_append),
             href = paste0("https://apps.chrisbrownlie.com/app/", query_to_append)
           ))
}
