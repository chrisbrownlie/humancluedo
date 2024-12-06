#' Set the player ID for a player in a game
#'
#' Set the ID used to identify players via their cookies
#'
#' @param game_id the ID of the game for which a player ID is being added
#' @param player the name of the player within the chosen game
#' @param id the ID to add for the chosen player
#' @param conn a SQL connection
#'
#' @return the output of dbExecute
#' @export
set_player_id <- function(id, player, game_id, conn) {
  DBI::dbExecute(
    conn,
    glue::glue_sql(
      "
        UPDATE players
        SET identifier = {id}
        WHERE player = {player}
        AND game_id = {game_id}
        ",
      .con = conn
    )
  )
}
