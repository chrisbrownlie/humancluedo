#' Generate a random name for a game
#'
#' @param n optional number of players
generate_game_name <- function(n = NULL) {

  adjs <- c("cold-blooded", "vicious", "merciless", "ruthless", "brutal",
           "savage", "relentless", "grim", "malevolent", "deadly",
           "sinister", "feral", "horrific", "bloody", "remorseless",
           "evil", "wicked", "cruel", "lethal", "predatory",
           "monstrous", "diabolical", "dark", "fiendish", "vengeful",
           "hateful", "fearsome", "hostile", "violent", "poisonous",
           "malicious", "unforgiving", "gory", "wrathful", "ferocious",
           "inhumane", "destructive", "menacing", "blood-soaked", "unrelenting",
           "tyrannical", "oppressive", "brutish", "dreadful", "nefarious",
           "barbaric", "villainous", "fatal", "wrathful", "atrocious",
           "inhuman")

  nouns <- c("killers", "murderers", "assassins", "slayers", "executioners",
             "predators", "butchers", "criminals", "culprits", "villains",
             "hooligans", "maniacs", "monsters", "fiends", "perpetrators",
             "stalkers", "brutes", "devils", "oppressors", "demons",
             "victims", "corpse", "cadavers", "remains", "bones",
             "carcasses", "lurkers", "thieves", "trespassers", "poachers",
             "terrorists", "arsonists", "warriors", "mercenaries", "guards",
             "thugs", "henchmen", "plunderers", "marauders", "warlords",
             "cutthroats", "scavengers", "outlaws", "renegades", "scoundrels",
             "bandits", "pirates", "traitors", "saboteurs", "executioners")

  if (n %in% 0:1) n <- NULL
  stringr::str_c("the", n, sample(adjs, 1), sample(nouns, 1), sep = " ") |>
    stringr::str_to_title()
}

test_game_id <- "000db5ed-752a-4e25-8788-c3cb59f8d70d"


clean_game <- function() {
  conn <- db_conn()

  DBI::dbRemoveTable(conn, "players")
  DBI::dbRemoveTable(conn, "games")
  DBI::dbRemoveTable(conn, "contracts")

  create_database()

  new <- GameState$new()

  test_contracts <- tibble::tibble(
    player = c("bob", "dave", "sandra"),
    item = c("a spoon", "a ball", "a pair of trousers"),
    location = c("in the kitchen", "by the pool", "behind the shed"),
    target = c("sandra", "bob", "dave")
  )

  new$create_game(
    test_contracts,
    admin_player = "bob"
  )

  clipr::write_clip(new$active_game)
}


create_a_game <- function(game_state, players, items, locations, admin, game_name) {

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

  random_name <- generate_game_name(n = length(players))
  if (length(game_name) == 0) game_name <- random_name
  if (game_name == "") game_name <- random_name

  game_state$create_game(contracts, game_name, admin_player = admin)
}
