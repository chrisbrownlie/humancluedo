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

generate_items <- function(n) {
  vals <- c(
    "a pen", "a spoon", "a fork", "a knife", "a plate", "a cup", "an apple", "a book", "a toothbrush", "a comb",
    "a remote control", "a phone charger", "a pair of scissors", "a notepad", "a light bulb", "a key", "a candle",
    "a battery", "a tissue", "an empty bottle", "a pencil", "an eraser", "a ruler", "a paperclip", "a stapler", "a lighter",
    "a toothbrush holder", "a hairbrush", "a bar of soap", "a sponge", "a tea bag", "a handful of salt",
    "a bottle of shampoo", "a towel", "a clock", "a coaster", "a mug", "nail clippers", "a screwdriver",
    "a packet of tissues", "a pair of tweezers", "a wallet", "a coin", "a rubber band", "a pair of headphones",
    "a USB drive", "a charger cable", "a soap dispenser", "a pepper grinder", "a packet of matches"
  )
  if (n > length(vals)) return(sample(vals, n, replace = TRUE))
  sample(vals, n)
}

generate_locations <- function(n) {
  vals <- c(
    "in the kitchen", "in the living room", "in the bathroom", "in a bedroom", "in a hallway", "in the dining room",
    "in the laundry room", "in the attic", "in the basement", "in the garage", "on the patio", "in the backyard",
    "on the front porch", "in the guest room", "in the entryway", "on the balcony", "in the walk-in closet", "on the staircase landing",
    "in the study", "in the foyer",

    "while touching a door", "next to a table", "next to any light switch", "while holding a railing or banister",
    "next to an open cupboard", "next to an open drawer",
    "on a rug", "next to an oven or bbq",
    "while sitting in a chair", "while standing on a doormat", "while touching a brick wall",
    "underneath a light or chandelier",
    "next to a mirror", "next to a heater or radiator", "while standing on grass or mud"
  )
  if (n > length(vals)) return(sample(vals, n, replace = TRUE))
  sample(vals, n)
}


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
