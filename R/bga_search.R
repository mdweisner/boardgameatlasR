#' Search Boardgame Atlas API
#'
#'
#' @name search_bga
#' @description search_bga() provides a wrapper for the search function of the Board Game Atlas (BGA) API.
#' documentation on the API can be found at https://www.boardgameatlas.com/api/docs.
#' Please note that this function only covers the search of the BGA API.
#' Game mechanic ID values can be found at https://www.boardgameatlas.com/api/docs/game/mechanics
#' Game category ID values can be found at https://www.boardgameatlas.com/api/docs/game/categories
#'
#' @param limit int - limits the number of results returned. The max limit is 100. The default is 30.
#' @param skip int - Skips the number of results provided. It's generally used for paging results.
#' @param ids string - The exact id(s) of the game(s) you want returned comma separated. If they exist, this will return the associated games.
#' @param kickstarter logical boolean T/F - This returns a list of kickstarters ordered by the created date in descending order. You can use order_by with the 'deadline', parameter to change it to be ordered in ascending order of when the campaign is over. This may override other parameters like limit.
#' @param random logical boolean T/F - This returns a random game.  This may override other parameters like limit.
#' @param name string - Name of a board game to search for. This search is based on the start of the strings.
#' @param name_exact logical boolean T/F - Use to make the name field only return games that match the name exactly (TRUE). The API default search is based on the start of the strings.
#' @param name_fuzzy logical boolean T/F - Use to make the name field return fuzzy matches like 'cata' to return 'catan' (TRUE). The API default search is based on the start of the strings.
#' @param designer string - Name of a board game designer to search for. This is an exact match parameter.
#' @param publisher string - Name of the board game publisher to search for. This is an exact match parameter.
#' @param artist string - Name of an artist for a board game to search for. This is an exact match parameter.
#' @param mechanics string - A list of mechanic ids to search for comma separated (e.g. "mechanicID1,mechanicID2").
#' @param categories string - A list of category ids to search for comma separated (e.g. "categoryID1,categoryID2").
#' @param order_by string - Provide the name of another parameter and it will order the results according to it. Default value is 'popularity' - which is calculated by the total number of commends on the Reddit.com /r/boardgames subreddit. The possible values include popularity, price, deadline, discount, average_user_rating, num_user_ratings, reddit_week_count, reddit_day_count, name, year_published, min_age, min_playtime, max_playtime, min_players, max_players
#' @param ascending string - This determines which direction the list is shown in based on the order_by parameter. FALSE by default.
#' @param min_players int - Minimum number of players, for exact number set min_players and max_players equal.
#' @param max_players int - Maximum number of players, for exact number set min_players and max_players equal.
#' @param min_playtime int - Minimum number of minutes, for exact number set min_playtime and max_time equal.
#' @param max_playtime int - Maximum number of minutes, for exact number set min_playtime and max_time equal.
#' @param min_age int - The minimum number of age that will be returned, for exact number set min_age and max_age equal.
#' @param max_age int - the highest value of minimum age that will be returned, for exact number set min_age and max_age equal.
#' @param min_year int - The earliest year that a game was published, for exact number set min_year and max_year equal.
#' @param max_year int - The latest year that a game was published, for exact number set min_year and max_year equal.
#' @param min_price dbl - The minimum price value.
#' @param max_price dbl - The maximum price value.
#' @param min_msrp dbl - The minimum manufacturer recommended sale price
#' @param max_msrp dbl - The maximum manufacturer recommended sale price
#' @param min_discount dbl - The minimum discount in decimal format. While typically between 0 and 1, this can be negative when games become more expensive than the msrp.
#' @param max_discount dbl - the maximum discount in decimal format. While typically between 0 and 1, this can be negative when games become more expensive than the msrp.
#' @param min_reddit_total int - Minimum number of times the game has been mentioned on reddit.com's /r/boardgames since 2018.
#' @param max_reddit_total int - Maximum number of times the game has been mentioned on reddit.com's /r/boardgames since 2018
#' @param min_reddit_week int - Minimum number of times the game has been mentioned on reddit.com's /r/boardgames in the past week.
#' @param max_reddit_week int - Maximum number of times the game has been mentioned on reddit.com's /r/boardgames in the past week.
#' @param min_reddit_day int - Minimum number of times the game has been mentioned on reddit.com's /r/boardgames in the past 24 hours.
#' @param max_reddit_day int - Maximum number of times the game has been mentioned on reddit.com's /r/boardgames in the past 24 hours.
#' @param client_id_pat string - your client_id (Sys.getenv('BOARDGAMEATLAS_PAT'))
#'
#' @importFrom attempt stop_if_all
#' @importFrom purrr compact
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom purrr map
#' @importFrom purrr map_dbl
#' @importFrom purrr map_int
#' @importFrom purrr map_chr
#' @export
#' @rdname searchbga
#'
#' @return search_bga returns the results from the search as a tidy dataframe.
#' @examples
#' \dontrun{
#' #Search for the top 30 (the default) most popular variations of Settlers of Catan:
#' search_bga(name = "Catan", name_fuzzy = TRUE, client_id_pat = Sys.getenv('BOARDGAMEATLAS_PAT'))
#'
#' #Search for the top 20 games of 2018 for players age 10 or under
#' search_bga(limit=20, max_age=10, min_year = 2018, max_year=2018)
#'
#' #Search for the top 5 games for ages between 2 and 5 from 2010 and later for at least 3 players
#' search_bga(limit=5, min_age=2, max_age=5, min_year=2010, min_players=3)
#'
#' #Search for the top 10 most heavily discounted games of 2019
#' search_bga(limit=5, order_by = "discount", min_year = 2019)
#' }



## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".x"))

## Function
search_bga <- function(limit = 30, skip = 0, ids = NULL,
                       kickstarter = NULL, random = NULL,
                       name = NULL, name_exact = NULL,
                       name_fuzzy = NULL, designer = NULL,
                       publisher = NULL, artist = NULL,
                       mechanics = NULL, categories = NULL,
                       order_by = "popularity", ascending = NULL,
                       min_players = NULL, max_players = NULL,
                       min_playtime = NULL, max_playtime = NULL,
                       min_age = NULL, max_age = NULL, min_year = NULL,
                       max_year = NULL, min_price = NULL,
                       max_price = NULL, min_msrp = NULL,
                       max_msrp = NULL, min_discount = NULL,
                       max_discount = NULL, min_reddit_total = NULL,
                       max_reddit_total = NULL, min_reddit_week = NULL,
                       max_reddit_week = NULL, min_reddit_day = NULL,
                       max_reddit_day = NULL, client_id_pat = Sys.getenv('BOARDGAMEATLAS_PAT')){


  # Check bga_pat_token
  bga_pat()

  # Check Arg Values
  limit_check(limit)
  skip_check(skip)
  ids_check(ids)
  kickstarter_check(kickstarter)
  random_check(random)
  name_check(name)
  exact_check(name_exact)
  fuzzy_match_check(name_fuzzy)
  fuzzy_exact_check(name_fuzzy, name_exact)
  designer_check(designer)
  publisher_check(publisher)
  artist_check(artist)
  mechanics_check(mechanics)
  categories_check(categories)
  order_by_check(order_by)

  min_players_check(min_players)
  max_players_check(max_players)
  player_range_check(min_players, max_players)

  min_playtime_check(min_playtime)
  max_playtime_check(max_playtime)
  playtime_range_check(min_playtime, max_playtime)

  min_age_check(min_age)
  max_age_check(max_age)
  age_range_check(min_age, max_age)

  min_year_check(min_year)
  max_year_check(max_year)
  year_range_check(min_year,max_year)

  min_price_check(min_price)
  max_price_check(max_price)
  price_range_check(min_price, max_price)

  min_msrp_check(min_msrp)
  max_msrp_check(max_msrp)
  msrp_range_check(min_msrp, max_msrp)

  min_discount_check(min_discount)
  max_discount_check(max_discount)
  discount_range_check(min_discount, max_discount)

  min_reddit_total_check(min_reddit_total)
  max_reddit_total_check(max_reddit_total)
  reddit_total_range(min_reddit_total, max_reddit_total)

  min_reddit_week_check(min_reddit_week)
  max_reddit_week_check(max_reddit_week)
  reddit_week_range(min_reddit_week, max_reddit_week)

  min_reddit_day_check(min_reddit_day)
  max_reddit_day_check(max_reddit_day)
  reddit_day_range(min_reddit_day, max_reddit_day)



  args <- list(limit = limit, skip = skip, ids = ids, kickstarter = tolower(as.character(kickstarter)),
               random = random, name = name, exact = tolower(as.character(name_exact)), fuzzy_match = tolower(as.character(name_fuzzy)),
               designer = designer, publisher = publisher, artist = artist, mechanics = mechanics,
               categories = categories, order_by = order_by, ascending = tolower(as.character(ascending)),
               gt_min_players = min_players-1, lt_max_players = max_players+1,
               gt_min_playtime = min_playtime-1, lt_max_playtime = max_playtime+1,
               gt_min_age = min_age-1, lt_min_age = max_age+1,
               gt_year_published = min_year-1, lt_year_published = max_year+1,
               gt_price = min_price-0.01,lt_price = max_price+0.01,
               gt_min_msrp = min_msrp-1, lt_max_msrp = max_msrp+1,
               gt_min_discount = min_discount-0.0001, lt_max_discount = max_discount+0.0001,
               gt_reddit_count = min_reddit_total-1, lt_reddit_count = max_reddit_total+1,
               gt_reddit_week_count = min_reddit_week-1, lt_reddit_week_count = max_reddit_week+1,
               gt_reddit_day_count = min_reddit_day-1, lt_reddit_day_count = max_reddit_day+1,
               client_id = client_id_pat)

  # Check that at least one argument is not null
  stop_if_all(args, is.null, "You need to specify at least one argument")
  # Check for internet
  check_internet()
  # Create the query
  res <- GET(base_url, query = compact(args))
  # Check the result
  check_status(res)
  # Get the content and return it as a data.frame
  raw_query <- content(res)
  raw_games_out <- raw_query$games

  # Clean Data
  game_df_out <- clean_single_categories(raw_games = raw_games_out)
  game_df_out <- clean_nested_categories(game_df = game_df_out, raw_games = raw_games_out)
  game_df_out <- clean_mechanics(game_df = game_df_out)
  game_df_out <- clean_categories(game_df = game_df_out)

  # Return data
  game_df_out
}

#' @export
#' @rdname searchbga
