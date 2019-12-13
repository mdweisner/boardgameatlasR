#' @importFrom attempt stop_if_not
#' @importFrom attempt stop_if
#' @importFrom attempt warn_if
#' @importFrom curl has_internet
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

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".x"))


check_internet <- function(){
  stop_if_not(.x = has_internet(), msg = "Please check your internet connection")
}

#' @importFrom httr status_code
check_status <- function(res){
  stop_if_not(.x = status_code(res),
              .p = ~ .x == 200,
              msg = paste0("The API returned an error - Status Code: ", status_code(res)," \n
           Note: Incorrect IDs may produce status code errors."))
}


# check BGA token set
bga_pat <- function() {
  pat <- Sys.getenv('BOARDGAMEATLAS_PAT')
  if (identical(pat, "")) {
    warning("Warning: Please set env var BOARDGAMEATLAS_PAT to your Board Game Atlas personal client_id. \n If you don't have a client_id, you can sign up at https://www.boardgameatlas.com/api/docs. \n This will not prevent you from manually entering your client_id value, but it's safer to add it to your R environment as BOARDGAMEATLAS_PAT so then search_bga() will use it automatically.",
         call. = FALSE)
  }
  pat
}

client_id <- Sys.getenv('BOARDGAMEATLAS_PAT')

base_url <- "https://www.boardgameatlas.com/api/search"

# args tests
limit_check <- function(limit){
  if (!is.null(limit)){
  stop_if(.x = limit,
              .p = ~ .x > 100 | .x < 1 | (.x %% 1) != 0 | !is.numeric(.x),
              msg = "limit must be an integer between 1 and 100")
}
}
 skip_check <- function(skip){
   if(!is.null(skip)){
   stop_if(.x = skip,
           .p = ~ .x < 0 | (.x %% 1) != 0 | !is.numeric(.x),
           msg = "skip must be an integer greater than 0")
   }
 }

 ids_check <- function(ids){
   if(!is.null(ids)){
   stop_if(.x = ids,
           .p = ~ grepl("\\s+", .x, perl = TRUE) | !is.character(ids),
           msg = "ids must be a single string value and cannot include spaces. The format must be 'firstID,secondID,thirdID'.")
   }
 }
kickstarter_check <- function(kickstarter){
  if(!is.null(kickstarter)) {
  stop_if_not(.x = kickstarter,
          .p = ~ is.logical(.x),
          msg = "kickstarter must be either TRUE or FALSE")
    warn_if(.x = kickstarter,
            .p = ~ .x == FALSE,
            msg = "Warning: an API issue will return kickstarter games if kickstarter is set to ANYTHING (TRUE, FALSE, or NULL!). \n To avoid kickstarter games DO NOT SPECIFY KICKSTARTER IN YOUR QUERY.")
  }
}
random_check <- function(random){
  if(!is.null(random)) {
    stop_if_not(.x = random,
          .p = ~ is.logical(.x),
          msg = "random must be either TRUE or FALSE")
  }
}

name_check <- function(name){
  if(!is.null(name)){
    stop_if_not(.x = name,
            .p = ~ is.character(name),
            msg = "name must be a string.")
  }
}

exact_check <- function(name_exact){
  if(!is.null(name_exact)) {
    stop_if_not(.x = name_exact,
                .p = ~ is.logical(.x),
                msg = "name_exact must be either TRUE or FALSE")
  }
}

fuzzy_match_check <- function(name_fuzzy){
  if(!is.null(name_fuzzy)) {
    stop_if_not(.x = name_fuzzy,
                .p = ~ is.logical(.x),
                msg = "name_fuzzy must be either TRUE or FALSE")
  }
}

fuzzy_exact_check <- function(name_fuzzy, name_exact){
  if(!is.null(name_fuzzy) & !is.null(name_exact))
  if(name_fuzzy == TRUE){
  warn_if(.x = name_exact,
          .p = ~ .x == TRUE,
          msg = "Both name_fuzzy and name_exact are set to TRUE. One should be FALSE or name_fuzzy will be override name_exact.")
}
}

designer_check <- function(designer){
  if(!is.null(designer)){
    stop_if_not(.x = designer,
                .p = ~ is.character(designer),
                msg = "designer must be a string.")
  }
}

publisher_check <- function(publisher){
  if(!is.null(publisher)){
    stop_if_not(.x = publisher,
                .p = ~ is.character(publisher),
                msg = "publisher must be a string.")
  }
}

artist_check <- function(artist){
  if(!is.null(artist)){
    stop_if_not(.x = artist,
                .p = ~ is.character(artist),
                msg = "artist must be a string.")
  }
}

mechanics_check <- function(mechanics){
  if(!is.null(mechanics)){
    stop_if(.x = mechanics,
            .p = ~ grepl("\\s+", .x, perl = TRUE) | !is.character(mechanics),
            msg = "mechanics must be a single string value and cannot include spaces. The format must be 'firstID,secondID,thirdID'.")
  }
}

categories_check <- function(categories){
  if(!is.null(categories)){
    stop_if(.x = categories,
            .p = ~ grepl("\\s+", .x, perl = TRUE) | !is.character(categories),
            msg = "categories must be a single string value and cannot include spaces. The format must be 'firstID,secondID,thirdID'.")
  }
}
order_by_check <- function(order_by){
  if(!is.null(order_by)){
    stop_if_not(.x = order_by,
            .p = ~ .x %in% c("popularity", "price", "discount", "reddit_week_count", "reddit_day_count", "name", "year_published", "min_age", "min_playtime", "max_playtime", "min_players", "max_players", "average_user_rating", "num_user_ratings", "deadline"),
            msg = "order_by must a string equal to one of the following: \n popularity, price, discount, reddit_week_count, reddit_day_count, name, year_published, min_age, min_playtime, max_playtime, min_players, max_players, average_user_rating, num_user_ratings"
    )
  }
}

ascending_check <- function(ascending){
  if(!is.null(ascending)) {
    stop_if_not(.x = ascending,
                .p = ~ is.logical(.x),
                msg = "ascending must be either TRUE or FALSE")
  }
}

min_players_check <- function(min_players){
  if (!is.null(min_players)){
    stop_if(.x = min_players,
            .p = ~ !is.numeric(.x),
            msg = "min_players must be an integer")
  warn_if(.x = min_players,
          .p = ~  (.x %% 1) != 0,
          msg = "Warning: min_players should be an whole number (integer)")
  }
}

max_players_check <- function(max_players){
  if (!is.null(max_players)){
    stop_if(.x = max_players,
            .p = ~ !is.numeric(.x),
            msg = "max_players must be an integer")
  warn_if(.x = max_players,
          .p = ~  (.x %% 1) != 0,
          msg = "Warning: max_players should be an whole number (integer)")
  }
}

player_range_check <- function(min_players, max_players){
  if(!is.null(min_players) & !is.null(max_players)){
    warn_if(.x = max_players,
            .p = ~ .x < min_players,
            msg = "Warning: Your minimum players are greater than your maximum players. \n This may return mislabeled games or none at all.")
  }
}

min_playtime_check <- function(min_playtime){
  if (!is.null(min_playtime)){
    stop_if(.x = min_playtime,
            .p = ~ !is.numeric(.x),
            msg = "min_playtime must be an integer")
    warn_if(.x = min_playtime,
            .p = ~  (.x %% 1) != 0,
            msg = "Warning: min_playtime should be an whole number (integer)")
  }
}

max_playtime_check <- function(max_playtime){
  if (!is.null(max_playtime)){
    stop_if(.x = max_playtime,
            .p = ~ !is.numeric(.x),
            msg = "max_playtime must be an integer")
    warn_if(.x = max_playtime,
            .p = ~  (.x %% 1) != 0,
            msg = "Warning: max_playtime should be an whole number (integer)")
  }
}

playtime_range_check <- function(min_playtime, max_playtime){
  if(!is.null(min_playtime) & !is.null(max_playtime)){
    warn_if(.x = max_playtime,
            .p = ~ .x < min_playtime,
            msg = "Warning: Your miniumum playtime is greater than your maximum playtime. \n This may return mislabeled games or none at all.")
  }
}


min_age_check <- function(min_age){
  if (!is.null(min_age)){
    stop_if(.x = min_age,
            .p = ~ !is.numeric(.x),
            msg = "min_age must be an integer")
    warn_if(.x = min_age,
            .p = ~  (.x %% 1) != 0,
            msg = "Warning: min_age should be an whole number (integer)")
  }
}

max_age_check <- function(max_age){
  if (!is.null(max_age)){
    stop_if(.x = max_age,
            .p = ~ !is.numeric(.x),
            msg = "max_age must be an integer")
    warn_if(.x = max_age,
            .p = ~  (.x %% 1) != 0,
            msg = "Warning: max_age should be an whole number (integer)")
  }
}

age_range_check <- function(min_age, max_age){
  if(!is.null(min_age) & !is.null(max_age)){
    warn_if(.x = max_age,
            .p = ~ .x < min_age,
            msg = "Warning: Your minimum age is greater than your maximum age. \n This may return mislabeled games or none at all.")
  }
}

min_year_check <- function(min_year){
  if (!is.null(min_year)){
    stop_if(.x = min_year,
            .p = ~ !is.numeric(.x),
            msg = "min_year must be an integer")
    warn_if(.x = min_year,
            .p = ~  (.x %% 1) != 0,
            msg = "Warning: min_year should be an whole number (integer)")
  }
}

max_year_check <- function(max_year){
  if (!is.null(max_year)){
    stop_if(.x = max_year,
            .p = ~ !is.numeric(.x),
            msg = "max_year must be an integer")
    warn_if(.x = max_year,
            .p = ~  (.x %% 1) != 0,
            msg = "Warning: max_year should be an whole number (integer)")
  }
}

year_range_check <- function(min_year, max_year){
  if(!is.null(min_year) & !is.null(max_year)){
    warn_if(.x = max_year,
            .p = ~ .x < min_year,
            msg = "Warning: Your minimum year published is greater than your maximum year published. \n This may return mislabeled games or none at all.")
  }
}

min_price_check <- function(min_price){
  if (!is.null(min_price)){
    stop_if(.x = min_price,
            .p = ~ !is.numeric(.x),
            msg = "min_price must be numeric")
  }
}

max_price_check <- function(max_price){
  if (!is.null(max_price)){
    stop_if(.x = max_price,
            .p = ~ !is.numeric(.x),
            msg = "max_price must be numeric")
  }
}

price_range_check <- function(min_price, max_price){
  if(!is.null(min_price) & !is.null(max_price)){
    warn_if(.x = max_price,
            .p = ~ .x < min_price,
            msg = "Warning: Your minimum price is greater than your maximum price. \n This may return mislabeled games or none at all")
  }
}

min_msrp_check <- function(min_msrp){
  if (!is.null(min_msrp)){
    stop_if(.x = min_msrp,
            .p = ~ !is.numeric(.x),
            msg = "min_msrp must be numeric")
  }
}

max_msrp_check<- function(max_msrp){
  if (!is.null(max_msrp)){
    stop_if(.x = max_msrp,
            .p = ~ !is.numeric(.x),
            msg = "max_msrp must be numeric")
  }
}

msrp_range_check <- function(min_msrp, max_msrp){
  if(!is.null(min_msrp) & !is.null(max_msrp)){
    warn_if(.x = max_msrp,
            .p = ~ .x < min_msrp,
            msg = "Warning: Your minimum MSRP is greater than your maximum MSRP. \n This may return mislabeled games or none at all.")
  }
}

min_discount_check <- function(min_discount){
  if (!is.null(min_discount)){
    stop_if(.x = min_discount,
            .p = ~ !is.numeric(.x),
            msg = "min_age must be numeric, typically a double value between 0 and 1")
    warn_if(.x = min_discount,
            .p = ~  .x > 1,
            msg = "Warning: min_discount is greater than 1, which may return games with a greater than 100% discount. \n This may return mislabeled games or none at all.")
  }
}

max_discount_check <- function(max_discount){
  if (!is.null(max_discount)){
    stop_if(.x = max_discount,
            .p = ~ !is.numeric(.x),
            msg = "min_age must be an numeric, typically a double value between 0 and 1")
    warn_if(.x = max_discount,
            .p = ~  .x > 1,
            msg = "Warning: max_discount is greater than 1, which may return games with a greater than 100% discount. \n This may return mislabeled games or none at all.")
  }
}

discount_range_check <- function(min_discount, max_discount){
  if(!is.null(min_discount) & !is.null(max_discount)){
    warn_if(.x = max_discount,
            .p = ~ .x < min_discount,
            msg = "Warning: Your minimum discount is greater than your maximum discount \n Tests show in these cases max_discount overrides min_discount. \n It may return mislabeled games or none at all.")
  }
}

min_reddit_total_check <- function(min_reddit_total){
  if (!is.null(min_reddit_total)){
    stop_if(.x = min_reddit_total,
            .p = ~ !is.numeric(.x),
            msg = "min_reddit_total must be an integer")
    warn_if(.x = min_reddit_total,
            .p = ~  (.x %% 1) != 0,
            msg = "Warning: min_reddit_total should be an whole number (integer)")
  }
}
max_reddit_total_check <- function(max_reddit_total){
  if (!is.null(max_reddit_total)){
    stop_if(.x = max_reddit_total,
            .p = ~ !is.numeric(.x),
            msg = "max_reddit_total must be an integer")
    warn_if(.x = max_reddit_total,
            .p = ~  (.x %% 1) != 0,
            msg = "Warning: max_reddit_total should be an whole number (integer)")
  }
}

reddit_total_range <- function(min_reddit_total, max_reddit_total){
  if(!is.null(min_reddit_total) & !is.null(max_reddit_total)){
    warn_if(.x = max_reddit_total,
            .p = ~ .x < min_reddit_total,
            msg = "Warning: Your minimum reddit comment total is greater than your maximum reddit comment total \n It may return mislabeled games or none at all.")
  }
}


min_reddit_week_check <- function(min_reddit_week){
  if (!is.null(min_reddit_week)){
    stop_if(.x = min_reddit_week,
            .p = ~ !is.numeric(.x),
            msg = "min_reddit_week must be an integer")
    warn_if(.x = min_reddit_week,
            .p = ~  (.x %% 1) != 0,
            msg = "Warning: min_reddit_week should be an whole number (integer)")
  }
}

max_reddit_week_check <- function(max_reddit_week){
  if (!is.null(max_reddit_week)){
    stop_if(.x = max_reddit_week,
            .p = ~ !is.numeric(.x),
            msg = "max_reddit_week must be an integer")
    warn_if(.x = max_reddit_week,
            .p = ~  (.x %% 1) != 0,
            msg = "Warning: max_reddit_week should be an whole number (integer)")
  }
}

reddit_week_range <- function(min_reddit_week, max_reddit_week){
  if(!is.null(min_reddit_week) & !is.null(max_reddit_week)){
    warn_if(.x = max_reddit_week,
            .p = ~ .x < min_reddit_week,
            msg = "Warning: Your minimum reddit weekly comment is greater than your maximum reddit weekly comment \n It may return mislabeled games or none at all.")
  }
}


min_reddit_day_check <- function(min_reddit_day){
  if (!is.null(min_reddit_day)){
    stop_if(.x = min_reddit_day,
            .p = ~ !is.numeric(.x),
            msg = "min_reddit_day must be an integer")
    warn_if(.x = min_reddit_day,
            .p = ~  (.x %% 1) != 0,
            msg = "Warning: min_reddit_day should be an whole number (integer)")
  }
}

max_reddit_day_check <- function(max_reddit_day){
  if (!is.null(max_reddit_day)){
    stop_if(.x = max_reddit_day,
            .p = ~ !is.numeric(.x),
            msg = "max_reddit_day must be an integer")
    warn_if(.x = max_reddit_day,
            .p = ~  (.x %% 1) != 0,
            msg = "Warning: max_reddit_day should be an whole number (integer)")
  }
}

reddit_day_range <- function(min_reddit_day, max_reddit_day){
  if(!is.null(min_reddit_day) & !is.null(max_reddit_day)){
    warn_if(.x = max_reddit_day,
            .p = ~ .x < min_reddit_day,
            msg = "Warning: Your minimum reddit daily comment is greater than your maximum reddit daily comment \n It may return mislabeled games or none at all.")
  }
}

# Data Cleaning

# SINGLE VALUES
single_value <- function(x, var_name) {
  if(as.character(typeof(x[[var_name]]) == "NULL")) {
    NA
  }
  else {
    x[[var_name]]
  }
}

# NESTED VALUES
nested_value <- function(x, var_name) {
  if(as.character(typeof(x[[var_name]]) != "list")) {
    NA
  }
  else {
    paste(x[[var_name]], collapse=", ")
  }
}

### Clean Single Categories
clean_single_categories <- function(raw_games) {
  # Build ID data frame
  game_df <- as.data.frame(cbind(map_chr(raw_games, ~ single_value(.x, var_name="id"))))
  colnames(game_df) <- "id"

  # Add names
  game_df <- game_df %>%
    mutate(name = map_chr(raw_games, ~ single_value(.x, var_name="name")))

  # Add year published
  game_df <- game_df %>%
    mutate(year_published = map_int(raw_games, ~ single_value(.x, var_name="year_published")))

  # add min and max players
  game_df <- game_df %>%
    mutate(min_players = map_int(raw_games, ~ single_value(.x, var_name="min_players")))

  game_df <- game_df %>%
    mutate(max_players = map_int(raw_games, ~ single_value(.x, var_name="max_players")))

  # add min and max playtime
  game_df <- game_df %>%
    mutate(min_playtime = map_int(raw_games, ~ single_value(.x, var_name="min_playtime")))

  game_df <- game_df %>%
    mutate(max_playtime = map_int(raw_games, ~ single_value(.x, var_name="max_playtime")))

  # add min age
  game_df <- game_df %>%
    mutate(min_age = map_int(raw_games, ~ single_value(.x, var_name="min_age")))

  # add description
  game_df <- game_df %>%
    mutate(description = map_chr(raw_games, ~ single_value(.x, var_name="description")))

  # add urls
  game_df <- game_df %>%
    mutate(image_url = map_chr(raw_games, ~ single_value(.x, var_name="image_url")))

  game_df <- game_df %>%
    mutate(url = map_chr(raw_games, ~ single_value(.x, var_name="url")))

  game_df <- game_df %>%
    mutate(official_url = map_chr(raw_games, ~ single_value(.x, var_name="official_url")))

  game_df <- game_df %>%
    mutate(rules_url = map_chr(raw_games, ~ single_value(.x, var_name="rules_url")))

  # add kickstarter ID
  game_df <- game_df %>%
    mutate(ks_id = map_chr(raw_games, ~ single_value(.x, var_name="ks_id")))

  # add kickstarter URL
  game_df <- game_df %>%
    mutate(kickstarter_url = map_chr(raw_games, ~ single_value(.x, var_name="kickstarter_url")))

  # Below is the old way of calculating kickstarter_plede which threw notes in devtools::build().
  # game_df <- game_df %>%
  #   mutate(kickstarter_pledge = map_chr(raw_games, ~ single_value(.x, var_name="kickstarter_pledge")))
  #
  #  game_df <- game_df %>%
  #    mutate(kickstarter_pledge = as.double(gsub("[^0-9.]", "", kickstarter_pledge)))
  kickstarter_pledge <- map_chr(raw_games, ~ single_value(.x, var_name="kickstarter_pledge"))

  kickstarter_pledge_cleaner <- function(game_df_raw, kickstarter_pledge_raw) {
    game_df_raw <- game_df_raw %>%
      mutate(kickstarter_pledge = as.double(gsub("[^0-9.]", "", kickstarter_pledge_raw)))
    return(game_df_raw)
  }

 game_df <- kickstarter_pledge_cleaner(game_df_raw = game_df, kickstarter_pledge_raw = kickstarter_pledge)


# Below is the old way of calculating kickstarter_goal which threw notes in devtools::build().
  # game_df <- game_df %>%
  #   mutate(kickstarter_goal = map_chr(raw_games, ~ single_value(.x, var_name="kickstarter_goal")))
  #
  # game_df <- game_df %>%
  #   mutate(kickstarter_goal = as.double(gsub("[^0-9.]", "", kickstarter_goal)))

 kickstarter_goal <- map_chr(raw_games, ~ single_value(.x, var_name="kickstarter_goal"))

 kickstarter_goal_cleaner <- function(game_df_raw, kickstarter_goal_raw) {
   game_df_raw <- game_df_raw %>%
     mutate(kickstarter_goal = as.double(gsub("[^0-9.]", "", kickstarter_goal_raw)))
   return(game_df_raw)
 }

 game_df <- kickstarter_goal_cleaner(game_df_raw = game_df, kickstarter_goal_raw = kickstarter_goal)

  game_df <- game_df %>%
    mutate(kickstarter_percent = map_int(raw_games, ~ single_value(.x, var_name="kickstarter_percent")))

  # Below is the old way of calculating kickstarter_deadline which threw notes in devtools::build().
  # game_df <- game_df %>%
  #   mutate(kickstarter_deadline = map_chr(raw_games, ~ single_value(.x, var_name="kickstarter_deadline")))
  #
  # game_df <- game_df %>%
  #   mutate(kickstarter_deadline = as.Date(kickstarter_deadline))

  kickstarter_deadline <- map_chr(raw_games, ~ single_value(.x, var_name="kickstarter_deadline"))

  kickstarter_deadline_cleaner <- function(game_df_raw, kickstarter_deadline_raw) {
    game_df_raw <- game_df_raw %>%
         mutate(kickstarter_deadline = as.Date(kickstarter_deadline_raw))
    return(game_df_raw)
  }

  game_df <- kickstarter_deadline_cleaner(game_df_raw = game_df, kickstarter_deadline_raw = kickstarter_deadline)

  game_df <- game_df %>%
    mutate(kickstarter_deadline_ago = map_chr(raw_games, ~ single_value(.x, var_name="kickstarter_deadline_ago")))

  # add price and discount
  game_df <- game_df %>%
    mutate(price = as.double(map_chr(raw_games, ~ single_value(.x, var_name="price"))))

  game_df <- game_df %>%
    mutate(msrp = as.double(map_chr(raw_games, ~ single_value(.x, var_name="msrp"))))

  game_df <- game_df %>%
    mutate(discount = as.double(map_chr(raw_games, ~ single_value(.x, var_name="discount"))))

  game_df <- game_df %>%
    mutate(historical_low_price = map_dbl(raw_games, ~ single_value(.x, var_name="historical_low_price")))

  # Add Primary Publisher
  game_df <- game_df %>%
    mutate(primary_publisher = map_chr(raw_games, ~ single_value(.x, var_name="primary_publisher")))

  # add user ratings
  game_df <- game_df %>%
    mutate(num_user_ratings = map_int(raw_games, ~ single_value(.x, var_name="num_user_ratings")))

  game_df <- game_df %>%
    mutate(average_user_rating = map_dbl(raw_games, ~ single_value(.x, var_name="average_user_rating")))

  # add game dimensions
  game_df <- game_df %>%
    mutate(size_height = map_dbl(raw_games, ~ single_value(.x, var_name="size_height")))

  game_df <- game_df %>%
    mutate(size_width = map_dbl(raw_games, ~ single_value(.x, var_name="size_width")))

  game_df <- game_df %>%
    mutate(size_depth = map_dbl(raw_games, ~ single_value(.x, var_name="size_depth")))

  game_df <- game_df %>%
    mutate(size_units = map_chr(raw_games, ~ single_value(.x, var_name="size_units")))

  # reddit comments
  game_df <- game_df %>%
    mutate(reddit_all_time_count = map_int(raw_games, ~ single_value(.x, var_name="reddit_all_time_count")))

  game_df <- game_df %>%
    mutate(reddit_week_count = map_int(raw_games, ~ single_value(.x, var_name="reddit_week_count")))

  game_df <- game_df %>%
    mutate(reddit_day_count = map_int(raw_games, ~ single_value(.x, var_name="reddit_day_count")))

  # Current Date
  game_df <- game_df %>%
    mutate(date_of_query = Sys.Date())

  return(game_df)
}

### Clean Nested Values
clean_nested_categories <- function(game_df, raw_games){

  # historical dates
  game_df <- game_df %>%
    mutate(historical_low_date = as.Date(map_chr(raw_games, ~ nested_value(.x, var_name="historical_low_date"))))

  # all publishers
  game_df <- game_df %>%
    mutate(all_publishers = map_chr(raw_games, ~ nested_value(.x, var_name="publishers")))

  # all designers
  game_df <- game_df %>%
    mutate(all_designers = map_chr(raw_games, ~ nested_value(.x, var_name="designers")))

  # all developers
  game_df <- game_df %>%
    mutate(all_developers = map_chr(raw_games, ~ nested_value(.x, var_name="developers")))

  # all artists
  game_df <- game_df %>%
    mutate(all_artists = map_chr(raw_games, ~ nested_value(.x, var_name="artists")))

  # all names
  game_df <- game_df %>%
    mutate(all_names = map_chr(raw_games, ~ nested_value(.x, var_name="names")))

  # game mechanics
  game_df <- game_df %>%
    mutate(mechanics_list = map_chr(raw_games, ~ nested_value(.x, var_name="mechanics")))

  # game categories
  game_df <- game_df %>%
    mutate(categories_list = map_chr(raw_games, ~ nested_value(.x, var_name="categories")))

  return(game_df)
}

### Clean Mechanics
clean_mechanics <- function(game_df) {
  game_df <- game_df %>%
    mutate(mech_acting = if_else(grepl("n1GtBt35Rd", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_action_movement = if_else(grepl("PGjmKGi26h", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_action_point_allowance  = if_else(grepl("oeg6wN9Eoc", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_action_selection  = if_else(grepl("Bc7R8pLoGk", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_area_control  = if_else(grepl("05zCZoLvQJ", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_area_enclosure  = if_else(grepl("3te2oybNR4", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_area_movement  = if_else(grepl("bgGxE0pI2B", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_asymmetric  = if_else(grepl("amcImLdOmD", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_auction  = if_else(grepl("AZxlPpi5oq", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_betting  = if_else(grepl("3tuJiW3pps", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_bluffing  = if_else(grepl("ZX3hYcF9H7", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_building  = if_else(grepl("UEzHyBWtz8", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_campaign  = if_else(grepl("xuphiSlrxI", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_card_drafting  = if_else(grepl("iWODHwRGuU", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_card_game  = if_else(grepl("ebJKldFVeS", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_card_placement  = if_else(grepl("ngCSHHk0H2", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_chit_pull  = if_else(grepl("Gan96fffLL", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_communication_limits  = if_else(grepl("0ez69aUfuJ", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_commodity_speculation  = if_else(grepl("K0ykGhTMa8", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_confusion  = if_else(grepl("AAi6B8NLaV", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_conversation  = if_else(grepl("hwyCo1W0hi", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_cooperative  = if_else(grepl("33UT4gTFqy", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_cooperative_play  = if_else(grepl("9mNukNBxfZ", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_crayon_rail_system  = if_else(grepl("xVDf5dmJts", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_currency  = if_else(grepl("9y0yU5xWRU", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_d6_rpg  = if_else(grepl("kZf6CVjtq7", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_deck_building  = if_else(grepl("vZsDDAdOoe", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_deduction  = if_else(grepl("GsNGxZFNCK", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_dexterity  = if_else(grepl("5kvyChnWuO", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_dice_building  = if_else(grepl("lVSHu9efHv", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_dice_movement  = if_else(grepl("zw4KMn5rcD", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_dice_rolling  = if_else(grepl("R0bGq4cAl4", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_drafting  = if_else(grepl("wV5peB05xs", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_drawing  = if_else(grepl("KfTS5BwIsu", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_drinking  = if_else(grepl("Ouo00D4ka7", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_engine_building  = if_else(grepl("yu3eas6v7A", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_euro_game  = if_else(grepl("hUn7uJHrYm", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_fighting  = if_else(grepl("kS8npG0jl8", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_flicking  = if_else(grepl("S0O9ucexPS", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_grid_movement  = if_else(grepl("qu5BcGjAzk", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_hand_management  = if_else(grepl("WPytek5P8l", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_hand_eye_coordination  = if_else(grepl("i3xnwJ7VPV", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_hex_counter  = if_else(grepl("6CyVyXRn1C", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_hidden_movement  = if_else(grepl("aQZ40lKv8O", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_hidden_traitor  = if_else(grepl("Voqy2dgrIM", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_legacy  = if_else(grepl("DwmsVEvNVd", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_line_drawing  = if_else(grepl("MaXzmoZUoX", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_larp  = if_else(grepl("UaLql58fua", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_mancala  = if_else(grepl("ar56ACXDNe", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_memory  = if_else(grepl("r6yIFvyXDD", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_modular_board  = if_else(grepl("U3zhCQH7Et", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_paper_pencil  = if_else(grepl("UHdPUeuqyZ", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_partnerships  = if_else(grepl("GNtouC8NLm", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_party  = if_else(grepl("3dFd0RwsY4", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_pattern_building  = if_else(grepl("9YdRn9J9oZ", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_pattern_recognition  = if_else(grepl("uZJS07nXF5", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_pick_up_deliver  = if_else(grepl("BbTMRkwL0b", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_player_elimination  = if_else(grepl("BGrhzIN69D", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_player_vs_player  = if_else(grepl("YyBg5CzHBF", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_point_to_point  = if_else(grepl("MEAoOygZsA", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_pool_building  = if_else(grepl("ea1eaPBQn8", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_press_luck  = if_else(grepl("hmipYN1R1I", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_puzzle  = if_else(grepl("24FWssBC3o", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_real_time  = if_else(grepl("E9VKQ8uMSP", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_resource_gathering  = if_else(grepl("Rt6V388y6M", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_rock_paper_scissors  = if_else(grepl("j7x8jY4ay2", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_roleplaying  = if_else(grepl("EVeAdboGUA", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_role_selection  = if_else(grepl("gRlslORtpI", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_roll_spin_move  = if_else(grepl("mGBzR68m8Z", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_roll_write  = if_else(grepl("zIPRS41oiN", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_rondel  = if_else(grepl("FA6HjbhdNW", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_route_building  = if_else(grepl("asw8k7EIJI", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_route_network_building  = if_else(grepl("c6gkRM7rSy", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_secret_unit_deployment  = if_else(grepl("RwgEONzIzc", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_set_collection  = if_else(grepl("lA3KUtVFCy", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_simulation  = if_else(grepl("eRe1jJCBFe", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_simultaneous_play  = if_else(grepl("DEvPj5twid", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_simultaneous_action  = if_else(grepl("za4PP1LH00", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_skirmish  = if_else(grepl("kuRNhFADjS", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_social_deduction  = if_else(grepl("x3wVCq1HEP", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_stock_holding  = if_else(grepl("jbLrZb1xIb", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_storytelling  = if_else(grepl("GUoWg3Mfh5", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_tactical_movement  = if_else(grepl("yDlgk7rXno", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_tactics  = if_else(grepl("jCRze30VP1", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_take_that  = if_else(grepl("T8JEFYwoqy", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_teams  = if_else(grepl("9jnCsVuRat", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_tile_placement  = if_else(grepl("8PN2HE86wg", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_time_track  = if_else(grepl("cXd5KaXXZo", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_tower_defense  = if_else(grepl("wEBvff5T5c", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_trading  = if_else(grepl("AVY6EvSQTP", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_trick_taking  = if_else(grepl("3GSQl800lk", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_variable_phase_order  = if_else(grepl("zzsE4jtI1b", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_player_powers  = if_else(grepl("XM2FYZmBHH", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_voting  = if_else(grepl("JYYdBW6UCE", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_wargame  = if_else(grepl("yomuPpKFx4", game_df$mechanics_list, ignore.case = TRUE), 1, 0),
           mech_worker_placement  = if_else(grepl("fBOTEBUAmV", game_df$mechanics_list, ignore.case = TRUE), 1, 0))

  return(game_df)
}

### Clean Categories
clean_categories <- function(game_df) {
  game_df <- game_df %>%
    mutate(cat_4x  = if_else(grepl("85OKv8p5Ow", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_bstract  = if_else(grepl("hBqZ3Ar4RJ", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_adventure  = if_else(grepl("KUBCKBkGxV", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_age_of_reason  = if_else(grepl("20iDvpbh7A", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_alternative_history  = if_else(grepl("nWDac9tQzt", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_american_west  = if_else(grepl("4mOtRRwSoj", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_ancient  = if_else(grepl("a8NM5cugJX", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_animals  = if_else(grepl("MWoxgHrOJD", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_apocalyptic  = if_else(grepl("eFaACC6y2c", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_art  = if_else(grepl("k0dglq5j6N", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_aviation  = if_else(grepl("QB4sEpx1Uu", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_bluffing  = if_else(grepl("PinhJrhnxU", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_campaign  = if_else(grepl("fW5vusE96B", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_card_game  = if_else(grepl("eX8uuNlQkQ", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_children_game  = if_else(grepl("HKaYVNIxAJ", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_city_building  = if_else(grepl("ODWOjWAJj3", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_civil_war  = if_else(grepl("w8XD66FUZ2", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_civilization  = if_else(grepl("329DxyFL9D", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_collectible_components  = if_else(grepl("vXxLT0FDTZ", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_comic_book_strip  = if_else(grepl("G5kfqnPBP6", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_conversation  = if_else(grepl("iTvYWFmD1c", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_cooperative  = if_else(grepl("ge8pIhEUGE", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_cyberpunk  = if_else(grepl("Ef4oYLHNhI", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_deduction  = if_else(grepl("bCBXJy9qDw", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_dexterity  = if_else(grepl("bKrxqD9mYc", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_dice  = if_else(grepl("mavSOM8vjH", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_dinosaur  = if_else(grepl("UuxiExraPF", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_drinking  = if_else(grepl("We3MM46qBr", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_dungeons_and_dragons  = if_else(grepl("ZEW7DPFAE6", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_economic  = if_else(grepl("N0TkEGfEsF", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_educational  = if_else(grepl("B3NRLMK4xD", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_electronic  = if_else(grepl("crxgUzJSEz", game_df$categories_list, ignore.case = TRUE), 1, 0),
           at_environmental  = if_else(grepl("gsekjrPJz0", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_espionage  = if_else(grepl("u5ZiYctU6T", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_eurogame  = if_else(grepl("h8wfZG0j3I", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_expansion  = if_else(grepl("v4SfYtS2Lr", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_exploration  = if_else(grepl("yq6hVlbM2R", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_family_game  = if_else(grepl("7rV11PKqME", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_fan_made  = if_else(grepl("ctumBZyj5l", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_fantasy  = if_else(grepl("ZTneo8TaIO", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_farming  = if_else(grepl("Wr8uXcoR9p", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_fighting  = if_else(grepl("upXZ8vNfNO", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_fishing  = if_else(grepl("zNxFBqBHXA", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_flicking  = if_else(grepl("3NDxCLUny4", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_food  = if_else(grepl("YrDuNj8lvr", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_gay  = if_else(grepl("H9Ef643lYf", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_halloween  = if_else(grepl("NR0vgCx5R7", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_horror  = if_else(grepl("cAIkk5aLdQ", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_humor  = if_else(grepl("TYnxiuiI3X", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_inudstry_manufacturing  = if_else(grepl("zqFmdU4Fp2", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_japan  = if_else(grepl("R7PTH00PmO", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_kickstarter  = if_else(grepl("rrvd68LjOR", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_legacy  = if_else(grepl("XeYUw9159M", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_luck  = if_else(grepl("nHZiDOXNla", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_mafia  = if_else(grepl("pIMmuVYnQp", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_math  = if_else(grepl("POlqwScVxD", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_mature_adult  = if_else(grepl("ZhlfIPxYsw", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_mecha  = if_else(grepl("c1AnMUJrTF", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_medical  = if_else(grepl("AeWXMxbm91", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_medeival  = if_else(grepl("QAYkTHK1Dd", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_memory  = if_else(grepl("AujCle9cUq", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_miniatures  = if_else(grepl("FC6ElKI9tk", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_modern_warfare  = if_else(grepl("L6NUwNdblq", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_movie_theme  = if_else(grepl("TJnR5obHsQ", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_movie_tv_radio_theme  = if_else(grepl("Sod2YBWMKi", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_murder_mystery  = if_else(grepl("Kk70K0524Z", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_mythology  = if_else(grepl("MHkqIVxwtx", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_napoleonic  = if_else(grepl("IpcJzp0TVC", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_nautical  = if_else(grepl("vqZ5XzGWQD", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_negotiation  = if_else(grepl("jZEDOpx07e", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_ninjas1  = if_else(grepl("mWb5kHTAg1", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_ninjas2  = if_else(grepl("rtslXnT90O", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_novel_based  = if_else(grepl("dO9HVl2TW7", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_party_game  = if_else(grepl("X8J7RM6dxX", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_pirates  = if_else(grepl("9EIayX6n5a", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_political  = if_else(grepl("TKQncFVX74", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_post_apocalyptic  = if_else(grepl("8Z7nWG2kOw", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_post_napoleonic  = if_else(grepl("5APB1MWk6X", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_prehistoric  = if_else(grepl("YyszHun1HP", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_print_play  = if_else(grepl("ov6sEmlkiC", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_prison_escape  = if_else(grepl("dAyk5NtNTV", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_puzzle  = if_else(grepl("WVMOS3s2pb", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_queer  = if_else(grepl("c6nnwyDdnl", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_roleplaying_game  = if_else(grepl("2Gu62aKdma", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_racing  = if_else(grepl("tQGLgwdbYH", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_realtime  = if_else(grepl("PzWI2uaif0", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_religious  = if_else(grepl("DRqeVkXWqX", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_renaissance  = if_else(grepl("nuHYRFmMjU", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_resource_management  = if_else(grepl("zyj9ZK3mHB", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_roman_empire  = if_else(grepl("KSBdPfxs6F", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_romance  = if_else(grepl("E5rYwP0Ybr", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_sci_fi  = if_else(grepl("3B3QpKvXD3", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_socialite  = if_else(grepl("c6ei4hkUxm", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_solo_solitaire  = if_else(grepl("VzyslQJGrG", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_space_exploration  = if_else(grepl("0MdRqhkNpw", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_spies_secret_agents  = if_else(grepl("Hc6vcim5DS", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_sports  = if_else(grepl("hShsL2DktG", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_strategy  = if_else(grepl("O0ogzwLUe8", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_superhero  = if_else(grepl("usFW8szGAq", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_tech  = if_else(grepl("yHTeXNjln0", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_territory_building  = if_else(grepl("buDTYyPw4D", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_theme_park  = if_else(grepl("vCzpbYT7RU", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_trains  = if_else(grepl("JwHcKqxh33", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_transportation  = if_else(grepl("CWYOF9xu7O", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_travel  = if_else(grepl("TR4CiP8Huj", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_trivia  = if_else(grepl("YGHGDjahKY", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_video_game_theme  = if_else(grepl("djokexoK0U", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_war  = if_else(grepl("ssZjU3HETz", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_wargame  = if_else(grepl("jX8asGGR6o", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_western  = if_else(grepl("EHUBCITA3t", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_word_game  = if_else(grepl("rHvAx4hH2f", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_world_war_i  = if_else(grepl("wTLJSVEbm6", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_world_war_ii  = if_else(grepl("fl3TogdUzX", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_world_war_iii  = if_else(grepl("OlkGBmu4Va", game_df$categories_list, ignore.case = TRUE), 1, 0),
           cat_zombies  = if_else(grepl("FmGV9rVu1c", game_df$categories_list, ignore.case = TRUE), 1, 0))

  return(game_df)
}
