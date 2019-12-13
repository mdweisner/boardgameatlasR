# devwork
library(devtools)
library(usethis)
library(desc)

# Remove default DESC
unlink("DESCRIPTION")
# Create and clean desc
my_desc <- description$new("!new")

# Set your package name
my_desc$set("Package", "boardgameatlasR")

#Set your name
my_desc$set("Authors@R", "person('Michael', 'Weisner', email = 'mw2931@columbia.edu', role = c('cre', 'aut'))")

# Remove some author fields
my_desc$del("Maintainer")

# Set the version
my_desc$set_version("0.0.1")

# The title of your package
my_desc$set(Title = "Board Game Atlas API Wrapper")
# The description of your package
my_desc$set(Description = "A basic API
            wrapper for querying the Board Game Atlas (BGA) API,
            found at https://www.boardgameatlas.com/api.
            BGA is an website that catalogs board game information
            and statistics, such as the mechanics, number of players,
            and playtime. It also includes metrics on review scores of the games,
            review text, recent comments of the game on www.reddit.com, and more.
            For full information on the API, please consult https://www.boardgameatlas.com/api/docs.")
# The urls
my_desc$set("URL", "https://github.com/mdweisner/boardgameatlasr")
# Save everyting
my_desc$write(file = "DESCRIPTION")

# If you want to use the MIT licence, code of conduct, and lifecycle badge
use_mit_license(name = "Michael WEISNER")
use_code_of_conduct()
use_lifecycle_badge("Experimental")
use_news_md()

# Get the dependencies
use_package("httr")
use_package("jsonlite")
use_package("curl")
use_package("attempt")
use_package("purrr")
use_package("tidyverse")
use_package("dplyr")

# Clean your description
use_tidy_description()
