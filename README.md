
<!-- README.md is generated from README.Rmd. Please edit that file -->

# boardgameatlasR

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of boardgameatlasR is to provide a basic API wrapper for
querying the Board Game Atlas (BGA) search API, found at
<https://www.boardgameatlas.com/api>.

BGA is an website that catalogs board game information and statistics,
such as the mechanics, number of players, and playtime. BGA also
includes metrics on review scores of the games, review text, recent
comments of the game on www.reddit.com, and more. For full information
on the API, please consult <https://www.boardgameatlas.com/api/docs>.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mdweisner/boardgameatlasr")
```

## Example

This will be a basic example once I functionally install the package is
a basic example:

``` r
library(dplyr)
library(kableExtra)
library(boardgameatlasR)
search_bga(limit=10, name = "Catan", name_fuzzy = TRUE, order_by = "popularity", ascending = FALSE, client_id_pat = Sys.getenv('BOARDGAMEATLAS_PAT')) %>%
  select(id, name, year_published) %>%
  kable("html") 
```

<table>

<thead>

<tr>

<th style="text-align:left;">

id

</th>

<th style="text-align:left;">

name

</th>

<th style="text-align:right;">

year\_published

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

OIXt3DmJU0

</td>

<td style="text-align:left;">

Catan

</td>

<td style="text-align:right;">

1995

</td>

</tr>

<tr>

<td style="text-align:left;">

ULWQvi77f5

</td>

<td style="text-align:left;">

Catan: Junior

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

iSVnYdrsZQ

</td>

<td style="text-align:left;">

Rivals for Catan

</td>

<td style="text-align:right;">

2010

</td>

</tr>

<tr>

<td style="text-align:left;">

b7EIdXzESo

</td>

<td style="text-align:left;">

Starfarers of Catan

</td>

<td style="text-align:right;">

2000

</td>

</tr>

<tr>

<td style="text-align:left;">

3f3PmrkzV8

</td>

<td style="text-align:left;">

Catan Board Game - Gallery Edition

</td>

<td style="text-align:right;">

2008

</td>

</tr>

<tr>

<td style="text-align:left;">

l2i97dfuvD

</td>

<td style="text-align:left;">

Star Trek: Catan

</td>

<td style="text-align:right;">

2012

</td>

</tr>

<tr>

<td style="text-align:left;">

E5TYKwLTf0

</td>

<td style="text-align:left;">

Catan: Cities and Knights Expansion

</td>

<td style="text-align:right;">

1998

</td>

</tr>

<tr>

<td style="text-align:left;">

lb4nZbOLOD

</td>

<td style="text-align:left;">

Catan: Legend of the Sea Robbers Expansion

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

qiJzLWCvPB

</td>

<td style="text-align:left;">

Catan: Explorers and Pirates Expansion

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

fz1hSbgUN2

</td>

<td style="text-align:left;">

A Game of Thrones Catan: Brotherhood of the Watch

</td>

<td style="text-align:right;">

2017

</td>

</tr>

</tbody>

</table>

# Before Starting

Every API call requires an client\_id in the URL. In order to query BGA
you will need to follow these steps to get a client\_id and create an
app:

1.  Log in or Sign up for a Board Game Atlas account.
2.  Create an app.
3.  Add client\_id={your\_client\_id} to each of your requests.

See the “getting started” page at
<https://www.boardgameatlas.com/api/docs>.

Once you have your client, the boardgameatlasR app requires you to save
your client\_id into your R environment as BOARDGAMEATLAS\_PAT. You can
edit this with Sys.setenv(BOARDGAMEATLAS\_PAT = “YOUR\_CLIENT\_ID”) or
through something like usethis::edit\_r\_environ()to save it to your
environment. This way you reduce the risk exposure of your key.

Please note: this only covers the [search of the BGA
API](https://www.boardgameatlas.com/api/docs/search), and more query
features will hopefully be added in the future.

# Acknowledgments

This code was built as part of Thomas Brambor’s Fall 2019 Modern Data
Structures course in the Quantitative Methods of Social Science program
at Columbia University, which covered many topics used in this project.

Additionally I’d like to acknowledge Colin Fay, who created a blogpost
about building API wrappers
(<https://colinfay.me/build-api-wrapper-package-r/>). Combining what I
learned in my course with the basic steps and framework Colin described
was invaluable for effectively moving through the process and building
in some sensible components and tests.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.
