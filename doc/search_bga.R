## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(knitr)
library(dplyr)

## ----setup---------------------------------------------------------------
library(boardgameatlasR)

## ---- evaluate = FALSE---------------------------------------------------
# Boardgame Atlas API Key
BOARDGAMEATLAS_PAT = "YOUR_CLIENT_ID"

## ---- evaluate = FALSE---------------------------------------------------
first_5 <- search_bga(limit = 5, skip = 0, order_by = "popularity")

## ---- evaluate = FALSE---------------------------------------------------
second_5 <-  search_bga(limit = 5, skip = 5, order_by = "popularity")

## ---- evaluate = FALSE---------------------------------------------------
first_100 <- search_bga(limit = 100, skip = 0, order_by = "popularity")
second_100 <- search_bga(limit = 100, skip = 100, order_by = "popularity")

## ---- evaluate = FALSE---------------------------------------------------
first_200 <- cbind(first_100, second_100)

## ------------------------------------------------------------------------
id_search <- search_bga(ids="SVQRjsXrhj,OIXt3DmJU0")

## ------------------------------------------------------------------------
publisher_search <- search_bga(publisher = "Catan Studio")

## ------------------------------------------------------------------------
dice_rolling_games <- search_bga(mechanics="R0bGq4cAl4")

## ------------------------------------------------------------------------
eurogame_games <- search_bga(categories="h8wfZG0j3I", limit = 10)

