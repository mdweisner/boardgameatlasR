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
usethis::edit_r_environ()

## ---- evaluate = FALSE---------------------------------------------------
# Boardgame Atlas API Key
BOARDGAMEATLAS_PAT = "YOUR_CLIENT_ID"

## ---- evaluate = FALSE---------------------------------------------------
search_bga(limit = 5, skip = 0, order_by = "popularity")

## ---- evaluate = FALSE---------------------------------------------------
search_bga(limit = 5, skip = 5, order_by = "popularity")

## ---- evaluate = FALSE---------------------------------------------------
games_1_to_100 <- search_bga(limit = 100, skip = 0, order_by = "popularity")
games_101_to_200 <- search_bga(limit = 100, skip = 100, order_by = "popularity")

## ---- evaluate = FALSE---------------------------------------------------
games_1_to_200 <- cbind(games_1_to_100, games_101_to_200)

