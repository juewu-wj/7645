## -----------------------------------------------------------------------------
##
##' [PROJ: EDH7645]
##' [FILE: Assignment 02]
##' [INIT: 2025]
##' [AUTH: Jue Wu]
##
## -----------------------------------------------------------------------------

setwd(this.path::here())

## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)

## ---------------------------
##' [Input]
## ---------------------------

data <- read_csv("data/hsls-small.csv")

## ---------------------------
##' [Prep]
## ---------------------------

## ---------------------------
##' [Analysis]
## ---------------------------

# 1a
# quickly look at the distribution of x1txmtscor

data |> 
  pull(x1txmtscor) |> 
  psych::describe()

data |> 
  select(x1txmtscor) |>
  summary()

data |> 
  # check distribution of x1txmtscor
  ggplot(aes(x = x1txmtscor)) +
  geom_histogram() 

# get average
data |> 
  filter(x1txmtscor != -8) |> 
  summarize(mean_math = mean(x1txmtscor))
  
# 1b
# calculate mean of x1txmtscor by x1sex

# check distribution of x1sex first
data |> 
  count(x1sex)

data |> 
  filter(x1txmtscor != -8,
         x1sex != -9) |> 
  group_by(x1sex) |> 
  summarize(mean_math = mean(x1txmtscor))

# 2a
# check distribution of x1poverty185 and x1famincome first
data |> count(x1poverty185)
data |> count(x1famincome)

data |> 
  filter(x1poverty185 == 1,
         !x1famincome %in% c(-8, -9)) |> 
  summarize(median = median(x1famincome))

# 3a
data |> count(x4hscompstat)

data |> 
  filter(x4hscompstat %in% c(1, 2)) |>
  group_by(x4hscompstat) |>
  summarize(n = n()) |>
  mutate(percentage = n / sum(n) * 100)

data |> 
  filter(x4hscompstat %in% c(1, 2)) |>
  count(x4hscompstat) |> 
  mutate(percentage = n / sum(n) * 100)

data |> 
  filter(x4hscompstat %in% c(1, 2)) |>
  summarize(percentage = mean(x4hscompstat == 2) * 100)

# 3b
data |> 
  filter(x4hscompstat %in% c(1, 2)) |>
  group_by(x1region, x4hscompstat) |>
  summarize(n = n()) |>
  mutate(percentage = n / sum(n) * 100) |> 
  filter(x4hscompstat == 2)

data |>
  filter(x4hscompstat %in% c(1,2)) |>
  group_by(x1region) |>
  count(x4hscompstat) |>
  mutate(perc = n / sum(n) * 100) |>
  filter(x4hscompstat == 2)

# 4a
data |> count(x4evratndclg)

data |> 
  filter(x4evratndclg != -8) |> 
  group_by(x4evratndclg) |>
  summarize(n = n()) |>
  mutate(percentage = n / sum(n) * 100)

data |>
  filter(x4evratndclg != -8) |>
  count(x4evratndclg) |> 
  mutate(percentage = n / sum(n) * 100)

data |>
  filter(x4evratndclg != -8) |>
  summarize(percentage = mean(x4evratndclg == 1) * 100)

# 4b
data |> count(x1famincome)
data |> count(x1region)

data |> 
  filter(x4evratndclg != -8,
         !x1famincome %in% c(-8, -9)) |>
  mutate(income_35k = ifelse(x1famincome %in% c(1,2), 0, 1)) |> 
  group_by(income_35k, x1region, x4evratndclg) |>
  summarize(n = n()) |> 
  mutate(percentage = n / sum(n) * 100) |> 
  filter(x4evratndclg == 1)


data |> 
  filter(x4evratndclg != -8,
         !x1famincome %in% c(-8, -9)) |>
  mutate(income_35k = ifelse(x1famincome %in% c(1,2), 0, 1)) |> 
  group_by(income_35k, x1region) |>
  count(x4evratndclg) |> 
  mutate(percentage = n / sum(n) * 100) |> 
  filter(x4evratndclg == 1)

data |> 
  filter(x4evratndclg != -8,
         !x1famincome %in% c(-8, -9)) |>
  mutate(income_35k = ifelse(x1famincome %in% c(1,2), 0, 1)) |> 
  group_by(income_35k, x1region) |> 
  summarize(percentage = mean(x4evratndclg == 1) * 100)

## ---------------------------
##' [Output]
## ---------------------------

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------
