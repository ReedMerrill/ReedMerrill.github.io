library(tidyverse)
rm(list = ls())

################################################################################
# Prep for Poststratify

poststrat_df <- read_csv("data-collection/data/input/pid/output/poststratdf-12.csv") #nolint

# load models
load("data-collection/models/brms-categorical-testing/2012-ces-models-specification-testing.RData") #nolint

# source poststratification functions
source("data-collection/scripts/pid/modeling-functions.r")

# add state-level predictors to the poststratification frame

# add state abbreviation
fips <- read.csv("data-collection/data/input/helpers/us-state-ansi-fips.csv")
fips <- fips |> select(-st, abbr = stusps)
fips$abbr <- fips$abbr |> str_trim()
poststrat_df <- poststrat_df |>
    left_join(
        fips,
        by = c("state" = "stname")
    )
# add state-level predictors, except region
state_df <- rio::import("data-collection/data/final.csv")
poststrat_df <- poststrat_df |>
    left_join(
        state_df |>
            select(
                state,
                year,
                rep_vote_share,
                pct_black,
                pct_urban_pop
            ),
        by = c("state", "year")
    )
# add region
region_df <- rio::import("data-collection/data/input/state-region.csv")
region_df <- region_df |> filter(state != "AK", state != "HI")
region_df <- region_df  |> rename(abbr = state)
poststrat_df <- poststrat_df |>
    left_join(
        region_df,
        by = "abbr"
    )

################################################################################
# Prep test data

# load training and testing data
load("data-collection/data/input/pid/input/ces/train-test-ces-12.RData") #nolint

# recode
test$dem_lean <- NA
test$dem_lean[test$pid_lean == 1] <- 1
test$dem_lean[test$pid_lean != 1] <- 0
test$rep_lean <- NA
test$rep_lean[test$pid_lean == 2] <- 1
test$rep_lean[test$pid_lean != 2] <- 0
test$ind_lean <- NA
test$ind_lean[test$pid_lean == 3] <- 1
test$ind_lean[test$pid_lean != 3] <- 0
disag_state_post <- test |>
    group_by(state, eth, male, educ, age) |>
    summarize(
        pct_dem = sum(dem_lean) / n(),
        pct_rep = sum(rep_lean) / n(),
        pct_ind = sum(ind_lean) / n()
    )
# join poststrat. table to survey summary of cells
poststrat_df_baseline <- poststrat_df |>
    select(
        eth,
        male,
        educ,
        age,
        state,
        n
    )
disag_state_post <- disag_state_post |>
    left_join(
        poststrat_df_baseline,
        by = c("eth", "male", "educ", "age", "state")
    )
# poststratify
disag_state_post <- disag_state_post |>
    group_by(state) |>
    summarize(
        pct_dem_post = sum(n * pct_dem) / sum(n),
        pct_rep_post = sum(n * pct_rep) / sum(n),
        pct_ind_post = sum(n * pct_ind) / sum(n)
    )
disag_state_post <- disag_state_post |>
    group_by(state) |>
    summarize(
        dem_est = mean(pct_dem_post),
        rep_est = mean(pct_rep_post),
        ind_est = mean(pct_ind_post)
    )

################################################################################
# Poststratify

##########
# Disaggregated

# recode
test$dem_lean <- NA
train$dem_lean[train$pid_lean == 1] <- 1
train$dem_lean[train$pid_lean != 1] <- 0
train$rep_lean <- NA
train$rep_lean[train$pid_lean == 2] <- 1
train$rep_lean[train$pid_lean != 2] <- 0
train$ind_lean <- NA
train$ind_lean[train$pid_lean == 3] <- 1
train$ind_lean[train$pid_lean != 3] <- 0
disag_state_post <- train |>
    group_by(state, eth, male, educ, age) |>
    summarize(
        pct_dem = sum(dem_lean) / n(),
        pct_rep = sum(rep_lean) / n(),
        pct_ind = sum(ind_lean) / n()
    )
# join poststrat. table to survey summary of cells
poststrat_df_baseline <- poststrat_df |>
    select(
        eth,
        male,
        educ,
        age,
        state,
        n
    )
disag_state_post <- disag_state_post |>
    left_join(
        poststrat_df_baseline,
        by = c("eth", "male", "educ", "age", "state")
    )
# poststratify
disag_state_post <- disag_state_post |>
    group_by(state) |>
    summarize(
        pct_dem_post = sum(n * pct_dem) / sum(n),
        pct_rep_post = sum(n * pct_rep) / sum(n),
        pct_ind_post = sum(n * pct_ind) / sum(n)
    )
disag_state_post <- disag_state_post |>
    group_by(state) |>
    summarize(
        dem_est = mean(pct_dem_post),
        rep_est = mean(pct_rep_post),
        ind_est = mean(pct_ind_post)
    )

##########
# Baseline

baseline_nat_output <- estimate_national_pid(
    baseline,
    poststrat_df
)
baseline_state_output <- estimate_state_pid(
    baseline,
    poststrat_df
)

##########
# stpreds (region and rep_vote_share)

stpreds_nat_output <- estimate_national_pid(
    stpreds,
    poststrat_df
)
stpreds_state_output <- estimate_state_pid(
    stpreds,
    poststrat_df
)

##########
# stpreds 3 (region, rep_vote_share, pct_black, pct_urban_pop)

stpreds3_nat_output <- estimate_national_pid(
    stpreds3,
    poststrat_df
)
stpreds3_state_output <- estimate_state_pid(
    stpreds3,
    poststrat_df
)

################################################################################
# Results

################################################################################
# Validate

# prep data

# Disaggregated data
disag_state_dem <- disag_state_post |>
    select(
        state,
        mean_pid = dem_est
    )
disag_state_rep <- disag_state_post |>
    select(
        state,
        mean_pid = rep_est
    )
disag_state_ind <- disag_state_post |>
    select(
        state,
        mean_pid = ind_est
    )
# Baseline data 
baseline_state_dem <- baseline_state_output |>
    filter(
        id == "dem"
    )
baseline_state_rep <- baseline_state_output |>
    filter(
        id == "rep"
    )
baseline_state_ind <- baseline_state_output |>
    filter(
        id == "ind"
    )
# Stpreds data 
stpreds_state_dem <- stpreds_state_output |>
    filter(
        id == "dem"
    )
stpreds_state_rep <- stpreds_state_output |>
    filter(
        id == "rep"
    )
stpreds_state_ind <- stpreds_state_output |>
    filter(
        id == "ind"
    )
# stpreds3 data
stpreds3_state_dem <- stpreds3_state_output |>
    filter(
        id == "dem"
    )
stpreds3_state_rep <- stpreds3_state_output |>
    filter(
        id == "rep"
    )
stpreds3_state_ind <- stpreds3_state_output |>
    filter(
        id == "ind"
    )
