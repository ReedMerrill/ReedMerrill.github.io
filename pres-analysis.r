library(tidyverse)
rm(list = ls())

################################################################################
# Example PS Frame

poststrat_df <- read_csv("data-collection/data/input/pid/output/poststratdf-12.csv")

psAL <- poststrat_df |> filter(state == "Alabama")
psAL$male <- psAL$male + .5
pshead <- psAL |> head(3)
ps_sample <- psAL |> sample_n(3)
ps_elipses <- tibble(
    year = "...",
    state = "...",
    eth = "...",
    male = "...",
    age = "...",
    educ = "...",
    n = "..."
)
pshead <- pshead |>
    transmute(
        year = as.character(year),
        state = as.character(state),
        eth = as.character(eth),
        male = as.character(male),
        age = as.character(age),
        educ = as.character(educ),
        n = as.character(n)
    )
ps_sample <- ps_sample |>
    transmute(
        year = as.character(year),
        state = as.character(state),
        eth = as.character(eth),
        male = as.character(male),
        age = as.character(age),
        educ = as.character(educ),
        n = as.character(n)
    )
psframe_ex <- bind_rows(pshead, ps_elipses, ps_sample)
psframe_ex <- psframe_ex |> select(-year)

################################################################################
# Prep for Poststratify

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
# Poststratify

# load training and testing data
load("data-collection/data/input/pid/input/ces/train-test-ces-12.RData") #nolint

##########
# Disaggregated

# recode
train$dem_lean <- NA
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
        pct_dem_post = mean(pct_dem_post),
        pct_rep_post = mean(pct_rep_post),
        pct_ind_post = mean(pct_ind_post)
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

################################################################################
# Results



################################################################################
# Validate

##########
# plots

compare_plot(
    estimates = disag_state_post,
    truth = test,
    estimates_col = "pct_dem_post",
    truth_col = "dem"
    )

##########
# Stats

compare_stats(
    estimates = disag12,
    truth = truth12,
    estimates_col = "pct_rep_post",
    truth_col = "rep",
    fit_name = "Disagg. 12 (Rep.)"
    )

####################
# Summarize Disag. Eval.

disag_stats <- bind_rows(
    stat_tbl1,
    stat_tbl2,
    stat_tbl3
    )

disag_stats$model <- disag_stats$model |> factor()
disag_stats$x <- 1:nrow(disag_stats)

disag_corr_plot <- ggplot(data = disag_stats, mapping = aes(x = x, y = corr)) +
    geom_point() +
    geom_line() +
    theme_minimal() +
    scale_x_discrete(limits = disag_stats$model) +
    labs(x = "Model", y = "Correlation to Truth")
disag_corr_plot

disag_mae_plot <- ggplot(data = disag_stats, mapping = aes(x = x, y = mae)) +
    geom_point() +
    geom_line() +
    theme_minimal() +
    scale_x_discrete(limits = disag_stats$model) +
    labs(x = "Model", y = "Mean Absolute Error")
disag_mae_plot

disag_mean_performance <- disag_stats |>
    summarize(
        corr = mean(corr),
        mae = mean(mae),
        mae_sd = mean(mae_sd),
        mae_range = mean(mae_range)
        )
disag_mean_performance

disag_mean_performance_year <- disag_stats |>
    group_by(year) |>
    summarize(
        corr = mean(corr),
        mae = mean(mae),
        mae_sd = mean(mae_sd),
        mae_range = mean(mae_range)
        )
disag_mean_performance_year$model <- "Disagg."
disag_mean_performance_year$model <- disag_mean_performance_year$model |> factor()

################################################################################
# Save needed output

save(
    psframe_ex,
    disag_state_post,
    baseline_state_output,
    stpreds_state_output,

    file = "presentations/saguaro-symposium/data.RData"
)
