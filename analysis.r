library(tidyverse)

source("presentations/saguaro-symposium/analysis-prep.r")

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
    disag_state_post,
    baseline_state_output,
    stpreds_state_output,
    file = "presentations/saguaro-symposium/data.RData"
)