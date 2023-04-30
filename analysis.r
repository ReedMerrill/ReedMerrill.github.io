library(tidyverse)
library(naniar)
rm(list = ls())

# Read in the data
load("/home/reed/data/anes-cummulative/anes-cdf.rds")
# This is based on the SPSS format official version. It has missing
    # values pre-coded as NA

################################################################################
# cleaning

##########
# initial cleaning

# Years of interest
yoi <- c(1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020)

raw_dt$VCF0016[is.na(raw_dt$VCF0016) & raw_dt$VCF0004 == 2020] <- 0
dt <- raw_dt |>
    filter(
        VCF0004 %in% yoi, # keep years of interest
        VCF0016 == 0 # remove panel responses
    ) |>
    select(
        -VCF0016,
        -c(VCF0009x:VCF0018b), # remove weights and admin. variables
        -c(VCF0070a:VCF0072b), # remove interviewer characteristics/notes
        -c(VCF0301, VCF0302, VCF0305) # remove other partisanship variables (collinear with the TARGET)
    )

##########
# missingness

# missingness by variable
missing_by_var <- dt |> miss_var_summary()

# completely missing variables
na_vars <- missing_by_var |>
    filter(
        pct_miss == 100
    ) |>
    pull(
        variable
    ) |>
    unique()

n_na_vars <- na_vars |> length()

# remove completely missing variables
dt <- dt |>
    select(
        -all_of(na_vars)
    )

# remove all variables that have all missing values within any year of interest

miss_by_var_by_year <- dt |>
    group_by(
        VCF0004
        ) |>
    miss_var_summary() |>
    distinct()

# get missingness counts for every year
n_miss_by_var_by_year <- miss_by_var_by_year |>
    group_by(
        VCF0004
        ) |>
    summarise(
        n_miss = sum(pct_miss == 100),
    )

# explore missingness
miss_by_var_by_year |> view()
n_miss_by_var_by_year |> view()
hist(miss_by_var_by_year$pct_miss)
hist(missing_by_var$pct_miss)

# remove variables that have greater than 0.2 missingness in any year
bad_vars <- miss_by_var_by_year |>
    filter(
        pct_miss > 20
    ) |>
    pull(
        variable
    ) |>
    unique()

dt <- dt |>
    select(
        -all_of(bad_vars)
    )

final_vars <- dt |> names()

################################################################################
# Imputation prep

impute_mat <- dt |> data.matrix()
save(impute_mat, file = "hpc-jobs/impute-mat-raw.rds")
