library(tidyverse)
library(naniar)
rm(list = ls())

# Read in the data
raw_dt <- rio::import("/home/reed/data/anes-cummulative/anes-cdf.sav")
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

##########
# Output for Imputation

impute_mat <- dt |> data.matrix()
save(impute_mat, file = "hpc-jobs/impute-mat-raw.rds")

################################################################################
# Variable Importance Analysis
library(party)

# load in the imputed data
load("/home/reed/Dropbox/01-samara-ra/data-collection/data/input/pid/input/anes/data/cummulative_all-88-20-vars_umputed.rds") #nolint

# get the data in the right format
imputed_dt <- mat_imputed$ximp |> as.data.frame()

rf_fit <- cforest(
    VCF0303 ~ .,
    data = imputed_dt
)

# get the variable importance
rf_full_vi <- varimp(rf_fit)

# sort and manually analyze variable importance
top_vi <- head(sort(desc(rf_full_vi)))
rf_full_vi <- sort(desc(rf_full_vi))
view(rf_full_vi)
view(sort(desc(rf_full_vi$row.names)))


