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

dt <- raw_dt |>
    filter(
        VCF0004 %in% yoi, # keep years of interest
        VCF0016 == 0 # remove panel responnses
    ) |>
    select(
        -c(VCF0009x:VCF0018b), # remove weights and admin. variables
        -c(VCF0070a:VCF0072b), # remove interviewer characteristics/notes
        -c(VCF0301:VCF0305)    # remove partisanship variables (the TARGET)
    )

##########
# missingness

# missingness by variable
missing_by_var <- miss_var_summary(dt)

# completely missing variables
na_vars <- missing_by_var |>
    filter(
        pct_miss == 100
    ) |>
    pull(
        variable
    )
n_na_vars <- length(na_vars)

# remove completely missing variables
dt <- dt |>
    select(
        -all_of(na_vars)
    )

# remove all variables that have all missing values for any year of interest
dim(dt)
train <- dt |>
    group_by(VCF0004) |>
    filter(
        across(
            .cols = everything(),
            .fns = ~ !all(is.na(.))
        )
    )
dim(train)
