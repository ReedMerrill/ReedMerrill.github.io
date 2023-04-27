library(tidyverse)

# Read in the data
raw_dt <- read_csv("anes.csv")

# Years of interest
yoi <- c(1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020)

# filter to years of interest
dt <- raw_dt |> 
    filter(
        VCF0004 %in% yoi,


dims <- dim(dt)
dims
