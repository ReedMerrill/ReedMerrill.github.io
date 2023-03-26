library(tidyverse)

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

save(psframe_ex, file = "presentations/saguaro-symposium/psframe_ex.RData")
