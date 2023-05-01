load("impute-mat-raw.rds")

# impute
mat_imputed <- missForest::missForest(
    mat,
    maxiter = 10,
    ntree = 100,
    verbose = TRUE,
    replace = TRUE,
    parallelize = "forests"
    )

save(mat_imputed, file = "mat-imputed.rds")

