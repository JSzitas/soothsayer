source("R/features.R")
source("R/compute_features.R")

load( "data/M_all.rda" )

future::plan("multisession")
feature_df <- compute_features(M_all)

# feature_df <- feature_df %>%
#   dplyr::select_if( ~!all(is.na(.x)) ) %>%
#   dplyr::select_if( ~!all( .x == .x[1] ))

qs::qsave(feature_df, file = "data/M_all_features.qs")
