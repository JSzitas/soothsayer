soothsayer_features_set <- list( entropy,
                                 lumpiness,
                                 nonlinearity,
                                 stability,
                                 box_pierce,
                                 acf_features,
                                 intermittent,
                                 # feasts::feat_acf,
                                 # feasts::feat_intermittent,
                                 spectral,
                                 # feasts::feat_spectral,
                                 arch_stat,
                                 # feasts::stat_arch_lm,
                                 # feasts::feat_stl,
                                 longest_flat_spot,
                                 # feasts::longest_flat_spot,
                                 n_crossing_points,
                                 # feasts::n_crossing_points,
                                 ljung_box,
                                 # feasts::ljung_box,
                                 unitroot_kpss,
                                 # feasts::unitroot_kpss,
                                 feasts::unitroot_ndiffs,
                                 feasts::unitroot_nsdiffs,
                                 unitroot_pp,
                                 feasts::shift_kl_max,
                                 feasts::shift_level_max,
                                 feasts::shift_var_max,
                                 catch22_feat
)

compute_features <- function(x,...) {
  UseMethod("compute_features",x)
}
compute_features.tbl_ts <- function( x, feature_set = soothsayer_features_set, values_from = "value", ... ) {
  # just a lambda function

  safe_set <- purrr::map( feature_set, ~ purrr::possibly(.f = .x, otherwise = NaN)  )

  transformer <- function( .x ) {
    .features <- purrr::map( safe_set,
                             function(.f) .f( .x[[values_from]] ))
    unlist(.features)
  }

  feature_sets <- x %>%
    as.data.frame() %>%
    dplyr::group_by( !!!tsibble::key(x) ) %>%
    dplyr::group_split() %>%
    furrr::future_map( transformer ) %>%
    dplyr::bind_rows()

  return(feature_sets)
  x %>%
    as.data.frame() %>%
    dplyr::select( tsibble::key_vars(x) ) %>%
    dplyr::distinct() %>%
    dplyr::bind_cols(feature_sets)
}
