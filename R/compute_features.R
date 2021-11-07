soothsayer_feature_set <- list( entropy,
                                lumpiness,
                                nonlinearity,
                                hurst,
                                stability,
                                box_pierce,
                                acf_features,
                                intermittent,
                                spectral,
                                arch_stat,
                                longest_flat_spot,
                                n_crossing_points,
                                ljung_box,
                                unitroot_kpss,
                                feasts::unitroot_ndiffs,
                                feasts::unitroot_nsdiffs,
                                unitroot_pp,
                                feasts::shift_kl_max,
                                feasts::shift_level_max,
                                feasts::shift_var_max,
                                 # heterogeneity,
                                positive,
                                negative,
                                zeros,
                                continuous,
                                count,
                                tslength,
                                period,
                                catch22_feat
)

compute_features <- function(x,...) {
  UseMethod("compute_features",x)
}
compute_features.tbl_ts <- function( x, feature_set = soothsayer_feature_set, values_from = "value", ... ) {
  # just a lambda function

  safe_set <- purrr::map( feature_set, ~ purrr::possibly(.f = .x, otherwise = NaN)  )

  transformer <- function( .x ) {
    .features <- purrr::map( safe_set,
                             function(.f) .f( .x[[values_from]] ))
    unlist(.features)
  }

  features <- x %>%
    as.data.frame() %>%
    dplyr::group_by( !!!tsibble::key(x) ) %>%
    dplyr::group_split()

  keys <- x %>%
    as.data.frame() %>%
    dplyr::select(!!!tsibble::key(x)) %>%
    unlist %>%
    unique()

  features <- suppressWarnings({
    features %>%
    furrr::future_map( transformer ) %>%
    dplyr::bind_rows()
  })

  return(cbind(keys, features))
}
