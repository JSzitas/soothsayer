#' Compute features for a tsibble
#'
#' @param x A tsibble.
#' @param ... Additional arguments.
#' @return A data.frame wth computed time series features, with ncol features and
#' nrow individual time series.
#' @export
compute_features <- function(x,...) {
  UseMethod("compute_features",x)
}
#' @export
compute_features.tbl_ts <- function( x, feature_set = soothsayer_feature_set, values_from = NULL,... ) {
  # get the name of the target variable
  if( is.null( values_from ) ) {
    values_from <- tsibble::measured_vars(x)
    if( length(values_from) != 1 ) {
      rlang::abort(
        paste0( "Cannot identify a single column to use for feature calculation ",
                "- please use the **values_from** argument of this function." ))
    }
  }
  transformer <- generate_feature_transformer( feature_set, values_from )
  # if we have an unkeyed table (as is case from within the soothsayer object)
  # we would crash here on the keyed version - thus we add a check.
  if( length(tsibble::key_vars(x)) != 0) {
    features <- as.data.frame(x)
    features <- dplyr::group_by(features,  !!!tsibble::key(x) )
    features <- as.list(dplyr::group_split(features))

    keys <- as.data.frame(x)
    keys <- dplyr::select(keys, !!!tsibble::key(x))
    keys <- dplyr::distinct(keys)
    # return(list(features, transformer))
    features <- if(requireNamespace("furrr",quietly = TRUE)) {
      furrr::future_map(features, transformer)
    }else {
      purrr::map(features, transformer)
    }

    features <- suppressWarnings({
      dplyr::bind_rows(features)
    })
    # the early return saves us an otherwise unnecessary else clause
    return(cbind(keys, features))
  }
  # for a single unkeyed dataset, we can just call the transformer directly
  features <- transformer(as.data.frame(x))
  feature_names <- names(features)
  # a bit of coercion magic so that we get the correctly formatted, correctly
  # named table (ie you cast the result to a matrix and THEN data.frame, since otherwise
  # you would get a single column data.frame (which we do not want))
  features <- as.data.frame(matrix( transformer(as.data.frame(x)), nrow = 1))
  colnames(features) <- feature_names
  return(features)
}
# we could probably have this within compute_features, but for testability
# and cleanliness, we already have it separated out
generate_feature_transformer <- function( feature_set, values_from ) {
  # just a wrapper so that the functions return NaN rather than a random error
  safe_set <- purrr::map( feature_set, ~ purrr::possibly(.f = .x, otherwise = NaN)  )
  # then we generate a closure which specifies what we will be calculating -
  # this also enables as to easily use future_map
  transformer <- function( .x ) {
    .features <- purrr::map( safe_set,
                             function(.f) .f( .x[[values_from]] ))
    unlist(.features)
  }
}
