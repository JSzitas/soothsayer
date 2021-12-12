#' @rdname combiners
#' Get combinations weights for model combinatiions
#'
#' @desccription Combination functions which return combination weights
#' @param .models The fitted models - this is a named or unnamed list of fitted fable compatible models.
#' @param prior_weights Prior weights to use for combinations - see details.
#' @param metric The metric to use when using greedy stacking (defaults to rmse).
#' @param ... Additional parameters (currently not implemented).
#' @return A vector of length .models containing the individual weights
#' @details Prior weights can be passed to combiner_custom and they will be the
#' only weights computed by that combiner. Other combiners take the average of prior_weights
#' and whatever weights they output.
#' @export
combiner_greedy_stacking <- function(.models, prior_weights = NULL, metric = rmse, ... ) {

  Z <- .models %>%
    purrr::map(~ fitted(.x[[1]]) ) %>%
    purrr::map(as.data.frame) %>%
    purrr::map( ~ .x[[".fitted"]] ) %>%
    dplyr::bind_cols() %>%
    as.matrix
  y <- .models[[1]][[1]][["data"]]
  measured_var <- tsibble::measured_vars(y)
  y <- y[[measured_var]]

  greedy_weights <- greedy_stacking( y, Z, metric = metric,... )

  colMeans( rbind( greedy_weights, prior_weights))
}
#' @rdname combiners
#' @export
combiner_mean <- function(.models, prior_weights = NULL, ... ) {

  avg_weights <- rep(1, length(.models))/length(.models)
  colMeans( rbind( avg_weights, prior_weights))
}
#' @rdname combiners
#' @export
combiner_custom <- function(.models, prior_weights = NULL, ... ) {
  prior_weights/sum(prior_weights)
}
#' @rdname combiners
#' @export
combiner_lm <- function(.models, prior_weights = NULL, ... ) {

  X <- .models %>%
    purrr::map(~ fitted(.x[[1]]) ) %>%
    purrr::map(as.data.frame) %>%
    purrr::map( ~ .x[[".fitted"]] ) %>%
    dplyr::bind_cols() %>%
    as.matrix
  y <- .models[[1]][[1]][["data"]]
  measured_var <- tsibble::measured_vars(y)
  y <- y[[measured_var]]

  weights <- stats::coef(stats::lm(y ~ X-1))
  weights <- weights/sum(weights)
  names(weights) <- NULL
  colMeans( rbind( weights, prior_weights))
}


