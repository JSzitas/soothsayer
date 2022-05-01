#' Combination functions which return combination weights
#'
#' @description Functions which calculate ensemble model combination weights
#' @param .models The fitted models - this is a named or unnamed list of fitted fable compatible models.
#' @param prior_weights Prior weights to use for combinations - see details.
#' @param metric The metric to use when using greedy stacking (defaults to rmse).
#' @param oracle_weights Weights returned by an oracle (if available).
#' @param ... Additional parameters (currently not implemented).
#' @return A vector of length .models containing the individual weights
#' @details Prior weights can be passed to combiner_custom and they will be the
#' only weights computed by that combiner. Other combiners take the average of prior_weights
#' and whatever weights they output.
#' @export
#' @rdname combiners
combiner_greedy_stacking <- function(.models, prior_weights = NULL, metric = rmse, oracle_weights = NULL, ... ) {

  Z <- purrr::map(.models, ~ fitted(.x[[1]]) )
  Z <- purrr::map(Z, as.data.frame)
  Z <- purrr::map(Z, ~ .x[[".fitted"]] )
  Z <- as.matrix(dplyr::bind_cols(Z))

  y <- .models[[1]][[1]][["data"]]
  measured_var <- tsibble::measured_vars(y)
  y <- y[[measured_var]]

  greedy_weights <- greedy_stacking( y, Z, metric = metric,... )
  colMeans( rbind( greedy_weights, prior_weights))
}
#' @rdname combiners
#' @export
combiner_equal <- function(.models, prior_weights = NULL, oracle_weights = NULL, ... ) {
  avg_weights <- rep(1, length(.models))/length(.models)
  colMeans( rbind( avg_weights, prior_weights))
}
#' @rdname combiners
#' @export
combiner_custom <- function(.models, prior_weights = NULL, ... ) {
  if(all(is.null(prior_weights))) {
    return(combiner_equal(.models))
  }
  prior_weights/sum(prior_weights)
}
#' @rdname combiners
#' @export
combiner_oracle <- function(.models, prior_weights = NULL, oracle_weights = NULL, ... ) {
  oracle_weights <- oracle_weights/sum(oracle_weights)
  prior_weights <- prior_weights/sum(prior_weights)
  colMeans( rbind( oracle_weights, prior_weights))
}
#' @rdname combiners
#' @export
combiner_lm <- function(.models, prior_weights = NULL, oracle_weights = NULL, ... ) {

  Z <- purrr::map(.models, ~ fitted(.x[[1]]) )
  Z <- purrr::map(Z, as.data.frame)
  Z <- purrr::map(Z, ~ .x[[".fitted"]] )
  Z <- as.matrix(dplyr::bind_cols(Z))

  y <- .models[[1]][[1]][["data"]]
  measured_var <- tsibble::measured_vars(y)
  y <- y[[measured_var]]

  weights <- stats::coef(stats::lm(y ~ Z-1))
  weights[is.na(weights)] <- 0
  weights <- weights/sum(weights)
  names(weights) <- NULL
  colMeans( rbind( weights, prior_weights))
}
#' @rdname combiners
# See 'Zou, Yang (2004) "Combining time series models for forecasting"'
# also at https://github.com/ykang/kllab-seminars/blob/master/2019_S2/20191018/
# Zou%20and%20Yang%202004%20-%20Combining%20time%20series%20models%20for%20forecasting.pdf
combiner_after <- function(.models, prior_weights = NULL, ... ) {
  Z <- purrr::map(.models, ~ fitted(.x[[1]]) )
  Z <- purrr::map(Z, as.data.frame)
  Z <- purrr::map(Z, ~ .x[[".fitted"]] )
  Z <- as.matrix(dplyr::bind_cols(Z))

  y <- .models[[1]][[1]][["data"]]
  measured_var <- tsibble::measured_vars(y)
  y <- y[[measured_var]]

}

AFTER <- function(x )  {
  x
}

roll_variance <- function(x) {
  x
}
