#' @rdname fast_accuracies
#' @param resid_sqr Squared residuals
#' @param resid Residuals
#' @param scale Scaling factor - typically mean of training data
#' @param scl_prc_resid Scaled percentage residuals
#' @param actual Actuals
#' @param na.rm Whether to remove NA values - defaults to **TRUE**
#' @param demean Whether to demean before calculating autocorrelations - defaults to **TRUE**
#' @export
rmsse <- function(resid_sqr, scale, na.rm = TRUE) {
  sqrt(mean(resid_sqr / scale, na.rm = na.rm))
}
#' @rdname fast_accuracies
#' @export
mase <- function(resid, scale, na.rm = TRUE) {
  mean(abs(resid / scale), na.rm = na.rm)
}
#' @rdname fast_accuracies
#' @export
mpe <- function(scl_prc_resid, na.rm = TRUE) {
  mean(scl_prc_resid, na.rm = na.rm)
}
#' @rdname fast_accuracies
#' @export
mape <- function(scl_prc_resid, na.rm = TRUE) {
  mean(abs(scl_prc_resid), na.rm = na.rm)
}
#' @rdname fast_accuracies
#' @export
maape <- function( scl_prc_resid, na.rm = TRUE ) {
  mean(atan(abs(scl_prc_resid)), na.rm = na.rm)
}
#' @rdname fast_accuracies
#' @export
me <- function(resid, na.rm = TRUE) {
  mean(resid, na.rm = TRUE)
}
#' @rdname fast_accuracies
#' @export
rmse <- function(resid_sqr, na.rm = TRUE) {
  sqrt(mean(resid_sqr, na.rm = na.rm))
}
#' @rdname fast_accuracies
#' @export
mae <- function(resid, na.rm = TRUE) {
  mean(abs(resid), na.rm = na.rm)
}
#' @rdname fast_accuracies
#' @export
acf1 <- function(resid, na.rm = TRUE, demean = TRUE) {
  stats::acf(resid,
    plot = FALSE, lag.max = 2, na.action = na.rm,
    demean = demean
  )$acf[2, 1, 1]
}
#' @rdname fast_accuracies
#' @export
fast_measure <- function(resid, actual, scale = 1, na.rm = TRUE) {
  f <- purrr::map(list(
    rmsse = rmsse,
    mase = mase,
    mpe = mpe,
    mape = mape,
    maape = maape,
    mae = mae,
    rmse = rmse,
    me = me
  ), ~ purrr::possibly(.x, otherwise = NA, quiet = FALSE))

  resid_sqr <- resid^2
  scl_prc_resid <- resid / actual * 100
  data.frame( metric = c("ME","RMSE","MAE","MPE","MAPE", "MAAPE",
                         "RMSSE","MASE"),
              value = c( f$me(resid), f$rmse(resid_sqr), f$mae(resid),
                 f$mpe(scl_prc_resid), f$mape(scl_prc_resid), f$maape( scl_prc_resid ),
                 f$rmsse(resid_sqr, scale), f$mase(resid, scale)
              )
  )
}
#' Fast accuracy measure computation
#'
#' @description A pretty fast way to compute commong accuracy measures
#' @param fable The model fable - we do not work with mables
#' @param test The test data corresponding to forecasts in the fable
#' @param train The original data used in fitting the models - if available,
#' scaled accuracy measures will be computed by default.
#' @param measures Currently only for future extensions, though **fast_measure**
#' does everything that was required so far
#' @param across The grouping variables across which to compute measures - by default
#' just the ".model" column, but you can easily pass something like "h" and it will work.
#' @param ... For future extensions :)
#' @return A data.frame with computed accuracy measures, grouped by whatever was supplied in
#' **across** and the keys present in **test**.
#' @rdname fast_accuracies
#' @export
fast_accuracy <- function(fable, test, train = NULL, measures = fast_measure,
                          across = c(".model", "h"), ...) {
  fable <- dplyr::select(
    as.data.frame(fable),
    -tidyselect::any_of(fabletools::distribution_var(fable))
  )

  full_dt <- dplyr::left_join(fable, test,
    by = c(
      tsibble::key_vars(test),
      tsibble::index_var(test)
    )
  )
  measure <- tsibble::measured_vars( test )
  full_dt <- dplyr::mutate(full_dt, .resid = !!rlang::sym(measure) - .data$.mean)
  full_dt <- dplyr::mutate(full_dt, .actual = !!rlang::sym(measure))

  if (!is.null(train)) {
    group <- tsibble::key_vars(train)
    target <- tsibble::measured_vars(train)
    train <- data.table::data.table(train)
    train <- data.table::setkeyv(train, group)
    train[["val_col"]] <- train[[target]]
    # appease R cmd check
    val_col <- NULL
    scale <- train[, mean(val_col^2), by = group]
    colnames(scale) <- c(group, ".scale")
    scale <- as.data.frame(scale)
  } else {
    group <- tsibble::key_vars(test)
    scale <- data.frame( unique(test[[group]]), NA )
    colnames(scale) <- c(group, "scale")
  }
  full_dt <- dplyr::left_join(full_dt, scale, by = group)
  # cast to dt and compute measures
  full_dt <- data.table::as.data.table(full_dt)
  # again, appease r cmd check
  .resid <- .actual <- .scale <- NULL
  res <- full_dt[, fast_measure(.resid, .actual, .scale), keyby = c( across, group) ]
  # prepare formula for dcast
  cast_fml_rhs <- paste0( c(across, group), collapse = "+" )
  cast_fml <- stats::as.formula( paste0( cast_fml_rhs, "~ metric" ))
  # cast to wide from long
  as.data.frame(data.table::dcast( res, cast_fml, value.var = "value" ))
}

