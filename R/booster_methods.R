#' @importFrom generics generate
#' @export
generate.soothsayer_booster <- function (x, new_data = NULL, h = NULL, specials = NULL, times = 1, bootstrap = FALSE, seed = NULL,
                                 ...)
{
  generated_distrs <- purrr::imap( x[["model_fits"]],
                                   function(model, id) {
                                     dist <- dplyr::bind_cols(
                                       generate(
                                         model[[1]][[1]],
                                         new_data = new_data,
                                         h = h,
                                         times = times,
                                         bootstrap = bootstrap,
                                         seed = seed,
                                         ...),
                                       id = id)
                                     tsibble::as_tibble(dist)
                                   })
  # have to group by first column - which is, happily, the time index variable
  index_var <- as.name(names(generated_distrs[[1]])[1])
  dists <- dplyr::group_by( dplyr::bind_rows(generated_distrs), !!index_var)
  dists <- dplyr::summarise( dists,
                             .sim = sum(.data$.sim),
                             .rep = unique(.data$.rep))
  tsibble::as_tsibble(dists, index = names(dists)[1])
}
#' @importFrom fabletools forecast
#' @export
forecast.soothsayer_booster <- function( object,
                                 new_data = NULL,
                                 specials = NULL,
                                 bootstrap = FALSE,
                                 times = 100,
                                 ... ) {
  fcsts <- purrr::map( object[["model_fits"]],
                       function(.x) {
                         fcst <- fabletools::forecast(
                           .x,
                           new_data = new_data,
                           bootstrap = bootstrap,
                           times = times,
                           ...)
                         get_distribution(fcst)
                       }
  )
  # get forecast means
  fcst_means <- purrr::map(fcsts,  ~mean(.x[[1]]) )
  fcst_means <- purrr::reduce( fcst_means, `+` )
  distributional::dist_degenerate( fcst_means )
}
#' @importFrom stats residuals
#' @export
residuals.soothsayer_booster <- function( object, ... ) {
  object[["residuals"]]
}
#' @importFrom stats fitted
#' @export
fitted.soothsayer_booster <- function( object, ... ) {
  object[["fitted"]]
}
#' @importFrom generics refit
#' @export
refit.soothsayer_booster <- function(x, new_data, specials = NULL, ... ) {
  train_soothsayer_booster( new_data, specials, ... )
}
