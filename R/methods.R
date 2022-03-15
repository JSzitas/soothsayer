#' @importFrom generics generate
#' @export
generate.soothsayer <- function (x, new_data = NULL, h = NULL, specials = NULL, times = 1, bootstrap = FALSE, seed = NULL,
                                 ...)
{
  weights <- x[["model_weights"]]
  names(weights) <- names(x[["model_fits"]])
  generated_distrs <- purrr::imap( x[["model_fits"]],
                                   function(model, name) {
                                     dplyr::bind_cols(
                                       generate(
                                         model[[1]],
                                         new_data = new_data,
                                         h = h,
                                         times = times,
                                         bootstrap = bootstrap,
                                         seed = seed,
                                         ...),
                                       model = name)
                                   })
  dists <- purrr::imap( generated_distrs, function(dist, name) {
    dists <- dplyr::mutate(dist, .sim = .data$.sim * weights[name])
    tsibble::as_tibble(dists)
  })
  dists <- dplyr::group_by( dplyr::bind_rows(dists), .data$Month)
  dists <- dplyr::summarise( dists,
                             .sim = sum(.data$.sim),
                             .rep = unique(.data$.rep))
  tsibble::as_tsibble(dists, index = "Month")
}
# to be fair, this is a method over a fable, but I do not want to write a generic for it
get_distribution <- function(x) {
  distr <- purrr::map_lgl( x, distributional::is_distribution )
  distr <- names(distr)[ which(distr) ]
  c(x[,distr])
}

#' @importFrom fabletools forecast
#' @export
forecast.soothsayer <- function( object,
                                 new_data = NULL,
                                 specials = NULL,
                                 bootstrap = FALSE,
                                 times = 100,
                                 ... ) {
  fcsts <- purrr::map( object[["model_fits"]],
              function(.x) {
                fcst <- fabletools::forecast(
                  .x[[1]],
                  new_data = new_data,
                  bootstrap = bootstrap,
                  times = times,
                  ...)
                  get_distribution(fcst)
                }
  )
  # if we only have one model, dont worry about anything else :)
  if( length(object[["model_fits"]]) == 1 ) {
    return(fcsts[[1]][[1]])
  }
  # otherwise, get weights
  weights <- object[["model_weights"]]
  # get forecast means
  fcst_means <- purrr::map(fcsts,  ~mean(.x[[1]]) )
  fcst_means <- as.matrix(dplyr::bind_cols(fcst_means))
  # and compute the final mean
  fcst_means <- c( fcst_means %*% weights )
  # also get forecast variances

  # I am quite unsure at this point that computing the variances is very sensible
  # for non-normal distributions its bad, but even for normal distributions... there is
  # no guarantee that the end result is normal. if its bimodal... then we are a bit
  # screwed either way. lets not do that.
  distributional::dist_degenerate( fcst_means )
}
#' @importFrom stats residuals
#' @export
residuals.soothsayer <- function( object, ... ) {
  object[["residuals"]]
}
#' @importFrom stats fitted
#' @export
fitted.soothsayer <- function( object, ... ) {
  object[["fitted"]]
}
#' @importFrom generics tidy
#' @export
tidy.soothsayer <- function( x, ... ) {

  models <- purrr::map_chr( x[["model_fits"]], ~ class(.x[[1]][["fit"]]))
  models <- c(models, "all")
  model_weights <- x[["model_weights"]]
  model_weights <- c(model_weights,1)
  residual_mean <- purrr::map_dbl( x[["model_fits"]],
                                   ~ mean( residuals(.x[[1]][["fit"]]),
                                           na.rm = TRUE)
                                   )
  residual_mean <- c( residual_mean, mean( x[["residuals"]], na.rm = TRUE) )
  residual_rmse <- purrr::map_dbl( x[["models"]],
                              ~ sqrt( mean( residuals(.x[[1]][["fit"]])^2,
                                            na.rm = TRUE ))
                              )
  residual_rmse <- c( residual_rmse,
                      sqrt( mean( x[["residuals"]]^2, na.rm = TRUE))
                      )

  tibble::tibble( models = models,
                  weights = model_weights,
                  avg_residual = residual_mean,
                  rmse_residual = residual_rmse )
}
#' @importFrom rlang .data
#' @importFrom generics glance
#' @export
glance.soothsayer <- function(x, ...) {
  x <- tidy(x)
  fit_rmse <- dplyr::filter( x, .data$models == "all")[["rmse_residual"]]
  x <- dplyr::filter( x, .data$models != "all" )
  total_models <- nrow(x)
  active_models <- sum(x[["model_weights"]] > 0)
  max_weight <- max(x[["model_weights"]])

  tibble::tibble(
    total_models = total_models,
    active_models = active_models,
    max_weight = max_weight,
    fit_rmse = fit_rmse,
    model_redundancy = total_models - active_models
    )
}
# refit
