#' @importFrom generics generate
#' @export
generate.soothsayer <- function (x, new_data = NULL, h = NULL, specials = NULL, times = 1, bootstrap = FALSE, seed = NULL,
                                 ...)
{
  weights <- x[["model_weights"]]
  names(weights) <- names(x[["model_fits"]])
  generated_distrs <- purrr::imap( x[["model_fits"]],
                                   function(model, name) {
                                     safe_gen <- purrr::possibly(generate,
                                                                 otherwise = data.frame( .sim = NA))
                                     dplyr::bind_cols(
                                       safe_gen(
                                         model[[1]],
                                         new_data = new_data,
                                         h = h,
                                         times = times,
                                         bootstrap = bootstrap,
                                         seed = seed,
                                         ...),
                                       model = name)
                                   })
  # check weight consistency - not all methods implement the generate() function
  valid_dists <- purrr::map_lgl( generated_distrs, ~ !all(is.na(.x[[".sim"]])) )

  if( !all(valid_dists) ) {
    warning(paste0("Generation failed for following models:\n",
                   paste0(names(x[["model_fits"]][!valid_dists]), collapse = ", "),
                   "\nThese models will be ignored when creating combined samples."
                   )
            )
  }
  # recompute weights
  weights <- weights[ valid_dists ]/sum(weights[valid_dists])
  generated_distrs <- generated_distrs[valid_dists]

  dists <- purrr::imap( generated_distrs, function(dist, name) {
    dists <- dplyr::mutate(dist, .sim = .data$.sim * weights[name])
    tsibble::as_tibble(dists)
  })
  # have to group by first column - which is, happily, the time index variable
  index_var <- as.name(names(dists[[1]])[1])
  dists <- dplyr::group_by( dplyr::bind_rows(dists), !!index_var, .data$.rep)
  dists <- dplyr::summarise( dists,
                             .sim = sum(.data$.sim),
                             .groups = "keep")
  tsibble::tsibble( dists, index = rlang::as_string(index_var), key = c(.data$.rep))
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
                  bootstrap = FALSE,
                  times = 0,
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

  tsibble::tibble( models = models,
                  weights = model_weights,
                  avg_residual = residual_mean )
}
#' @importFrom rlang .data
#' @importFrom generics glance
#' @export
glance.soothsayer <- function(x, ...) {
  x <- tidy(x)
  x <- dplyr::filter( x, .data$models != "all" )
  total_models <- nrow(x)
  active_models <- sum(x[["weights"]] > 0.01)
  max_weight <- max(x[["weights"]])

  tsibble::tibble(
    total_models = total_models,
    active_models = active_models,
    max_weight = max_weight,
    model_redundancy = total_models - active_models
    )
}
#' @importFrom generics refit
#' @export
refit.soothsayer <- function(x, new_data, specials = NULL, ... ) {
  train_soothsayer( new_data, specials, ... )
}
