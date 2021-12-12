#' @importFrom generics generate
#' @export
generate.soothsayer <- function (x, new_data = NULL, h = NULL, specials = NULL, times = 1, bootstrap = FALSE, seed = NULL,
                                 ...)
{
  weights <- x[["model_weights"]]
  names(weights) <- names(x[["models"]])
  generated_distrs <- purrr::imap( x[["models"]],
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
    dist %>%
    dplyr::mutate( .sim = .sim * weights[name]) %>%
      tsibble::as_tibble()
  }) %>%
    dplyr::bind_rows()
  dists %>%
    dplyr::group_by(.data$Month) %>%
    dplyr::summarise( .sim = sum(.data$.sim),
                      .rep = unique(.data$.rep)
    ) %>%
    tsibble::as_tsibble(index = "Month")

}
# to be fair, this is a method over a fable, but I do not want to write a generics for it
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
  fcsts <- purrr::map( object[["models"]],
              function(.x) {
                fabletools::forecast(
                  .x[[1]],
                  new_data = new_data,
                  bootstrap = bootstrap,
                  times = times, ...) %>%
                  get_distribution()
                }
  )
  # if we only have one model, dont worry about anything else :)
  if( length(object[["models"]]) == 1 ) {
    return(fcsts[[1]][[1]])
  }
  # otherwise, get weights
  weights <- object[["model_weights"]]
  # get forecast means
  fcst_means <- fcsts %>%
    purrr::map( ~mean(.x[[1]]) ) %>%
    dplyr::bind_cols() %>%
    as.matrix
  # and compute the final mean
  fcst_means <- c( fcst_means %*% weights )
  # also get forecast variances

  # I am quite unsure at this point that computing the variances is very sensible
  # for non-normal distributions its bad, but even for normal distributions... there is
  # no guarantee that the end result is normal. if its bimodal... then we are a bit
  # screwed either way. lets not do that.
  distributional::dist_degenerate( fcst_means )
}
