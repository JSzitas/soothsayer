#' @importFrom generics generate
#' @export
generate.soothsayer <- function (x, new_data = NULL, h = NULL, specials = NULL, times = 1, bootstrap = FALSE, seed = NULL,
                                 ...)
{
  combinator <- specials$combinator[[1]]
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
  dplyr::bind_rows( purrr::map( generated_distrs,
                                dplyr::as_tibble ) ) %>%
    dplyr::group_by(.data$Month) %>%
    dplyr::summarise( .sim = combinator(.data$.sim),
                      .rep = unique(.data$.rep)
    ) %>%
    tsibble::as_tsibble(index = "Month")

}
merge_distribution_forecasts <- function( dists, weights = NULL ) {

  if(is.null(weights)) {
    weights <- rep(1, length(dists))/length(dists)
  }
  # note that since purrr::pmap_dist doesnt exist, we have to use the list version
  # and then index into the list... which is ugly, but it works
  merged_dists <- purrr::pmap(dists,
                              ~ distributional::dist_mixture(.x,
                                                             weights = weights)
                              )[[1]]
  return( merged_dists )
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
  # for now mix the distributions with equal weights
  merge_distribution_forecasts(fcsts, NULL)
}
