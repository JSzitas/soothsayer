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
merge_distribution_forecasts <- function( dists, weights = NULL ) {
  if( length(dists) == 1 ) {
    return(dists[[1]])
  }
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
  # weights <- combine_greedy_stacking( object[["models"]] )
  dsts <<- fcsts
  # for now mix the distributions with equal weights
  merge_distribution_forecasts(fcsts, weights = NULL )
}

# IDEA: pdqr to the rescue!
# # Create a list of pdqr-functions
# norm_list <- list(
#   as_d(dnorm), as_d(dnorm, mean = 2, sd = 0.25), as_d(dnorm, mean = 4, sd = 0.5)
# )
#
# # Form a mixture with custom weights
# norm_mix <- form_mix(norm_list, weights = c(0.6, 0.2, 0.2))
#
# # Compute 95% highest density region
# (norm_hdr <- summ_hdr(norm_mix, level = 0.95))
# #>          left      right
# #> 1 -1.82442072 2.53095750
# #> 2  3.19649819 4.79334429
#
# # Visualize
# plot(norm_mix, main = "95% highest density region for normal mixture")
# region_draw(norm_hdr)


