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
# merge_distribution_forecasts <- function( dist_fcsts, weights = NULL ) {
#
#   if(is.null(weights)) {
#     weights <- rep(1, length(dist_fcsts))/length(dist_fcsts)
#   }
#   ind_var <- tsibble::index_var(dist_fcsts[[1]])
#   dates <- dist_fcsts[[1]][[ind_var]]
#
#   distrs <- purrr::map( dist_fcsts,  )
#
#
#
#
#   distributional::dist_mixture()
#
#
#
# }

#' @importFrom fabletools forecast
#' @export
forecast.soothsayer <- function( object,
                                 new_data = NULL,
                                 specials = NULL,
                                 bootstrap = FALSE,
                                 times = 100,
                                 ... ) {
  purrr::map( object[["models"]],
              function(.x) {
                fabletools::forecast(
                  .x[[1]],
                  new_data = new_data,
                  bootstrap = bootstrap,
                  times = times, ...)
              }
  )[[1]]
  # global_res_fcst <<- res
  # dplyr::bind_rows(res)#[[1]]
}
