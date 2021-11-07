

# first_k <- M_all$key %>%
#   unique() %>%
#   .[1:30]
#
# M_30 <- M_all %>%
#   dplyr::filter(key %in% first_k)
#
#
# models <- M_all %>%
#   fabletools::model( fable::AR( value ),
#                      fable::ARIMA(value))

# an attempt to provide a parallelised version of fabletools::forecast
# ie intended to replace fabletools:::forecast.mdl_df with a parallelised call to underlying methods on indivdual series

forecast2 <- function (object, new_data = NULL, h = NULL, point_forecast = list(.mean = mean),
          ...)
{
  mdls <- fabletools::mable_vars(object)
  if (!is.null(h) && !is.null(new_data)) {
    rlang::warn("Input forecast horizon `h` will be ignored as `new_data` has been provided.")
    h <- NULL
  }
  if (!is.null(new_data)) {
    object <- fabletools:::bind_new_data(object, new_data)
  }

  res <- object %>%
    dplyr::group_by( key ) %>%
    dplyr::group_split()

  res <- furrr::future_map( res, function(model_row){
    fabletools::forecast( object = fabletools::as_mable(model_row,
                                          key = "key",
                                          model = tidyselect::all_of(mdls)),
                        new_data = object[["new_data"]],
                        h = h,
                        point_forecast = point_forecast, ... )
  }) %>%
    dplyr::bind_rows()

  return(res)
}

