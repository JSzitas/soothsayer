
random_forecast_h <- function( x, min_h = 4, max_h = 12, ... ) {
  forecast_h <- min_h:max( min( length(x/2), max_h ), min_h)
  # use sample weights which are like, chisq or smth distributed
  sample( forecast_h, size = 1)
}
min_len_forecast_h <- function( x, min_h = 4 ) {
  forecast_h <- min_h:max( ceiling(length(x)*0.2), min_h )
  # use sample weights which are like, chisq or smth distributed
  sample( forecast_h, size = 1)
}
future::plan("multisession")


find_best_forecast <- function( ts_tbl,
                                models = list( #fable::ARIMA,
                                               # fable::THETA,
                                               # fable::ETS,
                                               # fable::NNETAR,
                                               # fable::CROSTON,
                                               # fable::SNAIVE,
                                               fable::AR),
                                values_from = "value",
                                forecast_h = min_len_forecast_h,
                                ... ) {

  model_set <- purrr::map( models, ~ .x( !!rlang::sym(values_from) )  )

  series <- ts_tbl %>%
    as.data.frame() %>%
    dplyr::group_by(!!!tsibble::key(ts_tbl)) %>%
    dplyr::group_split()

  unique_keys <- ts_tbl %>%
    as.data.frame() %>%
    dplyr::select( tsibble::key_vars(ts_tbl)) %>%
    dplyr::distinct() %>%
    unlist()

  if( is.function(forecast_h)) {
    forecast_h <- purrr::map_int( series, ~ forecast_h( .x[[values_from]] ) )
  }
  else{
    forecast_h <- rep(forecast_h, length(series))
  }

  train <- purrr::map( seq_len( length(series)),
                       ~ dplyr::filter( series[[.x]],
                                        index < max(index) - forecast_h[[.x]] + 1 ) %>%
                         tsibble::build_tsibble(index = "index", key = "key")) %>%
    dplyr::bind_rows()

  # series <- series %>%
  #   dplyr::bind_rows()

  res <- train %>%
    fabletools::model( !!! model_set ) #%>%
    # fabletools::forecast( new_data = ts_tbl, times = 0 )

  return(list( train, ts_tbl, res))
}

# forecasts <- forecast2( models[[3]], new_data = models[[2]]  )

