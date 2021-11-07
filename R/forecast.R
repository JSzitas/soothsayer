
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

mult_theta <- function( formula, ... ) {
  form <- ~ season( method = c("multiplicative"))
  rlang::f_lhs(form) <- rlang::enexpr( formula )
  fable::THETA( !!form )
}
add_theta <- function( formula, ... ) {
  form <- ~ season( method = c("additive"))
  rlang::f_lhs(form) <- rlang::enexpr( formula )
  fable::THETA( !!form )
}

ts_train_test <- function( ts_tbl,
                           values_from = "value",
                           forecast_h = random_forecast_h,
                           ... ) {

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

  test <- purrr::map( seq_len( length(series)),
                                 ~ dplyr::filter( series[[.x]],
                                                  index > max(index) - forecast_h[[.x]]  ) %>%
                                   tsibble::build_tsibble(index = "index", key = "key")) %>%
    dplyr::bind_rows()

  return(list( train = train, test = test, values_from = values_from))
}

fit_models <- function( train,
                        models = list( ar = fable::AR),
                        values_from = "value") {
  model_set <- purrr::map( models, ~ .x( !!rlang::sym(values_from) )  )

  mdls <- train %>%
    fabletools::model( !!!model_set, .safely = TRUE )
  return( list( models = mdls))
}

forecast_models <- function( test,
                             fitted_models ) {
  fcst <- fabletools::forecast( fitted_models, new_data = test, times = 0 )
  return( list( forecasts = fcst) )
}
