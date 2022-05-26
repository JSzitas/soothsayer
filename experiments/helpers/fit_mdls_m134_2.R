pkgload::load_all()
series <- qs::qread(paste0("nonstandard_data/M_all_2.qs"))

series <- series %>%
  dplyr::select( -period ) %>%
  tsibble::as_tsibble( index = "index", key = "key" )

res <- series %>%
  dplyr::group_by( key ) %>%
  dplyr::mutate( train = index < max( index) - 14 + 1) %>%
  dplyr::ungroup()

train <- res %>%
  dplyr::filter( train ) %>%
  dplyr::select(-train)
test <- res %>%
  dplyr::filter( !train ) %>%
  dplyr::select(-train)

fit_models <- function( train,
                        models = list( ar = fable::AR)) {

  values_from <- tsibble::measured_vars(train)
  model_set <- purrr::map( models, ~ .x( !!rlang::sym(values_from))  )

  fabletools::model(train, !!!model_set, .safely = TRUE )
}

progressr::handlers(global = TRUE)
future::plan("multisession")

fitted_mdls <- fit_models( train, list( arima = fable::ARIMA,
                                        snaive = fable::SNAIVE,
                                        rw = fable::RW,
                                        theta = fable::THETA,
                                        ets = fable::ETS,
                                        nnetar = fable::NNETAR,
                                        croston = fable::CROSTON,
                                        ar = fable::AR,
                                        ar1 = fix_model_parameters(fable::AR, order(1)),
                                        ar3 = fix_model_parameters(fable::AR, order(3)),
                                        arma11 = fix_model_parameters(fable::ARIMA, pdq(1,0,1)),
                                        arma31 = fix_model_parameters(fable::ARIMA, pdf(3,0,1)),
                                        bats = fable.tbats::BATS,
                                        tbats = fable.tbats::TBATS) )

qs::qsave(fitted_mdls, "mdls2.qs")

fcsts <- fabletools::forecast( fitted_mdls, h = 14, times = 0 )

qs::qsave( fcsts, "fcsts2.qs" )


