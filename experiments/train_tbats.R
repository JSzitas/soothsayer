pkgload::load_all()
# series <- qs::qread(paste0("nonstandard_data/M_all_1.qs"))
series <- qs::qread(paste0("nonstandard_data/M_all_2.qs"))

series <- series %>%
  # dplyr::bind_rows(series2) %>%
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

# progressr::handlers(global = TRUE)
future::plan("multisession")


# fitting various tbats models
# tbats specials
# function (trend = NULL, damped = NULL, box_cox = NULL, seasonal_periods = NULL,
#           arma_errors = TRUE, bias_adj = FALSE, bc_lower = 0, bc_higher = 1)

# train <- train[1000,]

progressr::with_progress(
{
fitted_mdls <- fabletools::model( train,
                                  tbats_trend = fable.tbats::TBATS( value ~
                                    parameters( trend = TRUE, damped = FALSE, box_cox = FALSE, arma_errors = FALSE)  ),
                                  tbats_trend_bc = fable.tbats::TBATS( value ~ parameters( trend = TRUE,
                                                                                        damped = FALSE,
                                                                                        box_cox = TRUE,
                                                                                        arma_errors = FALSE)  ),
                                  tbats_trend_bc_arma = fable.tbats::TBATS( value ~ parameters( trend = TRUE,
                                                                                        damped = FALSE,
                                                                                        box_cox = TRUE,
                                                                                        arma_errors = TRUE)  ),
                                  tbats_damp_trend = fable.tbats::TBATS( value ~
                                                                      parameters( trend = TRUE, damped = TRUE,
                                                                                  box_cox = FALSE, arma_errors = FALSE)  ),
                                  tbats_damp_trend_bc = fable.tbats::TBATS( value ~ parameters(
                                    trend = TRUE, damped = TRUE,
                                    box_cox = TRUE, arma_errors = FALSE)  ),
                                  tbats_damp_trend_arma = fable.tbats::TBATS( value ~ parameters(
                                    trend = TRUE, damped = TRUE,
                                    box_cox = FALSE, arma_errors = TRUE)  ),
                                  tbats_damp_trend_bc_arma = fable.tbats::TBATS( value ~ parameters(
                                    trend = TRUE, damped = TRUE,
                                    box_cox = TRUE, arma_errors = TRUE)  )
                                  )
})

qs::qsave(fitted_mdls, "tbats_mdls2.qs")

qs::qread( "tbats_mdls2.qs" ) -> mdls

progressr::with_progress({
  fcsts <- fabletools::forecast( mdls, h = 14, times = 0 )

})

qs::qsave( fcsts, "tbats_fcsts2.qs" )
