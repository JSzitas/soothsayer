
pkgload::load_all()
series <- qs::qread(paste0("nonstandard_data/M_all_2.qs"))

future::plan("multisession")

res <- soothsayer_forecaster( series,
                       models = list( arima = fable::ARIMA,
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
                                                  arma31 = fix_model_parameters(fable::ARIMA, pdf(3,0,1))
                                   ),
                                   forecast_h = random_forecast_h,
                                   save_forecast_experiment = TRUE,
                                   save_outfile = "h_random_part_2",
                                   save_folder = paste0("experiment_",
                                                        Sys.Date()
                                   ),
                                   save_n_threads = 12)
