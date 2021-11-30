remove(list = ls())
pkgload::load_all()
#
# readRDS("data/soothsayer_default_accuracy_tbl.rds") -> accuracy_tbl
# readRDS("data/soothsayer_default_feature_tbl.rds") -> features
#
#
# random_oracle <-  new_soothsayer_oracle( oracle_name = "random_oracle",
#                                          feature_data = c(1),
#                                          forecast_accuracies = c(1),
#                                          train = function( accuracy_tbl, features ) {
#                                            list( models = c("ar",
#                                                             "arima",
#                                                             "croston",
#                                                             "ets",
#                                                             "nnetar",
#                                                             "rw",
#                                                             "snaive",
#                                                             "theta")
#                                                  )
#                                          },
#                                          predict = function( oracle, features ) {
#                                            sample( oracle$models, 1 )
#                                          }
# )
#
# random_oracle <- fit( random_oracle )
#
# random_pred <- predict(random_oracle, accuracy_tbl[1,])
#
#
# ranger_oracle <- new_soothsayer_oracle( oracle_name = "ranger_oracle",
#                                         feature_data = features,
#                                         forecast_accuracies = accuracy_tbl,
#                                         train = function( accuracy_tbl, features ) {
#
#                                           df <- dplyr::select( accuracy_tbl,
#                                                                 tidyselect::all_of(c(".model","RMSE","key"))
#                                                                 ) %>%
#                                             dplyr::filter( !is.nan(RMSE)) %>%
#                                             dplyr::filter( RMSE != 0 ) %>%
#                                             dplyr::group_by( key ) %>%
#                                             dplyr::mutate( inv_RMSE = 1/RMSE ) %>%
#                                             dplyr::mutate( score = inv_RMSE/sum(inv_RMSE, na.rm = TRUE) ) %>%
#                                             dplyr::ungroup() %>%
#                                             dplyr::full_join( features,
#                                                               by = c("key" = "keys")) %>%
#                                             dplyr::select( -tidyselect::all_of( c("RMSE",
#                                                                                   "inv_RMSE"   ))) %>%
#                                             na.omit() %>%
#                                             dplyr::group_by(key) %>%
#                                             dplyr::mutate( rank = dplyr::dense_rank(score) ) %>%
#                                             dplyr::ungroup() %>%
#                                             dplyr::filter( rank == 1) %>%
#                                             dplyr::select( -tidyselect::all_of(c("score", "rank","key")) ) %>%
#                                             dplyr::rename("model" = ".model") %>%
#                                             dplyr::mutate( model = as.factor(model) )
#
#                                           ranger::ranger( data = df,
#                                                           num.trees = 1000,
#                                                           probability = TRUE,
#                                                           dependent.variable.name = "model" )
#                                         },
#                                         predict = function( oracle, features ) {
#
#                                           predictions <- predict( oracle, features )[["predictions"]]
#                                           colnames(predictions)[which.max(predictions)]
#                                         }
#                                       )
#
#
# ranger_oracle <- fit( ranger_oracle )
#
# preds <- predict(ranger_oracle, features[1,])




soothsayer_model <-
  soothsayer(Value ~ # rule(.period > 12 -> AR) +
  # rule(.length < 50 -> ETS) +
  # alternatively just
  rules(
    AR ~ .period > 12,
    ETS ~ .length < 50
  ) +
    # for readability?
    oracle(ETS, AR, ARIMA,
      fitted_oracle = some_oracle,
      min_certainty = 0.1,
      mix = FALSE
    ) +
    model_aliases(NULL) )

ex_data <- tsibbledata::aus_livestock %>%
  as.data.frame() %>%
  dplyr::group_by(Month) %>%
  dplyr::summarise(count = sum(Count)) %>%
  dplyr::ungroup() %>%
  tsibble::as_tsibble(index = "Month")

fabletools::model(
  ex_data,
  # fable::AR( count ),
  soothsayer(count ~ rules(
    ARIMA ~ .length > 12,
    # AR ~ TRUE,
    ETS ~ .length > 15
  ))
) -> fitted

# generated <- generate(fitted)
forecasted <- forecast(fitted)

distributional::dist_mixture(global_res_fcst[["ARIMA"]]$count,
                             global_res_fcst[["ETS"]]$count,
                             weights = c(0.5,0.5)) %>%
  distributional::variance()



# fcst <- forecast(fitted[[1]][[1]][["fit"]], h = 8)


