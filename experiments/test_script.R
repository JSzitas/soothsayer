remove(list = ls())
pkgload::load_all()

# readRDS("data/soothsayer_default_accuracy_tbl.rds") -> accuracy_tbl
# readRDS("data/soothsayer_default_feature_tbl.rds") -> features


random_oracle <-  new_soothsayer_oracle( oracle_name = "random_oracle",
                                         predict = function( oracle, features ) {
                                           models = c("ar",
                                                      "arima",
                                                      # "croston",
                                                      "ets",
                                                      "nnetar",
                                                      "rw",
                                                      "snaive",
                                                      "theta")

                                           sample( models, 1 )
                                         }
)


random_pred <- predict(random_oracle)
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

train <- dplyr::filter(ex_data, Month < tsibble::yearmonth("2017 Jan"))
test <- dplyr::filter( ex_data, Month > tsibble::yearmonth("2017 Jan") )

fabletools::model(
  train,
  ar = fable::ARIMA( count ),
  soothsayer = soothsayer(count ~ rules(
    arima ~ .length > 12,
    ar ~ TRUE,
    ets ~ .length > 15
  ) + oracle(random_oracle) +
    combiner(combiner_greedy_stacking) ),
  soothsayer2 = soothsayer(count ~ rules(
    arima ~ .length > 12,
    ar ~ TRUE,
    ets ~ .length > 15
  ) + oracle(random_oracle) +
    combiner(combiner_lm))
) -> fitted

# generated <- generate(fitted)
forecasted <- forecast(fitted, new_data = test)
# library(fabletools)
# autoplot(forecasted, test)


