
pkgload::load_all()

readRDS("data/soothsayer_default_accuracy_tbl.rds") -> accuracy_tbl
readRDS("data/soothsayer_default_feature_tbl.rds") -> features


random_oracle <-  new_soothsayer_oracle( oracle_name = "random_oracle",
                                         feature_data = c(1),
                                         forecast_accuracies = c(1),
                                         train = function( accuracy_tbl, features ) {
                                           list( models = c("ar",
                                                            "arima",
                                                            "croston",
                                                            "ets",
                                                            "nnetar",
                                                            "rw",
                                                            "snaive",
                                                            "theta")
                                                 )
                                         },
                                         predict = function( oracle, features ) {
                                           sample( oracle$models, 1 )
                                         }
)

random_oracle <- fit( random_oracle )

predict(random_oracle, accuracy_tbl[1,])


# ranger_oracle <- new_soothsayer_oracle( oracle_name = "ranger_oracle",
#                                         feature_data = features,
#                                         forecast_accuracies = accuracy_tbl,
#                                         train = function( accuracy_tbl, features ) {
#                                           sample( c("ets","ar"), 1 )
#                                         },
#                                         predict = function( oracle, features ) {
#                                           sample( c("ets","ar"), 1 )
#                                         }
#                                       )


# ranger_oracle <- fit( ranger_oracle )

# predict(ranger_oracle, accuracy_tbl[1,])




# soothsayer_model <-
#   soothsayer(Value ~ # rule(.period > 12 -> AR) +
#   # rule(.length < 50 -> ETS) +
#   # alternatively just
#   rules(
#     AR ~ .period > 12,
#     ETS ~ .length < 50
#   ) +
#     # for readability?
#     oracle(ETS, AR, ARIMA,
#       fitted_oracle = some_oracle,
#       min_certainty = 0.1,
#       mix = FALSE
#     ) +
#     model_aliases(NULL) )
#
# ex_data <- tsibbledata::aus_livestock %>%
#   as.data.frame() %>%
#   dplyr::group_by(Month) %>%
#   dplyr::summarise(count = sum(Count)) %>%
#   dplyr::ungroup() %>%
#   # dplyr::mutate(key = "aggreg") %>%
#   tsibble::as_tsibble(index = "Month")#, key = "key")
#
#
# fabletools::model(
#   ex_data,
#   # fable::AR( count ),
#   soothsayer(count ~ rules(
#     AR ~ .length > 12,
#     ETS ~ .length > 15
#   ))
# ) -> fitted
