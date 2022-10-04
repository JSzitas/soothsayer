remove(list = ls())
pkgload::load_all()

# ex_data <- tsibbledata::aus_livestock %>%
#   as.data.frame() %>%
#   dplyr::group_by(Month) %>%
#   dplyr::summarise(count = sum(Count)) %>%
#   dplyr::ungroup() %>%
#   tsibble::as_tsibble(index = "Month")

# random_oracle <- new_soothsayer_oracle(
#   oracle_name = "random_oracle",
#   predict = function(oracle, features) {
#     models <- c(
#       "ar",
#       "arima",
#       # "croston",
#       "ets",
#       "nnetar",
#       "rw",
#       "snaive",
#       "theta"
#     )
#
#     sample(models, 1)
#   }
# )
#
snaive_oracle <- new_soothsayer_oracle(
  oracle_name = "snaive_oracle",
  predict = function(oracle, features) {
    "snaive"
  }
)
#
#
df <- tsibbledata::aus_livestock
df <- dplyr::filter( df, State == State[1] & Animal %in% unique(Animal)[1:3] )
train <- dplyr::filter(df , Month <= tsibble::yearmonth("2017 Jan"))
test <- dplyr::filter(df, Month > tsibble::yearmonth("2017 Jan"))

fabletools::model(
  train,
  arima = fable::ARIMA(Count),
  ets = fable::ETS(Count)
) -> fitted

soothsayer <- soothsayer(Count ~ rules(
  arima ~ .length > 12,
  ar ~ TRUE,
  ets ~ .length > 15
) + oracle(snaive_oracle) +
  combiner(combiner_greedy_stacking) +
  model_aliases(arima = fable::ARIMA,
                ar = fable::AR,
                ets = fable::ETS)
)

soothsayer2 <- soothsayer(Count ~ rules(
  ets ~ .length > 15
) +
  model_aliases(ets = fable::ETS)
)


fabletools::model(
  train,
  arima = fable::ARIMA(Count),
  ets = fable::ETS(Count),
  soothsayer = soothsayer(Count ~ rules(
    arima ~ .length > 12,
    ar ~ TRUE,
    ets ~ .length > 15
  ) + oracle(snaive_oracle) +
    combiner(combiner_greedy_stacking) +
    model_aliases(arima = fable::ARIMA,
                  ar = fable::AR,
                  ets = fable::ETS))
) -> fitted2

add_soothsayer(fitted, soothsayer = soothsayer, sooth2 = soothsayer2) -> res
