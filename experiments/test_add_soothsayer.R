remove(list = ls())
pkgload::load_all()

ex_data <- tsibbledata::aus_livestock %>%
  as.data.frame() %>%
  dplyr::group_by(Month) %>%
  dplyr::summarise(count = sum(Count)) %>%
  dplyr::ungroup() %>%
  tsibble::as_tsibble(index = "Month")

random_oracle <- new_soothsayer_oracle(
  oracle_name = "random_oracle",
  predict = function(oracle, features) {
    models <- c(
      "ar",
      "arima",
      # "croston",
      "ets",
      "nnetar",
      "rw",
      "snaive",
      "theta"
    )

    sample(models, 1)
  }
)

snaive_oracle <- new_soothsayer_oracle(
  oracle_name = "snaive_oracle",
  predict = function(oracle, features) {
    "snaive"
  }
)



train <- dplyr::filter(ex_data, Month < tsibble::yearmonth("2017 Jan"))
test <- dplyr::filter(ex_data, Month > tsibble::yearmonth("2017 Jan"))

fabletools::model(
  train,
  arima = fable::ARIMA(count),
  ets = fable::ETS(count)
) -> fitted


soothsayer <- soothsayer(count ~ rules(
  arima ~ .length > 12,
  ar ~ TRUE,
  ets ~ .length > 15
) + oracle(snaive_oracle) +
  combiner(combiner_greedy_stacking))

fabletools::model(
  train,
  arima = fable::ARIMA(count),
  ets = fable::ETS(count),
  soothsayer = soothsayer(count ~ rules(
    arima ~ .length > 12,
    ar ~ TRUE,
    ets ~ .length > 15
  ) + oracle(snaive_oracle) +
    combiner(combiner_greedy_stacking))
) -> fitted2

add_soothsayer(fitted, soothsayer = soothsayer) -> res


