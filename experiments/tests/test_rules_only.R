# test rule only soothsayer
remove(list = ls())
pkgload::load_all()

ex_data <- tsibbledata::aus_livestock %>%
  as.data.frame() %>%
  dplyr::group_by(Month) %>%
  dplyr::summarise(count = sum(Count)) %>%
  dplyr::ungroup() %>%
  tsibble::as_tsibble(index = "Month")


train <- dplyr::filter(ex_data, Month < tsibble::yearmonth("2017 Jan"))
test <- dplyr::filter(ex_data, Month > tsibble::yearmonth("2017 Jan"))


fabletools::model(
  train,
  arima = fable::ARIMA(count),
  ets = fable::ETS(count),
  soothsayer = soothsayer(count ~ rules(
    arima ~ .length > 12,
    ar ~ TRUE,
    ets ~ .length > 15
  ) +
    combiner(combiner_greedy_stacking))
) -> fitted



