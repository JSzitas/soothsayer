remove(list = ls())
pkgload::load_all()

ex_data <- tsibbledata::aus_livestock %>%
  as.data.frame() %>%
  dplyr::group_by(Month, Animal) %>%
  dplyr::summarise(count = sum(Count)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider( names_from = "Animal", values_from = "count" ) %>%
  tsibble::as_tsibble(index = "Month")

train <- dplyr::filter(ex_data, Month <= tsibble::yearmonth("2017 Jan"))
test <- dplyr::filter( ex_data, Month > tsibble::yearmonth("2017 Jan") )

fabletools::model(
  train,
  arima = fable::ARIMA(Lambs),
  # ar = fable::AR(Lambs ~ order(0) + 1 + Sheep),
  soothsayer = soothsayer(Lambs ~ rules(
    arima ~ .length > 12#,
    # ets ~ TRUE,
    # ar ~ TRUE,
    # theta ~ TRUE
  ) +
    model_aliases(
      # ar = fix_model_parameters(fable::AR, order(0:3) + 1 + Sheep),
      ets = fable::ETS,
      arima = fable::ARIMA,
      theta = fable::THETA) +
    combiner(combiner_greedy_stacking) +
    Sheep )
) -> fitted

fcsts <- forecast( fitted, test )
gens <- generate( fitted, test, bootstrap = TRUE, times = 5 )


