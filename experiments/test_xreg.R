remove(list = ls())
pkgload::load_all()

ex_data <- tsibbledata::aus_livestock %>%
  as.data.frame() %>%
  dplyr::group_by(Month, Animal) %>%
  dplyr::summarise(count = sum(Count)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider( names_from = "Animal", values_from = "count" ) %>%
  tsibble::as_tsibble(index = "Month")

train <- dplyr::filter(ex_data, Month < tsibble::yearmonth("2017 Jan"))
test <- dplyr::filter( ex_data, Month > tsibble::yearmonth("2017 Jan") )

fabletools::model(
  train,
  ar = fable::AR(Lambs ~ order(0) + 1 + Sheep),
  soothsayer = soothsayer(Lambs ~ rules(
    arima ~ .length > 12,
    ar ~ TRUE
  ) +
    model_aliases(
      ar = fix_model_parameters(fable::AR, order(0:3) + 1 + Sheep),
                   arima = fable::ARIMA) +
    combiner(combiner_greedy_stacking) +
    Sheep )
) -> fitted


