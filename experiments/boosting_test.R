remove(list = ls())
pkgload::load_all()

ex_data <- tsibbledata::aus_livestock %>%
  as.data.frame() %>%
  dplyr::group_by(Month, State) %>%
  dplyr::summarise(count = sum(Count)) %>%
  dplyr::ungroup() %>%
  tsibble::as_tsibble(index = "Month", key= "State")

train <- dplyr::filter(ex_data, Month < tsibble::yearmonth("2017 Jan"))
test <- dplyr::filter( ex_data, Month > tsibble::yearmonth("2017 Jan") )

fabletools::model(
  train,
  booster = soothsayer_booster( count ~  models( fable::AR( count ) %>%
                                                   fable::ARIMA( count ))
                                )#,
  # arima = fable::ARIMA(count),
  # ets = fable::ETS(count)
) -> fitted

# fcst <- forecast( fitted, new_data = test )
# gnrrt <- generate(fitted, h = 10)





