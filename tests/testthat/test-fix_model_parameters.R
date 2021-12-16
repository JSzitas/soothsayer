# setup test data
ex_data <- tsibbledata::aus_livestock %>%
  as.data.frame() %>%
  dplyr::group_by(Month) %>%
  dplyr::summarise(count = sum(Count)) %>%
  dplyr::ungroup() %>%
  tsibble::as_tsibble(index = "Month")

train <- dplyr::filter(ex_data, Month < tsibble::yearmonth("2017 Jan"))
test <- dplyr::filter( ex_data, Month > tsibble::yearmonth("2017 Jan") )

test_that("Fixing parameters for a fable model works", {
  # create a fable model definition
  fable_ar <- fable::AR( count ~ order(0:3) )
  # create a fixed model definition
  new_fixed_ar <- fix_model_parameters( fable::AR, order(0:3) )
  fixed_ar <- new_fixed_ar( count )
  # fit both models and generate forecasts
  models <- fabletools::model( train, fixed_ar, fable_ar )
  forecasts <- fabletools::forecast(models, test)
  # get both forecasts - means, distributions, indices
  forecasts_fable <- dplyr::filter( forecasts, .model == "fable_ar" ) %>%
    dplyr::select(-.model)
  forecasts_fixed <- dplyr::filter( forecasts, .model == "fixed_ar" ) %>%
    dplyr::select(-.model)
  # compare - should be equal
  expect_equal(forecasts_fable, forecasts_fixed)

  # TEST 2 - with ets.
  # create a fable model definition
  fable_theta <- fable::THETA( count ~ season(method = "additive") )
  # create a fixed model definition
  new_fixed_theta <- fix_model_parameters( fable::THETA, season(method = "additive") )
  fixed_theta <- new_fixed_theta( count )
  # fit both models and generate forecasts
  models <- fabletools::model( train, fixed_theta, fable_theta )
  forecasts <- fabletools::forecast(models, test)
  # get both forecasts - means, distributions, indices
  forecasts_fable <- dplyr::filter( forecasts, .model == "fable_theta" ) %>%
    dplyr::select(-.model)
  forecasts_fixed <- dplyr::filter( forecasts, .model == "fixed_theta" ) %>%
    dplyr::select(-.model)
  # compare - should be equal
  expect_equal(forecasts_fable, forecasts_fixed)
})
