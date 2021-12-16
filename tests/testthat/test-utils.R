test_that( "Failing if a condition is not met works", {
  expect_error(fail_if_cond(TRUE, "debug"), "debug")
})

test_that("Resolving model names works", {
  # take all models selected by either method
  expect_equal(  model_resolver( rule_models = c("ets","arima"),
                                 oracle_models = c("arima","ar"),
                                 which_model = "all",
                                 resolution = "both"
  ), c("ets","arima","ar"))
  # take first model selected by either method
  expect_equal(  model_resolver( rule_models = c("ets","arima"),
                                 oracle_models = c("arima","ar","ets"),
                                 which_model = "first",
                                 resolution = "both"
  ), c("ets"))
  # take all models selected by rules
  expect_equal(  model_resolver( rule_models = c("ets","arima"),
                                 oracle_models = c("prophet","theta"),
                                 which_model = "all",
                                 resolution = "rule"
  ), c("ets","arima"))
  # take first model selected by rules
  expect_equal(  model_resolver( rule_models = c("ets","arima"),
                                 oracle_models = c("prophet","theta"),
                                 which_model = "first",
                                 resolution = "rule"
  ), c("ets"))
  # take all models selected by oracle
  expect_equal(  model_resolver( rule_models = c("ets","arima"),
                                 oracle_models = c("prophet","theta"),
                                 which_model = "all",
                                 resolution = "oracle"
  ), c("prophet","theta"))
  # take first model selected by oracle
  expect_equal(  model_resolver( rule_models = c("ets","arima"),
                                 oracle_models = c("prophet","theta"),
                                 which_model = "first",
                                 resolution = "oracle"
  ), c("prophet"))
})

